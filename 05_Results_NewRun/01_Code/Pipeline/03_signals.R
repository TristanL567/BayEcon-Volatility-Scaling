config_candidates <- c("00_config.R", file.path("01_Code", "Pipeline", "00_config.R"))
config_path <- config_candidates[file.exists(config_candidates)][1]
if (is.na(config_path)) stop("Could not locate 00_config.R")
source(config_path)

message("Running 03_signals.R ...")

suppressPackageStartupMessages({
  library(factorstochvol)
})

data_bundle <- readRDS(file.path(PATH_DATA, "data_bundle.rds"))
model_summary <- readRDS(file.path(PATH_DATA, "fsv_model_summary.rds"))

simple_returns_xts <- data_bundle$simple_returns_xts[, TICKERS]
simple_returns_mat <- as.matrix(simple_returns_xts)
log_returns_xts_all <- data_bundle$log_returns_xts[, TICKERS_FSV]
log_returns_mat_all <- as.matrix(log_returns_xts_all)

n_t <- nrow(simple_returns_mat)
n_decision <- n_t - 1L
decision_dates <- as.Date(zoo::index(simple_returns_xts)[1:n_decision])
return_dates <- as.Date(zoo::index(simple_returns_xts)[2:n_t])

train_end_date <- as.Date(model_summary$train_end_date)
if (is.na(train_end_date)) train_end_date <- TRAIN_END_DATE

rolling_var_spy <- rolling_realized_variance_vector(simple_returns_mat[, "SPY"], window = N_ROLL_SINGLE)
sigma_roll_array <- rolling_covariance_array(simple_returns_mat, window = N_ROLL_MULTI)
dimnames(sigma_roll_array) <- list(TICKERS, TICKERS, as.character(decision_dates))

sigma_fsv_array <- array(NA_real_, dim = c(length(TICKERS), length(TICKERS), n_decision))
dimnames(sigma_fsv_array) <- list(TICKERS, TICKERS, as.character(decision_dates))
fsv_var_spy <- rep(NA_real_, n_decision)

rf_proxy_next <- as.numeric(simple_returns_xts[, RISKFREE_PROXY_ASSET])[2:n_t]
borrowing_rate_daily <- calc_borrowing_rate_daily(rf_proxy_next)

rebal_start_idx <- which(decision_dates >= TEST_START_DATE)[1]
if (is.na(rebal_start_idx)) {
  stop("No decision dates on or after TEST_START_DATE.")
}
rebal_idx_raw <- seq(rebal_start_idx, n_decision, by = REBALANCE_EVERY_DAYS)
min_hist <- max(N_ROLL_SINGLE, N_ROLL_MULTI)
rebal_idx <- rebal_idx_raw[rebal_idx_raw >= min_hist]
if (length(rebal_idx) == 0L) {
  stop("No rebalance dates satisfy lookback requirements.")
}
# Optional smoke-test override to cap number of expanding-window refits.
max_refits_override_raw <- Sys.getenv("MAX_REBAL_REFITS_OVERRIDE", unset = "")
if (!identical(max_refits_override_raw, "")) {
  max_refits_override <- suppressWarnings(as.integer(max_refits_override_raw))
  if (!is.na(max_refits_override) && max_refits_override > 0L) {
    rebal_idx <- rebal_idx[seq_len(min(length(rebal_idx), max_refits_override))]
  } else {
    warning("Ignoring invalid MAX_REBAL_REFITS_OVERRIDE='", max_refits_override_raw, "'.")
  }
}

fsv_formals <- names(formals(factorstochvol::fsvsample))
default_iw <- formals(factorstochvol::fsvsample)[["interweaving"]]
interweaving_arg <- if (is.numeric(default_iw)) 4L else "deep"

fit_args_base <- list(
  factors = N_FACTORS,
  draws = MCMC_DRAWS,
  burnin = MCMC_BURNIN,
  thin = MCMC_THIN,
  interweaving = interweaving_arg,
  zeromean = TRUE
)
fit_args_base <- fit_args_base[names(fit_args_base) %in% fsv_formals]

extract_fsv_core <- function(fit_obj, end_idx) {
  facload_draws <- fit_obj[["facload"]]
  if (is.null(facload_draws)) stop("facload draws missing in fit object.")
  lambda_mean <- if (length(dim(facload_draws)) == 3L) {
    apply(facload_draws, c(1, 2), mean)
  } else {
    facload_draws
  }
  rownames(lambda_mean) <- TICKERS_FSV
  colnames(lambda_mean) <- paste0("Factor", seq_len(ncol(lambda_mean)))

  runningstore <- fit_obj[["runningstore"]]
  if (!is.list(runningstore) || is.null(runningstore[["logvar"]])) {
    stop("runningstore$logvar missing in fit object.")
  }
  h_mean <- t(runningstore[["logvar"]][, , 1, drop = FALSE][, , 1])
  rownames(h_mean) <- c(TICKERS_FSV, paste0("Factor", seq_len(N_FACTORS)))

  n_series <- nrow(h_mean)
  mu_hat <- rep(NA_real_, n_series)
  phi_hat <- rep(NA_real_, n_series)
  sigma_hat <- rep(NA_real_, n_series)

  for (j in seq_len(n_series)) {
    y <- as.numeric(h_mean[j, 2:end_idx])
    x <- as.numeric(h_mean[j, 1:(end_idx - 1L)])
    fit_ols <- stats::lm(y ~ x)
    beta <- stats::coef(fit_ols)
    phi_j <- clamp(unname(beta[2]), -0.999, 0.999)
    c_j <- unname(beta[1])
    mu_hat[j] <- c_j / (1 - phi_j)
    sigma_hat[j] <- stats::sd(stats::residuals(fit_ols), na.rm = TRUE)
    phi_hat[j] <- phi_j
  }
  names(mu_hat) <- rownames(h_mean)
  names(phi_hat) <- rownames(h_mean)
  names(sigma_hat) <- rownames(h_mean)

  list(
    lambda_mean = lambda_mean,
    mu = mu_hat,
    phi = phi_hat,
    sigma = sigma_hat,
    h_last = as.numeric(h_mean[, end_idx]),
    series_names = rownames(h_mean)
  )
}

forecast_sigma_components <- function(core, horizon_days) {
  lambda_mean <- core$lambda_mean
  mu <- core$mu
  phi <- core$phi
  sigma <- core$sigma
  h_state <- core$h_last
  names(h_state) <- core$series_names

  m <- length(TICKERS_FSV)
  r <- N_FACTORS
  idio_idx <- seq_len(m)
  factor_idx <- m + seq_len(r)

  sigma_sum <- matrix(0, nrow = m, ncol = m)
  rownames(sigma_sum) <- TICKERS_FSV
  colnames(sigma_sum) <- TICKERS_FSV
  sigma_t1 <- NULL

  for (k in seq_len(horizon_days)) {
    h_state <- mu + phi * (h_state - mu)
    h_for_var <- h_state + 0.5 * sigma^2
    Vf <- diag(exp(h_for_var[factor_idx]), nrow = r)
    Ve <- diag(exp(h_for_var[idio_idx]), nrow = m)
    sigma_k <- lambda_mean %*% Vf %*% t(lambda_mean) + Ve
    sigma_k <- force_psd(sigma_k)
    if (k == 1L) sigma_t1 <- sigma_k
    sigma_sum <- sigma_sum + sigma_k
  }
  list(
    sigma_t1 = sigma_t1,
    sigma_horizon_avg = sigma_sum / horizon_days
  )
}

perm_fsv_to_main <- match(TICKERS, TICKERS_FSV)
refit_log <- list()

# Training-period FSV SPY variance proxy from the initial training fit.
# This is used only to calibrate Moreira-Muir scaling constants on training data.
fsv_var_spy_train_proxy <- rep(NA_real_, n_decision)
lv_mean_train <- model_summary$lv_mean
if (!is.null(lv_mean_train) && ncol(lv_mean_train) > 1L) {
  h_mean_train <- if (identical(model_summary$lv_semantics, "log_variance")) lv_mean_train else 2 * lv_mean_train
  train_series_names <- rownames(h_mean_train)

  core_train <- list(
    lambda_mean = model_summary$lambda_mean,
    mu = model_summary$ar1_mu[train_series_names],
    phi = model_summary$ar1_phi[train_series_names],
    sigma = model_summary$ar1_sigma[train_series_names],
    series_names = train_series_names
  )

  train_col_map <- match(as.character(decision_dates), colnames(h_mean_train))
  train_decision_idx <- which(decision_dates <= train_end_date & !is.na(train_col_map))

  for (t_idx in train_decision_idx) {
    core_t <- core_train
    core_t$h_last <- as.numeric(h_mean_train[, train_col_map[t_idx]])
    names(core_t$h_last) <- core_t$series_names

    sigma_components_train <- forecast_sigma_components(core_t, horizon_days = FSV_FORECAST_HORIZON_DAYS)
    sigma_h_train_fsv_order <- force_psd(sigma_components_train$sigma_horizon_avg)
    sigma_h_train_main <- sigma_h_train_fsv_order[perm_fsv_to_main, perm_fsv_to_main, drop = FALSE]
    rownames(sigma_h_train_main) <- TICKERS
    colnames(sigma_h_train_main) <- TICKERS

    fsv_var_spy_train_proxy[t_idx] <- sigma_h_train_main["SPY", "SPY"]
  }
}

fsv_var_spy <- fsv_var_spy_train_proxy

n_cores <- as.integer(Sys.getenv("N_PARALLEL_CORES", unset = "4"))
omp_threads <- max(1L, floor(parallel::detectCores() / n_cores))
message(sprintf("Parallelising refits: %d workers, %d OMP thread(s) each", n_cores, omp_threads))

run_one_refit <- function(i) {
  Sys.setenv(OMP_NUM_THREADS = omp_threads)
  t_idx <- rebal_idx[i]
  d <- decision_dates[t_idx]
  if (d <= train_end_date) return(NULL)

  message(sprintf("Expanding-window refit %d/%d at decision date %s", i, length(rebal_idx), d))

  fit_window <- log_returns_mat_all[1:t_idx, , drop = FALSE]
  fit_window_demeaned <- sweep(fit_window, 2, colMeans(fit_window, na.rm = TRUE), "-")

  fit_args <- fit_args_base
  if ("y" %in% fsv_formals) {
    fit_args$y <- fit_window_demeaned
  } else if ("data" %in% fsv_formals) {
    fit_args$data <- fit_window_demeaned
  } else {
    stop("fsvsample() signature not recognized: neither 'y' nor 'data' argument found.")
  }

  core <- tryCatch({
    fit_obj <- do.call(factorstochvol::fsvsample, fit_args)
    extract_fsv_core(fit_obj, end_idx = nrow(fit_window_demeaned))
  }, error = function(e) {
    warning("FSV refit failed at decision date ", d, ": ", conditionMessage(e))
    NULL
  })
  if (is.null(core)) return(NULL)

  sigma_components <- forecast_sigma_components(core, horizon_days = FSV_FORECAST_HORIZON_DAYS)
  sigma_t1_fsv_order <- force_psd(sigma_components$sigma_t1)
  sigma_h_fsv_order <- force_psd(sigma_components$sigma_horizon_avg)

  sigma_t1_main <- sigma_t1_fsv_order[perm_fsv_to_main, perm_fsv_to_main, drop = FALSE]
  rownames(sigma_t1_main) <- TICKERS
  colnames(sigma_t1_main) <- TICKERS

  sigma_h_main <- sigma_h_fsv_order[perm_fsv_to_main, perm_fsv_to_main, drop = FALSE]
  rownames(sigma_h_main) <- TICKERS
  colnames(sigma_h_main) <- TICKERS

  list(
    t_idx        = t_idx,
    sigma_t1_main = sigma_t1_main,
    fsv_var_spy_val = sigma_h_main["SPY", "SPY"],
    refit_log_entry = data.frame(
      rebalance_index = t_idx,
      decision_date   = d,
      train_window_n  = nrow(fit_window_demeaned),
      stringsAsFactors = FALSE
    )
  )
}

refit_results <- parallel::mclapply(
  seq_along(rebal_idx),
  run_one_refit,
  mc.cores       = n_cores,
  mc.preschedule = FALSE
)

for (res in refit_results) {
  if (is.null(res)) next
  sigma_fsv_array[, , res$t_idx] <- res$sigma_t1_main
  fsv_var_spy[res$t_idx]         <- res$fsv_var_spy_val
  refit_log[[length(refit_log) + 1L]] <- res$refit_log_entry
}

refit_log_df <- if (length(refit_log) == 0L) {
  data.frame(rebalance_index = integer(0), decision_date = as.Date(character(0)), train_window_n = integer(0))
} else {
  do.call(rbind, refit_log[order(sapply(refit_log, `[[`, "rebalance_index"))])
}

signals <- list(
  decision_dates = decision_dates,
  return_dates = return_dates,
  rebalance_indices = rebal_idx,
  rebalance_dates = decision_dates[rebal_idx],
  sigma_fsv_array = sigma_fsv_array,
  sigma_roll_array = sigma_roll_array,
  rolling_var_spy = rolling_var_spy,
  fsv_var_spy = fsv_var_spy,
  fsv_var_spy_train_proxy = fsv_var_spy_train_proxy,
  borrowing_rate_daily = borrowing_rate_daily,
  train_end_date = train_end_date,
  test_start_date = TEST_START_DATE,
  rebalance_every_days = REBALANCE_EVERY_DAYS,
  fsv_forecast_horizon_days = FSV_FORECAST_HORIZON_DAYS,
  refit_log = refit_log_df,
  caveats = c(
    "CAVEAT: Expanding-window refit uses only data available up to each rebalance date; no future leakage.",
    "DEVIATION: For Part 2, sigma_fsv_array stores the one-step-ahead (t+1) covariance forecast at rebalance dates.",
    "DEVIATION: For Part 1, fsv_var_spy stores the 22-day horizon-average SPY variance forecast at rebalance dates.",
    "DEVIATION: Training-period FSV variance proxy for MM scaling calibration comes from the initial training fit object.",
    "DEVIATION: test financing rate uses SHY daily return + 30 bps annual spread, floored at zero."
  )
)

saveRDS(signals, file.path(PATH_DATA, "signals.rds"))
write_csv_safe(refit_log_df, file.path(PATH_TABLE, "03_fsv_refit_log.csv"))

message("03_signals.R complete.")
