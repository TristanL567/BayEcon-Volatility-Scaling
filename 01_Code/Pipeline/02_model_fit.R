config_candidates <- c("00_config.R", file.path("01_Code", "Pipeline", "00_config.R"))
config_path <- config_candidates[file.exists(config_candidates)][1]
if (is.na(config_path)) stop("Could not locate 00_config.R")
source(config_path)

message("Running 02_model_fit.R ...")

suppressPackageStartupMessages({
  library(factorstochvol)
})

data_bundle <- readRDS(file.path(PATH_DATA, "data_bundle.rds"))
log_returns_xts_all <- data_bundle$log_returns_xts

# Reorder assets to anchor lower-triangular identification:
# SPY (Factor 1), TLT (Factor 2), HYG (Factor 3).
log_returns_xts_all <- log_returns_xts_all[, TICKERS_FSV]
all_dates <- as.Date(zoo::index(log_returns_xts_all))

train_idx <- which(all_dates <= TRAIN_END_DATE)
if (length(train_idx) < 500L) {
  stop("Insufficient training observations up to TRAIN_END_DATE: ", TRAIN_END_DATE)
}
log_returns_xts_train <- log_returns_xts_all[train_idx, ]
log_returns_mat_train <- as.matrix(log_returns_xts_train)
log_returns_demeaned_train <- sweep(log_returns_mat_train, 2, colMeans(log_returns_mat_train, na.rm = TRUE), "-")

train_last_date <- max(as.Date(zoo::index(log_returns_xts_train)))
message("Training sample end date used for FSV fit: ", train_last_date)

set.seed(1234)

fsv_formals <- names(formals(factorstochvol::fsvsample))
default_iw <- formals(factorstochvol::fsvsample)[["interweaving"]]
interweaving_arg <- if (is.numeric(default_iw)) 4L else "deep"

fsv_args <- list(
  factors = N_FACTORS,
  draws = MCMC_DRAWS,
  burnin = MCMC_BURNIN,
  thin = MCMC_THIN,
  interweaving = interweaving_arg,
  zeromean = TRUE
)

if ("y" %in% fsv_formals) {
  fsv_args$y <- log_returns_demeaned_train
} else if ("data" %in% fsv_formals) {
  fsv_args$data <- log_returns_demeaned_train
} else {
  stop("fsvsample() signature not recognized: neither 'y' nor 'data' argument found.")
}
fsv_args <- fsv_args[names(fsv_args) %in% fsv_formals]

message("Fitting factorstochvol on training sample only. This may take substantial time.")
fit <- do.call(factorstochvol::fsvsample, fsv_args)

# Version-robust extraction:
facload_draws <- fit[["facload"]]
if (is.null(facload_draws)) {
  stop("Could not extract factor loadings from fit object (fit[['facload']] missing).")
}
lambda_mean <- if (length(dim(facload_draws)) == 3L) {
  apply(facload_draws, c(1, 2), mean)
} else {
  facload_draws
}

runningstore <- fit[["runningstore"]]
if (is.list(runningstore) && !is.null(runningstore[["logvar"]])) {
  # runningstore$logvar has dims [T, m+r, moments], with moments: mean/sd
  lv_mean <- t(runningstore[["logvar"]][, , 1, drop = FALSE][, , 1])
  lv_semantics <- "log_variance"
} else {
  stop("Could not extract latent volatility path from fit object (runningstore$logvar missing).")
}

if (is.list(runningstore) && !is.null(runningstore[["fac"]])) {
  factor_mean <- runningstore[["fac"]][, , 1, drop = FALSE][, , 1]
} else if (!is.null(fit[["fac"]])) {
  fac_draws <- fit[["fac"]]
  factor_mean <- if (length(dim(fac_draws)) == 3L) {
    apply(fac_draws, c(1, 2), mean)
  } else {
    fac_draws
  }
} else {
  factor_mean <- matrix(NA_real_, nrow = N_FACTORS, ncol = ncol(lv_mean))
}

rownames(lambda_mean) <- TICKERS_FSV
colnames(lambda_mean) <- paste0("Factor", seq_len(ncol(lambda_mean)))

sv_series_names <- c(TICKERS_FSV, paste0("Factor", seq_len(N_FACTORS)))
rownames(lv_mean) <- sv_series_names
colnames(lv_mean) <- as.character(zoo::index(log_returns_xts_train))
if (!is.null(dim(factor_mean)) && ncol(factor_mean) == ncol(log_returns_xts_train)) {
  rownames(factor_mean) <- paste0("Factor", seq_len(nrow(factor_mean)))
  colnames(factor_mean) <- as.character(zoo::index(log_returns_xts_train))
}

# DEVIATION: AR(1) SV parameters are estimated via OLS on posterior-mean log-variance
# paths for tractability, instead of full Bayesian propagation of parameter uncertainty.
h_mean <- if (identical(lv_semantics, "log_variance")) lv_mean else 2 * lv_mean
n_series <- nrow(h_mean)
n_t <- ncol(h_mean)

mu_hat <- rep(NA_real_, n_series)
phi_hat <- rep(NA_real_, n_series)
sigma_hat <- rep(NA_real_, n_series)

for (j in seq_len(n_series)) {
  y <- as.numeric(h_mean[j, 2:n_t])
  x <- as.numeric(h_mean[j, 1:(n_t - 1L)])
  fit_ols <- stats::lm(y ~ x)

  beta <- stats::coef(fit_ols)
  phi_j <- clamp(unname(beta[2]), -0.999, 0.999)
  c_j <- unname(beta[1])
  mu_j <- c_j / (1 - phi_j)
  sigma_j <- stats::sd(stats::residuals(fit_ols), na.rm = TRUE)

  mu_hat[j] <- mu_j
  phi_hat[j] <- phi_j
  sigma_hat[j] <- sigma_j
}

names(mu_hat) <- sv_series_names
names(phi_hat) <- sv_series_names
names(sigma_hat) <- sv_series_names
h_last_train <- as.numeric(h_mean[, ncol(h_mean)])
names(h_last_train) <- sv_series_names

ar1_params <- data.frame(
  series = sv_series_names,
  mu = mu_hat,
  phi = phi_hat,
  sigma = sigma_hat,
  stringsAsFactors = FALSE
)

model_summary <- list(
  tickers_fsv = TICKERS_FSV,
  n_factors = N_FACTORS,
  train_end_date = train_last_date,
  test_start_date = TEST_START_DATE,
  estimation_mode = "initial_train_fit_reference_only",
  lambda_mean = lambda_mean,
  lv_mean = lv_mean,
  factor_mean = factor_mean,
  lv_semantics = lv_semantics,
  h_last_train_log_variance = h_last_train,
  ar1_mu = mu_hat,
  ar1_phi = phi_hat,
  ar1_sigma = sigma_hat,
  mcmc_settings = list(
    draws = MCMC_DRAWS,
    burnin = MCMC_BURNIN,
    thin = MCMC_THIN,
    interweaving = interweaving_arg
  ),
  caveat_notes = c(
    "CAVEAT: This object stores the initial training-only fit for traceability.",
    "CAVEAT: Expanding-window refits used for test-period signals are produced in 03_signals.R."
  ),
  deviation_notes = c(
    "DEVIATION: AR(1) SV parameters estimated by OLS on posterior-mean log-variance paths."
  )
)

saveRDS(fit, file.path(PATH_DATA, "fsv_fit_train.rds"))
saveRDS(fit, file.path(PATH_DATA, "fsv_fit_full.rds")) # backward-compatible filename
saveRDS(model_summary, file.path(PATH_DATA, "fsv_model_summary.rds"))
write_csv_safe(ar1_params, file.path(PATH_TABLE, "02_fsv_ar1_params.csv"))

message("02_model_fit.R complete.")
