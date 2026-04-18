options(stringsAsFactors = FALSE)

# ---------------------------
# Project root and path setup
# ---------------------------
detect_project_root <- function(start_dir = getwd()) {
  current <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)

  repeat {
    has_structure <- all(file.exists(
      file.path(current, "01_Code"),
      file.path(current, "02_Data"),
      file.path(current, "03_Results")
    ))
    if (has_structure) return(current)

    parent <- dirname(current)
    if (identical(parent, current)) {
      stop(
        "Could not detect project root containing 01_Code, 02_Data, and 03_Results. ",
        "Please run from within the project directory."
      )
    }
    current <- parent
  }
}

PATH_ROOT <- detect_project_root()
PATH_CODE <- file.path(PATH_ROOT, "01_Code", "Pipeline")
PATH_DATA <- file.path(PATH_ROOT, "02_Data")
PATH_RESULTS <- file.path(PATH_ROOT, "03_Results")
PATH_CHART <- file.path(PATH_RESULTS, "Charts")
PATH_TABLE <- file.path(PATH_RESULTS, "Tables")
PATH_OBJECT <- file.path(PATH_RESULTS, "Objects")

invisible(lapply(
  list(PATH_DATA, PATH_RESULTS, PATH_CHART, PATH_TABLE, PATH_OBJECT),
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
))

# ---------------------------
# Required packages
# ---------------------------
REQUIRED_PACKAGES <- c(
  "quantmod", "factorstochvol", "ggplot2", "patchwork",
  "reshape2", "lubridate", "xts", "roll", "coda", "scales"
)

require_packages <- function(pkgs) {
  missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0L) {
    stop(
      "Missing required package(s): ",
      paste(missing_pkgs, collapse = ", "),
      ". Install them before running the pipeline."
    )
  }
}

require_packages(REQUIRED_PACKAGES)

# ---------------------------
# Study design constants
# ---------------------------
TICKERS <- c("SPY", "QQQ", "IWM", "EFA", "EEM", "TLT", "SHY", "LQD", "HYG", "GLD")
TICKERS_WITH_VIX <- c(TICKERS, "^VIX")

# Order used for factorstochvol identification anchoring:
# SPY -> Factor 1, TLT -> Factor 2, HYG -> Factor 3.
TICKERS_FSV <- c("SPY", "TLT", "HYG", "QQQ", "IWM", "EFA", "EEM", "SHY", "LQD", "GLD")

BUCKET_WEIGHTS <- list(
  equity = c(SPY = 0.40, QQQ = 0.20, IWM = 0.20, EFA = 0.10, EEM = 0.10),
  bonds = c(TLT = 0.30, SHY = 0.30, LQD = 0.25, HYG = 0.15),
  gold = c(GLD = 1.00)
)

BUDGETS <- list(
  pf1 = c(equity = 0.45, bonds = 0.45, gold = 0.10),
  pf2 = c(equity = 0.60, bonds = 0.40, gold = 0.00)
)

CRISIS_WINDOWS <- list(
  gfc = c(as.Date("2007-10-01"), as.Date("2009-06-30")),
  covid = c(as.Date("2020-02-01"), as.Date("2020-06-30")),
  rate_shock_2022 = c(as.Date("2022-01-01"), as.Date("2022-12-31"))
)

N_FACTORS <- 3L
get_env_int <- function(name, default) {
  raw <- Sys.getenv(name, unset = "")
  if (identical(raw, "")) return(as.integer(default))
  val <- suppressWarnings(as.integer(raw))
  if (is.na(val) || val <= 0L) {
    warning("Ignoring invalid integer env var ", name, "='", raw, "'. Using default ", default, ".")
    return(as.integer(default))
  }
  val
}

# Optional runtime overrides for faster pilot runs:
# MCMC_DRAWS_OVERRIDE, MCMC_BURNIN_OVERRIDE, MCMC_THIN_OVERRIDE
MCMC_DRAWS <- get_env_int("MCMC_DRAWS_OVERRIDE", 50000L)
MCMC_BURNIN <- get_env_int("MCMC_BURNIN_OVERRIDE", 10000L)
MCMC_THIN <- get_env_int("MCMC_THIN_OVERRIDE", 1L)
if (MCMC_BURNIN >= MCMC_DRAWS) {
  stop("MCMC_BURNIN must be smaller than MCMC_DRAWS. Got draws=", MCMC_DRAWS, ", burnin=", MCMC_BURNIN, ".")
}
N_SCENARIOS <- MCMC_DRAWS - MCMC_BURNIN

N_ROLL_SINGLE <- 22L
N_ROLL_MULTI <- 22L
REBALANCE_EVERY_DAYS <- 22L
FSV_FORECAST_HORIZON_DAYS <- 22L
ROLLING_RATIO_WINDOW <- 252L

TARGET_ANNUAL_VOL_SINGLE <- 0.15
LEVERAGE_CAP_SINGLE <- 2.0
LEVERAGE_SPREAD_ANNUAL <- 0.003
RISKFREE_PROXY_ASSET <- "SHY"
MEANVAR_RISK_AVERSION <- 10

TCOST_BPS <- c(0, 5)
TRADING_DAYS <- 252
ES_ALPHA <- 0.05

START_DATE <- as.Date("2006-01-03")
END_DATE <- Sys.Date()
TRAIN_END_DATE <- as.Date("2018-12-31")
TEST_START_DATE <- as.Date("2019-01-01")
if (TEST_START_DATE <= TRAIN_END_DATE) {
  stop("TEST_START_DATE must be after TRAIN_END_DATE.")
}

# ---------------------------
# Utility helpers
# ---------------------------
clamp <- function(x, lower = -Inf, upper = Inf) pmin(pmax(x, lower), upper)

softmax <- function(z) {
  z_shift <- z - max(z)
  p <- exp(z_shift)
  p / sum(p)
}

force_psd <- function(S, min_eig = 1e-8) {
  dn <- dimnames(S)
  # Guard: 1x1 matrix -- diag(length-1 vector) in R creates a 0x0 identity,
  # not a 1x1 diagonal, so the eigendecomposition path would crash.
  if (ncol(S) == 1L) {
    S_psd <- matrix(max(S[1L, 1L], min_eig), 1L, 1L, dimnames = dn)
    return(S_psd)
  }
  S <- (S + t(S)) / 2
  eig <- eigen(S, symmetric = TRUE)
  eig$values[eig$values < min_eig] <- min_eig
  S_psd <- eig$vectors %*% diag(eig$values) %*% t(eig$vectors)
  S_psd <- (S_psd + t(S_psd)) / 2
  # Restore dimnames: matrix multiply strips them.
  dimnames(S_psd) <- dn
  S_psd
}

safe_cov <- function(x) {
  if (is.null(dim(x)) || nrow(x) < 2L) {
    p <- if (is.null(dim(x))) 1L else ncol(x)
    out <- matrix(NA_real_, nrow = p, ncol = p)
    return(out)
  }
  stats::cov(x)
}

rolling_covariance_array <- function(returns_matrix, window) {
  n_t <- nrow(returns_matrix)
  n_a <- ncol(returns_matrix)
  out <- array(NA_real_, dim = c(n_a, n_a, n_t - 1L))

  for (t in seq_len(n_t - 1L)) {
    if (t < window) next
    x <- returns_matrix[(t - window + 1L):t, , drop = FALSE]
    out[, , t] <- safe_cov(x)
  }
  out
}

rolling_variance_vector <- function(x, window) {
  n <- length(x)
  out <- rep(NA_real_, n - 1L)
  for (t in seq_len(n - 1L)) {
    if (t < window) next
    out[t] <- stats::var(x[(t - window + 1L):t])
  }
  out
}

rolling_realized_variance_vector <- function(x, window) {
  # Moreira-Muir style realized variance proxy from past daily returns.
  n <- length(x)
  out <- rep(NA_real_, n - 1L)
  for (t in seq_len(n - 1L)) {
    if (t < window) next
    rw <- x[(t - window + 1L):t]
    out[t] <- mean(rw^2, na.rm = TRUE)
  }
  out
}

moreira_muir_scale_constant <- function(var_hat) {
  x <- var_hat[is.finite(var_hat) & var_hat > 0]
  if (length(x) == 0L) return(NA_real_)
  # Moreira-Muir style scaling constant: average managed exposure close to 1.
  1 / mean(1 / x)
}

annualized_return_geometric <- function(r, scale = TRADING_DAYS) {
  r <- r[is.finite(r)]
  if (length(r) == 0L) return(NA_real_)
  prod(1 + r)^(scale / length(r)) - 1
}

annualized_volatility <- function(r, scale = TRADING_DAYS) {
  r <- r[is.finite(r)]
  if (length(r) < 2L) return(NA_real_)
  stats::sd(r) * sqrt(scale)
}

return_vol_ratio <- function(r, scale = TRADING_DAYS) {
  ann_ret <- annualized_return_geometric(r, scale = scale)
  ann_vol <- annualized_volatility(r, scale = scale)
  if (!is.finite(ann_vol) || ann_vol <= 0) return(NA_real_)
  ann_ret / ann_vol
}

max_drawdown <- function(r) {
  r <- r[is.finite(r)]
  if (length(r) == 0L) return(NA_real_)
  wealth <- cumprod(1 + r)
  drawdown <- wealth / cummax(wealth) - 1
  min(drawdown, na.rm = TRUE)
}

expected_shortfall <- function(r, alpha = ES_ALPHA) {
  r <- r[is.finite(r)]
  if (length(r) == 0L) return(NA_real_)
  cutoff <- stats::quantile(r, probs = alpha, na.rm = TRUE, names = FALSE)
  tail_vals <- r[r <= cutoff]
  if (length(tail_vals) == 0L) return(NA_real_)
  mean(tail_vals)
}

annualized_turnover <- function(turnover_daily, scale = TRADING_DAYS) {
  x <- turnover_daily[is.finite(turnover_daily)]
  if (length(x) == 0L) return(NA_real_)
  sum(x) * scale / length(x)
}

calc_borrowing_rate_daily <- function(rf_proxy_daily, spread_annual = LEVERAGE_SPREAD_ANNUAL, scale = TRADING_DAYS) {
  # DEVIATION: proxy financing rate uses SHY daily simple return + 30 bps annual spread.
  # Floor at zero to avoid negative borrowing costs from noisy daily proxy moves.
  pmax(0, rf_proxy_daily + spread_annual / scale)
}

risk_contrib_variance <- function(w, S) {
  mrc <- as.numeric(S %*% w)
  w * mrc
}

optimize_budget_weights <- function(S, method = c("invvol", "risk_parity"), alpha = ES_ALPHA) {
  method <- match.arg(method)
  S <- force_psd(S)
  n <- ncol(S)
  asset_names <- colnames(S)
  if (is.null(asset_names)) asset_names <- paste0("Asset", seq_len(n))

  if (n == 1L) {
    out <- 1
    names(out) <- asset_names
    return(out)
  }

  if (method == "invvol") {
    iv <- 1 / sqrt(pmax(diag(S), 1e-12))
    w <- iv / sum(iv)
    names(w) <- asset_names
    return(w)
  }

  objective <- function(z) {
    w <- softmax(z)
    rc <- risk_contrib_variance(w, S)
    rc <- pmax(rc, 1e-12)
    rc_share <- rc / sum(rc)
    target <- rep(1 / n, n)
    sum((rc_share - target)^2)
  }

  opt <- stats::optim(
    par = rep(0, n),
    fn = objective,
    method = "L-BFGS-B",
    lower = rep(-20, n),
    upper = rep(20, n),
    control = list(maxit = 500L)
  )
  w <- softmax(opt$par)
  names(w) <- asset_names
  w
}

build_bucketed_weights <- function(S, budget_vector, bucket_weights, method, alpha = ES_ALPHA) {
  all_assets <- colnames(S)
  w_full <- setNames(rep(0, length(all_assets)), all_assets)

  for (bucket_name in names(budget_vector)) {
    budget_val <- unname(budget_vector[[bucket_name]])
    if (budget_val <= 0) next

    assets <- names(bucket_weights[[bucket_name]])
    assets <- intersect(assets, all_assets)
    if (length(assets) == 0L) next

    S_sub <- S[assets, assets, drop = FALSE]
    w_sub <- optimize_budget_weights(S_sub, method = method, alpha = alpha)
    w_full[assets] <- budget_val * w_sub[assets]
  }

  if (sum(w_full) > 0) {
    w_full <- w_full / sum(w_full)
  }
  w_full
}

optimize_mean_variance_weights <- function(S, mu, risk_aversion = MEANVAR_RISK_AVERSION) {
  S <- force_psd(S)
  n <- ncol(S)
  asset_names <- colnames(S)
  if (is.null(asset_names)) asset_names <- paste0("Asset", seq_len(n))

  if (n == 1L) {
    out <- 1
    names(out) <- asset_names
    return(out)
  }

  mu <- as.numeric(mu)
  if (length(mu) != n) stop("mu length must match covariance dimension.")
  mu[!is.finite(mu)] <- 0

  objective <- function(z) {
    w <- softmax(z)
    ret_term <- sum(w * mu)
    risk_term <- as.numeric(t(w) %*% S %*% w)
    -(ret_term - 0.5 * risk_aversion * risk_term)
  }

  opt <- stats::optim(
    par = rep(0, n),
    fn = objective,
    method = "L-BFGS-B",
    lower = rep(-20, n),
    upper = rep(20, n),
    control = list(maxit = 500L)
  )
  w <- softmax(opt$par)
  names(w) <- asset_names
  w
}

optimize_min_variance_weights <- function(S) {
  S <- force_psd(S)
  n <- ncol(S)
  asset_names <- colnames(S)
  if (is.null(asset_names)) asset_names <- paste0("Asset", seq_len(n))

  if (n == 1L) {
    out <- 1
    names(out) <- asset_names
    return(out)
  }

  objective <- function(z) {
    w <- softmax(z)
    as.numeric(t(w) %*% S %*% w)
  }

  opt <- stats::optim(
    par = rep(0, n),
    fn = objective,
    method = "L-BFGS-B",
    lower = rep(-20, n),
    upper = rep(20, n),
    control = list(maxit = 500L)
  )
  w <- softmax(opt$par)
  names(w) <- asset_names
  w
}

build_bucketed_weights_meanvar <- function(S, mu, budget_vector, bucket_weights, risk_aversion = MEANVAR_RISK_AVERSION) {
  all_assets <- colnames(S)
  w_full <- setNames(rep(0, length(all_assets)), all_assets)

  for (bucket_name in names(budget_vector)) {
    budget_val <- unname(budget_vector[[bucket_name]])
    if (budget_val <= 0) next

    assets <- names(bucket_weights[[bucket_name]])
    assets <- intersect(assets, all_assets)
    if (length(assets) == 0L) next

    S_sub <- S[assets, assets, drop = FALSE]
    mu_sub <- mu[assets]
    w_sub <- optimize_mean_variance_weights(S_sub, mu_sub, risk_aversion = risk_aversion)
    w_full[assets] <- budget_val * w_sub[assets]
  }

  if (sum(w_full) > 0) w_full <- w_full / sum(w_full)
  w_full
}

compute_crisis_equity_weight <- function(weight_df, equity_assets, crisis_windows = CRISIS_WINDOWS) {
  out <- lapply(names(crisis_windows), function(nm) {
    wnd <- crisis_windows[[nm]]
    idx <- weight_df$date >= wnd[1] & weight_df$date <= wnd[2]
    eq_w <- rowSums(weight_df[idx, equity_assets, drop = FALSE], na.rm = TRUE)
    data.frame(
      period = nm,
      mean_equity_weight = if (all(!is.finite(eq_w))) NA_real_ else mean(eq_w, na.rm = TRUE)
    )
  })
  do.call(rbind, out)
}

draw_strategy_metrics <- function(strategy_returns, turnover_daily) {
  data.frame(
    annualized_return = annualized_return_geometric(strategy_returns),
    annualized_volatility = annualized_volatility(strategy_returns),
    return_volatility_ratio = return_vol_ratio(strategy_returns),
    max_drawdown = max_drawdown(strategy_returns),
    realized_es_5pct = expected_shortfall(strategy_returns, alpha = 0.05),
    annualized_turnover_one_way = annualized_turnover(turnover_daily),
    stringsAsFactors = FALSE
  )
}

calc_net_returns <- function(gross_returns, turnover_daily, tcost_bps) {
  gross_returns - (tcost_bps / 10000) * turnover_daily
}

rolling_return_vol_ratio <- function(r, window = ROLLING_RATIO_WINDOW) {
  out <- rep(NA_real_, length(r))
  if (length(r) < window) return(out)

  for (i in seq.int(window, length(r))) {
    rw <- r[(i - window + 1L):i]
    out[i] <- return_vol_ratio(rw)
  }
  out
}

write_csv_safe <- function(df, path) {
  utils::write.csv(df, path, row.names = FALSE)
  invisible(path)
}
