config_candidates <- c("00_config.R", file.path("01_Code", "Pipeline", "00_config.R"))
config_path <- config_candidates[file.exists(config_candidates)][1]
if (is.na(config_path)) stop("Could not locate 00_config.R")
source(config_path)

message("Running 04_backtest_singleasset.R ...")

data_bundle <- readRDS(file.path(PATH_DATA, "data_bundle.rds"))
signals <- readRDS(file.path(PATH_DATA, "signals.rds"))

simple_returns_xts <- data_bundle$simple_returns_xts
vix_xts <- data_bundle$prices_vix_xts

spy_returns <- as.numeric(simple_returns_xts[, "SPY"])
decision_dates <- signals$decision_dates
return_dates <- signals$return_dates
borrowing_rate_daily <- signals$borrowing_rate_daily

rolling_var_spy <- signals$rolling_var_spy
fsv_var_spy <- signals$fsv_var_spy
fsv_var_spy_train_proxy <- signals$fsv_var_spy_train_proxy
rebal_idx <- signals$rebalance_indices
n_decision <- length(decision_dates)

strategy_names <- c(
  benchmark = "SPY_Benchmark",
  naive = "Naive_MM_Realized22d",
  fsv = "FSV_MM_Forecast22d"
)

rebal_valid <- rebal_idx[
  is.finite(rolling_var_spy[rebal_idx]) &
    rolling_var_spy[rebal_idx] > 0 &
    is.finite(fsv_var_spy[rebal_idx]) &
    fsv_var_spy[rebal_idx] > 0 &
    decision_dates[rebal_idx] >= TEST_START_DATE
]

if (length(rebal_valid) < 3L) {
  stop("Insufficient valid rebalance dates for single-asset backtest.")
}

# Moreira-Muir scaling constant calibration on training data only.
rebal_all_idx <- seq(max(N_ROLL_SINGLE, N_ROLL_MULTI), n_decision, by = REBALANCE_EVERY_DAYS)
rebal_train_idx <- rebal_all_idx[decision_dates[rebal_all_idx] <= TRAIN_END_DATE]

calib_naive_idx <- rebal_train_idx[
  is.finite(rolling_var_spy[rebal_train_idx]) &
    rolling_var_spy[rebal_train_idx] > 0
]
calib_fsv_idx <- rebal_train_idx[
  is.finite(fsv_var_spy_train_proxy[rebal_train_idx]) &
    fsv_var_spy_train_proxy[rebal_train_idx] > 0
]

if (length(calib_naive_idx) < 12L) {
  stop("Not enough training observations to calibrate MM naive scaling constant.")
}

c_naive <- moreira_muir_scale_constant(rolling_var_spy[calib_naive_idx])
c_fsv <- moreira_muir_scale_constant(fsv_var_spy_train_proxy[calib_fsv_idx])
if (!is.finite(c_fsv) || c_fsv <= 0) {
  warning("FSV MM scaling constant could not be calibrated from training proxy; falling back to naive constant.")
  c_fsv <- c_naive
}

w_rebal_benchmark <- rep(1, length(rebal_valid))
w_rebal_naive <- clamp(c_naive / rolling_var_spy[rebal_valid], lower = 0, upper = LEVERAGE_CAP_SINGLE)
w_rebal_fsv <- clamp(c_fsv / fsv_var_spy[rebal_valid], lower = 0, upper = LEVERAGE_CAP_SINGLE)

analysis_idx <- seq(rebal_valid[1], n_decision)
analysis_dates <- decision_dates[analysis_idx]
ret_next <- spy_returns[analysis_idx + 1L]
ret_dates <- return_dates[analysis_idx]
borrow_next <- borrowing_rate_daily[analysis_idx]

expand_hold <- function(rebal_positions, rebal_weights, total_len) {
  out <- rep(NA_real_, total_len)
  for (j in seq_along(rebal_positions)) {
    s <- rebal_positions[j]
    e <- if (j < length(rebal_positions)) rebal_positions[j + 1L] - 1L else total_len
    out[s:e] <- rebal_weights[j]
  }
  out
}

rebal_pos <- match(rebal_valid, analysis_idx)
w_benchmark <- expand_hold(rebal_pos, w_rebal_benchmark, length(analysis_idx))
w_naive <- expand_hold(rebal_pos, w_rebal_naive, length(analysis_idx))
w_fsv <- expand_hold(rebal_pos, w_rebal_fsv, length(analysis_idx))

turnover_from_rebal <- function(rebal_positions, rebal_weights, total_len) {
  trn <- rep(0, total_len)
  if (length(rebal_positions) < 2L) return(trn)
  for (j in 2:length(rebal_positions)) {
    trn[rebal_positions[j]] <- abs(rebal_weights[j] - rebal_weights[j - 1L])
  }
  trn
}

turn_benchmark <- rep(0, length(analysis_idx))
turn_naive <- turnover_from_rebal(rebal_pos, w_rebal_naive, length(analysis_idx))
turn_fsv <- turnover_from_rebal(rebal_pos, w_rebal_fsv, length(analysis_idx))

gross_benchmark <- w_benchmark * ret_next
gross_naive <- w_naive * ret_next
gross_fsv <- w_fsv * ret_next

borrow_cost_benchmark <- pmax(0, abs(w_benchmark) - 1) * borrow_next
borrow_cost_naive <- pmax(0, abs(w_naive) - 1) * borrow_next
borrow_cost_fsv <- pmax(0, abs(w_fsv) - 1) * borrow_next

gross_after_funding_benchmark <- gross_benchmark - borrow_cost_benchmark
gross_after_funding_naive <- gross_naive - borrow_cost_naive
gross_after_funding_fsv <- gross_fsv - borrow_cost_fsv

single_metrics <- list()
single_returns_long <- list()

for (tc in TCOST_BPS) {
  net_benchmark <- calc_net_returns(gross_after_funding_benchmark, turn_benchmark, tc)
  net_naive <- calc_net_returns(gross_after_funding_naive, turn_naive, tc)
  net_fsv <- calc_net_returns(gross_after_funding_fsv, turn_fsv, tc)

  single_metrics[[as.character(tc)]] <- rbind(
    cbind(strategy = strategy_names[["benchmark"]], tcost_bps = tc, draw_strategy_metrics(net_benchmark, turn_benchmark)),
    cbind(strategy = strategy_names[["naive"]], tcost_bps = tc, draw_strategy_metrics(net_naive, turn_naive)),
    cbind(strategy = strategy_names[["fsv"]], tcost_bps = tc, draw_strategy_metrics(net_fsv, turn_fsv))
  )

  single_returns_long[[as.character(tc)]] <- rbind(
    data.frame(date = ret_dates, strategy = strategy_names[["benchmark"]], tcost_bps = tc, ret = net_benchmark),
    data.frame(date = ret_dates, strategy = strategy_names[["naive"]], tcost_bps = tc, ret = net_naive),
    data.frame(date = ret_dates, strategy = strategy_names[["fsv"]], tcost_bps = tc, ret = net_fsv)
  )
}

single_metrics_df <- do.call(rbind, single_metrics)
single_returns_df <- do.call(rbind, single_returns_long)
row.names(single_metrics_df) <- NULL
row.names(single_returns_df) <- NULL

weights_df <- data.frame(
  date = analysis_dates,
  SPY_Benchmark = w_benchmark,
  Naive_MM_Realized22d = w_naive,
  FSV_MM_Forecast22d = w_fsv
)

crisis_weight_single <- do.call(
  rbind,
  lapply(names(CRISIS_WINDOWS), function(nm) {
    wnd <- CRISIS_WINDOWS[[nm]]
    idx <- weights_df$date >= wnd[1] & weights_df$date <= wnd[2]
    data.frame(
      period = nm,
      SPY_Benchmark = if (any(idx)) mean(weights_df$SPY_Benchmark[idx], na.rm = TRUE) else NA_real_,
      Naive_MM_Realized22d = if (any(idx)) mean(weights_df$Naive_MM_Realized22d[idx], na.rm = TRUE) else NA_real_,
      FSV_MM_Forecast22d = if (any(idx)) mean(weights_df$FSV_MM_Forecast22d[idx], na.rm = TRUE) else NA_real_
    )
  })
)

exposure_summary <- rbind(
  data.frame(
    strategy = strategy_names[["benchmark"]],
    mean_exposure = mean(w_benchmark, na.rm = TRUE),
    median_exposure = stats::median(w_benchmark, na.rm = TRUE),
    p95_exposure = unname(stats::quantile(w_benchmark, 0.95, na.rm = TRUE)),
    max_exposure = max(w_benchmark, na.rm = TRUE),
    pct_at_cap = mean(w_benchmark >= LEVERAGE_CAP_SINGLE - 1e-8, na.rm = TRUE)
  ),
  data.frame(
    strategy = strategy_names[["naive"]],
    mean_exposure = mean(w_naive, na.rm = TRUE),
    median_exposure = stats::median(w_naive, na.rm = TRUE),
    p95_exposure = unname(stats::quantile(w_naive, 0.95, na.rm = TRUE)),
    max_exposure = max(w_naive, na.rm = TRUE),
    pct_at_cap = mean(w_naive >= LEVERAGE_CAP_SINGLE - 1e-8, na.rm = TRUE)
  ),
  data.frame(
    strategy = strategy_names[["fsv"]],
    mean_exposure = mean(w_fsv, na.rm = TRUE),
    median_exposure = stats::median(w_fsv, na.rm = TRUE),
    p95_exposure = unname(stats::quantile(w_fsv, 0.95, na.rm = TRUE)),
    max_exposure = max(w_fsv, na.rm = TRUE),
    pct_at_cap = mean(w_fsv >= LEVERAGE_CAP_SINGLE - 1e-8, na.rm = TRUE)
  )
)

fsv_var_rebal <- fsv_var_spy[rebal_valid]
fsv_var_daily <- expand_hold(rebal_pos, fsv_var_rebal, length(analysis_idx))
rolling_var_rebal <- rolling_var_spy[rebal_valid]
rolling_var_daily <- expand_hold(rebal_pos, rolling_var_rebal, length(analysis_idx))
vix_aligned <- as.numeric(vix_xts[as.character(analysis_dates), 1]) / 100
vol_overlay_df <- data.frame(
  date = analysis_dates,
  fsv_conditional_vol = sqrt(fsv_var_daily) * sqrt(TRADING_DAYS),
  # Use rebalance-and-hold signal values (22-day schedule) for like-for-like comparison.
  rolling_vol_22d = sqrt(rolling_var_daily) * sqrt(TRADING_DAYS),
  vix = vix_aligned
)

single_obj <- list(
  returns_long = single_returns_df,
  metrics = single_metrics_df,
  weights = weights_df,
  crisis_weights = crisis_weight_single,
  exposure_summary = exposure_summary,
  vol_overlay = vol_overlay_df,
  scaling_constants = data.frame(
    strategy = c(strategy_names[["naive"]], strategy_names[["fsv"]]),
    constant_c = c(c_naive, c_fsv),
    stringsAsFactors = FALSE
  ),
  borrowing_cost = data.frame(
    date = ret_dates,
    SPY_Benchmark = borrow_cost_benchmark,
    Naive_MM_Realized22d = borrow_cost_naive,
    FSV_MM_Forecast22d = borrow_cost_fsv
  ),
  rebalance_dates = decision_dates[rebal_valid]
)

saveRDS(single_obj, file.path(PATH_OBJECT, "04_singleasset_backtest.rds"))
write_csv_safe(single_metrics_df, file.path(PATH_TABLE, "04_singleasset_metrics.csv"))
write_csv_safe(crisis_weight_single, file.path(PATH_TABLE, "04_singleasset_crisis_weights.csv"))
write_csv_safe(exposure_summary, file.path(PATH_TABLE, "04_singleasset_exposure_summary.csv"))
write_csv_safe(single_obj$scaling_constants, file.path(PATH_TABLE, "04_singleasset_scaling_constants.csv"))

message("04_backtest_singleasset.R complete.")
