config_candidates <- c("00_config.R", file.path("01_Code", "Pipeline", "00_config.R"))
config_path <- config_candidates[file.exists(config_candidates)][1]
if (is.na(config_path)) stop("Could not locate 00_config.R")
source(config_path)

message("Running 04_backtest_multiasset.R ...")

data_bundle <- readRDS(file.path(PATH_DATA, "data_bundle.rds"))
signals <- readRDS(file.path(PATH_DATA, "signals.rds"))

simple_returns_xts <- data_bundle$simple_returns_xts[, TICKERS]
simple_returns_mat <- as.matrix(simple_returns_xts)

sigma_roll <- signals$sigma_roll_array
sigma_fsv <- signals$sigma_fsv_array
decision_dates <- signals$decision_dates
return_dates <- signals$return_dates
borrowing_rate_daily <- signals$borrowing_rate_daily
rebal_idx <- signals$rebalance_indices
n_decision <- length(decision_dates)
n_assets <- length(TICKERS)

rebal_valid <- rebal_idx[
  decision_dates[rebal_idx] >= TEST_START_DATE &
    vapply(rebal_idx, function(t) all(is.finite(sigma_roll[, , t])), logical(1)) &
    vapply(rebal_idx, function(t) all(is.finite(sigma_fsv[, , t])), logical(1)) &
    is.finite(borrowing_rate_daily[rebal_idx])
]
if (length(rebal_valid) < 3L) {
  stop("Insufficient valid rebalance dates for multi-asset test backtest.")
}

strategy_defs_budget <- list(
  Rolling_MeanVar_22d = list(cov_source = "roll", allocation = "bucket_meanvar"),
  FSV_MeanVar_22d = list(cov_source = "fsv", allocation = "bucket_meanvar")
)

strategy_defs_minvar <- list(
  Rolling_MinVar_22d = list(cov_source = "roll", allocation = "global_minvar"),
  FSV_MinVar_22d = list(cov_source = "fsv", allocation = "global_minvar")
)

compute_weights_rebalance <- function(strategy_name, def, budget_vector, rebal_indices) {
  w_mat <- matrix(NA_real_, nrow = length(rebal_indices), ncol = n_assets)
  mu_mat <- matrix(NA_real_, nrow = length(rebal_indices), ncol = n_assets)
  colnames(w_mat) <- TICKERS
  colnames(mu_mat) <- TICKERS

  prev_w <- setNames(rep(1 / n_assets, n_assets), TICKERS)

  for (i in seq_along(rebal_indices)) {
    t_idx <- rebal_indices[i]
    S <- if (def$cov_source == "roll") sigma_roll[, , t_idx] else sigma_fsv[, , t_idx]
    rownames(S) <- TICKERS
    colnames(S) <- TICKERS
    S <- force_psd(S)

    # Causal expected return estimate: expanding mean up to rebalance date t.
    mu_hat <- colMeans(simple_returns_mat[1:t_idx, , drop = FALSE], na.rm = TRUE)
    names(mu_hat) <- TICKERS
    mu_hat[!is.finite(mu_hat)] <- 0

    w_i <- tryCatch({
      if (identical(def$allocation, "bucket_meanvar")) {
        build_bucketed_weights_meanvar(
          S = S,
          mu = mu_hat,
          budget_vector = budget_vector,
          bucket_weights = BUCKET_WEIGHTS,
          risk_aversion = MEANVAR_RISK_AVERSION
        )
      } else if (identical(def$allocation, "global_minvar")) {
        # No bucket constraints here: global long-only minimum-variance portfolio.
        optimize_min_variance_weights(S)
      } else {
        stop("Unknown allocation mode: ", def$allocation)
      }
    }, error = function(e) prev_w)

    if (any(!is.finite(w_i)) || abs(sum(w_i) - 1) > 1e-6) {
      w_i <- prev_w
    }

    w_mat[i, ] <- w_i[TICKERS]
    mu_mat[i, ] <- mu_hat[TICKERS]
    prev_w <- w_i[TICKERS]
  }
  list(weights = w_mat, mu = mu_mat)
}

expand_hold_matrix <- function(rebal_positions, rebal_weights_mat, total_len) {
  out <- matrix(NA_real_, nrow = total_len, ncol = ncol(rebal_weights_mat))
  colnames(out) <- colnames(rebal_weights_mat)
  for (j in seq_along(rebal_positions)) {
    s <- rebal_positions[j]
    e <- if (j < length(rebal_positions)) rebal_positions[j + 1L] - 1L else total_len
    out[s:e, ] <- matrix(rebal_weights_mat[j, ], nrow = e - s + 1L, ncol = ncol(rebal_weights_mat), byrow = TRUE)
  }
  out
}

turnover_from_rebal_matrix <- function(rebal_positions, rebal_weights_mat, total_len) {
  trn <- rep(0, total_len)
  if (nrow(rebal_weights_mat) < 2L) return(trn)
  for (j in 2:nrow(rebal_weights_mat)) {
    trn[rebal_positions[j]] <- 0.5 * sum(abs(rebal_weights_mat[j, ] - rebal_weights_mat[j - 1L, ]))
  }
  trn
}

analysis_idx <- seq(rebal_valid[1], n_decision)
analysis_dates <- decision_dates[analysis_idx]
ret_dates <- return_dates[analysis_idx]
ret_next_mat <- simple_returns_mat[analysis_idx + 1L, , drop = FALSE]
borrow_next <- borrowing_rate_daily[analysis_idx]
rebal_pos <- match(rebal_valid, analysis_idx)

multi_metrics_list <- list()
multi_returns_list <- list()
weight_objects <- list()
equity_weights_list <- list()
crisis_equity_list <- list()

run_strategy_backtest <- function(strategy_name, def, budget_name, budget_vec = NULL) {
  rebal_data <- compute_weights_rebalance(strategy_name, def, budget_vec, rebal_valid)
  w_rebal <- rebal_data$weights
  mu_rebal <- rebal_data$mu
  w_daily <- expand_hold_matrix(rebal_pos, w_rebal, length(analysis_idx))

  gross_ret <- rowSums(w_daily * ret_next_mat)
  gross_exposure <- rowSums(abs(w_daily))
  borrow_cost <- pmax(0, gross_exposure - 1) * borrow_next
  gross_after_funding <- gross_ret - borrow_cost

  turnover <- turnover_from_rebal_matrix(rebal_pos, w_rebal, length(analysis_idx))

  eq_weight <- rowSums(w_daily[, names(BUCKET_WEIGHTS$equity), drop = FALSE])
  eq_weight_df <- data.frame(
    date = analysis_dates,
    budget = budget_name,
    strategy = strategy_name,
    equity_weight = eq_weight
  )
  equity_weights_list[[paste(budget_name, strategy_name, sep = "_")]] <<- eq_weight_df

  crisis_eq <- do.call(
    rbind,
    lapply(names(CRISIS_WINDOWS), function(nm) {
      wnd <- CRISIS_WINDOWS[[nm]]
      idx <- eq_weight_df$date >= wnd[1] & eq_weight_df$date <= wnd[2]
      data.frame(
        budget = budget_name,
        strategy = strategy_name,
        period = nm,
        mean_equity_weight = if (any(idx)) mean(eq_weight_df$equity_weight[idx], na.rm = TRUE) else NA_real_
      )
    })
  )
  crisis_equity_list[[paste(budget_name, strategy_name, sep = "_")]] <<- crisis_eq

  for (tc in TCOST_BPS) {
    net_ret <- calc_net_returns(gross_after_funding, turnover, tc)
    metrics_row <- cbind(
      budget = budget_name,
      strategy = strategy_name,
      tcost_bps = tc,
      draw_strategy_metrics(net_ret, turnover)
    )
    multi_metrics_list[[paste(budget_name, strategy_name, tc, sep = "_")]] <<- metrics_row

    multi_returns_list[[paste(budget_name, strategy_name, tc, sep = "_")]] <<- data.frame(
      date = ret_dates,
      budget = budget_name,
      strategy = strategy_name,
      tcost_bps = tc,
      ret = net_ret
    )
  }

  weight_objects[[paste(budget_name, strategy_name, sep = "_")]] <<- list(
    date = ret_dates,
    budget = budget_name,
    strategy = strategy_name,
    turnover = turnover,
    borrowing_cost = borrow_cost,
    rebalance_dates = decision_dates[rebal_valid],
    rebalance_mu = mu_rebal,
    rebalance_weights = w_rebal,
    weights = w_daily
  )
}

for (budget_name in names(BUDGETS)) {
  budget_vec <- BUDGETS[[budget_name]]
  for (strategy_name in names(strategy_defs_budget)) {
    def <- strategy_defs_budget[[strategy_name]]
    run_strategy_backtest(strategy_name, def, budget_name, budget_vec = budget_vec)
  }
}

# Additional extension: global minimum-variance portfolio (no class-budget constraints).
for (strategy_name in names(strategy_defs_minvar)) {
  def <- strategy_defs_minvar[[strategy_name]]
  run_strategy_backtest(strategy_name, def, budget_name = "global_minvar", budget_vec = NULL)
}

multi_metrics_df <- do.call(rbind, multi_metrics_list)
multi_returns_df <- do.call(rbind, multi_returns_list)
equity_weights_df <- do.call(rbind, equity_weights_list)
crisis_equity_df <- do.call(rbind, crisis_equity_list)

row.names(multi_metrics_df) <- NULL
row.names(multi_returns_df) <- NULL
row.names(equity_weights_df) <- NULL
row.names(crisis_equity_df) <- NULL

multi_obj <- list(
  returns_long = multi_returns_df,
  metrics = multi_metrics_df,
  weight_objects = weight_objects,
  equity_weights = equity_weights_df,
  crisis_equity_weights = crisis_equity_df
)

saveRDS(multi_obj, file.path(PATH_OBJECT, "04_multiasset_backtest.rds"))
write_csv_safe(multi_metrics_df, file.path(PATH_TABLE, "04_multiasset_metrics.csv"))
write_csv_safe(crisis_equity_df, file.path(PATH_TABLE, "04_multiasset_crisis_equity_weights.csv"))

message("04_backtest_multiasset.R complete.")
