config_candidates <- c("00_config.R", file.path("01_Code", "Pipeline", "00_config.R"))
config_path <- config_candidates[file.exists(config_candidates)][1]
if (is.na(config_path)) stop("Could not locate 00_config.R")
source(config_path)

message("Running 05_results.R ...")

suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
  library(reshape2)
  library(scales)
})


signals <- readRDS(file.path(PATH_DATA, "signals.rds"))
single_obj <- readRDS(file.path(PATH_OBJECT, "04_singleasset_backtest.rds"))
multi_obj <- readRDS(file.path(PATH_OBJECT, "04_multiasset_backtest.rds"))

single_returns <- single_obj$returns_long
multi_returns <- multi_obj$returns_long

single_metrics <- single_obj$metrics
multi_metrics <- multi_obj$metrics

single_returns <- single_returns[order(single_returns$tcost_bps, single_returns$strategy, single_returns$date), ]
multi_returns <- multi_returns[order(multi_returns$budget, multi_returns$tcost_bps, multi_returns$strategy, multi_returns$date), ]

single_returns$cum_ret <- ave(
  single_returns$ret,
  single_returns$strategy,
  single_returns$tcost_bps,
  FUN = function(x) cumprod(1 + x) - 1
)
single_returns$cum_wealth <- ave(
  single_returns$ret,
  single_returns$strategy,
  single_returns$tcost_bps,
  FUN = function(x) cumprod(1 + x)
)

multi_returns$cum_ret <- ave(
  multi_returns$ret,
  multi_returns$budget,
  multi_returns$strategy,
  multi_returns$tcost_bps,
  FUN = function(x) cumprod(1 + x) - 1
)

multi_returns_mv <- subset(
  multi_returns,
  strategy %in% c("Rolling_MeanVar_22d", "FSV_MeanVar_22d")
)
multi_returns_minvar <- subset(
  multi_returns,
  strategy %in% c("Rolling_MinVar_22d", "FSV_MinVar_22d")
)


# ──────────────────────────────────────────────────────────────────────────────
# Branding palette
# ──────────────────────────────────────────────────────────────────────────────
PAL <- c(
  blue   = "#004890",
  orange = "#f37021",
  green  = "#21a68a",
  red    = "#b22222",
  grey   = "#9ba7c0"
)

# ──────────────────────────────────────────────────────────────────────────────
# Shared theme
# ──────────────────────────────────────────────────────────────────────────────
theme_bayes <- function(base_size = 14) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      # Titles
      plot.title    = ggplot2::element_text(face = "bold", size = base_size + 4, hjust = 0),
      plot.subtitle = ggplot2::element_text(size = base_size,     hjust = 0, colour = "grey35"),

      # Axis text
      axis.title.x  = ggplot2::element_text(face = "bold", size = base_size + 1),
      axis.title.y  = ggplot2::element_text(face = "bold", size = base_size + 1),
      axis.text.x   = ggplot2::element_text(face = "bold", size = base_size - 1),
      axis.text.y   = ggplot2::element_text(face = "bold", size = base_size - 1),

      # Facet strip labels
      strip.text    = ggplot2::element_text(face = "bold", size = base_size),

      # Legend — always at the bottom
      legend.position    = "bottom",
      legend.direction   = "horizontal",
      legend.title       = ggplot2::element_text(face = "bold", size = base_size),
      legend.text        = ggplot2::element_text(size = base_size - 1),
      legend.key.width   = ggplot2::unit(2, "cm"),

      # Grid
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = "grey90"),
      panel.grid.major.y = ggplot2::element_line(colour = "grey90"),

      # Margins
      plot.margin = ggplot2::margin(10, 14, 10, 14)
    )
}

# ──────────────────────────────────────────────────────────────────────────────
# Strategy colour maps
# ──────────────────────────────────────────────────────────────────────────────
# Part 1: three strategies
col_single <- c(
  "SPY_Benchmark"        = unname(PAL["blue"]),
  "Naive_MM_Realized22d" = unname(PAL["orange"]),
  "FSV_MM_Forecast22d"   = unname(PAL["green"])
)
single_strategy_levels <- c("SPY_Benchmark", "Naive_MM_Realized22d", "FSV_MM_Forecast22d")
single_strategy_labels <- c("SPY", "Realized Vol. Model", "Stochastic Vol. Model")
single_returns$strategy_label <- factor(
  single_returns$strategy,
  levels = single_strategy_levels,
  labels = single_strategy_labels
)

# Part 2: two strategies
col_multi <- c(
  "Rolling_MeanVar_22d" = unname(PAL["orange"]),
  "FSV_MeanVar_22d"     = unname(PAL["blue"])
)

# Volatility overlay: three series
col_vol <- c(
  "FSV conditional vol" = unname(PAL["blue"]),
  "Rolling 22d vol"     = unname(PAL["red"]),
  "VIX (scaled)"        = unname(PAL["grey"])
)

# Correlation overlay
col_corr <- c(
  "FSV conditional" = unname(PAL["blue"]),
  "Rolling 22d"     = unname(PAL["orange"])
)

# ──────────────────────────────────────────────────────────────────────────────
# Part 1: Cumulative return
# ──────────────────────────────────────────────────────────────────────────────
single_returns_part1 <- subset(
  single_returns,
  tcost_bps == 0 & strategy %in% c("Naive_MM_Realized22d", "FSV_MM_Forecast22d")
)
col_single_display <- c(
  "Realized Vol. Model" = unname(PAL["orange"]),
  "Stochastic Vol. Model" = unname(PAL["green"])
)

p_single_cum <- ggplot(single_returns_part1, aes(x = date, y = cum_wealth, colour = strategy_label)) +
  geom_line(linewidth = 1.2) +
  scale_colour_manual(values = col_single_display, name = NULL) +
  scale_y_log10(labels = scales::label_number(accuracy = 0.01)) +
  theme_bayes() +
  theme(
    axis.text.x = element_text(face = "bold", size = 18),
    axis.text.y = element_text(face = "bold", size = 18),
    legend.position = "bottom",
    legend.text = element_text(face = "bold", size = 17),
    legend.title = element_blank()
  ) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = NULL,
    y = "Wealth Index (log scale)"
  )

# ------------------------------------------------------------------------------#
# Part 1: Equity weight comparison (managed models only)
# ------------------------------------------------------------------------------#
weights_part1 <- single_obj$weights
weights_part1$date <- as.Date(weights_part1$date)

weights_part1_long <- rbind(
  data.frame(date = weights_part1$date, model = "Realized Vol. Model", weight = weights_part1$Naive_MM_Realized22d),
  data.frame(date = weights_part1$date, model = "Stochastic Vol. Model", weight = weights_part1$FSV_MM_Forecast22d)
)

p_single_weight_compare <- ggplot(weights_part1_long, aes(x = date, y = weight, colour = model)) +
  geom_step(linewidth = 1.2) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50", linewidth = 0.8) +
  scale_colour_manual(
    values = c(
      "Realized Vol. Model" = unname(PAL["orange"]),
      "Stochastic Vol. Model" = unname(PAL["green"])
    ),
    name = NULL
  ) +
  theme_bayes() +
  theme(
    axis.text.x = element_text(face = "bold", size = 18),
    axis.text.y = element_text(face = "bold", size = 18),
    legend.position = "bottom",
    legend.text = element_text(face = "bold", size = 17),
    legend.title = element_blank()
  ) +
  labs(
    title = "Part 1: SPY Equity Weight Comparison",
    subtitle = "22-day rebalance with weights held constant between rebalance dates",
    x = NULL,
    y = "SPY weight"
  )

weights_part1_deviation <- data.frame(
  date = weights_part1$date,
  deviation_vs_realized = weights_part1$FSV_MM_Forecast22d - weights_part1$Naive_MM_Realized22d
)

p_single_weight_deviation <- ggplot(weights_part1_deviation, aes(x = date, y = deviation_vs_realized)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.8) +
  geom_line(linewidth = 1.2, colour = unname(PAL["green"])) +
  theme_bayes() +
  theme(
    axis.text.x = element_text(face = "bold", size = 18),
    axis.text.y = element_text(face = "bold", size = 18)
  ) +
  labs(
    title = "Part 1: Weight Deviation vs Realized Vol. Model",
    subtitle = "Stochastic Vol. Model minus Realized Vol. Model",
    x = NULL,
    y = "Weight deviation"
  )

# ──────────────────────────────────────────────────────────────────────────────
# Part 2: Cumulative return
# ──────────────────────────────────────────────────────────────────────────────
p_multi_cum <- ggplot(multi_returns_mv, aes(x = date, y = cum_ret, colour = strategy)) +
  geom_line(linewidth = 1.2) +
  facet_grid(
    budget ~ tcost_bps, scales = "free_y",
    labeller = labeller(
      tcost_bps = function(x) paste0("TC: ", x, " bps"),
      budget    = c(pf1 = "pf1: 45/45/10", pf2 = "pf2: 60/40/0")
    )
  ) +
  scale_colour_manual(
    values = col_multi,
    breaks = c("Rolling_MeanVar_22d", "FSV_MeanVar_22d"),
    labels = c("Realized Vol. Model", "Stochastic Vol. Model"),
    name = NULL
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bayes() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(face = "bold", size = 16),
    legend.title = element_blank()
  ) +
  labs(
    title    = "Part 2: Multi-Asset Cumulative Return",
    subtitle = "Causal rolling vs FSV covariance with expanding mean-variance weights",
    x        = NULL,
    y        = "Cumulative return"
  )

# Part 2: Cumulative return difference (FSV minus rolling)
multi_cum_diff <- reshape2::dcast(
  multi_returns_mv,
  date + budget + tcost_bps ~ strategy,
  value.var = "cum_ret"
)
multi_cum_diff$cum_ret_diff <- multi_cum_diff$FSV_MeanVar_22d - multi_cum_diff$Rolling_MeanVar_22d

p_multi_cum_diff <- ggplot(multi_cum_diff, aes(x = date, y = cum_ret_diff)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.8) +
  geom_line(linewidth = 1.2, colour = unname(PAL["blue"])) +
  facet_grid(
    budget ~ tcost_bps, scales = "free_y",
    labeller = labeller(
      tcost_bps = function(x) paste0("TC: ", x, " bps"),
      budget    = c(pf1 = "pf1: 45/45/10", pf2 = "pf2: 60/40/0")
    )
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  theme_bayes() +
  labs(
    title = "Part 2: Cumulative Return Difference",
    subtitle = "Stochastic Vol. Model minus Realized Vol. Model",
    x = NULL,
    y = "Cumulative return spread"
  )

# ──────────────────────────────────────────────────────────────────────────────
# Part 2: Equity weight over time
# ──────────────────────────────────────────────────────────────────────────────
equity_weights <- multi_obj$equity_weights
equity_weights$date <- as.Date(equity_weights$date)
equity_weights <- subset(
  equity_weights,
  budget %in% names(BUDGETS) &
    strategy %in% c("Rolling_MeanVar_22d", "FSV_MeanVar_22d")
)
p_equity_weight <- ggplot(equity_weights, aes(x = date, y = equity_weight, colour = strategy)) +
  geom_step(linewidth = 1.4) +
  facet_wrap(
    ~budget, ncol = 1,
    labeller = labeller(budget = c(pf1 = "pf1: 45/45/10", pf2 = "pf2: 60/40/0"))
  ) +
  scale_colour_manual(values = col_multi, name = "Strategy") +
  scale_y_continuous(
    limits = c(0.40, 0.65),
    breaks = seq(0.40, 0.65, by = 0.05),
    labels = scales::percent_format(accuracy = 1)
  ) +
  theme_bayes() +
  labs(
    title    = "Portfolio Equity Weight Over Time",
    subtitle = "Weights derived from causal mean-variance optimisation within fixed budgets",
    x        = NULL,
    y        = "Equity bucket weight"
  )

# â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
# Part 2: Within-equity composition (normalised inside equity bucket)
# â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
equity_assets <- names(BUCKET_WEIGHTS$equity)
equity_comp_list <- lapply(multi_obj$weight_objects, function(obj) {
  if (!(obj$budget[1] %in% names(BUDGETS))) return(NULL)
  if (!(obj$strategy[1] %in% c("Rolling_MeanVar_22d", "FSV_MeanVar_22d"))) return(NULL)
  w <- as.data.frame(obj$weights)
  w$date <- as.Date(obj$date)
  w_equity <- w[, equity_assets, drop = FALSE]
  eq_total <- rowSums(w_equity, na.rm = TRUE)
  eq_total_safe <- ifelse(eq_total > 0, eq_total, NA_real_)
  w_within <- sweep(w_equity, 1, eq_total_safe, "/")

  long <- reshape2::melt(
    data.frame(date = w$date, w_within, check.names = FALSE),
    id.vars = "date",
    variable.name = "asset",
    value.name = "within_equity_share"
  )
  long$budget <- obj$budget[1]
  long$strategy <- obj$strategy[1]
  long
})
equity_comp_list <- Filter(Negate(is.null), equity_comp_list)
if (length(equity_comp_list) == 0L) {
  equity_comp_df <- data.frame(
    date = as.Date(character(0)),
    asset = character(0),
    within_equity_share = numeric(0),
    budget = character(0),
    strategy = character(0),
    stringsAsFactors = FALSE
  )
} else {
  equity_comp_df <- do.call(rbind, equity_comp_list)
  row.names(equity_comp_df) <- NULL
}
equity_comp_df <- equity_comp_df[is.finite(equity_comp_df$within_equity_share), , drop = FALSE]
if (nrow(equity_comp_df) > 0L) {
  grp_n <- aggregate(
    within_equity_share ~ budget + strategy + asset,
    data = equity_comp_df,
    FUN = length
  )
  valid_grp <- grp_n[grp_n$within_equity_share >= 2L, c("budget", "strategy", "asset"), drop = FALSE]
  equity_comp_df <- merge(
    equity_comp_df,
    valid_grp,
    by = c("budget", "strategy", "asset"),
    all = FALSE
  )
}

equity_asset_colors <- c(
  SPY = unname(PAL["blue"]),
  QQQ = unname(PAL["orange"]),
  IWM = unname(PAL["green"]),
  EFA = unname(PAL["red"]),
  EEM = unname(PAL["grey"])
)

if (nrow(equity_comp_df) == 0L) {
  p_equity_comp <- ggplot() +
    theme_void() +
    labs(
      title = "Part 2: Within-Equity Composition Over Time",
      subtitle = "No finite within-equity composition observations available for this run."
    )
} else {
  p_equity_comp <- ggplot(equity_comp_df, aes(x = date, y = within_equity_share, fill = asset)) +
    geom_area(position = "stack", alpha = 0.9, na.rm = TRUE) +
    facet_grid(
      budget ~ strategy,
      labeller = labeller(
        budget = c(pf1 = "pf1: 45/45/10", pf2 = "pf2: 60/40/0"),
        strategy = c(
          Rolling_MeanVar_22d = "Realized Vol. Model",
          FSV_MeanVar_22d = "Stochastic Vol. Model"
        )
      )
    ) +
    scale_fill_manual(values = equity_asset_colors, name = "Equity Asset") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    theme_bayes() +
    labs(
      title = "Part 2: Within-Equity Composition Over Time",
      subtitle = "Each panel is normalised to 100% of the equity bucket",
      x = NULL,
      y = "Share within equity bucket"
    )
}

# ──────────────────────────────────────────────────────────────────────────────
# Part 1: Rolling 252-day return/vol ratio
# ──────────────────────────────────────────────────────────────────────────────
single_returns$rolling_ratio_252 <- ave(
  single_returns$ret,
  single_returns$strategy,
  single_returns$tcost_bps,
  FUN = function(x) rolling_return_vol_ratio(x, window = ROLLING_RATIO_WINDOW)
)

p_single_roll_ratio <- ggplot(single_returns, aes(x = date, y = rolling_ratio_252, colour = strategy_label)) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.6) +
  facet_wrap(
    ~tcost_bps, ncol = 1,
    labeller = labeller(tcost_bps = function(x) paste0("Transaction cost: ", x, " bps"))
  ) +
  scale_colour_manual(values = col_single_display, name = NULL) +
  theme_bayes() +
  theme(
    axis.text.x = element_text(face = "bold", size = 18),
    axis.text.y = element_text(face = "bold", size = 18),
    legend.position = "bottom",
    legend.text = element_text(face = "bold", size = 17),
    legend.title = element_blank()
  ) +
  labs(
    title = "Part 1: Rolling 252-Day Return/Volatility Ratio",
    x     = NULL,
    y     = "Return / volatility ratio"
  )

# ──────────────────────────────────────────────────────────────────────────────
# Part 2: Rolling 252-day return/vol ratio
# ──────────────────────────────────────────────────────────────────────────────
multi_returns_mv$rolling_ratio_252 <- ave(
  multi_returns_mv$ret,
  multi_returns_mv$budget,
  multi_returns_mv$strategy,
  multi_returns_mv$tcost_bps,
  FUN = function(x) rolling_return_vol_ratio(x, window = ROLLING_RATIO_WINDOW)
)

p_multi_roll_ratio <- ggplot(multi_returns_mv, aes(x = date, y = rolling_ratio_252, colour = strategy)) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.6) +
  facet_grid(
    budget ~ tcost_bps,
    labeller = labeller(
      tcost_bps = function(x) paste0("TC: ", x, " bps"),
      budget    = c(pf1 = "pf1: 45/45/10", pf2 = "pf2: 60/40/0")
    )
  ) +
  scale_colour_manual(values = col_multi, name = "Strategy") +
  theme_bayes() +
  labs(
    title = "Part 2: Rolling 252-Day Return/Volatility Ratio",
    x     = NULL,
    y     = "Return / volatility ratio"
  )

# Appendix extension: unconstrained global minimum-variance portfolios.
if (nrow(multi_returns_minvar) > 0L) {
  col_minvar <- c(
    "Rolling_MinVar_22d" = unname(PAL["orange"]),
    "FSV_MinVar_22d" = unname(PAL["blue"])
  )

  p_multi_minvar_cum <- ggplot(multi_returns_minvar, aes(x = date, y = cum_ret, colour = strategy)) +
    geom_line(linewidth = 1.2) +
    facet_wrap(
      ~tcost_bps, ncol = 1,
      labeller = labeller(tcost_bps = function(x) paste0("Transaction cost: ", x, " bps"))
    ) +
    scale_colour_manual(
      values = col_minvar,
      breaks = c("Rolling_MinVar_22d", "FSV_MinVar_22d"),
      labels = c("Realized Vol. Model", "Stochastic Vol. Model"),
      name = NULL
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_bayes() +
    theme(
      axis.text.x = element_text(face = "bold", size = 18),
      axis.text.y = element_text(face = "bold", size = 18),
      legend.position = "bottom",
      legend.text = element_text(face = "bold", size = 17),
      legend.title = element_blank()
    ) +
    labs(
      title = "Appendix: Global Minimum-Variance Cumulative Return",
      subtitle = "No class-budget constraints; long-only across all assets",
      x = NULL,
      y = "Cumulative return"
    )

  multi_returns_minvar$rolling_ratio_252 <- ave(
    multi_returns_minvar$ret,
    multi_returns_minvar$strategy,
    multi_returns_minvar$tcost_bps,
    FUN = function(x) rolling_return_vol_ratio(x, window = ROLLING_RATIO_WINDOW)
  )

  p_multi_minvar_roll_ratio <- ggplot(multi_returns_minvar, aes(x = date, y = rolling_ratio_252, colour = strategy)) +
    geom_line(linewidth = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.6) +
    facet_wrap(
      ~tcost_bps, ncol = 1,
      labeller = labeller(tcost_bps = function(x) paste0("Transaction cost: ", x, " bps"))
    ) +
    scale_colour_manual(
      values = col_minvar,
      breaks = c("Rolling_MinVar_22d", "FSV_MinVar_22d"),
      labels = c("Realized Vol. Model", "Stochastic Vol. Model"),
      name = NULL
    ) +
    theme_bayes() +
    theme(
      axis.text.x = element_text(face = "bold", size = 18),
      axis.text.y = element_text(face = "bold", size = 18),
      legend.position = "bottom",
      legend.text = element_text(face = "bold", size = 17),
      legend.title = element_blank()
    ) +
    labs(
      title = "Appendix: Global Minimum-Variance Rolling 252-Day Return/Volatility Ratio",
      x = NULL,
      y = "Return / volatility ratio"
    )
}

# ──────────────────────────────────────────────────────────────────────────────
# Signal: SPY-TLT conditional correlation
# ──────────────────────────────────────────────────────────────────────────────
decision_dates <- signals$decision_dates
sigma_fsv  <- signals$sigma_fsv_array
sigma_roll <- signals$sigma_roll_array

fsv_corr <- vapply(seq_len(dim(sigma_fsv)[3]), function(i) {
  s <- sigma_fsv[, , i]
  s["SPY", "TLT"] / sqrt(s["SPY", "SPY"] * s["TLT", "TLT"])
}, numeric(1))

roll_corr <- vapply(seq_len(dim(sigma_roll)[3]), function(i) {
  s <- sigma_roll[, , i]
  if (any(!is.finite(s[c("SPY", "TLT"), c("SPY", "TLT")]))) return(NA_real_)
  s["SPY", "TLT"] / sqrt(s["SPY", "SPY"] * s["TLT", "TLT"])
}, numeric(1))

corr_df <- rbind(
  data.frame(date = decision_dates, method = "FSV conditional", correlation = fsv_corr),
  data.frame(date = decision_dates, method = "Rolling 22d",     correlation = roll_corr)
)

p_corr <- ggplot(corr_df, aes(x = date, y = correlation, colour = method)) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.6) +
  scale_colour_manual(values = col_corr, name = "Method") +
  theme_bayes() +
  labs(
    title    = "Conditional Correlation: SPY vs TLT",
    subtitle = "FSV implied covariance vs rolling sample covariance",
    x        = NULL,
    y        = "Correlation"
  )

# ──────────────────────────────────────────────────────────────────────────────
# Signal: volatility overlay
# ──────────────────────────────────────────────────────────────────────────────
vol_overlay <- single_obj$vol_overlay
rebalance_dates_single <- as.Date(single_obj$rebalance_dates)
vol_overlay_models <- vol_overlay[vol_overlay$date %in% rebalance_dates_single, , drop = FALSE]

vol_points <- rbind(
  data.frame(date = vol_overlay_models$date, series = "FSV conditional vol", value = vol_overlay_models$fsv_conditional_vol),
  data.frame(date = vol_overlay_models$date, series = "Rolling 22d vol",     value = vol_overlay_models$rolling_vol_22d)
)
vol_vix <- data.frame(
  date = vol_overlay$date,
  series = "VIX (scaled)",
  value = vol_overlay$vix
)

p_vol_overlay <- ggplot() +
  geom_line(
    data = vol_vix,
    aes(x = date, y = value, colour = series),
    linewidth = 1.0
  ) +
  geom_step(
    data = vol_points,
    aes(x = date, y = value, colour = series, group = series),
    linewidth = 1.2
  ) +
  geom_point(
    data = vol_points,
    aes(x = date, y = value, colour = series),
    size = 2.0,
    alpha = 0.95
  ) +
  scale_colour_manual(values = col_vol, name = "Series") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bayes() +
  labs(
    title    = "SPY Volatility Signal Comparison",
    subtitle = "FSV/rolling signals shown as 22-day step functions; VIX shown daily",
    x        = NULL,
    y        = "Annualized volatility"
  )

# Multiplot: pairwise VIX comparisons and signal deviations at rebalance dates.
p_vix_vs_fsv <- ggplot() +
  geom_line(
    data = vol_vix,
    aes(x = date, y = value, colour = series),
    linewidth = 1.0
  ) +
  geom_step(
    data = data.frame(date = vol_overlay_models$date, value = vol_overlay_models$fsv_conditional_vol),
    aes(x = date, y = value, colour = "FSV conditional vol"),
    linewidth = 1.2
  ) +
  scale_colour_manual(
    values = col_vol[c("FSV conditional vol", "VIX (scaled)")],
    breaks = c("FSV conditional vol", "VIX (scaled)"),
    name = NULL
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bayes() +
  labs(
    title = "VIX vs FSV Forecast",
    x = NULL,
    y = "Annualized volatility"
  )

p_vix_vs_roll <- ggplot() +
  geom_line(
    data = vol_vix,
    aes(x = date, y = value, colour = series),
    linewidth = 1.0
  ) +
  geom_step(
    data = data.frame(date = vol_overlay_models$date, value = vol_overlay_models$rolling_vol_22d),
    aes(x = date, y = value, colour = "Rolling 22d vol"),
    linewidth = 1.2
  ) +
  scale_colour_manual(
    values = col_vol[c("Rolling 22d vol", "VIX (scaled)")],
    breaks = c("Rolling 22d vol", "VIX (scaled)"),
    name = NULL
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bayes() +
  labs(
    title = "VIX vs Realized Vol. Signal",
    x = NULL,
    y = "Annualized volatility"
  )

dev_df <- data.frame(
  date = vol_overlay_models$date,
  `FSV - VIX` = vol_overlay_models$fsv_conditional_vol - vol_overlay_models$vix,
  `Realized - VIX` = vol_overlay_models$rolling_vol_22d - vol_overlay_models$vix,
  check.names = FALSE
)
dev_long <- reshape2::melt(
  dev_df,
  id.vars = "date",
  variable.name = "pair",
  value.name = "deviation"
)

p_vix_deviation <- ggplot(dev_long, aes(x = date, y = deviation, colour = pair, group = pair)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.8) +
  geom_step(linewidth = 1.2) +
  geom_point(size = 1.4) +
  scale_colour_manual(
    values = c(
      `FSV - VIX` = unname(PAL["blue"]),
      `Realized - VIX` = unname(PAL["red"])
    ),
    name = NULL
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bayes() +
  labs(
    title = "Signal Deviation vs VIX",
    subtitle = "Rebalance dates only",
    x = NULL,
    y = "Signal - VIX"
  )

p_vix_signal_deviation_multiplot <- (p_vix_vs_fsv / p_vix_vs_roll / p_vix_deviation) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# ──────────────────────────────────────────────────────────────────────────────
# Save all charts
# ──────────────────────────────────────────────────────────────────────────────
ggsave(
  filename = file.path(PATH_CHART, "part1_cumulative_return.png"),
  plot     = p_single_cum,
  width    = 14, height = 9, dpi = 180
)
ggsave(
  filename = file.path(PATH_CHART, "part1_equity_weight_comparison.png"),
  plot     = p_single_weight_compare,
  width    = 14, height = 8, dpi = 180
)
ggsave(
  filename = file.path(PATH_CHART, "part1_equity_weight_deviation_vs_realized.png"),
  plot     = p_single_weight_deviation,
  width    = 14, height = 8, dpi = 180
)
ggsave(
  filename = file.path(PATH_CHART, "part2_cumulative_return.png"),
  plot     = p_multi_cum,
  width    = 14, height = 10, dpi = 180
)
ggsave(
  filename = file.path(PATH_CHART, "part2_cumulative_return_difference.png"),
  plot     = p_multi_cum_diff,
  width    = 14, height = 10, dpi = 180
)
ggsave(
  filename = file.path(PATH_CHART, "part2_equity_weight_over_time.png"),
  plot     = p_equity_weight,
  width    = 14, height = 9, dpi = 180
)
tryCatch(
  ggsave(
    filename = file.path(PATH_CHART, "part2_equity_within_bucket_composition.png"),
    plot     = p_equity_comp,
    width    = 14, height = 10, dpi = 180
  ),
  error = function(e) {
    warning("Could not render part2_equity_within_bucket_composition.png: ", conditionMessage(e))
    fallback_plot <- ggplot() +
      theme_void() +
      labs(
        title = "Part 2: Within-Equity Composition Over Time",
        subtitle = "Chart unavailable for this run (insufficient composition points)."
      )
    ggsave(
      filename = file.path(PATH_CHART, "part2_equity_within_bucket_composition.png"),
      plot     = fallback_plot,
      width    = 14, height = 10, dpi = 180
    )
  }
)
ggsave(
  filename = file.path(PATH_CHART, "part1_rolling_252_ratio.png"),
  plot     = p_single_roll_ratio,
  width    = 14, height = 9, dpi = 180
)
ggsave(
  filename = file.path(PATH_CHART, "part2_rolling_252_ratio.png"),
  plot     = p_multi_roll_ratio,
  width    = 14, height = 10, dpi = 180
)
if (exists("p_multi_minvar_cum", inherits = FALSE)) {
  ggsave(
    filename = file.path(PATH_CHART, "appendix_minvar_cumulative_return.png"),
    plot     = p_multi_minvar_cum,
    width    = 14, height = 9, dpi = 180
  )
}
if (exists("p_multi_minvar_roll_ratio", inherits = FALSE)) {
  ggsave(
    filename = file.path(PATH_CHART, "appendix_minvar_rolling_252_ratio.png"),
    plot     = p_multi_minvar_roll_ratio,
    width    = 14, height = 9, dpi = 180
  )
}
ggsave(
  filename = file.path(PATH_CHART, "signal_spy_tlt_conditional_correlation.png"),
  plot     = p_corr,
  width    = 14, height = 7, dpi = 180
)
ggsave(
  filename = file.path(PATH_CHART, "part1_spy_vol_vs_vix.png"),
  plot     = p_vol_overlay,
  width    = 14, height = 7, dpi = 180
)
ggsave(
  filename = file.path(PATH_CHART, "part1_vix_signal_deviation_multiplot.png"),
  plot     = p_vix_signal_deviation_multiplot,
  width    = 14, height = 14, dpi = 180
)

write_csv_safe(single_metrics, file.path(PATH_TABLE, "05_part1_metrics.csv"))
write_csv_safe(multi_metrics, file.path(PATH_TABLE, "05_part2_metrics.csv"))

combined_metrics <- rbind(
  {
    part1 <- cbind(part = "Part1_SingleAsset", single_metrics)
    part2 <- cbind(part = "Part2_MultiAsset", multi_metrics)
    all_cols <- union(names(part1), names(part2))

    align_cols <- function(df, cols) {
      miss <- setdiff(cols, names(df))
      if (length(miss) > 0L) {
        for (m in miss) df[[m]] <- NA
      }
      df[, cols, drop = FALSE]
    }

    rbind(align_cols(part1, all_cols), align_cols(part2, all_cols))
  }
)
write_csv_safe(combined_metrics, file.path(PATH_TABLE, "05_combined_metrics.csv"))

caveat_lines <- c(
  "# CAVEATS",
  "",
  "- CAVEAT: Initial train/test split uses training data <= 2018-12-31 and test data >= 2019-01-01.",
  "- CAVEAT: Expanding-window FSV refits use only data available up to each rebalance date (no future leakage).",
  "- DEVIATION: Part 2 uses one-step-ahead (t+1) FSV covariance forecasts at rebalance dates.",
  "- DEVIATION: Part 1 uses a 22-day horizon-average FSV variance forecast for MM scaling.",
  "- DEVIATION: Expected returns for multi-asset optimization use an expanding historical mean up to each rebalance date.",
  "- DEVIATION: borrowing rate proxy uses SHY daily return + 30 bps annual spread, floored at zero.",
  "- DEVIATION: AR(1) SV parameters are estimated by OLS on posterior-mean log-variance paths for tractability."
)
writeLines(caveat_lines, con = file.path(PATH_RESULTS, "CAVEATS.md"))

message("05_results.R complete.")
