config_candidates <- c("00_config.R", file.path("01_Code", "Pipeline", "00_config.R"))
config_path <- config_candidates[file.exists(config_candidates)][1]
if (is.na(config_path)) stop("Could not locate 00_config.R")
source(config_path)

message("Running 99_validate_pilot.R ...")

path_part1 <- file.path(PATH_TABLE, "05_part1_metrics.csv")
path_part2 <- file.path(PATH_TABLE, "05_part2_metrics.csv")
path_single <- file.path(PATH_OBJECT, "04_singleasset_backtest.rds")
path_multi <- file.path(PATH_OBJECT, "04_multiasset_backtest.rds")
path_model <- file.path(PATH_DATA, "fsv_model_summary.rds")
path_signals <- file.path(PATH_DATA, "signals.rds")

required_paths <- c(path_part1, path_part2, path_single, path_multi, path_model, path_signals)
missing_paths <- required_paths[!file.exists(required_paths)]
if (length(missing_paths) > 0L) {
  stop("Missing output files: ", paste(missing_paths, collapse = ", "))
}

t1 <- utils::read.csv(path_part1, stringsAsFactors = FALSE)
t2 <- utils::read.csv(path_part2, stringsAsFactors = FALSE)
single_obj <- readRDS(path_single)
multi_obj <- readRDS(path_multi)
model_summary <- readRDS(path_model)
signals <- readRDS(path_signals)

checks <- list()

# Distinct strategy outcomes
checks$part1_unique_annret_tcost0 <- length(unique(round(subset(t1, tcost_bps == 0)$annualized_return, 10)))
checks$part2_unique_annret_tcost0 <- length(unique(round(subset(t2, tcost_bps == 0)$annualized_return, 10)))

# Transaction costs should not improve annualized return
m1 <- merge(
  subset(t1, tcost_bps == 0),
  subset(t1, tcost_bps == 5),
  by = "strategy",
  suffixes = c("_0", "_5")
)
m2 <- merge(
  subset(t2, tcost_bps == 0),
  subset(t2, tcost_bps == 5),
  by = c("budget", "strategy"),
  suffixes = c("_0", "_5")
)
checks$part1_tcost_penalty_all <- all(m1$annualized_return_5 <= m1$annualized_return_0 + 1e-12)
checks$part2_tcost_penalty_all <- all(m2$annualized_return_5 <= m2$annualized_return_0 + 1e-12)

# Turnover sign sanity
checks$part1_turnover_nonneg <- all(t1$annualized_turnover_one_way >= -1e-12)
checks$part2_turnover_nonneg <- all(t2$annualized_turnover_one_way >= -1e-12)

# Weight-path sanity: sums close to 1
w_sums_ok <- TRUE
for (nm in names(multi_obj$weight_objects)) {
  w <- multi_obj$weight_objects[[nm]]$weights
  if (!all(abs(rowSums(w) - 1) < 1e-8, na.rm = TRUE)) {
    w_sums_ok <- FALSE
    break
  }
}
checks$multi_weight_sums_close_to_one <- w_sums_ok

# Multi-strategy differentiation check (pf1)
w1 <- multi_obj$weight_objects[["pf1_Rolling_MeanVar_22d"]]$weights
w2 <- multi_obj$weight_objects[["pf1_FSV_MeanVar_22d"]]$weights
checks$mean_abs_wdiff_pf1_rolling_vs_fsv <- mean(abs(w1 - w2))

# Settings traceability
checks$mcmc_draws_used <- model_summary$mcmc_settings$draws
checks$mcmc_burnin_used <- model_summary$mcmc_settings$burnin
checks$mcmc_thin_used <- model_summary$mcmc_settings$thin
checks$lv_semantics <- model_summary$lv_semantics

# Signal dimensional sanity
checks$signal_dim_fsv <- paste(dim(signals$sigma_fsv_array), collapse = "x")
checks$signal_dim_roll <- paste(dim(signals$sigma_roll_array), collapse = "x")

print(checks)

# Single-asset diagnostics to interpret large performance gaps
single_ret_0 <- subset(single_obj$returns_long, tcost_bps == 0)
single_weights <- single_obj$weights
simple_returns_xts <- readRDS(file.path(PATH_DATA, "simple_returns_xts.rds"))
idx <- match(single_weights$date, as.Date(zoo::index(simple_returns_xts)))
spy_next <- as.numeric(simple_returns_xts[, "SPY"])[idx + 1L]

diag_rows <- lapply(c("Naive_MM_Realized22d", "FSV_MM_Forecast22d"), function(nm) {
  w <- single_weights[[nm]]
  data.frame(
    strategy = nm,
    weight_mean = mean(w, na.rm = TRUE),
    weight_median = stats::median(w, na.rm = TRUE),
    weight_sd = stats::sd(w, na.rm = TRUE),
    weight_p95 = unname(stats::quantile(w, 0.95, na.rm = TRUE)),
    weight_max = max(w, na.rm = TRUE),
    weight_pct_at_cap = mean(w >= (LEVERAGE_CAP_SINGLE - 1e-8), na.rm = TRUE),
    corr_weight_next_abs_return = stats::cor(w, abs(spy_next), use = "complete.obs"),
    corr_weight_next_sq_return = stats::cor(w, spy_next^2, use = "complete.obs"),
    stringsAsFactors = FALSE
  )
})
single_diag <- do.call(rbind, diag_rows)
print(single_diag)

for (st in unique(single_ret_0$strategy)) {
  x <- single_ret_0$ret[single_ret_0$strategy == st]
  cat(
    st,
    ": mean_daily=", mean(x),
    ", p1=", unname(stats::quantile(x, 0.01)),
    ", p99=", unname(stats::quantile(x, 0.99)),
    ", worst=", min(x),
    ", best=", max(x),
    "\n",
    sep = ""
  )
}

message("99_validate_pilot.R complete.")
