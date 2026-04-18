config_candidates <- c("00_config.R", file.path("01_Code", "Pipeline", "00_config.R"))
config_path <- config_candidates[file.exists(config_candidates)][1]
if (is.na(config_path)) stop("Could not locate 00_config.R")
source(config_path)

message("Running 01_data.R ...")

suppressPackageStartupMessages({
  library(quantmod)
  library(xts)
})

data_env <- new.env(parent = emptyenv())
symbol_object_map <- setNames(rep(NA_character_, length(TICKERS_WITH_VIX)), TICKERS_WITH_VIX)

for (tk in TICKERS_WITH_VIX) {
  message("Downloading: ", tk)
  assigned_name <- suppressWarnings(
    quantmod::getSymbols(
      Symbols = tk,
      src = "yahoo",
      from = START_DATE,
      to = END_DATE,
      auto.assign = TRUE,
      env = data_env
    )
  )
  symbol_object_map[tk] <- assigned_name[1]
}

extract_adjusted <- function(symbol) {
  object_name <- symbol_object_map[[symbol]]
  if (!is.finite(match(object_name, ls(envir = data_env)))) {
    stop("Downloaded symbol object not found in environment: ", symbol)
  }
  x <- get(object_name, envir = data_env)
  out <- quantmod::Ad(x)
  colnames(out) <- gsub("^\\^", "", symbol)
  out
}

# Keep ETF prices and VIX as separate objects because VIX is used for plotting only.
prices_list <- lapply(TICKERS, extract_adjusted)
prices_etf_xts <- do.call(merge, prices_list)
prices_etf_xts <- na.omit(prices_etf_xts)

vix_xts <- extract_adjusted("^VIX")
vix_xts <- vix_xts[index(vix_xts) %in% index(prices_etf_xts)]
vix_xts <- na.locf(vix_xts, na.rm = FALSE)
vix_xts <- na.omit(vix_xts)

# Align ETF prices after NA handling to ensure identical index across assets.
prices_etf_xts <- prices_etf_xts[index(prices_etf_xts) %in% index(vix_xts)]
prices_etf_xts <- na.omit(prices_etf_xts)

log_returns_xts <- diff(log(prices_etf_xts))
log_returns_xts <- na.omit(log_returns_xts)

# Use arithmetic returns for compounding/backtesting as specified.
simple_returns_xts <- exp(log_returns_xts) - 1

log_returns_mat <- as.matrix(log_returns_xts)
log_returns_demeaned_mat <- sweep(log_returns_mat, 2, colMeans(log_returns_mat, na.rm = TRUE), "-")

descriptive_stats <- data.frame(
  ticker = colnames(simple_returns_xts),
  n_obs = nrow(simple_returns_xts),
  mean_daily = colMeans(simple_returns_xts, na.rm = TRUE),
  sd_daily = apply(simple_returns_xts, 2, sd, na.rm = TRUE),
  annualized_return = apply(simple_returns_xts, 2, function(r) prod(1 + r)^(TRADING_DAYS / length(r[is.finite(r)])) - 1),
  annualized_volatility = apply(simple_returns_xts, 2, sd, na.rm = TRUE) * sqrt(TRADING_DAYS),
  stringsAsFactors = FALSE
)

saveRDS(prices_etf_xts, file.path(PATH_DATA, "prices_etf_xts.rds"))
saveRDS(vix_xts, file.path(PATH_DATA, "prices_vix_xts.rds"))
saveRDS(log_returns_xts, file.path(PATH_DATA, "log_returns_xts.rds"))
saveRDS(simple_returns_xts, file.path(PATH_DATA, "simple_returns_xts.rds"))
saveRDS(log_returns_demeaned_mat, file.path(PATH_DATA, "log_returns_demeaned_matrix.rds"))
saveRDS(
  list(
    tickers = TICKERS,
    tickers_fsv = TICKERS_FSV,
    prices_etf_xts = prices_etf_xts,
    prices_vix_xts = vix_xts,
    log_returns_xts = log_returns_xts,
    simple_returns_xts = simple_returns_xts,
    log_returns_demeaned_matrix = log_returns_demeaned_mat
  ),
  file.path(PATH_DATA, "data_bundle.rds")
)

write_csv_safe(descriptive_stats, file.path(PATH_TABLE, "01_descriptive_stats.csv"))

message("01_data.R complete.")
