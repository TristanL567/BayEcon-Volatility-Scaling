suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
})

TRADING_DAYS <- 252L

# ── Load both signals ────────────────────────────────────────────────────────
sig_local <- readRDS("C:/Users/Tristan Leiter/Documents/BayEcon-Volatility-Scaling/02_Data/signals.rds")
sig_gcp   <- readRDS("C:/Users/Tristan Leiter/Documents/BayEcon-Volatility-Scaling/05_Results_NewRun/02_Data/signals.rds")

# ── Extract FSV vol at rebalance dates ──────────────────────────────────────
extract_fsv_vol <- function(sig) {
  rebal_idx   <- sig$rebalance_indices
  dates       <- sig$decision_dates[rebal_idx]
  fsv_var     <- sig$fsv_var_spy[rebal_idx]
  valid       <- is.finite(fsv_var) & fsv_var > 0
  data.frame(
    date = dates[valid],
    fsv_vol = sqrt(fsv_var[valid]) * sqrt(TRADING_DAYS)
  )
}

df_local <- extract_fsv_vol(sig_local)
df_gcp   <- extract_fsv_vol(sig_gcp)

# ── Align on common dates ────────────────────────────────────────────────────
common_dates <- intersect(as.character(df_local$date), as.character(df_gcp$date))

df_plot <- rbind(
  data.frame(date = df_local$date[as.character(df_local$date) %in% common_dates],
             vol  = df_local$fsv_vol[as.character(df_local$date) %in% common_dates],
             run  = "Local (2000 draws / 400 burn-in)"),
  data.frame(date = df_gcp$date[as.character(df_gcp$date) %in% common_dates],
             vol  = df_gcp$fsv_vol[as.character(df_gcp$date) %in% common_dates],
             run  = "GCP (5000 draws / 1000 burn-in)")
)

# ── Theme (mirrors theme_bayes) ──────────────────────────────────────────────
theme_compare <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", size = 16, hjust = 0),
      plot.subtitle = ggplot2::element_text(size = 12, hjust = 0, colour = "grey35"),
      legend.position    = "bottom",
      legend.title       = ggplot2::element_text(face = "bold"),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = "grey90"),
      panel.grid.major.y = ggplot2::element_line(colour = "grey90"),
      plot.margin = ggplot2::margin(10, 14, 10, 14)
    )
}

PAL <- c(
  "GCP (5000 draws / 1000 burn-in)"   = "#2166AC",
  "Local (2000 draws / 400 burn-in)"  = "#D6604D"
)

# ── Plot ─────────────────────────────────────────────────────────────────────
p <- ggplot(df_plot, aes(x = date, y = vol, colour = run, group = run)) +
  geom_step(linewidth = 1.2) +
  geom_point(size = 2.0, alpha = 0.9) +
  scale_colour_manual(values = PAL, name = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_compare() +
  labs(
    title    = "FSV SPY Volatility Forecast: GCP vs Local Run",
    subtitle = "22-day ahead conditional vol at each rebalance date; annualised",
    x        = NULL,
    y        = "Annualised Volatility"
  )

# ── Save ─────────────────────────────────────────────────────────────────────
out_path <- "C:/Users/Tristan Leiter/Documents/BayEcon-Volatility-Scaling/05_Results_NewRun/fsv_vol_gcp_vs_local.png"
ggsave(out_path, plot = p, width = 12, height = 5, dpi = 150)
message("Saved: ", out_path)
