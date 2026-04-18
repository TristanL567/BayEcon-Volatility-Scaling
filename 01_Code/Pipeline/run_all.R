config_candidates <- c("00_config.R", file.path("01_Code", "Pipeline", "00_config.R"))
config_path <- config_candidates[file.exists(config_candidates)][1]
if (is.na(config_path)) stop("Could not locate 00_config.R")
source(config_path)

message("Running full pipeline ...")

pipeline_steps <- c(
  file.path(PATH_CODE, "01_data.R"),
  file.path(PATH_CODE, "02_model_fit.R"),
  file.path(PATH_CODE, "03_signals.R"),
  file.path(PATH_CODE, "04_backtest_singleasset.R"),
  file.path(PATH_CODE, "04_backtest_multiasset.R"),
  file.path(PATH_CODE, "05_results.R")
)

for (step in pipeline_steps) {
  message("Sourcing: ", step)
  source(step)
}

message("Pipeline complete.")
