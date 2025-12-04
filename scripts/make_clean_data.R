#!/usr/bin/env Rscript
options(repos = c(CRAN = "https://cloud.r-project.org"))

here <- getwd()
cfg_path <- file.path(here, "config", "config.R")
if (!file.exists(cfg_path)) stop("config/config.R not found; run from project root.")
source(cfg_path)

req <- c("readr")
new <- req[!(req %in% installed.packages()[, "Package"])]
if (length(new)) install.packages(new, dependencies = TRUE)

source(file.path(CFG$dirs$utils, "data_nhanes.R"))
source(file.path(CFG$dirs$utils, "cleaning.R"))
source(file.path(CFG$dirs$utils, "io.R"))

waves <- CFG$data$nhanes_waves
if (is.null(waves) || length(waves) == 0) waves <- CFG$data$nhanes_wave

# Ensure raw CSVs exist
for (w in waves) {
  demo_path <- file.path(CFG$dirs$res, paste0("DEMO_", w, ".csv"))
  smq_path  <- file.path(CFG$dirs$res, paste0("SMQ_",  w, ".csv"))
  if (!file.exists(demo_path) || !file.exists(smq_path)) {
    stop("Missing raw NHANES files for wave ", w, ". Run scripts/fetch_nhanes_raw.R first.")
  }
}

# Build analysis dataset
sim_df <- prepare_nhanes_core_multi(waves, CFG$dirs$res)

# Save raw data overview + per-wave counts
dir.create(CFG$dirs$out, showWarnings = FALSE, recursive = TRUE)
save_data_overview_tex(sim_df, CFG$dirs$out, label = CFG$data$source_label)
save_wave_counts_tex(sim_df, CFG$dirs$out, which = "raw")

# Prepare clean matrices
dir.create(CFG$dirs$clean, showWarnings = FALSE, recursive = TRUE)
prep <- prepare_features_labels(sim_df)

# Save clean-data notes and tables
save_clean_overview_tex(prep$meta, CFG$dirs$out)
# save_processing_notes_tex(prep$meta, CFG$dirs$out)  # temporarily disabled to avoid LaTeX escape issues
save_smoker_counts_tex(prep$df, CFG$dirs$out)
save_wave_counts_tex(prep$df, CFG$dirs$out, which = "clean")

message("Clean data written to ", normalizePath(CFG$dirs$clean, winslash = "/", mustWork = FALSE))
