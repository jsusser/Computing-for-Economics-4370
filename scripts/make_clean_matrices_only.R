#!/usr/bin/env Rscript
options(repos = c(CRAN = "https://cloud.r-project.org"))
source(file.path("config", "config.R"))
source(file.path("utils", "data_nhanes.R"))
source(file.path("utils", "cleaning.R"))
source(file.path("utils", "io.R"))
suppressMessages(library(readr))
suppressMessages(library(dplyr))

waves <- CFG$data$nhanes_waves
if (is.null(waves) || length(waves) == 0) waves <- CFG$data$nhanes_wave

# Ensure organized output subfolders exist for tables, figures, and artifacts
dir.create(CFG$dirs$out, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(CFG$dirs$out, "tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(CFG$dirs$out, "figures"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(CFG$dirs$out, "artifacts"), showWarnings = FALSE, recursive = TRUE)

# Document raw assets (n, k, years) before merges
save_raw_assets_overview_tex(CFG$dirs$out, CFG, CFG$dirs$res_nhanes, CFG$dirs$res_mort, waves)

df_core <- prepare_nhanes_core_multi(waves, CFG$dirs$res_nhanes)

# Ensure mortality CSV exists; if only .dat exists, convert it to CSV
dat_path <- file.path(CFG$dirs$res_mort, CFG$data$mortality$dat_file)
csv_path <- sub("[.]dat$", ".csv", dat_path, ignore.case = TRUE)
if (file.exists(dat_path) && !file.exists(csv_path)) {
  suppressMessages(library(readr))
  write_mortality_csv(dat_path, csv_path)
}

# Read mortality CSV if present and merge; fallback to core if not present
if (file.exists(csv_path)) {
  mort <- readr::read_csv(csv_path, show_col_types = FALSE)
  names(mort) <- tolower(names(mort))
  df <- merge_core_mortality(df_core, mort)
} else {
  df <- df_core
}
dir.create(CFG$dirs$clean, showWarnings = FALSE, recursive = TRUE)

# Pre-clean overview (before imputation), for accurate missingness reporting
save_data_overview_tex(df, CFG$dirs$out, label = CFG$data$source_label)

prep <- prepare_features_labels(df)
# Optional: write imputation counts table for report
source(file.path("utils", "io.R"))
save_imputation_counts_tex(prep$meta, CFG$dirs$out)

# Heads of cleaned_full and meta
meta_csv_path <- file.path(CFG$dirs$clean, CFG$files$meta_csv)
save_heads_clean_and_meta_tex(prep$df, meta_csv_path, CFG$dirs$out)

# Dictionaries and metrics for X and y
save_feature_dictionary_tex(colnames(prep$X), CFG$dirs$out)
save_y_dictionary_tex(prep$meta$label, CFG$dirs$out)
save_X_metrics_tex(prep$X, CFG$dirs$out)
save_y_metrics_tex(prep$y, CFG$dirs$out)
save_class_weights_tex(prep$y, CFG$dirs$out)

# Clean overview and processing notes + smoker counts
save_clean_overview_tex(prep$meta, CFG$dirs$out)
save_processing_notes_tex(prep$meta, CFG$dirs$out)
save_smoker_counts_tex(prep$df, CFG$dirs$out)
cat("Wrote:", file.path(CFG$dirs$clean, CFG$files$X_csv), "\n")
cat("Wrote:", file.path(CFG$dirs$clean, CFG$files$y_csv), "\n")
cat("Wrote:", file.path(CFG$dirs$clean, CFG$files$cleaned_full_csv), "\n")

# Sanitize generated LaTeX tables for inclusion in custom report_final.tex
source(file.path("utils", "io.R"))
sanitize_tables_dir(CFG$dirs$out)
