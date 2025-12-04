CFG <- list(
  seed = 123,
  n = 1000,

  packages = c(
    "tidyverse", "broom", "sandwich", "lmtest", "car", "ggplot2", "modelsummary", "tinytex", "glmnet", "nhanesA",
    "pROC", "PRROC", "ResourceSelection"
  ),

  dirs = list(
    out   = "output",
    res   = "resources",
    res_nhanes = "resources/nhanes",
    res_mort   = "resources/mortality",
    clean = "clean-data",
    utils = "utils",
    core  = "core",
    config = "config"
  ),

  #  

  model = list(
    robust_se_type = "HC1",
    ci_z = 1.96,
    lasso = list(
      alpha = 1,          # 1 = lasso
      nfolds = 10,
      standardize = TRUE,
      family = "binomial" # logistic for binary outcomes
    )
  ),

  plots = list(
    width = 7,
    height = 5,
    dpi = 300
  ),

  tables = list(
    stars = c("*" = .1, "**" = .05, "***" = .01)
  ),

  clean = list(
    min_age = 18,
    drop_smoker_unknown = TRUE
  ),

  # Simulation settings for Monte Carlo only
  sim = list(
    n = 1000,
    seed = 123,
    sex_levels = c("female", "male"),
    smoker_levels = c("Never", "Former", "Current")
  ),

  files = list(
    simulated_data = "artifacts/simulated_smoking_mortality.csv",
    residuals = "figures/ols_residuals_vs_fitted.png",
    qq = "figures/ols_qq_plot.png",
    preds = "figures/ols_predicted_by_smoker.png",
    table_txt = "artifacts/ols_coefficients.txt",
    table_tex = "tables/ols_coefficients.tex",
    summary_numeric = "tables/data_summary_numeric.tex",
    summary_categorical = "tables/data_summary_categorical.tex",
    vif = "artifacts/vif.txt",
    report_tex = "report.tex",
    lasso_coefs_txt = "artifacts/lasso_coefficients.txt",
    lasso_coefs_tex = "tables/lasso_coefficients.tex",
    lasso_cv_plot = "figures/lasso_cv_plot.png",
    coef_compare_txt = "artifacts/ols_lasso_coef_comparison.txt",
    coef_compare_tex = "tables/ols_lasso_coef_comparison.tex",
    # Multi-panel diagnostics per model
    ols_panel = "figures/ols_diagnostics_panel.png",
    lasso_panel = "figures/lasso_diagnostics_panel.png",
    # Individual diagnostics figures per model
    lasso_residuals = "figures/lasso_residuals_vs_fitted.png",
    lasso_qq = "figures/lasso_qq_plot.png",
    # # NN outputs removed
    model_metrics_tex = "tables/model_comparison_metrics.tex",
    model_metrics_txt = "artifacts/model_comparison_metrics.txt",
    model_metrics_in_tex = "tables/model_metrics_in_sample.tex",
    model_metrics_out_tex = "tables/model_metrics_out_of_sample.tex",
    # Per-model classification metrics (logit, regularized logit)
    logit_metrics_in_tex = "tables/logit_metrics_in_sample.tex",
    logit_metrics_out_tex = "tables/logit_metrics_out_of_sample.tex",
    lasso_cls_metrics_in_tex = "tables/regularized_logit_metrics_in_sample.tex",
    lasso_cls_metrics_out_tex = "tables/regularized_logit_metrics_out_of_sample.tex",
    # ROC/PR/Calibration figures
    logit_roc = "figures/logit_roc.png",
    logit_pr = "figures/logit_pr.png",
    logit_cal = "figures/logit_calibration.png",
    lasso_roc = "figures/regularized_logit_roc.png",
    lasso_pr = "figures/regularized_logit_pr.png",
    lasso_cal = "figures/regularized_logit_calibration.png",
    # Per-model in/out tables
    ols_metrics_in_tex = "tables/ols_metrics_in_sample.tex",
    ols_metrics_out_tex = "tables/ols_metrics_out_of_sample.tex",
    lasso_metrics_in_tex = "tables/lasso_metrics_in_sample.tex",
    lasso_metrics_out_tex = "tables/lasso_metrics_out_of_sample.tex",
    # # NN outputs removed
    # Logistic (mortality) outputs
    logit_table_txt = "logit_mortality_table.txt",
    logit_table_tex = "logit_mortality_table.tex",
    logit_lasso_coefs_txt = "logit_lasso_coefficients.txt",
    logit_lasso_coefs_tex = "logit_lasso_coefficients.tex",
    logit_coef_compare_txt = "logit_coef_comparison.txt",
    logit_coef_compare_tex = "logit_coef_comparison.tex",
    # Monte Carlo outputs removed
    data_overview_tex = "tables/data_overview.tex",
    missing_by_var_tex = "tables/missing_by_variable.tex",
    clean_overview_tex = "tables/clean_data_overview.tex",
    head_X_tex = "tables/head_features_X.tex",
    head_y_tex = "tables/head_label_y.tex",
    head_cleaned_full_tex = "tables/head_cleaned_full_frame.tex",
    head_meta_tex = "tables/head_meta_ids_missing.tex",
    feature_dict_tex = "tables/feature_dictionary_X.tex",
    y_dict_tex = "tables/label_dictionary_y.tex",
    x_metrics_numeric_tex = "tables/x_numeric_metrics.tex",
    x_metrics_categorical_tex = "tables/x_categorical_metrics.tex",
    y_metrics_tex = "tables/y_metrics_overview.tex",
    class_weights_tex = "tables/class_weights_overview.tex",
    raw_assets_overview_tex = "tables/raw_assets_overview.tex",
    X_csv = "X.csv",
    y_csv = "y.csv",
    cleaned_full_csv = "cleaned_full.csv",
    meta_csv = "meta.csv",
    processing_notes_tex = "tables/processing_notes.tex",
    smoker_counts_tex = "tables/smoker_category_counts.tex"
    , wave_counts_raw_tex = "tables/wave_counts_raw.tex"
    , wave_counts_clean_tex = "tables/wave_counts_clean.tex"
    , imputation_counts_tex = "tables/imputation_counts_by_variable.tex"
  ),

  report = list(
    title = "Smoking and Mortality Risk (NHANES 2013--2014)"
  )
)

# Data source config
CFG$data <- list(
  # Pull NHANES tables (DEMO/SMQ) and mortality assets
  use_nhanes = TRUE,
  # NHANES cycle code (e.g., "H" = 2013â€“2014)
  # One or more NHANES cycles to pull; e.g., c("H","I","J") for 2013â€“14, 2015â€“16, 2017â€“18
  nhanes_waves = c("H"),
  # Always fetch mortality assets into resources (raw)
  use_mortality = TRUE,
  # Mortality assets (public-use) â€” filenames saved under resources/, and their source URLs
  mortality = list(
    # Output filenames under resources/
    dat_file = "NHANES_2013_2014_MORT_2019_PUBLIC.dat",      # raw .dat file
    setup_file = "R_ReadInProgramAllSurveys.R",               # CDC R setup reader
    # CDC source URLs (public)
    dat_url = "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/datalinkage/linked_mortality/NHANES_2013_2014_MORT_2019_PUBLIC.dat",
    setup_url = "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/datalinkage/linked_mortality/R_ReadInProgramAllSurveys.R",
    # Name of the reader function defined in the setup script; change if CDC updates it
    setup_function = "read_nchs_lmf"
  ),
  # Label used in the reportâ€™s Data section
  source_label = "NHANES core (multiple waves)"
)

