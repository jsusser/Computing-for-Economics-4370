# Project: Smoking & Mortality Risk (NHANES 2013–2014)


# Config and packages -------------------------------------
config_dir <- "config"
source(file.path(config_dir, "config.R"))

req_pkgs <- CFG$packages
new <- req_pkgs[!(req_pkgs %in% installed.packages()[, "Package"])]
if (length(new)) {
  repos <- getOption("repos")
  if (length(repos) == 0 || is.null(repos[["CRAN"]]) || identical(unname(repos[["CRAN"]]), "@CRAN@")) {
    options(repos = c(CRAN = "https://cran.rstudio.com"))
  }
  try(install.packages(new, dependencies = TRUE), silent = TRUE)
}

library(tidyverse)
library(broom)
library(sandwich)
library(lmtest)
library(car)
library(ggplot2)
suppressMessages(library(modelsummary))
suppressed <- try(suppressMessages(library(doParallel)), silent = TRUE)

# --- Project directories -------------------------------------------------------
out_dir  <- CFG$dirs$out
res_dir  <- CFG$dirs$res
res_nhanes <- CFG$dirs$res_nhanes
res_mort   <- CFG$dirs$res_mort
utils_dir <- CFG$dirs$utils
core_dir  <- CFG$dirs$core
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(res_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "figures"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "artifacts"), showWarnings = FALSE, recursive = TRUE)

# Set up parallel backend for glmnet cross-validation
cl <- NULL
try({
  ncores <- max(1L, parallel::detectCores() - 1L)
  cl <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)
  message(sprintf("Parallel CV enabled with %d workers", ncores))
}, silent = TRUE)

# Clean old outputs (remove previous artifacts, tables, figures) but keep report_final.tex if present
try({
  old_tabs <- list.files(file.path(out_dir, "tables"), full.names = TRUE, recursive = TRUE)
  old_figs <- list.files(file.path(out_dir, "figures"), full.names = TRUE, recursive = TRUE)
  old_art  <- list.files(file.path(out_dir, "artifacts"), full.names = TRUE, recursive = TRUE)
  if (length(old_tabs)) file.remove(old_tabs)
  if (length(old_figs)) file.remove(old_figs)
  if (length(old_art))  file.remove(old_art)
  # Remove prior report files except report_final.tex
  keep <- file.path(out_dir, "report_final.tex")
  for (f in list.files(out_dir, full.names = TRUE)) {
    if (!identical(normalizePath(f, winslash = "/", mustWork = FALSE), normalizePath(keep, winslash = "/", mustWork = FALSE))) {
      if (grepl("^report\\.(tex|pdf|log|aux|fls|fdb_latexmk)$", basename(f))) try(file.remove(f), silent = TRUE)
    }
  }
}, silent = TRUE)

# Recreate raw assets overview table if missing (for report_final.tex)
if (!file.exists(file.path(out_dir, CFG$files$raw_assets_overview_tex))) {
  if (exists("save_raw_assets_overview_tex")) {
    save_raw_assets_overview_tex(out_dir, CFG, CFG$dirs$res_nhanes, CFG$dirs$res_mort, CFG$data$nhanes_waves)
  }
}

# Rebuild cleaned-data LaTeX artifacts to ensure report_final.tex inputs exist
try(source(file.path("scripts", "make_clean_matrices_only.R")), silent = TRUE)

# Source modular code
source(file.path(utils_dir, "helpers.R"))
source(file.path(utils_dir, "io.R"))
source(file.path(utils_dir, "cleaning.R"))
source(file.path(utils_dir, "data_nhanes.R"))
source(file.path(utils_dir, "reporting.R"))
source(file.path(core_dir,  "modeling.R"))
source(file.path(core_dir,  "modeling_lasso.R"))
source(file.path(core_dir,  "diagnostics.R"))
## Monte Carlo module removed
## NN removed per request; not sourcing modeling_nn.R

# 1) Load cleaned matrices (X, y) ---------------------------------------------

set.seed(CFG$seed)
X_path <- file.path(CFG$dirs$clean, CFG$files$X_csv)
y_path <- file.path(CFG$dirs$clean, CFG$files$y_csv)
if (!file.exists(X_path) || !file.exists(y_path)) {
  stop("Missing clean-data matrices. Run scripts/make_clean_matrices_only.R first.")
}
X <- readr::read_csv(X_path, show_col_types = FALSE)
y_df <- readr::read_csv(y_path, show_col_types = FALSE)
label_var <- names(y_df)[1]
sim_df <- dplyr::bind_cols(X, !!label_var := y_df[[1]])
if (label_var != "risk_index") {
  sim_df$risk_index <- sim_df[[label_var]]
}
CFG$data$source_label <- paste0("Cleaned matrices (label = ", label_var, ") from clean-data/")
# Compute class weights (inverse-prevalence, normalized)
prev <- mean(sim_df$risk_index == 1, na.rm = TRUE)
w_vec <- ifelse(sim_df$risk_index == 1, ifelse(prev>0, 1/prev, 1), ifelse(prev<1, 1/(1-prev), 1))
w_vec <- w_vec / mean(w_vec, na.rm = TRUE)
## Save weights overview table for report
save_class_weights_tex(sim_df$risk_index, out_dir)


# 2) Fit logistic regression (Logit) ------------------------------
results <- fit_logit_model(sim_df, weights = w_vec)

# 3) Robust (HC1) standard errors -----------------------------

cat("\n=== Logit with Robust SEs (HC1) ===\n")
print(results$robust)

# 4) Model fit statistics -------------------------------------

cat("\nModel fit stats:\n")
print(results$fit_stats)

# 5) Multicollinearity check (VIF) ---------------------------

cat("\nVariance Inflation Factors (VIF; linearized):\n")
if (!is.null(results$vif)) print(results$vif) else cat("(not available)\n")

# 6) Diagnostics plots ----------------------------------------
plots <- make_diagnostics_plots(results$model, model_label = "Logit")
p3    <- make_predictions_plot(results$model, sim_df, model_label = "Logit", scale_to_five = TRUE)

print(plots$p1)
print(plots$p2)
print(p3)

save_plots(plots$p1, plots$p2, p3, out_dir)

# 7) Pretty regression table (robust SEs) ---------------------

save_regression_table(results$model, results$vcov_hc, out_dir)
save_regression_table_tex(results$model, results$vcov_hc, out_dir)
cat('\nSaved robust-SE regression tables to output/.\n')

# Save summary stats and VIF for LaTeX report
save_summary_stats_tex(sim_df, out_dir)
save_vif_text(results$vif, out_dir)
save_data_overview_tex(sim_df, out_dir, label = CFG$data$source_label)
## Wave counts omitted per request

# Clean artifacts are produced by scripts/make_clean_matrices_only.R; do not re-clean here
if (file.exists(file.path(CFG$dirs$out, CFG$files$clean_overview_tex))) {
  cat("Using existing cleaned-data LaTeX artifacts from output/.\n")
}

# 8) Smoking coefficients and interpretation hint -------------
report_console(results)

# 9) LASSO with cross-validation (Logistic) -------------------
lasso <- fit_lasso_cv(sim_df, weights = w_vec)
save_lasso_coefs(lasso, out_dir)
save_lasso_coefs_tex(lasso, out_dir)
report_console_lasso(lasso)

# Save CV plot and coefficient comparison
save_lasso_cv_plot(lasso$cvfit, out_dir)
coef_tab <- build_coef_comparison(results, lasso)
save_coef_comparison(coef_tab, out_dir)

# Build multi-panel diagnostics
# Logit panel: residuals vs fitted and QQ
ols_panel_path <- file.path(out_dir, CFG$files$ols_panel)
save_panel_grid(list(plots$p1, plots$p2), ols_panel_path, nrow = 1, ncol = 2)

# LASSO panel: residuals vs fitted (using lambda.min predictions), QQ, and CV error plot
mm_lasso <- model.matrix(risk_index ~ smoker + age + age_sq + sex + log_income, data = sim_df)[, -1, drop = FALSE]
lasso_pred <- as.numeric(predict(lasso$cvfit, newx = mm_lasso, s = "lambda.min", type = "response"))
lasso_resid <- as.numeric(sim_df$risk_index) - lasso_pred
lasso_resid_plot <- make_resid_plot_from_pred(sim_df$risk_index, lasso_pred, model_label = "LASSO")
lasso_qq_plot <- make_qq_from_resid(lasso_resid, model_label = "LASSO")
lasso_cv_plot_gg <- make_lasso_cv_plot_gg(lasso$cvfit)
lasso_panel_path <- file.path(out_dir, CFG$files$lasso_panel)
save_panel_grid(list(lasso_resid_plot, lasso_qq_plot, lasso_cv_plot_gg), lasso_panel_path, nrow = 2, ncol = 2)
## Save individual LASSO diagnostics
dir.create(file.path(out_dir, "figures"), showWarnings = FALSE, recursive = TRUE)
ggplot2::ggsave(filename = file.path(out_dir, CFG$files$lasso_residuals), plot = lasso_resid_plot, width = CFG$plots$width, height = CFG$plots$height, dpi = CFG$plots$dpi)
ggplot2::ggsave(filename = file.path(out_dir, CFG$files$lasso_qq), plot = lasso_qq_plot, width = CFG$plots$width, height = CFG$plots$height, dpi = CFG$plots$dpi)

## NN removed per request

# Classification metrics and plots -------------------------------------------
yy <- as.numeric(sim_df$risk_index)
# Logit preds (probabilities)
logit_prob <- as.numeric(fitted(results$model, type = "response"))
logit_cls <- compute_classification_metrics(yy, logit_prob, threshold = 0.5)
save_single_class_metrics_tex(logit_cls, file.path(out_dir, CFG$files$logit_metrics_in_tex))
# LASSO preds (same formula basis as modeling_lasso.R)
mm_lasso <- model.matrix(risk_index ~ smoker + age + age_sq + sex + log_income, data = sim_df)[, -1, drop = FALSE]
lasso_prob <- as.numeric(predict(lasso$cvfit, newx = mm_lasso, s = "lambda.min", type = "response"))
lasso_cls <- compute_classification_metrics(yy, lasso_prob, threshold = 0.5)
save_single_class_metrics_tex(lasso_cls, file.path(out_dir, CFG$files$lasso_cls_metrics_in_tex))
## ROC/PR/Calibration (full sample)
save_roc_pr_calibration(yy, logit_prob, prefix = "logit", out_dir = out_dir)
save_roc_pr_calibration(yy, lasso_prob, prefix = "regularized_logit", out_dir = out_dir)

# In-sample vs Out-of-sample metrics (consistent 80/20 split) -----
set.seed(CFG$seed)
n_all <- nrow(sim_df)
idx_all <- sample.int(n_all)
n_train <- floor(0.8 * n_all)
tr_idx <- idx_all[1:n_train]
te_idx <- idx_all[(n_train + 1):n_all]

## Logit train/test
w_tr <- w_vec[tr_idx]; logit_tr <- stats::glm(risk_index ~ smoker + age + age_sq + sex + log_income, data = sim_df[tr_idx, , drop = FALSE], family = stats::binomial(), weights = w_tr)
logit_pred_tr <- as.numeric(fitted(logit_tr, type = "response"))
logit_pred_te <- as.numeric(predict(logit_tr, newdata = sim_df[te_idx, , drop = FALSE], type = "response"))
in_logit_cls <- compute_classification_metrics(as.numeric(sim_df$risk_index[tr_idx]), logit_pred_tr, threshold = 0.5)
out_logit_cls <- compute_classification_metrics(as.numeric(sim_df$risk_index[te_idx]), logit_pred_te, threshold = 0.5)

## LASSO train/test
mm_tr <- model.matrix(risk_index ~ smoker + age + age_sq + sex + log_income, data = sim_df[tr_idx, , drop = FALSE])[, -1, drop = FALSE]
mm_te <- model.matrix(risk_index ~ smoker + age + age_sq + sex + log_income, data = sim_df[te_idx, , drop = FALSE])[, -1, drop = FALSE]
yy_tr <- as.numeric(sim_df$risk_index[tr_idx])
yy_te <- as.numeric(sim_df$risk_index[te_idx])
cv_tr <- glmnet::cv.glmnet(x = mm_tr, y = yy_tr, alpha = CFG$model$lasso$alpha, nfolds = CFG$model$lasso$nfolds, standardize = CFG$model$lasso$standardize, family = CFG$model$lasso$family, weights = w_tr, parallel = TRUE)
lasso_pred_tr <- as.numeric(predict(cv_tr, newx = mm_tr, s = "lambda.min", type = "response"))
lasso_pred_te <- as.numeric(predict(cv_tr, newx = mm_te, s = "lambda.min", type = "response"))
in_lasso_cls <- compute_classification_metrics(yy_tr, lasso_pred_tr, threshold = 0.5)
out_lasso_cls <- compute_classification_metrics(yy_te, lasso_pred_te, threshold = 0.5)

# Save per-model in/out tables for report sections
save_single_class_metrics_tex(in_logit_cls, file.path(out_dir, CFG$files$logit_metrics_in_tex))
save_single_class_metrics_tex(out_logit_cls, file.path(out_dir, CFG$files$logit_metrics_out_tex))
save_single_class_metrics_tex(in_lasso_cls, file.path(out_dir, CFG$files$lasso_cls_metrics_in_tex))
save_single_class_metrics_tex(out_lasso_cls, file.path(out_dir, CFG$files$lasso_cls_metrics_out_tex))
## NN per-model metrics removed

# Render LaTeX report (and attempt compile if tinytex is installed)
render_latex_report(out_dir)
# Ensure raw assets overview exists before report_final compilation
if (!file.exists(file.path(out_dir, CFG$files$raw_assets_overview_tex))) {
  if (exists("save_raw_assets_overview_tex")) {
    save_raw_assets_overview_tex(out_dir, CFG, CFG$dirs$res_nhanes, CFG$dirs$res_mort, CFG$data$nhanes_waves)
  }
}

# Sanitize generated tables for inclusion in custom report_final.tex
source(file.path(utils_dir, "io.R"))
sanitize_tables_dir(out_dir)
# Force-recompile PDF so updated figures are included
compile_report(out_dir, force = TRUE)
# Also compile report_final.tex if present, to follow the final structure
final_tex <- file.path(out_dir, "report_final.tex")
if (file.exists(final_tex)) {
  if (requireNamespace("tinytex", quietly = TRUE)) {
    owd <- getwd(); on.exit(setwd(owd), add = TRUE)
    setwd(out_dir)
    try(system2("latexmk", c("-g", "-pdf", basename(final_tex))), silent = TRUE)
    setwd(owd)
  }
}

# Stop cluster if started (MC runs after report but is short; no need to keep cluster)
try(if (!is.null(cl)) parallel::stopCluster(cl), silent = TRUE)

## Monte Carlo removed per request
