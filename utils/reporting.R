report_console <- function(results) {
  cat("\n=== Logit with Robust SEs (HC1) ===\n")
  print(results$robust)
  cat("\nModel fit stats:\n"); print(results$fit_stats)
  if (!is.null(results$vif)) { cat("\nVariance Inflation Factors (VIF; linearized):\n"); print(results$vif) }
  smoke_terms <- extract_smoking_terms(results$model)
  cat("\nSmoking coefficients (conventional SEs):\n"); print(smoke_terms)
}

report_console_lasso <- function(lasso) {
  cat("\n=== LASSO with Cross-Validation ===\n")
  cat(sprintf("lambda.min: %.6f\n", lasso$lambda_min))
  cat(sprintf("lambda.1se: %.6f\n", lasso$lambda_1se))
  cat(sprintf("Nonzero coefficients @ lambda.min: %d\n", nrow(lasso$coefs_min)))
  print(lasso$coefs_min)
  cat(sprintf("\nNonzero coefficients @ lambda.1se: %d\n", nrow(lasso$coefs_1se)))
  print(lasso$coefs_1se)
}

render_latex_report <- function(out_dir = CFG$dirs$out, title = CFG$report$title, cfg = CFG) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  tex_path <- file.path(out_dir, cfg$files$report_tex)
  have <- function(fname) file.exists(file.path(out_dir, fname))
  sanitize <- function(fname) {
    f <- file.path(out_dir, fname)
    if (!file.exists(f)) return(invisible(NULL))
    lines <- try(readLines(f, warn = FALSE), silent = TRUE)
    if (inherits(lines, "try-error")) return(invisible(NULL))
    bad1 <- grepl("^[[:space:]]*\\\\begin", lines)
    bad2 <- grepl("^[[:space:]]*\\\\end", lines)
    bad3 <- grepl("^[[:space:]]*\\\\centering[[:space:]]*$", lines, perl = TRUE)
    keep <- !(bad1 | bad2 | bad3)
    writeLines(lines[keep], f)
    invisible(NULL)
  }
  to_sanitize <- character(0)
  lines <- c(
    "\\documentclass{article}",
    "\\usepackage{graphicx}",
    "\\usepackage{booktabs}",
    "\\usepackage[margin=1in]{geometry}",
    "\\usepackage{caption}",
    "\\usepackage{verbatim}",
    "\\usepackage{float}",
    "\\usepackage{tabularray}",
    "\\usepackage{amsmath}",
    "\\usepackage{siunitx}",
    "\\UseTblrLibrary{booktabs}",
    "\\UseTblrLibrary{siunitx}",
    "\\graphicspath{{.}{figures/}}",
    paste0("\\title{", title, "}"),
    "\\date{\\today}",
    "\\begin{document}",
    "\\maketitle",
    "\\section{Data}",
    "This section details the source datasets, how they are obtained, and the basic shape and missingness patterns. DEMO and SMQ tables are joined on SEQN per NHANES cycle; mortality is linked via public-use files.",
    if (have(cfg$files$raw_assets_overview_tex)) c(paste0("\\input{", cfg$files$raw_assets_overview_tex, "}"), "\\medskip") else NULL,
    paste0("\\input{", cfg$files$data_overview_tex, "}"),
    "\\medskip",
    # Wave counts omitted by design
    "\\section{Cleaned Data}",
    "We construct X and y from the raw join: pick a single label (dead preferred) and drop rows only if the label is missing. Features are imputed (numeric: median + missing flags; categorical: Unknown level); age and PIR are standardized; we add age\\_sq and log\\_income. Categorical factors (smoker, sex) are used directly.",
    paste0("\\input{", cfg$files$clean_overview_tex, "}"),
    "\\noindent \\textbf{Head of Feature Matrix (X)}",
    "\\par",
    paste0("\\input{", cfg$files$head_X_tex, "}"),
    "\\medskip",
    "\\noindent \\textbf{Head of Label Vector (y)}",
    "\\par",
    paste0("\\input{", cfg$files$head_y_tex, "}"),
    "\\medskip",
    if (have(cfg$files$head_cleaned_full_tex)) c(
      "\\noindent \\textbf{Head of Cleaned Full Frame}",
      "\\par",
      paste0("\\input{", cfg$files$head_cleaned_full_tex, "}"),
      "\\medskip"
    ) else NULL,
    if (have(cfg$files$head_meta_tex)) c(
      "\\noindent \\textbf{Head of Meta (IDs and Missing Flags)}",
      "\\par",
      paste0("\\input{", cfg$files$head_meta_tex, "}"),
      "\\medskip"
    ) else NULL,
    "\\noindent \\textbf{Imputation Counts by Variable}",
    "\\par",
    paste0("\\input{", cfg$files$imputation_counts_tex, "}"),
    "\\medskip",
    # Wave counts omitted by design
    "\\section{Feature/Label Dictionaries and Metrics}",
    "We document each feature and the label, then summarize numeric distributions and categorical compositions to contextualize model inputs.",
    if (have(cfg$files$feature_dict_tex)) c(
      "\\noindent \\textbf{Feature Dictionary (X)}",
      "\\par",
      paste0("\\input{", cfg$files$feature_dict_tex, "}"),
      "\\medskip"
    ) else NULL,
    if (have(cfg$files$y_dict_tex)) c(
      "\\noindent \\textbf{Label Dictionary (y)}",
      "\\par",
      paste0("\\input{", cfg$files$y_dict_tex, "}"),
      "\\medskip"
    ) else NULL,
    if (have(cfg$files$x_metrics_numeric_tex)) c(
      "\\noindent \\textbf{X Metrics (Numeric)}",
      "\\par",
      paste0("\\input{", cfg$files$x_metrics_numeric_tex, "}"),
      "\\medskip"
    ) else NULL,
    if (have(cfg$files$x_metrics_categorical_tex)) c(
      "\\noindent \\textbf{X Metrics (Categorical)}",
      "\\par",
      paste0("\\input{", cfg$files$x_metrics_categorical_tex, "}"),
      "\\medskip"
    ) else NULL,
    if (have(cfg$files$y_metrics_tex)) c(
      "\\noindent \\textbf{y Metrics}",
      "\\par",
      paste0("\\input{", cfg$files$y_metrics_tex, "}"),
      "\\medskip"
    ) else NULL,
    "\\subsection*{Class Imbalance and Class Weights}",
    "Let $\\pi = P(y=1)$ denote the prevalence of deaths. When $y$ is imbalanced ($\\pi$ small), unweighted logistic loss can prioritize the majority class.",
    " A common remedy is to use inverse-prevalence weights: $w_1 = 1/\\pi$ for positives and $w_0 = 1/(1-\\pi)$ for negatives, often normalized so the average weight equals one.",
    " Weighted log-likelihood becomes $L(\\beta) = -\\sum_i w_i\\,[y_i \\log p_i + (1-y_i) \\log(1-p_i)]$, with $p_i = \\text{logit}^{-1}(X_i\\beta)$.",
    if (have(cfg$files$class_weights_tex)) c(
      "\\noindent \\textbf{Prevalence and Suggested Class Weights}",
      "\\par",
      paste0("\\input{", cfg$files$class_weights_tex, "}"),
      "\\medskip"
    ) else NULL,
    "\\section{Transformations}",
    "This section explains the transformation logic and motivations: label-only dropping preserves sample size; standardized numerics help comparability; categorical factors (smoker, sex) are used directly; smoker categories follow CDC coding of SMQ020/040.",
    paste0("\\input{", cfg$files$processing_notes_tex, "}"),
    "\\noindent \\textbf{Smoker Categories (Counts)}",
    "\\par",
    paste0("\\input{", cfg$files$smoker_counts_tex, "}"),
    "\\medskip",
    "\\section{Models}",
    "We estimate two logistic models on the cleaned matrices: (1) a baseline weighted logit with HC1-robust standard errors, and (2) a weighted regularized logit selected by cross-validation.",
    "\\subsection*{Model 1: Logit (Weighted, HC1 robust)}",
    "\\noindent Math: $\\displaystyle y = \\beta_0 + \\beta_1 \\cdot 1\\{smoker=Former\\} + \\beta_2 \\cdot 1\\{smoker=Current\\} + \\beta_3 age + \\beta_4 1\\{sex=male\\} + \\beta_5 income + \\varepsilon$.",
    "\\par\\noindent Coefficients (robust SEs):",
    paste0("\\input{", cfg$files$table_tex, "}"),
    "\\medskip",
    "\\noindent \\textbf{Classification metrics (in-sample)}",
    "\\par",
    paste0("\\input{", cfg$files$logit_metrics_in_tex, "}"),
    "\\medskip",
    "\\noindent \\textbf{Classification metrics (out-of-sample)}",
    "\\par",
    paste0("\\input{", cfg$files$logit_metrics_out_tex, "}"),
    "\\medskip",
    "\\subsection*{Model 2: Regularized Logit (CV, Weighted)}",
    "\\noindent Math: $\\displaystyle \\hat{\\beta} = \\arg\\min_{\\beta} \\; \\frac{1}{2n} \\lVert y - X\\beta \\rVert_2^2 + \\lambda \\lVert \\beta \\rVert_1$, with $\\lambda$ chosen by cross-validation.",
    "\\par\\noindent Nonzero coefficients:",
    paste0("\\input{", cfg$files$lasso_coefs_tex, "}"),
    "\\medskip",
    "\\noindent \\textbf{Classification metrics (in-sample)}",
    "\\par",
    paste0("\\input{", cfg$files$lasso_cls_metrics_in_tex, "}"),
    "\\medskip",
    "\\noindent \\textbf{Classification metrics (out-of-sample)}",
    "\\par",
    paste0("\\input{", cfg$files$lasso_cls_metrics_out_tex, "}"),
    "\\medskip",
    
    "\\section{Diagnostics}",
    "We start with multicollinearity (VIF), then show per-model diagnostic plots.",
    "\\paragraph{Variance Inflation Factors (VIF).} ",
    paste0("\\verbatiminput{", cfg$files$vif, "}"),
    "\\subsection*{Model 1: Logit}",
    "We estimate a weighted logistic regression; residual plots use deviance residuals. Predicted probabilities are also displayed as a 1--5 risk index.",
    "\\begin{figure}[h]\\centering",
    paste0("\\includegraphics[width=0.75\\textwidth]{", cfg$files$residuals, "}"),
    "\\caption{Logit: Residuals vs Fitted}\\end{figure}",
    "\\begin{figure}[h]\\centering",
    paste0("\\includegraphics[width=0.75\\textwidth]{", cfg$files$qq, "}"),
    "\\caption{Logit: Normal Q-Q Plot}\\end{figure}",
    "\\begin{figure}[h]\\centering",
    paste0("\\includegraphics[width=0.75\\textwidth]{", cfg$files$preds, "}"),
    "\\caption{Logit: Predicted Risk by Smoking Status (scaled 1--5)}\\end{figure}",
    if (have(cfg$files$logit_roc)) c(
      "\\begin{figure}[h]\\centering",
      paste0("\\includegraphics[width=0.75\\textwidth]{", cfg$files$logit_roc, "}"),
      "\\caption{Logit: ROC Curve}\\end{figure}"
    ) else NULL,
    if (have(cfg$files$logit_pr)) c(
      "\\begin{figure}[h]\\centering",
      paste0("\\includegraphics[width=0.75\\textwidth]{", cfg$files$logit_pr, "}"),
      "\\caption{Logit: Precision--Recall Curve}\\end{figure}"
    ) else NULL,
    if (have(cfg$files$logit_cal)) c(
      "\\begin{figure}[h]\\centering",
      paste0("\\includegraphics[width=0.75\\textwidth]{", cfg$files$logit_cal, "}"),
      "\\caption{Logit: Calibration by Risk Quintile}\\end{figure}"
    ) else NULL,
    "\\subsection*{Model 2: Regularized Logit (CV)}",
    "We fit a logistic regression with an $L_1$ penalty and 10-fold cross-validation, using the same class weights as Model 1. Residuals/QQ use predictions at $\\lambda_{min}$.",
    "\\begin{figure}[h]\\centering",
    paste0("\\includegraphics[width=0.75\\textwidth]{", cfg$files$lasso_residuals, "}"),
    "\\caption{Regularized Logit: Residuals vs Fitted (lambda.min)}\\end{figure}",
    "\\begin{figure}[h]\\centering",
    paste0("\\includegraphics[width=0.75\\textwidth]{", cfg$files$lasso_qq, "}"),
    "\\caption{Regularized Logit: Normal Q-Q Plot (lambda.min)}\\end{figure}",
    "\\begin{figure}[h]\\centering",
    paste0("\\includegraphics[width=0.75\\textwidth]{", cfg$files$lasso_cv_plot, "}"),
    "\\caption{Regularized Logit: CV Error vs Lambda (glmnet)}\\end{figure}",
    if (have(cfg$files$lasso_roc)) c(
      "\\begin{figure}[h]\\centering",
      paste0("\\includegraphics[width=0.75\\textwidth]{", cfg$files$lasso_roc, "}"),
      "\\caption{Regularized Logit: ROC Curve}\\end{figure}"
    ) else NULL,
    if (have(cfg$files$lasso_pr)) c(
      "\\begin{figure}[h]\\centering",
      paste0("\\includegraphics[width=0.75\\textwidth]{", cfg$files$lasso_pr, "}"),
      "\\caption{Regularized Logit: Precision--Recall Curve}\\end{figure}"
    ) else NULL,
    if (have(cfg$files$lasso_cal)) c(
      "\\begin{figure}[h]\\centering",
      paste0("\\includegraphics[width=0.75\\textwidth]{", cfg$files$lasso_cal, "}"),
      "\\caption{Regularized Logit: Calibration by Risk Quintile}\\end{figure}"
    ) else NULL,
    
    # Model comparison tables omitted; we report per-model in/out metrics above.
    "\\section{Coefficient Comparison}",
    "\\noindent \\textbf{Logit vs Regularized Logit (lambda.min and lambda.1se)}",
    "\\par",
    paste0("\\input{", cfg$files$coef_compare_tex, "}"),
    "\\medskip",
    # Monte Carlo section removed by design
    "\\end{document}"
  )
  writeLines(lines, tex_path)
  invisible(tex_path)
}

compile_report <- function(out_dir = CFG$dirs$out, cfg = CFG, force = FALSE) {
  tex_path <- file.path(out_dir, cfg$files$report_tex)
  if (!file.exists(tex_path)) return(invisible(FALSE))
  if (requireNamespace("tinytex", quietly = TRUE)) {
    if (force) {
      owd <- getwd(); on.exit(setwd(owd), add = TRUE)
      setwd(out_dir)
      try(system2("latexmk", c("-g", "-pdf", basename(tex_path))), silent = TRUE)
    } else {
      try(tinytex::latexmk(tex_path), silent = TRUE)
    }
  }
  invisible(TRUE)
}


