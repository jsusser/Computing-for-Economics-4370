save_simulated_data <- function(df, res_dir = CFG$dirs$res, filename = CFG$files$simulated_data) {
  dir.create(res_dir, showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(df, file.path(res_dir, filename))
}

save_plots <- function(p1, p2, p3, out_dir = CFG$dirs$out, cfg = CFG) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  ggplot2::ggsave(file.path(out_dir, cfg$files$residuals), p1, width = cfg$plots$width, height = cfg$plots$height, dpi = cfg$plots$dpi)
  ggplot2::ggsave(file.path(out_dir, cfg$files$qq),         p2, width = cfg$plots$width, height = cfg$plots$height, dpi = cfg$plots$dpi)
  ggplot2::ggsave(file.path(out_dir, cfg$files$preds),      p3, width = cfg$plots$width, height = cfg$plots$height, dpi = cfg$plots$dpi)
}

save_panel_grid <- function(plots, out_path, nrow = 1, ncol = length(plots), cfg = CFG) {
  dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
  # Open device
  grDevices::png(filename = out_path, width = cfg$plots$width * 100, height = cfg$plots$height * 100, units = "px", res = cfg$plots$dpi)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()
  # Create layout
  push_vp <- function(row, col) {
    grid::pushViewport(grid::viewport(layout.pos.row = row, layout.pos.col = col))
  }
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow, ncol)))
  k <- 1
  for (i in seq_len(nrow)) {
    for (j in seq_len(ncol)) {
      if (k <= length(plots)) {
        push_vp(i, j)
        p <- plots[[k]]
        # Coerce ggplot to grob and draw
        if (inherits(p, "ggplot")) {
          g <- ggplot2::ggplotGrob(p)
          grid::grid.draw(g)
        } else {
          # Assume already a grob
          grid::grid.draw(p)
        }
        grid::upViewport()
      }
      k <- k + 1
    }
  }
  grid::upViewport()
  invisible(out_path)
}

save_regression_table <- function(model, vcov_hc, out_dir = CFG$dirs$out, cfg = CFG) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  title <- "OLS: risk_index"
  if (inherits(model, "glm")) {
    fam <- tryCatch(model$family$family, error = function(e) NULL)
    if (!is.null(fam) && fam == "binomial") title <- "Logit: risk_index"
  }
  modelsummary::msummary(
    list(title = model),
    vcov = vcov_hc,
    stars = cfg$tables$stars,
    gof_omit = "IC|Log|F$",
    output = file.path(out_dir, cfg$files$table_txt)
  )
}

save_regression_table_tex <- function(model, vcov_hc, out_dir = CFG$dirs$out, cfg = CFG) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  title <- "OLS: risk_index"
  if (inherits(model, "glm")) {
    fam <- tryCatch(model$family$family, error = function(e) NULL)
    if (!is.null(fam) && fam == "binomial") title <- "Logit: risk_index"
  }
  modelsummary::msummary(
    list(title = model),
    vcov = vcov_hc,
    stars = cfg$tables$stars,
    gof_omit = "IC|Log|F$",
    output = file.path(out_dir, cfg$files$table_tex)
  )
}

save_summary_stats_tex <- function(df, out_dir = CFG$dirs$out, cfg = CFG) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  # Numeric variables summary
  suppressWarnings(
    modelsummary::datasummary_skim(
      df,
      type = "numeric",
      histogram = FALSE,
      output = file.path(out_dir, cfg$files$summary_numeric)
    )
  )
  # Categorical variables summary
  suppressWarnings(
    modelsummary::datasummary_skim(
      df,
      type = "categorical",
      histogram = FALSE,
      output = file.path(out_dir, cfg$files$summary_categorical)
    )
  )
}

save_vif_text <- function(vif_obj, out_dir = CFG$dirs$out, cfg = CFG) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  vif_path <- file.path(out_dir, cfg$files$vif)
  capture.output(print(vif_obj), file = vif_path)
  invisible(vif_path)
}

save_lasso_coefs <- function(lasso, out_dir = CFG$dirs$out, cfg = CFG) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  txt_path <- file.path(out_dir, cfg$files$lasso_coefs_txt)
  cat("LASSO (CV) coefficients at lambda.min = ", lasso$lambda_min, "\n", sep = "", file = txt_path)
  capture.output(print(lasso$coefs_min), append = TRUE, file = txt_path)
  cat("\nLASSO (CV) coefficients at lambda.1se = ", lasso$lambda_1se, "\n", sep = "", append = TRUE, file = txt_path)
  capture.output(print(lasso$coefs_1se), append = TRUE, file = txt_path)
  invisible(txt_path)
}

save_lasso_coefs_tex <- function(lasso, out_dir = CFG$dirs$out, cfg = CFG) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  # Combine into a single table with a column indicating lambda
  df_min <- dplyr::mutate(lasso$coefs_min, lambda = "lambda.min")
  df_1se <- dplyr::mutate(lasso$coefs_1se, lambda = "lambda.1se")
  tab <- dplyr::bind_rows(df_min, df_1se) |>
    dplyr::select(lambda, term, estimate)
  suppressWarnings(
    modelsummary::datasummary_df(
      tab,
      output = file.path(out_dir, cfg$files$lasso_coefs_tex)
    )
  )
}

save_lasso_cv_plot <- function(cvfit, out_dir = CFG$dirs$out, cfg = CFG) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(out_dir, cfg$files$lasso_cv_plot)
  grDevices::png(filename = path, width = cfg$plots$width, height = cfg$plots$height, units = "in", res = cfg$plots$dpi)
  on.exit(grDevices::dev.off(), add = TRUE)
  plot(cvfit)
  invisible(path)
}

save_coef_comparison <- function(tab, out_dir = CFG$dirs$out, cfg = CFG) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  # Text
  txt_path <- file.path(out_dir, cfg$files$coef_compare_txt)
  capture.output(print(tab), file = txt_path)
  # LaTeX
  suppressWarnings(
    modelsummary::datasummary_df(
      tab,
      output = file.path(out_dir, cfg$files$coef_compare_tex)
    )
  )
  invisible(list(txt = txt_path,
                 tex = file.path(out_dir, cfg$files$coef_compare_tex)))
}

# Simple combined LaTeX for LASSO MC (selection + nonzero summary)
save_nn_metrics <- function(nn_res, out_dir = CFG$dirs$out, cfg = CFG) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  tab <- tibble::tibble(
    metric = c("Accuracy", "LogLoss", "MSE", "R2"),
    value = c(nn_res$acc, nn_res$logloss, nn_res$mse, nn_res$r2)
  )
  suppressWarnings(
    modelsummary::datasummary_df(
      tab,
      output = file.path(out_dir, cfg$files$nn_metrics_tex)
    )
  )
  # Text summary
  txt_path <- file.path(out_dir, cfg$files$nn_summary_txt)
  capture.output(print(tab), file = txt_path)
  invisible(list(tex = file.path(out_dir, cfg$files$nn_metrics_tex), txt = txt_path))
}

save_model_metrics_tex <- function(metrics_list, out_dir = CFG$dirs$out, cfg = CFG) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  # Build dynamically for any models present
  all_metrics <- c("MSE", "RMSE", "MAE", "R2")
  model_names <- names(metrics_list)
  rows <- lapply(all_metrics, function(m) {
    row <- list(Metric = m)
    for (nm in model_names) {
      row[[nm]] <- if (!is.null(metrics_list[[nm]][[m]])) metrics_list[[nm]][[m]] else NA_real_
    }
    row
  })
  tab <- do.call(rbind, lapply(rows, function(r) as.data.frame(r, stringsAsFactors = FALSE)))
  for (nm in setdiff(names(tab), "Metric")) tab[[nm]] <- as.numeric(tab[[nm]])
  suppressWarnings(
    modelsummary::datasummary_df(
      tab,
      output = file.path(out_dir, cfg$files$model_metrics_tex)
    )
  )
  # Text
  txt <- capture.output(print(tab))
  writeLines(txt, file.path(out_dir, cfg$files$model_metrics_txt))
  invisible(list(tex = file.path(out_dir, cfg$files$model_metrics_tex), txt = file.path(out_dir, cfg$files$model_metrics_txt)))
}

save_in_out_model_metrics_tex <- function(in_metrics, out_metrics, out_dir = CFG$dirs$out, cfg = CFG) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  # Helper to build a table from a named list of lists
  build_tab <- function(m) {
    all_metrics <- c("MSE", "RMSE", "MAE", "R2")
    model_names <- names(m)
    rows <- lapply(all_metrics, function(k) {
      row <- list(Metric = k)
      for (nm in model_names) row[[nm]] <- if (!is.null(m[[nm]][[k]])) m[[nm]][[k]] else NA_real_
      row
    })
    tab <- do.call(rbind, lapply(rows, function(r) as.data.frame(r, stringsAsFactors = FALSE)))
    for (nm in setdiff(names(tab), "Metric")) tab[[nm]] <- as.numeric(tab[[nm]])
    tab
  }
  tab_in <- build_tab(in_metrics)
  tab_out <- build_tab(out_metrics)
  suppressWarnings(modelsummary::datasummary_df(tab_in, output = file.path(out_dir, cfg$files$model_metrics_in_tex)))
  suppressWarnings(modelsummary::datasummary_df(tab_out, output = file.path(out_dir, cfg$files$model_metrics_out_tex)))
  invisible(list(in_sample = file.path(out_dir, cfg$files$model_metrics_in_tex), out_sample = file.path(out_dir, cfg$files$model_metrics_out_tex)))
}

save_single_model_metrics_tex <- function(metrics, out_path) {
  dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
  rows <- data.frame(
    Metric = c("MSE", "RMSE", "MAE", "R2"),
    Value = c(
      if (!is.null(metrics$MSE)) metrics$MSE else NA_real_,
      if (!is.null(metrics$RMSE)) metrics$RMSE else NA_real_,
      if (!is.null(metrics$MAE)) metrics$MAE else NA_real_,
      if (!is.null(metrics$R2)) metrics$R2 else NA_real_
    ),
    stringsAsFactors = FALSE
  )
  suppressWarnings(modelsummary::datasummary_df(rows, output = out_path))
  invisible(out_path)
}

# --- Classification metrics and plots (logit) -------------------------------

compute_classification_metrics <- function(y_true, y_prob, threshold = 0.5) {
  y_true <- as.numeric(y_true)
  y_prob <- as.numeric(y_prob)
  ok <- is.finite(y_true) & is.finite(y_prob)
  y_true <- y_true[ok]
  y_prob <- y_prob[ok]
  # Accuracy at threshold
  y_hat <- as.integer(y_prob >= threshold)
  acc <- mean(y_hat == y_true)
  # LogLoss (cross-entropy)
  eps <- 1e-15
  p <- pmin(pmax(y_prob, eps), 1 - eps)
  logloss <- -mean(y_true * log(p) + (1 - y_true) * log(1 - p))
  # Brier score
  brier <- mean((y_true - y_prob)^2)
  # ROC AUC
  auc <- tryCatch({
    roc_obj <- pROC::roc(response = y_true, predictor = y_prob, quiet = TRUE)
    as.numeric(pROC::auc(roc_obj))
  }, error = function(e) NA_real_)
  # PR AUC
  pr_auc <- tryCatch({
    pr <- PRROC::pr.curve(scores.class0 = y_prob[y_true == 1], scores.class1 = y_prob[y_true == 0], curve = FALSE)
    as.numeric(pr$auc.integral)
  }, error = function(e) NA_real_)
  list(Accuracy = acc, LogLoss = logloss, Brier = brier, ROC_AUC = auc, PR_AUC = pr_auc)
}

save_single_class_metrics_tex <- function(metrics, out_path) {
  dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
  rows <- data.frame(
    Metric = c("Accuracy", "LogLoss", "Brier", "ROC_AUC", "PR_AUC"),
    Value = c(
      metrics$Accuracy, metrics$LogLoss, metrics$Brier, metrics$ROC_AUC, metrics$PR_AUC
    ),
    stringsAsFactors = FALSE
  )
  suppressWarnings(modelsummary::datasummary_df(rows, output = out_path))
  invisible(out_path)
}

save_in_out_class_metrics_tex <- function(in_metrics, out_metrics, out_dir = CFG$dirs$out, cfg = CFG,
                                          in_path, out_path) {
  # Save per-model in/out tables
  save_single_class_metrics_tex(in_metrics, file.path(out_dir, in_path))
  save_single_class_metrics_tex(out_metrics, file.path(out_dir, out_path))
  invisible(list(in_sample = file.path(out_dir, in_path), out_sample = file.path(out_dir, out_path)))
}

save_roc_pr_calibration <- function(y_true, y_prob, prefix = c("logit", "regularized_logit"),
                                    out_dir = CFG$dirs$out, cfg = CFG) {
  prefix <- match.arg(prefix)
  dir.create(file.path(out_dir, "figures"), showWarnings = FALSE, recursive = TRUE)
  # ROC
  roc_obj <- tryCatch({ pROC::roc(y_true, y_prob, quiet = TRUE) }, error = function(e) NULL)
  if (!is.null(roc_obj)) {
    df_roc <- data.frame(tpr = roc_obj$sensitivities, fpr = 1 - roc_obj$specificities)
    p_roc <- ggplot2::ggplot(df_roc, ggplot2::aes(x = fpr, y = tpr)) +
      ggplot2::geom_line() +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2) +
      ggplot2::labs(title = paste("ROC:", prefix), x = "False Positive Rate", y = "True Positive Rate")
    f_roc <- if (prefix == "logit") cfg$files$logit_roc else cfg$files$lasso_roc
    ggplot2::ggsave(filename = file.path(out_dir, f_roc), plot = p_roc, width = cfg$plots$width, height = cfg$plots$height, dpi = cfg$plots$dpi)
  }
  # PR
  pr <- tryCatch({ PRROC::pr.curve(scores.class0 = y_prob[y_true == 1], scores.class1 = y_prob[y_true == 0], curve = TRUE) }, error = function(e) NULL)
  if (!is.null(pr)) {
    df_pr <- data.frame(recall = pr$curve[,1], precision = pr$curve[,2])
    p_pr <- ggplot2::ggplot(df_pr, ggplot2::aes(x = recall, y = precision)) +
      ggplot2::geom_line() +
      ggplot2::labs(title = paste("PR Curve:", prefix), x = "Recall", y = "Precision")
    f_pr <- if (prefix == "logit") cfg$files$logit_pr else cfg$files$lasso_pr
    ggplot2::ggsave(filename = file.path(out_dir, f_pr), plot = p_pr, width = cfg$plots$width, height = cfg$plots$height, dpi = cfg$plots$dpi)
  }
  # Calibration (quintiles; also maps to 1-5 scale)
  q <- cut(y_prob, breaks = quantile(y_prob, probs = seq(0,1, by = 0.2), na.rm = TRUE), include.lowest = TRUE, labels = FALSE)
  calib <- stats::aggregate(list(pred = y_prob, obs = y_true), by = list(bin = q), FUN = mean)
  calib$bin <- factor(calib$bin, levels = sort(unique(calib$bin)))
  p_cal <- ggplot2::ggplot(calib, ggplot2::aes(x = bin, y = pred)) +
    ggplot2::geom_point(color = "steelblue", size = 3) +
    ggplot2::geom_point(ggplot2::aes(y = obs), color = "firebrick", size = 3) +
    ggplot2::labs(title = paste("Calibration (quintiles):", prefix), x = "Risk bin (1=low ... 5=high)", y = "Mean probability")
  f_cal <- if (prefix == "logit") cfg$files$logit_cal else cfg$files$lasso_cal
  ggplot2::ggsave(filename = file.path(out_dir, f_cal), plot = p_cal, width = cfg$plots$width, height = cfg$plots$height, dpi = cfg$plots$dpi)
  invisible(TRUE)
}

save_data_overview_tex <- function(df, out_dir = CFG$dirs$out, label = CFG$data$source_label, cfg = CFG, res_dir = CFG$dirs$res, waves = CFG$data$nhanes_waves) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  n <- nrow(df)
  k <- ncol(df)
  total_na <- sum(is.na(df))
  total_cells <- n * k
  pct_na <- if (total_cells > 0) total_na / total_cells else 0

  # Describe NHANES waves and assets if available
  wave_years <- function(code) {
    switch(toupper(as.character(code)),
      H = "2013--2014",
      I = "2015--2016",
      J = "2017--2018",
      K = "2017--Mar 2020",
      NA_character_
    )
  }
  waves <- unique(waves)
  waves <- waves[!is.na(waves)]
  wave_desc <- character(0)
  if (length(waves)) {
    pairs <- vapply(waves, function(w) {
      yy <- wave_years(w)
      if (is.na(yy)) yy <- "unknown years"
      paste0(w, " (", yy, ")")
    }, character(1))
    wave_desc <- paste(pairs, collapse = ", ")
  }

  # List raw files present
  files_present <- character(0)
  try({
    for (w in waves) {
      demo_path <- file.path(res_dir, paste0("DEMO_", w, ".csv"))
      smq_path  <- file.path(res_dir, paste0("SMQ_",  w, ".csv"))
      if (file.exists(demo_path)) files_present <- c(files_present, basename(demo_path))
      if (file.exists(smq_path))  files_present <- c(files_present,  basename(smq_path))
    }
    # Mortality assets
    mort_dat   <- file.path(res_dir, cfg$data$mortality$dat_file)
    mort_setup <- file.path(res_dir, cfg$data$mortality$setup_file)
    if (file.exists(mort_dat))   files_present <- c(files_present, basename(mort_dat))
    if (file.exists(mort_setup)) files_present <- c(files_present, basename(mort_setup))
  }, silent = TRUE)

  overview_lines <- c(
    "\\noindent ",
    paste0("\\textbf{Data source:} ", label, "\\\\"),
    if (length(wave_desc)) paste0("\\textbf{NHANES cycles:} ", wave_desc, "\\\\") else NULL,
    if (length(files_present)) paste0("\\textbf{Raw files in resources/:} ", paste(files_present, collapse = ", "), "\\\\") else NULL,
    paste0("\\textbf{Observations (n):} ", n, "\\\\"),
    paste0("\\textbf{Variables (k):} ", k, "\\\\"),
    paste0("\\textbf{Total missing cells:} ", total_na, " (", sprintf("%.2f", 100*pct_na), "\\%)")
  )
  writeLines(overview_lines, file.path(out_dir, cfg$files$data_overview_tex))

  # Missingness by variable table
  miss_tab <- tibble::tibble(
    variable = names(df),
    n_missing = vapply(df, function(x) sum(is.na(x)), integer(1)),
    pct_missing = vapply(df, function(x) mean(is.na(x)), numeric(1))
  )
  miss_tab$pct_missing <- round(100*miss_tab$pct_missing, 2)
  suppressWarnings(
    modelsummary::datasummary_df(
      miss_tab,
      output = file.path(out_dir, cfg$files$missing_by_var_tex)
    )
  )

  invisible(list(
    overview_tex = file.path(out_dir, cfg$files$data_overview_tex),
    missing_tex  = file.path(out_dir, cfg$files$missing_by_var_tex)
  ))
}

save_clean_overview_tex <- function(meta, out_dir = CFG$dirs$out, cfg = CFG) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  dropped_cols <- names(meta$na_by_col[meta$na_by_col > 0])
  esc <- function(x) {
    x <- as.character(x)
    x <- gsub("_", "\\_", x, fixed = TRUE)
    x
  }
  feats <- esc(meta$features)
  dropped_cols_esc <- esc(dropped_cols)
  std_names <- esc(names(meta$std_stats))
  lines <- c(
    "\\noindent ",
    paste0("\\textbf{Label variable:} ", esc(meta$label), "\\\\"),
    paste0("\\textbf{Features used:} ", paste(feats, collapse = ", "), "\\\\"),
    paste0("\\textbf{Initial n:} ", meta$initial_n, "; \\textbf{Dropped on label only:} ", meta$dropped_n, "; \\textbf{Final n:} ", meta$final_n, "\\\\"),
    if (length(dropped_cols_esc)) paste0("\\textbf{Columns with missing (pre-impute counts):} ", paste(dropped_cols_esc, collapse = ", "), "\\\\") else NULL,
    "\\textbf{Modeled numeric features:} age (standardized), age\\_sq, log\\_income (from PIR; raw PIR is standardized for preprocessing but not used directly in modeling)",
    "\\textbf{Categoricals in X:} smoker and sex as factors. Race and education are excluded from X and kept only for descriptive/meta.",
    if ("smoker" %in% names(meta$head_X)) paste0("\\textbf{Smoker categories:} Never/Former/Current coded as 1/2/3 in smoker\\_code (counts shown in summary stats).") else NULL
  )
  writeLines(lines, file.path(out_dir, cfg$files$clean_overview_tex))

  # Save heads as LaTeX tables
  suppressWarnings(
    modelsummary::datasummary_df(meta$head_X, output = file.path(out_dir, cfg$files$head_X_tex))
  )
  suppressWarnings(
    modelsummary::datasummary_df(meta$head_y, output = file.path(out_dir, cfg$files$head_y_tex))
  )
  invisible(list(
    clean_overview_tex = file.path(out_dir, cfg$files$clean_overview_tex),
    head_X_tex = file.path(out_dir, cfg$files$head_X_tex),
    head_y_tex = file.path(out_dir, cfg$files$head_y_tex)
  ))
}

# Save heads for cleaned_full and meta data frames
save_heads_clean_and_meta_tex <- function(cleaned_df, meta_csv_path, out_dir = CFG$dirs$out, cfg = CFG) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  # cleaned_full head
  suppressWarnings(
    modelsummary::datasummary_df(utils::head(cleaned_df, 6), output = file.path(out_dir, cfg$files$head_cleaned_full_tex))
  )
  # meta head (read from disk)
  if (file.exists(meta_csv_path)) {
    meta_df <- readr::read_csv(meta_csv_path, show_col_types = FALSE)
    suppressWarnings(
      modelsummary::datasummary_df(utils::head(meta_df, 6), output = file.path(out_dir, cfg$files$head_meta_tex))
    )
  }
  invisible(list(
    head_cleaned_full_tex = file.path(out_dir, cfg$files$head_cleaned_full_tex),
    head_meta_tex = file.path(out_dir, cfg$files$head_meta_tex)
  ))
}

# Feature and label dictionaries
save_feature_dictionary_tex <- function(X_cols, out_dir = CFG$dirs$out, cfg = CFG) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  describe <- function(col) {
    if (col == "age") return("Age (years), standardized")
    if (col == "age_sq") return("Age squared, from standardized age")
    if (col == "sex") return("Sex factor: female/male")
    if (col == "income") return("Income-to-poverty ratio (PIR), standardized")
    if (col == "log_income") return("log1p(PIR) from imputed income")
    if (col == "poverty_lt1") return("Indicator: 1 if PIR < 1, else 0")
    if (col == "smoker") return("Smoking status factor: Never/Former/Current from SMQ020/040")
    if (col == "smoker_code") return("Ordinal smoking code: Never=1, Former=2, Current=3; 0 if imputed")
            return("Derived/engineered feature")
  }
  dict <- tibble::tibble(
    column = X_cols,
    description = vapply(X_cols, describe, character(1))
  )
  suppressWarnings(
    modelsummary::datasummary_df(dict, output = file.path(out_dir, cfg$files$feature_dict_tex))
  )
  invisible(file.path(out_dir, cfg$files$feature_dict_tex))
}

save_y_dictionary_tex <- function(label_var, out_dir = CFG$dirs$out, cfg = CFG) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  desc <- switch(label_var,
    dead = "Mortality status from public-use linked mortality file (MORTSTAT==1)",
    risk_index = "Simulated mortality risk index (1â€“5)",
    smoker_code = "Smoking ordinal code (Never=1, Former=2, Current=3)",
    paste0("Label variable: ", label_var)
  )
  tab <- tibble::tibble(label = label_var, description = desc)
  suppressWarnings(
    modelsummary::datasummary_df(tab, output = file.path(out_dir, cfg$files$y_dict_tex))
  )
  invisible(file.path(out_dir, cfg$files$y_dict_tex))
}

# Metrics for X and y
save_X_metrics_tex <- function(X, out_dir = CFG$dirs$out, cfg = CFG) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  # Numeric summary
  num_cols <- names(X)[vapply(X, is.numeric, logical(1))]
  if (length(num_cols)) {
    suppressWarnings(
      modelsummary::datasummary_skim(
        X[, num_cols, drop = FALSE],
        type = "numeric",
        histogram = FALSE,
        output = file.path(out_dir, cfg$files$x_metrics_numeric_tex)
      )
    )
  }
  # Categorical summary
  cat_cols <- setdiff(names(X), num_cols)
  if (length(cat_cols)) {
    suppressWarnings(
      modelsummary::datasummary_skim(
        X[, cat_cols, drop = FALSE],
        type = "categorical",
        histogram = FALSE,
        output = file.path(out_dir, cfg$files$x_metrics_categorical_tex)
      )
    )
  }
  invisible(list(
    numeric = file.path(out_dir, cfg$files$x_metrics_numeric_tex),
    categorical = file.path(out_dir, cfg$files$x_metrics_categorical_tex)
  ))
}

save_y_metrics_tex <- function(y, out_dir = CFG$dirs$out, cfg = CFG) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  if (is.logical(y) || is.numeric(y) && all(y %in% c(0,1,NA))) {
    # Binary counts
    tab <- as.data.frame(table(y, useNA = "ifany"))
    names(tab) <- c("y", "count")
    suppressWarnings(
      modelsummary::datasummary_df(tibble::as_tibble(tab), output = file.path(out_dir, cfg$files$y_metrics_tex))
    )
  } else if (is.numeric(y)) {
    suppressWarnings(
      modelsummary::datasummary_skim(
        tibble::tibble(y = y),
        type = "numeric",
        histogram = FALSE,
        output = file.path(out_dir, cfg$files$y_metrics_tex)
      )
    )
  } else {
    tab <- as.data.frame(table(y, useNA = "ifany"))
    names(tab) <- c("y", "count")
    suppressWarnings(
      modelsummary::datasummary_df(tibble::as_tibble(tab), output = file.path(out_dir, cfg$files$y_metrics_tex))
    )
  }
  invisible(file.path(out_dir, cfg$files$y_metrics_tex))
}

# Compute and save class imbalance and suggested class weights for logistic loss
save_class_weights_tex <- function(y, out_dir = CFG$dirs$out, cfg = CFG) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  yn <- y
  yn <- as.numeric(yn)
  yn <- yn[!is.na(yn)]
  n <- length(yn)
  n1 <- sum(yn == 1)
  n0 <- sum(yn == 0)
  prev <- if (n > 0) n1 / n else NA_real_
  # Inverse-prevalence weights
  w1 <- if (!is.na(prev) && prev > 0) 1 / prev else NA_real_
  w0 <- if (!is.na(prev) && prev < 1) 1 / (1 - prev) else NA_real_
  # Normalize to average weight = 1
  denom <- if (is.finite(w1) && is.finite(w0)) (prev * w1 + (1 - prev) * w0) else NA_real_
  w1n <- if (is.finite(denom)) w1 / denom else NA_real_
  w0n <- if (is.finite(denom)) w0 / denom else NA_real_

  tab <- tibble::tibble(
    quantity = c("n_total", "n_positive (y=1)", "n_negative (y=0)", "prevalence (y=1)", "w1 = 1/prevalence", "w0 = 1/(1-prevalence)", "w1_normalized", "w0_normalized"),
    value = c(n, n1, n0, prev, w1, w0, w1n, w0n)
  )
  # Round useful rows
  tab$value[tab$quantity %in% c("prevalence (y=1)", "w1 = 1/prevalence", "w0 = 1/(1-prevalence)", "w1_normalized", "w0_normalized")] <- 
    round(as.numeric(tab$value[tab$quantity %in% c("prevalence (y=1)", "w1 = 1/prevalence", "w0 = 1/(1-prevalence)", "w1_normalized", "w0_normalized")]), 4)

  suppressWarnings(
    modelsummary::datasummary_df(
      tab,
      output = file.path(out_dir, cfg$files$class_weights_tex)
    )
  )
  invisible(file.path(out_dir, cfg$files$class_weights_tex))
}

# Raw assets overview: list raw files, n, k, and years
save_raw_assets_overview_tex <- function(out_dir = CFG$dirs$out, cfg = CFG, res_nhanes = CFG$dirs$res_nhanes, res_mort = CFG$dirs$res_mort, waves = CFG$data$nhanes_waves) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  wave_years <- function(code) {
    switch(toupper(as.character(code)),
      H = "2013--2014",
      I = "2015--2016",
      J = "2017--2018",
      K = "2017--Mar 2020",
      NA_character_
    )
  }
  rows <- list()
  for (w in waves) {
    for (tbl in c("DEMO", "SMQ")) {
      path <- file.path(res_nhanes, paste0(tbl, "_", w, ".csv"))
      if (file.exists(path)) {
        df <- suppressWarnings(readr::read_csv(path, show_col_types = FALSE, n_max = Inf))
        rows[[length(rows)+1]] <- tibble::tibble(
          file = basename(path),
          wave = w,
          years = wave_years(w),
          n = nrow(df),
          k = ncol(df)
        )
      }
    }
  }
  # Mortality (CSV converted from .dat if present)
  mort_dat <- file.path(res_mort, cfg$data$mortality$dat_file)
  mort_csv <- sub("[.]dat$", ".csv", mort_dat, ignore.case = TRUE)
  if (file.exists(mort_csv)) {
    dfm <- suppressWarnings(readr::read_csv(mort_csv, show_col_types = FALSE))
    rows[[length(rows)+1]] <- tibble::tibble(
      file = basename(mort_csv), wave = NA_character_, years = "Mortality 2019 linkage", n = nrow(dfm), k = ncol(dfm)
    )
  }
  if (length(rows) > 0) {
    tab <- dplyr::bind_rows(rows)
    suppressWarnings(
      modelsummary::datasummary_df(tab, output = file.path(out_dir, cfg$files$raw_assets_overview_tex))
    )
  }
  invisible(file.path(out_dir, cfg$files$raw_assets_overview_tex))
}

save_processing_notes_tex <- function(meta, out_dir = CFG$dirs$out, cfg = CFG) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  # Compose detailed notes on transformations
  notes <- c(
    "\\section*{Processing Notes}",
    "\\noindent ",
    "Joins and Scope: Person-level joins are performed on \\texttt{SEQN} within each NHANES cycle (DEMO + SMQ), then rows are stacked across cycles.",
    "\\\n",
    "Smoker Derivation (SMQ): ",
    "\\begin{itemize}",
    "  \\item If \\texttt{SMQ020 == 2} (\\textit{has not smoked 100 cigarettes}): label as Never.",
    "  \\item If \\texttt{SMQ020 == 1} and \\texttt{SMQ040 == 3} (\\textit{currently do not smoke}): label as Former.",
    "  \\item If \\texttt{SMQ020 == 1} and SMQ040 equals 1 or 2 (\\textit{every day or some days}): label as Current.",
    "  \\item Otherwise: smoker is set to NA (insufficient or inconsistent information).",
    "\\end{itemize}",
    "Smoker Coding: Factor levels Never/Former/Current and an ordinal code \\texttt{smoker\\_code} = 1/2/3 are created for modeling.",
    "\\\n",
    "Other Harmonization: ",
    "\\begin{itemize}",
    "  \\item Sex normalized to \\texttt{female/male} factor.",
    "  \\item Income set to NHANES PIR (\\texttt{INDFMPIR}) and standardized jointly with age.",
    "  \\item Rows with missing among \\texttt{id, age, sex, income, smoker, smoker\\_code,} and the label (\\texttt{", meta$label, "}) are dropped for the clean matrix.",
    "\\end{itemize}",
    "Standardization: Numeric predictors (age, income) are centered and scaled to unit variance.",
    "\\\n",
    paste0("Label: The modeling label is \\texttt{", meta$label, "}.")
  )
  writeLines(notes, file.path(out_dir, cfg$files$processing_notes_tex))
  invisible(file.path(out_dir, cfg$files$processing_notes_tex))
}

save_smoker_counts_tex <- function(df, out_dir = CFG$dirs$out, cfg = CFG) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  tab <- as.data.frame(table(df$smoker, useNA = "ifany"))
  names(tab) <- c("smoker", "count")
  tab$smoker <- as.character(tab$smoker)
  suppressWarnings(
    modelsummary::datasummary_df(
      tibble::as_tibble(tab),
      output = file.path(out_dir, cfg$files$smoker_counts_tex)
    )
  )
  invisible(file.path(out_dir, cfg$files$smoker_counts_tex))
}

save_imputation_counts_tex <- function(meta, out_dir = CFG$dirs$out, cfg = CFG) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  imp <- meta$imputed_counts
  if (is.null(imp) || length(imp) == 0) return(invisible(NULL))
  tab <- tibble::tibble(
    variable = names(imp),
    n_imputed = as.integer(unlist(imp))
  )
  suppressWarnings(
    modelsummary::datasummary_df(
      tab,
      output = file.path(out_dir, cfg$files$imputation_counts_tex)
    )
  )
  invisible(file.path(out_dir, cfg$files$imputation_counts_tex))
}

save_wave_counts_tex <- function(df, out_dir = CFG$dirs$out, cfg = CFG, which = c("raw","clean")) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  which <- match.arg(which)
  if (!"wave" %in% names(df)) return(invisible(NULL))
  tab <- df |>
    dplyr::count(wave, name = "n") |>
    dplyr::arrange(wave)
  out_file <- if (which == "raw") cfg$files$wave_counts_raw_tex else cfg$files$wave_counts_clean_tex
  suppressWarnings(
    modelsummary::datasummary_df(
      tab,
      output = file.path(out_dir, out_file)
    )
  )
  invisible(file.path(out_dir, out_file))
}

# Remove LaTeX table wrappers from generated .tex so they can be \input inside custom environments
sanitize_tables_dir <- function(out_dir = CFG$dirs$out, cfg = CFG) {
  tbl_dir <- file.path(out_dir, "tables")
  if (!dir.exists(tbl_dir)) return(invisible(FALSE))
  files <- list.files(tbl_dir, pattern = "[.]tex$", full.names = TRUE)
  for (f in files) {
    lines <- tryCatch(readLines(f, warn = FALSE), error = function(e) NULL)
    if (is.null(lines)) next
    bad1 <- grepl("^\\s*\\\\begin\\{table\\}", lines)
    bad2 <- grepl("^\\s*\\\\end\\{table\\}", lines)
    bad3 <- grepl("^\\s*\\\\centering\\s*$", lines)
    keep <- !(bad1 | bad2 | bad3)
    # Only write back if any change
    if (any(!keep)) writeLines(lines[keep], f)
  }
  invisible(TRUE)
}



