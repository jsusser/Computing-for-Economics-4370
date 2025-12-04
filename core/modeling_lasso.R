fit_lasso_cv <- function(df, cfg = CFG, label = NULL, weights = NULL) {
  # Build model matrix without intercept; glmnet handles its own intercept
  if (is.null(label)) label <- if ("dead" %in% names(df)) "dead" else "risk_index"
  mm <- model.matrix(stats::as.formula(paste(label, "~ smoker + age + age_sq + sex + log_income")), data = df)[, -1, drop = FALSE]
  y  <- df[[label]]

  cvfit <- glmnet::cv.glmnet(
    x = mm,
    y = y,
    alpha = cfg$model$lasso$alpha,
    nfolds = cfg$model$lasso$nfolds,
    standardize = cfg$model$lasso$standardize,
    family = cfg$model$lasso$family,
    parallel = TRUE,
    weights = weights
  )

  # Extract coefficients at lambda.min and lambda.1se
  coef_min <- as.matrix(stats::coef(cvfit, s = "lambda.min"))
  coef_1se <- as.matrix(stats::coef(cvfit, s = "lambda.1se"))

  tidy_coefs <- function(mat) {
    tibble::tibble(
      term = rownames(mat),
      estimate = as.numeric(mat)
    ) |>
      dplyr::filter(estimate != 0)
  }

  list(
    cvfit = cvfit,
    lambda_min = cvfit$lambda.min,
    lambda_1se = cvfit$lambda.1se,
    coefs_min = tidy_coefs(coef_min),
    coefs_1se = tidy_coefs(coef_1se)
  )
}

build_coef_comparison <- function(base_results, lasso, base_name = "Base") {
  # Base model estimates (e.g., Logit)
  base_df <- broom::tidy(base_results$model) |>
    dplyr::select(term, estimate) |>
    dplyr::rename(base_estimate = estimate)

  # LASSO coefficients at lambda.min and lambda.1se
  lmin <- dplyr::rename(lasso$coefs_min, lasso_min = estimate)
  l1se <- dplyr::rename(lasso$coefs_1se, lasso_1se = estimate)

  tab <- base_df |>
    dplyr::full_join(lmin, by = "term") |>
    dplyr::full_join(l1se, by = "term") |>
    dplyr::arrange(term)

  tab
}
