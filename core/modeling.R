fit_ols_model <- function(df, robust_type = CFG$model$robust_se_type) {
  ols_mod <- stats::lm(risk_index ~ smoker + age + sex + income, data = df)
  vcov_hc <- sandwich::vcovHC(ols_mod, type = robust_type)
  robust  <- lmtest::coeftest(ols_mod, vcov. = vcov_hc)

  fit_stats <- broom::glance(ols_mod) |>
    dplyr::select(r.squared, adj.r.squared, statistic, p.value, df, df.residual)

  vifs <- car::vif(ols_mod)

  list(model = ols_mod, vcov_hc = vcov_hc, robust = robust, fit_stats = fit_stats, vif = vifs)
}

extract_smoking_terms <- function(model) {
  smoke_rows <- grep("^smoker", rownames(coef(summary(model))))
  coef(summary(model))[smoke_rows, , drop = FALSE]
}

# Logistic regression (binary outcome 'dead' or generic 'risk_index' treated as 0/1)
fit_logit_model <- function(df, robust_type = CFG$model$robust_se_type, label = NULL, weights = NULL) {
  # Determine binary label column
  if (is.null(label)) {
    label <- if ("dead" %in% names(df)) "dead" else "risk_index"
  }
  fml <- stats::as.formula(paste(label, "~ smoker + age + age_sq + sex + log_income"))
  if (is.null(weights)) {
    logit_mod <- stats::glm(fml, data = df, family = stats::binomial(link = "logit"))
  } else {
    logit_mod <- stats::glm(fml, data = df, family = stats::binomial(link = "logit"), weights = weights)
  }
  vcov_hc <- sandwich::vcovHC(logit_mod, type = robust_type)
  robust  <- lmtest::coeftest(logit_mod, vcov. = vcov_hc)
  # Fit stats
  fit_stats <- tibble::tibble(
    AIC = tryCatch(stats::AIC(logit_mod), error = function(e) NA_real_),
    BIC = tryCatch(stats::BIC(logit_mod), error = function(e) NA_real_),
    deviance = tryCatch(stats::deviance(logit_mod), error = function(e) NA_real_),
    df.residual = tryCatch(logit_mod$df.residual, error = function(e) NA_real_)
  )
  # VIF for transparency (computed on an OLS with same RHS as an approximation)
  vif_vals <- try({
    lm_mod <- stats::lm(update(fml, . ~ .), data = df)
    car::vif(lm_mod)
  }, silent = TRUE)
  if (inherits(vif_vals, "try-error")) vif_vals <- NULL
  list(model = logit_mod, vcov_hc = vcov_hc, robust = robust, fit_stats = fit_stats, vif = vif_vals)
}
