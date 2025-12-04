make_diagnostics_plots <- function(model, model_label = "Model") {
  # For glm, use appropriate residuals
  fitted_vals <- tryCatch(stats::fitted(model, type = if (inherits(model, "glm")) "response" else "response"), error = function(e) fitted(model))
  resid_vals  <- tryCatch(residuals(model, type = if (inherits(model, "glm")) "deviance" else "response"), error = function(e) resid(model))
  p1 <- ggplot2::ggplot(mapping = ggplot2::aes(x = fitted_vals, y = resid_vals)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::labs(x = "Fitted values", y = "Residuals", title = paste0("Residuals vs Fitted (", model_label, ")"))

  qq_df <- data.frame(stdres = tryCatch(rstandard(model), error = function(e) scale(resid_vals)[,1]))
  p2 <- ggplot2::ggplot(qq_df, ggplot2::aes(sample = stdres)) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line() +
    ggplot2::labs(title = paste0("Normal Q-Q Plot of Standardized Residuals (", model_label, ")"))

  list(p1 = p1, p2 = p2)
}

make_predictions_plot <- function(model, df, ci_z = CFG$model$ci_z, model_label = "Model", scale_to_five = TRUE) {
  # Drop unknown/NA smokers and unused levels to avoid stray categories
  dff <- df
  if ("smoker" %in% names(dff)) {
    dff <- dff[!is.na(dff$smoker) & as.character(dff$smoker) != "Unknown", , drop = FALSE]
    if (is.factor(dff$smoker)) dff$smoker <- droplevels(dff$smoker)
  }

  newdat <- dff |>
    dplyr::group_by(smoker, .drop = TRUE) |>
    dplyr::summarize(
      age    = mean(age),
      sex    = names(sort(table(sex), decreasing = TRUE))[1],
      log_income = mean(log_income, na.rm = TRUE),
      .groups = "drop"
    )
  # If model includes age_sq (engineered feature), ensure it exists in newdata
  if ("age_sq" %in% names(dff)) {
    newdat$age_sq <- newdat$age^2
  }

  if (inherits(model, "glm")) {
    p <- stats::predict(model, newdata = newdat, type = "response", se.fit = TRUE)
    newdat$fit <- as.numeric(p$fit)
    newdat$se  <- as.numeric(p$se.fit)
  } else {
    p <- stats::predict(model, newdata = newdat, se.fit = TRUE)
    newdat$fit <- as.numeric(p$fit)
    newdat$se  <- as.numeric(p$se.fit)
  }
  # Simple normal-based CI on scale of fit
  newdat$lwr <- newdat$fit - ci_z * newdat$se
  newdat$upr <- newdat$fit + ci_z * newdat$se
  # Optionally scale probabilities to 1-5 for presentation
  if (isTRUE(scale_to_five)) {
    s <- function(x) 1 + 4 * x
    newdat$fit <- s(newdat$fit)
    newdat$lwr <- s(pmax(0, newdat$lwr))
    newdat$upr <- s(pmin(1, newdat$upr))
  }

  p3 <- ggplot2::ggplot(newdat, ggplot2::aes(x = smoker, y = fit)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lwr, ymax = upr), width = 0.15) +
    ggplot2::labs(
      title = paste0("Predicted Mortality Risk by Smoking Status (", model_label, ")"),
      x = "Smoking status",
      y = "Predicted risk (1 = low, 5 = high)"
    )

  p3 
}

make_resid_plot_from_pred <- function(y, yhat, model_label = "Model") {
  df <- data.frame(fitted = as.numeric(yhat), resid = as.numeric(y) - as.numeric(yhat))
  ggplot2::ggplot(df, ggplot2::aes(x = fitted, y = resid)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::labs(x = "Fitted values", y = "Residuals", title = paste0("Residuals vs Fitted (", model_label, ")"))
}

make_qq_from_resid <- function(resid, model_label = "Model") {
  df <- data.frame(stdres = scale(as.numeric(resid))[,1])
  ggplot2::ggplot(df, ggplot2::aes(sample = stdres)) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line() +
    ggplot2::labs(title = paste0("Normal Q-Q Plot of Standardized Residuals (", model_label, ")"))
}

make_lasso_cv_plot_gg <- function(cvfit) {
  # Build a ggplot version of cv.glmnet plot
  df <- data.frame(
    lambda = cvfit$lambda,
    log_lambda = log(cvfit$lambda),
    cvm = cvfit$cvm,
    cvsd = cvfit$cvsd
  )
  lmin <- log(cvfit$lambda.min)
  l1se <- log(cvfit$lambda.1se)
  ggplot2::ggplot(df, ggplot2::aes(x = log_lambda, y = cvm)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = cvm - cvsd, ymax = cvm + cvsd), alpha = 0.2) +
    ggplot2::geom_line() +
    ggplot2::geom_vline(xintercept = lmin, linetype = 2, color = "red") +
    ggplot2::geom_vline(xintercept = l1se, linetype = 3, color = "blue") +
    ggplot2::labs(x = "log(lambda)", y = "CV Error", title = "LASSO CV Error vs log(lambda)", subtitle = "Red: lambda.min, Blue: lambda.1se")
}
