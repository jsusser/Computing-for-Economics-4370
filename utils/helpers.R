quick_peek <- function(df) {
  print(summary(df))
  print(table(df$smoker))
  invisible(NULL)
}

# Simulation utility used for Monte Carlo evaluation only
simulate_smoking_data <- function(n = CFG$n, seed = NULL, cfg = CFG) {
  if (!is.null(seed)) set.seed(seed)

  sim_df <- tibble::tibble(
    age   = sample(18:80, n, replace = TRUE),
    sex   = factor(sample(c("female", "male"), n, replace = TRUE, prob = c(0.52, 0.48)),
                   levels = c("female", "male")),
    income = rlnorm(n, meanlog = log(50000), sdlog = 0.5),
    smoker = factor(
      sample(c("Never", "Former", "Current"), n, replace = TRUE, prob = c(0.55, 0.2, 0.25)),
      levels = c("Never", "Former", "Current")
    )
  )

  base <- 1.5 +
    0.07 * (sim_df$age - 18) / 10 +
    0.5  * (sim_df$smoker == "Former") +
    1.0  * (sim_df$smoker == "Current") -
    0.000004 * sim_df$income

  eps <- rnorm(n, mean = 0, sd = 0.4)
  risk_raw <- base + eps
  sim_df$risk_index <- pmin(pmax(risk_raw, 1), 5)

  sim_df$smoker_code <- dplyr::case_when(
    sim_df$smoker == "Never"   ~ 1L,
    sim_df$smoker == "Former"  ~ 2L,
    sim_df$smoker == "Current" ~ 3L,
    TRUE ~ NA_integer_
  )

  sim_df
}
