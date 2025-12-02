# ============================================================
# Project: Smoking & Mortality Risk (Simulated Data)
# Goal: Linear regression in R (for Computing for Economics)
# ============================================================

# 0) Packages -------------------------------------------------
req_pkgs <- c("tidyverse", "broom", "sandwich", "lmtest", "car", "ggplot2", "modelsummary")
new <- req_pkgs[!(req_pkgs %in% installed.packages()[, "Package"])]
if (length(new)) install.packages(new, dependencies = TRUE)

library(tidyverse)
library(broom)
library(sandwich)
library(lmtest)
library(car)
library(ggplot2)
library(modelsummary)

set.seed(123)  # for reproducibility

# 1) Simulate a realistic dataset, as good as we can -----------------------------

n <- 1000  # number of observations

sim_df <- tibble(
  age   = sample(18:80, n, replace = TRUE),
  sex   = factor(sample(c("female", "male"), n, replace = TRUE, prob = c(0.52, 0.48))),
  income = rlnorm(n, meanlog = log(50000), sdlog = 0.5),  # skewed like real income
  #Behavioral Risk Factor Surveillance System (BRFSS, CDC):
  #Current smokers: ~14–16% ; Former smokers: ~22–25% ; Never smokers: ~55–60%
  #National Health Interview Survey (NHIS): Current: ~12–15% ; Former: ~25% ; Never: ~60%
  #Set prob because no dataset went well with our code and investigation
  smoker = factor(
    sample(c("Never", "Former", "Current"), n, replace = TRUE, prob = c(0.55, 0.2, 0.25)),
    levels = c("Never", "Former", "Current")
  )
)

# Generate a "mortality risk" index between 1 and 5
# Higher values = worse health / higher risk.
# Let smoking and age increase the risk.

base <- 1.5 +
  0.07 * (sim_df$age - 18) / 10 +          # age effect: older → higher risk
  0.5  * (sim_df$smoker == "Former") +     # former smokers worse than never
  1.0  * (sim_df$smoker == "Current") -    # current smokers much worse
  0.000004 * sim_df$income                 # higher income slightly protective

# random noise
eps <- rnorm(n, mean = 0, sd = 0.4)

risk_raw <- base + eps

# clamp between 1 and 5 to mimic a health/risk scale
sim_df$risk_index <- pmin(pmax(risk_raw, 1), 5)

# quick peek
summary(sim_df)
table(sim_df$smoker)

# 2) Fit linear regression (OLS) ------------------------------

# Model:
# risk_index = beta0 + beta1*Former + beta2*Current + beta3*age + beta4*sex + beta5*income + error

ols_mod <- lm(risk_index ~ smoker + age + sex + income, data = sim_df)

# 3) Robust (HC1) standard errors -----------------------------

vcov_hc <- vcovHC(ols_mod, type = "HC1")
ols_robust <- coeftest(ols_mod, vcov. = vcov_hc)

cat("\n=== OLS with Robust SEs (HC1) ===\n")
print(ols_robust)

# 4) Model fit statistics -------------------------------------

fit_stats <- glance(ols_mod) %>%
  select(r.squared, adj.r.squared, statistic, p.value, df, df.residual)

cat("\nModel fit stats:\n")
print(fit_stats)

# 5) Multicollinearity check (VIF) ---------------------------

cat("\nVariance Inflation Factors (VIF):\n")
print(vif(ols_mod))

# 6) Diagnostics plots ----------------------------------------

# Residual vs Fitted
p1 <- ggplot(mapping = aes(x = fitted(ols_mod), y = resid(ols_mod))) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "Fitted values",
       y = "Residuals",
       title = "Residuals vs Fitted")

# QQ plot of standardized residuals
qq_df <- data.frame(stdres = rstandard(ols_mod))
p2 <- ggplot(qq_df, aes(sample = stdres)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Normal Q-Q Plot of Standardized Residuals")

# Predicted risk by smoking status (holding other vars at typical values)
newdat <- sim_df %>%
  group_by(smoker) %>%
  summarize(
    age   = mean(age),
    sex   = names(sort(table(sex), decreasing = TRUE))[1],
    income = mean(income),
    .groups = "drop"
  )

preds <- predict(ols_mod, newdata = newdat, se.fit = TRUE)
newdat$fit <- preds$fit
newdat$se  <- preds$se.fit
newdat$lwr <- newdat$fit - 1.96 * newdat$se
newdat$upr <- newdat$fit + 1.96 * newdat$se

p3 <- ggplot(newdat, aes(x = smoker, y = fit)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.15) +
  labs(
    title = "Predicted Mortality Risk by Smoking Status",
    x = "Smoking status",
    y = "Predicted risk_index (1 = low risk, 5 = high risk)"
  )

print(p1)
print(p2)
print(p3)

# 7) Pretty regression table (robust SEs) ---------------------

msummary(
  list("OLS: risk_index" = ols_mod),
  vcov = vcov_hc,
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_omit = "IC|Log|F$",
  output = "smoking_ols_table.txt"
)

cat('\nSaved robust-SE regression table to "smoking_ols_table.txt" in your working directory.\n')

# 8) Smoking coefficients and interpretation hint -------------

smoke_rows <- grep("^smoker", rownames(coef(summary(ols_mod))))
smoke_terms <- coef(summary(ols_mod))[smoke_rows, , drop = FALSE]

cat("\nSmoking coefficients (conventional SEs):\n")
print(smoke_terms)

cat("\nINTERPRETATION GUIDE:\n")
cat("- Coefficient on smokerFormer: difference in average risk_index for FORMER vs NEVER smokers,\n")
cat("  holding age, sex, and income constant.\n")
cat("- Coefficient on smokerCurrent: difference for CURRENT vs NEVER smokers.\n")
cat("- Positive coefficient = higher mortality risk index (worse health).\n")
cat("- Use the robust-SE table above (HC1) when you report significance in your write-up.\n")
