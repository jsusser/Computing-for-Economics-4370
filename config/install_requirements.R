options(repos = c(CRAN = "https://cloud.r-project.org"))

req <- scan(file.path("config", "requirements.txt"), what = character(), quiet = TRUE)
new <- req[!(req %in% installed.packages()[, "Package"])]
if (length(new)) install.packages(new, dependencies = TRUE)

invisible(NULL)

