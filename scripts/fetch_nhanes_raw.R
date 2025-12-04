#!/usr/bin/env Rscript
options(repos = c(CRAN = "https://cloud.r-project.org"))

here <- getwd()
cfg_path <- file.path(here, "config", "config.R")
if (!file.exists(cfg_path)) stop("config/config.R not found; run from project root.")
source(cfg_path)

req <- c("nhanesA", "readr")
new <- req[!(req %in% installed.packages()[, "Package"])]
if (length(new)) install.packages(new, dependencies = TRUE)

res_nhanes <- CFG$dirs$res_nhanes
res_mort   <- CFG$dirs$res_mort
dir.create(res_nhanes, showWarnings = FALSE, recursive = TRUE)
dir.create(res_mort,   showWarnings = FALSE, recursive = TRUE)

waves <- CFG$data$nhanes_waves
if (is.null(waves) || length(waves) == 0) waves <- CFG$data$nhanes_wave
if (length(waves) == 1 && is.na(waves)) stop("No NHANES waves configured in CFG$data$nhanes_waves or nhanes_wave.")

message("Downloading NHANES tables to ", res_nhanes, " for waves: ", paste(waves, collapse=", "), " ...")
for (w in waves) {
  demo_tbl <- paste0("DEMO_", w)
  smq_tbl  <- paste0("SMQ_",  w)
  demo <- nhanesA::nhanes(demo_tbl)
  readr::write_csv(demo, file.path(res_nhanes, paste0(demo_tbl, ".csv")))
  smq  <- nhanesA::nhanes(smq_tbl)
  readr::write_csv(smq,  file.path(res_nhanes, paste0(smq_tbl, ".csv")))
  message("Saved ", demo_tbl, ", ", smq_tbl)
}

if (isTRUE(CFG$data$use_mortality)) {
  dat_url   <- CFG$data$mortality$dat_url
  setup_url <- CFG$data$mortality$setup_url
  dat_dest   <- file.path(res_mort, CFG$data$mortality$dat_file)
  setup_dest <- file.path(res_mort, CFG$data$mortality$setup_file)

  if (!is.null(dat_url) && nzchar(dat_url) && !file.exists(dat_dest)) {
    message("Downloading mortality .dat ...")
    utils::download.file(dat_url, dat_dest, mode = "wb")
  } else if (!file.exists(dat_dest)) {
    message("mortality .dat URL not set in config; skipping.")
  }

  if (!is.null(setup_url) && nzchar(setup_url) && !file.exists(setup_dest)) {
    message("Downloading mortality setup R script ...")
    utils::download.file(setup_url, setup_dest, mode = "wb")
  } else if (!file.exists(setup_dest)) {
    message("mortality setup URL not set in config; skipping.")
  }
}

message("Done.")
