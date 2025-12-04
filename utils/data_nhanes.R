fetch_nhanes_table_raw <- function(table, res_dir = CFG$dirs$res_nhanes) {
  df <- nhanesA::nhanes(table)
  dir.create(res_dir, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(res_dir, paste0(table, ".csv"))
  readr::write_csv(df, path)
  list(path = path, n = nrow(df), k = ncol(df), table = table)
}

prepare_nhanes_core <- function(demo_df, smq_df) {
  # Use DEMO as the anchor; keep all rows from DEMO and attach SMQ when available
  core0 <- dplyr::left_join(demo_df, smq_df, by = "SEQN")

  # Resolve column names case-insensitively
  nm <- names(core0)
  get_col <- function(upper, lower) {
    if (upper %in% nm) upper else if (lower %in% nm) lower else NA_character_
  }
  smq020_name <- get_col("SMQ020", "smq020")
  smq040_name <- get_col("SMQ040", "smq040")

  # Pull raw columns
  core_raw <- core0 |>
    dplyr::transmute(
      id    = .data[[get_col("SEQN", "seqn")]],
      age   = .data[[get_col("RIDAGEYR", "ridageyr")]],
      sex_raw = .data[[get_col("RIAGENDR", "riagendr")]],
      race_raw = .data[[get_col("RIDRETH1", "ridreth1")]],
      race3_raw = .data[[get_col("RIDRETH3", "ridreth3")]],
      educ_raw = .data[[get_col("DMDEDUC2", "dmdeduc2")]],
      pir   = .data[[get_col("INDFMPIR", "indfmpir")]],
      smq020_raw = .data[[smq020_name]],
      smq040_raw = .data[[smq040_name]]
    )

  # Coerce SMQ variables to numeric codes if they are character
  to_int <- function(x) suppressWarnings(as.integer(x))
  smq020_num <- to_int(core_raw$smq020_raw)
  # If coercion failed (all NA) or mixed strings, map text Yes/No
  if (all(is.na(smq020_num))) {
    x <- tolower(as.character(core_raw$smq020_raw))
    smq020_num <- ifelse(x == "yes", 1L, ifelse(x == "no", 2L, NA_integer_))
  }
  smq040_num <- to_int(core_raw$smq040_raw)
  if (all(is.na(smq040_num))) {
    x <- tolower(as.character(core_raw$smq040_raw))
    smq040_num <- ifelse(grepl("every", x), 1L,
                    ifelse(grepl("some", x), 2L,
                    ifelse(grepl("not", x), 3L, NA_integer_)))
  }

  # Robust race mapping: handle either numeric codes or descriptive strings
  map_race <- function(r1, r3) {
    # Prefer race3 (RIDRETH3) text if available
    r_out <- rep(NA_character_, length(r1))
    to_str <- function(x) tolower(trimws(as.character(x)))
    r3s <- to_str(r3)
    r1s <- to_str(r1)
    # Use RIDRETH3 first
    has3 <- !is.na(r3s) & nzchar(r3s)
    r_out[has3 & grepl("mexican", r3s)] <- "MexicanAmerican"
    r_out[has3 & grepl("other hispanic", r3s)] <- "OtherHispanic"
    r_out[has3 & grepl("non-hispanic white", r3s)] <- "NHWhite"
    r_out[has3 & grepl("non-hispanic black", r3s)] <- "NHBlack"
    r_out[has3 & (grepl("asian", r3s) | grepl("other", r3s) | grepl("multi", r3s))] <- "Other"
    # Fallback to RIDRETH1 when still NA
    need1 <- is.na(r_out)
    if (any(need1)) {
      r_out[need1 & grepl("mexican", r1s)] <- "MexicanAmerican"
      r_out[need1 & grepl("other hispanic", r1s)] <- "OtherHispanic"
      r_out[need1 & grepl("non-hispanic white", r1s)] <- "NHWhite"
      r_out[need1 & grepl("non-hispanic black", r1s)] <- "NHBlack"
      r_out[need1 & (grepl("asian", r1s) | grepl("other", r1s) | grepl("multi", r1s))] <- "Other"
      # If numeric codes slipped through
      r1n <- suppressWarnings(as.integer(r1[need1]))
      if (any(!is.na(r1n))) {
        tmp <- rep(NA_character_, sum(need1))
        tmp[r1n %in% c(1L)] <- "MexicanAmerican"
        tmp[r1n %in% c(2L)] <- "OtherHispanic"
        tmp[r1n %in% c(3L)] <- "NHWhite"
        tmp[r1n %in% c(4L)] <- "NHBlack"
        tmp[r1n %in% c(5L, 6L, 8L)] <- "Other"
        r_out[need1] <- ifelse(is.na(r_out[need1]), tmp, r_out[need1])
      }
    }
    factor(r_out, levels = c("NHWhite", "NHBlack", "MexicanAmerican", "OtherHispanic", "Other"))
  }

  # Robust education mapping: handle descriptive strings or numeric DMDEDUC2
  map_educ <- function(e2) {
    es <- tolower(trimws(as.character(e2)))
    out <- rep(NA_character_, length(es))
    out[grepl("less than 9", es) | grepl("<9", es)] <- "LT_HS"
    out[grepl("9th", es) | grepl("9-11|9–11|11th", es)] <- "HS_Incomplete"
    out[grepl("high school", es) | grepl("ged", es)] <- "HS_GED"
    out[grepl("some college", es) | grepl("aa degree", es)] <- "SomeCollege_AA"
    out[grepl("college graduate", es) | grepl("college plus", es)] <- "CollegePlus"
    # Fallback on numeric codes if any
    en <- suppressWarnings(as.integer(e2))
    need_num <- is.na(out) & !is.na(en)
    out[need_num & en == 1L] <- "LT_HS"
    out[need_num & en == 2L] <- "HS_Incomplete"
    out[need_num & en == 3L] <- "HS_GED"
    out[need_num & en == 4L] <- "SomeCollege_AA"
    out[need_num & en == 5L] <- "CollegePlus"
    factor(out, levels = c("LT_HS", "HS_Incomplete", "HS_GED", "SomeCollege_AA", "CollegePlus"))
  }

  core <- core_raw |>
    dplyr::transmute(
      id,
      age,
      sex = dplyr::if_else(sex_raw == 1 | tolower(as.character(sex_raw)) == "male", "male", "female"),
      race_lab = map_race(race_raw, race3_raw),
      educ_lab = map_educ(educ_raw),
      # provide race/educ placeholders for downstream mutate; will be reconciled later
      race = race_lab,
      educ = educ_lab,
      pir,
      smq020 = smq020_num,
      smq040 = smq040_num
    )

  # Derive smoker category robustly (tolerate NAs):
  # - If smq020 == 2 (no 100 cigarettes): Never
  # - If smq020 == 1 and smq040 == 3: Former
  # - If smq020 == 1 and smq040 in 1:2: Current
  core <- core |>
    dplyr::mutate(
      smoker = dplyr::case_when(
        !is.na(smq020) & smq020 == 2                       ~ "Never",
        !is.na(smq020) & smq020 == 1 & smq040 == 3         ~ "Former",
        !is.na(smq020) & smq020 == 1 & smq040 %in% c(1, 2) ~ "Current",
        TRUE                                               ~ NA_character_
      ),
      smoker = factor(smoker, levels = c("Never", "Former", "Current")),
      sex    = factor(sex, levels = c("female", "male")),
      smoker_code = dplyr::case_when(
        smoker == "Never"   ~ 1L,
        smoker == "Former"  ~ 2L,
        smoker == "Current" ~ 3L,
        TRUE                 ~ NA_integer_
      ),
      # Label race categories to human-readable groups
      race = dplyr::case_when(
        race %in% c(1L, 7L) ~ "MexicanAmerican",
        race %in% c(2L)     ~ "OtherHispanic",
        race %in% c(3L)     ~ "NHWhite",
        race %in% c(4L)     ~ "NHBlack",
        race %in% c(5L, 6L, 8L) ~ "Other",
        TRUE ~ NA_character_
      ),
      race = factor(race, levels = c("NHWhite", "NHBlack", "MexicanAmerican", "OtherHispanic", "Other")),
      educ = factor(educ, levels = c("LT_HS", "HS_Incomplete", "HS_GED", "SomeCollege_AA", "CollegePlus"))
    )

  # If upstream recoding produced NA due to string inputs, fall back to label mappings
  core <- core |>
    dplyr::mutate(
      race = dplyr::coalesce(race, race_lab),
      educ = dplyr::coalesce(educ, educ_lab)
    ) |>
    dplyr::select(-dplyr::any_of(c("race_lab", "educ_lab")))

  core
}

fetch_nhanes_tables_raw <- function(waves = CFG$data$nhanes_waves, res_dir = CFG$dirs$res_nhanes) {
  metas <- list()
  for (w in waves) {
    demo_tbl <- paste0("DEMO_", w)
    smq_tbl  <- paste0("SMQ_",  w)
    metas[[demo_tbl]] <- fetch_nhanes_table_raw(demo_tbl, res_dir)
    metas[[smq_tbl]]  <- fetch_nhanes_table_raw(smq_tbl,  res_dir)
  }
  metas
}

prepare_nhanes_core_multi <- function(waves = CFG$data$nhanes_waves, res_dir = CFG$dirs$res_nhanes) {
  cores <- list()
  for (w in waves) {
    demo_path <- file.path(res_dir, paste0("DEMO_", w, ".csv"))
    smq_path  <- file.path(res_dir, paste0("SMQ_",  w, ".csv"))
    if (!file.exists(demo_path) || !file.exists(smq_path)) {
      stop("Missing raw NHANES files for wave ", w, ". Run scripts/fetch_nhanes_raw.R first.")
    }
    demo_df <- readr::read_csv(demo_path, show_col_types = FALSE)
    smq_df  <- readr::read_csv(smq_path,  show_col_types = FALSE)
    core_df <- prepare_nhanes_core(demo_df, smq_df)
    core_df$wave <- w
    cores[[w]] <- core_df
  }
  dplyr::bind_rows(cores)
}

# Attempt to read mortality ASCII (.dat) using a CDC-provided setup reader if available.
# Expects the .dat and an R setup script path under resources/ (or absolute).
read_mortality_public <- function(dat_path, setup_reader_path = NULL, cfg = CFG) {
  dat_path <- normalizePath(dat_path, winslash = "/", mustWork = FALSE)
  setup_reader_path <- if (!is.null(setup_reader_path)) normalizePath(setup_reader_path, winslash = "/", mustWork = FALSE) else NULL
  if (!file.exists(dat_path)) {
    stop("Mortality .dat file not found: ", dat_path)
  }
  if (!is.null(setup_reader_path) && file.exists(setup_reader_path)) {
    # Source the CDC reader which should expose a function like read_nchs_lmf()
    env <- new.env(parent = baseenv())
    owd <- getwd(); on.exit(setwd(owd), add = TRUE)
    setwd(dirname(dat_path))
    assign("setwd", function(...) invisible(NULL), envir = env)
    sys.source(setup_reader_path, envir = env)
    # Function name from config, with fallbacks (CDC scripts may change names)
    fn_names <- unique(c(cfg$data$mortality$setup_function,
                         "read_nchs_lmf", "read.NCHS.LMF"))
    for (fn in fn_names) {
      if (!is.null(env[[fn]]) && is.function(env[[fn]])) {
        return(env[[fn]](dat_path))
      }
    }
  }
  stop("Mortality setup reader not found or incompatible. Provide a CDC R setup script that defines read_nchs_lmf().")
}

merge_core_mortality <- function(core_df, mort_df) {
  mort_names <- names(mort_df)
  mort_id <- if ("SEQN" %in% mort_names) "SEQN" else if ("seqn" %in% mort_names) "seqn" else stop("Mortality data has no SEQN/seqn column")
  joined <- dplyr::left_join(core_df, mort_df, by = setNames(mort_id, "id"))
  if (!"wave" %in% names(joined)) joined$wave <- NA_character_
  mortstat_col <- if ("MORTSTAT" %in% names(joined)) "MORTSTAT" else if ("mortstat" %in% names(joined)) "mortstat" else NA_character_
  permthexm_col <- if ("PERMTH_EXM" %in% names(joined)) "PERMTH_EXM" else if ("permth_exm" %in% names(joined)) "permth_exm" else NA_character_
  joined |>
    dplyr::transmute(
      id,
      wave,
      age,
      sex,
      # carry forward race/educ from core_df for downstream dummies
      race = if ("race" %in% names(joined)) race else NA,
      educ = if ("educ" %in% names(joined)) educ else NA,
      income = pir,
      smoker = factor(smoker, levels = c("Never", "Former", "Current")),
      smoker_code,
      dead = if (!is.na(mortstat_col)) (.data[[mortstat_col]] == 1) else NA,
      followup_months = if (!is.na(permthexm_col)) .data[[permthexm_col]] else NA
    )
}

# Directly read NHANES public-use mortality .dat using fixed-width specs from CDC script
read_mortality_nhanes_dat <- function(dat_path) {
  dat_path <- normalizePath(dat_path, winslash = "/", mustWork = TRUE)
  readr::read_fwf(
    file = dat_path,
    col_types = "iiiiiiii",
    col_positions = readr::fwf_cols(
      seqn = c(1, 6),
      eligstat = c(15, 15),
      mortstat = c(16, 16),
      ucod_leading = c(17, 19),
      diabetes = c(20, 20),
      hyperten = c(21, 21),
      permth_int = c(43, 45),
      permth_exm = c(46, 48)
    ),
    na = c("", ".")
  )
}

write_mortality_csv <- function(dat_path, out_csv) {
  df <- read_mortality_nhanes_dat(dat_path)
  readr::write_csv(df, out_csv)
  out_csv
}

download_mortality_files <- function(res_dir = CFG$dirs$res, cfg = CFG) {
  dir.create(res_dir, showWarnings = FALSE, recursive = TRUE)
  dat_dest <- file.path(res_dir, cfg$data$mortality$dat_file)
  setup_dest <- file.path(res_dir, cfg$data$mortality$setup_file)

  status <- list(dat = FALSE, setup = FALSE)

  if (!file.exists(dat_dest)) {
    if (!is.null(cfg$data$mortality$dat_url) && nzchar(cfg$data$mortality$dat_url)) {
      try(utils::download.file(cfg$data$mortality$dat_url, dat_dest, mode = "wb"), silent = TRUE)
      status$dat <- file.exists(dat_dest)
    }
  } else status$dat <- TRUE

  if (!file.exists(setup_dest)) {
    if (!is.null(cfg$data$mortality$setup_url) && nzchar(cfg$data$mortality$setup_url)) {
      try(utils::download.file(cfg$data$mortality$setup_url, setup_dest, mode = "wb"), silent = TRUE)
      status$setup <- file.exists(setup_dest)
    }
  } else status$setup <- TRUE

  status
}
