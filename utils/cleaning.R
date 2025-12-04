standardize_numeric <- function(df, cols) {
  stats <- lapply(cols, function(cn) {
    x <- df[[cn]]
    m <- mean(x, na.rm = TRUE)
    s <- stats::sd(x, na.rm = TRUE)
    list(mean = m, sd = s)
  })
  names(stats) <- cols
  for (cn in cols) {
    m <- stats[[cn]]$mean; s <- stats[[cn]]$sd
    if (!is.finite(s) || s == 0) s <- 1
    df[[cn]] <- (df[[cn]] - m) / s
  }
  list(data = df, stats = stats)
}

impute_features <- function(df, cfg = CFG) {
  imputed <- df
  imp_counts <- list()
  # Numeric: age, income -> median impute + indicator
  for (cn in intersect(c("age", "income"), names(imputed))) {
    na_idx <- is.na(imputed[[cn]])
    imp_counts[[cn]] <- sum(na_idx)
    if (imp_counts[[cn]] > 0) {
      med <- stats::median(imputed[[cn]], na.rm = TRUE)
      imputed[[paste0(cn, "_missing")]] <- as.integer(na_idx)
      imputed[[cn]][na_idx] <- med
    } else {
      imputed[[paste0(cn, "_missing")]] <- 0L
    }
  }
  # Categorical: sex, smoker -> add Unknown level (race/educ removed from pipeline)
  for (cn in intersect(c("sex", "smoker"), names(imputed))) {
    if (!is.factor(imputed[[cn]])) imputed[[cn]] <- factor(imputed[[cn]])
    if (!("Unknown" %in% levels(imputed[[cn]]))) {
      imputed[[cn]] <- forcats::fct_expand(imputed[[cn]], "Unknown")
    }
    na_idx <- is.na(imputed[[cn]])
    imp_counts[[cn]] <- sum(na_idx)
    if (imp_counts[[cn]] > 0) {
      imputed[[cn]][na_idx] <- "Unknown"
    }
  }
  # smoker_code numeric: set 0 for unknown and add indicator
  if ("smoker_code" %in% names(imputed)) {
    na_idx <- is.na(imputed$smoker_code)
    imp_counts[["smoker_code"]] <- sum(na_idx)
    imputed$smoker_code_missing <- as.integer(na_idx)
    imputed$smoker_code[na_idx] <- 0L
  }
  list(data = imputed, imputed_counts = imp_counts)
}

prepare_features_labels <- function(df, cfg = CFG, drop_na_label_only = TRUE) {
  out_dir_clean <- cfg$dirs$clean
  dir.create(out_dir_clean, showWarnings = FALSE, recursive = TRUE)

  # Ensure id exists
  if (!"id" %in% names(df)) df$id <- seq_len(nrow(df))
  # Rename pir to income if present
  if ("INDFMPIR" %in% names(df) && !"pir" %in% names(df)) df$pir <- df$INDFMPIR
  if ("pir" %in% names(df) && !"income" %in% names(df)) df$income <- df$pir

  # Harmonize sex and smoker as factors
  if ("sex" %in% names(df)) df$sex <- factor(df$sex, levels = cfg$sim$sex_levels)
  if ("smoker" %in% names(df)) df$smoker <- factor(df$smoker, levels = cfg$sim$smoker_levels)

  # Determine label with fallback order: dead -> risk_index -> smoker_code
  label_var <- NULL
  if ("dead" %in% names(df)) label_var <- "dead"
  if (is.null(label_var) && "risk_index" %in% names(df)) label_var <- "risk_index"
  if (is.null(label_var) && "smoker_code" %in% names(df)) label_var <- "smoker_code"
  if (is.null(label_var)) stop("No suitable label variable found (expected one of 'dead', 'risk_index', 'smoker_code').")

  # Select modeling columns (race/educ removed per user request)
  keep_cols <- c("id", "age", "sex", "income", "smoker", "smoker_code", label_var)
  # Keep wave if present to allow per-wave reporting after cleaning
  if ("wave" %in% names(df)) keep_cols <- c(keep_cols, "wave")
  present <- keep_cols[keep_cols %in% names(df)]
  df_small <- df[, present, drop = FALSE]

  # Optional age filter and smoker Unknown handling before imputation
  if (!is.null(cfg$clean$min_age) && "age" %in% names(df_small)) {
    df_small <- df_small[df_small$age >= cfg$clean$min_age | is.na(df_small$age), , drop = FALSE]
  }
  if (isTRUE(cfg$clean$drop_smoker_unknown) && "smoker" %in% names(df_small)) {
    df_small <- df_small[!(is.na(df_small$smoker) | df_small$smoker == "Unknown"), , drop = FALSE]
  }

  # NA accounting; optionally drop only on label if requested
  initial_n <- nrow(df_small)
  na_by_col <- vapply(df_small, function(x) sum(is.na(x)), integer(1))
  if (isTRUE(drop_na_label_only)) {
    df_clean <- df_small[!is.na(df_small[[label_var]]), , drop = FALSE]
  } else {
    df_clean <- df_small
  }
  final_n <- nrow(df_clean)
  dropped_n <- initial_n - final_n

  # Impute features (not label)
  imp <- impute_features(df_clean, cfg)
  df_imp <- imp$data

  # Standardize numeric predictors (not label)
  num_cols <- intersect(c("age", "income"), names(df_imp))
  std <- standardize_numeric(df_imp, num_cols)
  df_std <- std$data

  # Add engineered features (post-imputation):
  # - Age squared
  # - Log income (log1p to be safe)
  df_std$age_sq <- df_std$age^2
  df_std$log_income <- log1p(pmax(df_imp$income, 0))

  # Drop race/education entirely if present (ensure not saved downstream)
  drop_cols <- intersect(c("race", "educ"), names(df_std))
  if (length(drop_cols)) {
    df_std <- df_std[, setdiff(names(df_std), drop_cols), drop = FALSE]
  }

  # Build X and y (exclude id, wave, and *_missing from X; write meta separately)
  y <- df_std[[label_var]]
  missing_cols <- grep("_missing$", names(df_std), value = TRUE)
  # Exclude original factor columns (race, educ) when using dummies to avoid multicollinearity
  feature_exclude <- c(label_var, "id", "wave", "race", "educ", "income", missing_cols)
  X <- df_std[, setdiff(names(df_std), feature_exclude), drop = FALSE]

  # Metadata: id, wave, and missing indicators
  meta_cols <- c("id", intersect(c("wave"), names(df_std)), missing_cols)
  meta_df <- df_std[, meta_cols, drop = FALSE]

  # Save artifacts
  readr::write_csv(X, file.path(out_dir_clean, cfg$files$X_csv))
  readr::write_csv(tibble::tibble(!!label_var := y), file.path(out_dir_clean, cfg$files$y_csv))
  readr::write_csv(df_std, file.path(out_dir_clean, cfg$files$cleaned_full_csv))
  readr::write_csv(meta_df, file.path(out_dir_clean, cfg$files$meta_csv))

  # Prepare reporting snippets
  head_X <- utils::head(X, 6)
  head_y <- utils::head(tibble::tibble(!!label_var := y), 6)
  meta <- list(
    label = label_var,
    features = names(X),
    initial_n = initial_n,
    final_n = final_n,
    dropped_n = dropped_n,
    na_by_col = na_by_col,
    imputed_counts = imp$imputed_counts,
    std_stats = std$stats,
    head_X = head_X,
    head_y = head_y,
    meta_cols = meta_cols
  )

  list(X = X, y = y, df = df_std, meta = meta)
}
