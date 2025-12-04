# Smoking and Mortality (NHANES 2013â€“2014)

Analyze smoking and mortality using NHANES DEMO/SMQ linked to NCHS mortality. The pipeline is modular, reproducible, and produces a LaTeX report with tables and figures in `output/`.

## Repository Layout

- `config/`
  - `config.R`: Central configuration (paths, packages, model settings).
  - `requirements.txt`: R package list (informational; the main script installs as needed).
- `utils/` (utility modules)
  - `data_nhanes.R`: Download/read NHANES DEMO/SMQ and mortality raw files.
  - `cleaning.R`: Build feature matrix `X` and label `y` with imputation and standardization.
  - `io.R`: Table/figure writers and report helpers.
  - `reporting.R`: Programmatic LaTeX report generator (`output/report.tex`).
  - `helpers.R`: Small helpers (plot saving, panels, sanitization).
- `core/` (modeling & diagnostics)
  - `modeling.R`: Weighted logistic regression (HC1 robust SEs).
  - `modeling_lasso.R`: Regularized logistic regression (glmnet with CV).
  - `diagnostics.R`: Residual, QQ, and prediction plots.
- `scripts/`
  - `fetch_nhanes_raw.R`: Download raw NHANES and mortality assets into `resources/`.
  - `make_clean_matrices_only.R`: Construct `clean-data/X.csv`, `y.csv`, `cleaned_full.csv`, `meta.csv` and write clean-data LaTeX snippets to `output/tables/`.
- `resources/`
  - `nhanes/` and `mortality/`: Raw input files as downloaded (never overwritten by cleaning).
- `clean-data/`
  - Model-ready matrices (`X.csv`, `y.csv`) and a full cleaned frame (`cleaned_full.csv`).
- `output/`
  - `report.tex`, `report.pdf`: Generated LaTeX and PDF.
  - `report_final.tex`, `report_final.pdf`: Curated final report and its PDF.
  - `tables/`, `figures/`, `artifacts/`: All auxiliary outputs for the report.

## Requirements

- R 4.4+ with Rscript on PATH (Windows path example: `C:\\Program Files\\R\\R-4.4.2\\bin\\Rscript.exe`).
- TeX Live/tinytex or equivalent LaTeX toolchain (latexmk/pdflatex).
- Internet access to download packages and NHANES/mortality assets on first run.

Required R packages (installed automatically by the main script):
- tidyverse, broom, sandwich, lmtest, car, modelsummary, tinytex, glmnet, nhanesA, pROC, PRROC, ResourceSelection, doParallel

If your R has no CRAN mirror configured, the main script sets `https://cran.rstudio.com`.

## Quick Start

1) Generate clean matrices (from downloaded NHANES CSVs if already present, or fetch on demand if your flow includes `fetch_nhanes_raw.R`):

- Windows example:
  - `powershell.exe -NoProfile -Command "& 'C:\\Program Files\\R\\R-4.4.2\\bin\\Rscript.exe' 'scripts\\make_clean_matrices_only.R'"`

2) Build models and the report:

- Windows example:
  - `powershell.exe -NoProfile -Command "& 'C:\\Program Files\\R\\R-4.4.2\\bin\\Rscript.exe' 'Group Final Project.R'"`

Outputs:
- `output/report.pdf` (programmatically generated from utils/reporting.R)
- `output/report_final.pdf` (curated final LaTeX template, compiled after the pipeline writes all inputs)

## Data Pipeline (Chronological)

1. Raw ingestion (optional if CSVs already present):
   - DEMO_`<wave>`.csv (demographics) and SMQ_`<wave>`.csv (smoking) pulled via `nhanesA`.
   - Mortality public-use `.dat` read via CDCâ€™s setup script and saved to CSV.
   - All raw assets saved under `resources/nhanes` and `resources/mortality`.

2. Join and cleaning (scripts/make_clean_matrices_only.R):
   - Join DEMO and SMQ by `SEQN`, left-join mortality by `SEQN`.
   - Construct modeling variables:
     - `smoker`: Never/Former/Current from SMQ020/SMQ040.
     - `age`, `sex`, `log_income` (log1p PIR), plus `age_sq` (quadratic age).
   - Imputation and standardization:
     - Numeric (age, PIR): median impute + missing flags; standardized for modeling; build `log_income` from PIR.
     - Categorical (sex, smoker): missing -> explicit `Unknown` level (optionally dropped if configured).
   - Label-only row dropping: remove rows only if label is missing (`dead` preferred; fallback if needed).
   - Save outputs: `clean-data/X.csv`, `y.csv`, `cleaned_full.csv`, `meta.csv`.
   - Write data summaries (n/k, heads, dictionaries, imputation counts) into `output/tables/`.

3. Modeling and diagnostics (Group Final Project.R):
   - Weighted Logit (HC1 robust) with inverse-prevalence class weights.
   - Regularized Logit (glmnet CV) with the same features.
   - Diagnostics and metrics:
     - Residual and QQ plots, predicted risk by smoking (scaled 1â€“5 for presentation).
     - Classification metrics (Accuracy, LogLoss, Brier, ROC AUC) IN/OUT sample.
     - LASSO CV plot and coefficient tables.
   - Report build:
     - Generate `output/report.tex` and compile to `output/report.pdf`.
     - Compile `output/report_final.tex` using the generated tables.

## Reproducibility

- `CFG$seed` sets RNG for splits and CV; class weighting is deterministic given data.
- All paths and filenames are centralized in `config/config.R`.

## Troubleshooting

- Rscript not found: update the path in your shell command to your local R installation.
- CRAN mirror error: the main script sets a mirror if none is defined.
- LaTeX compile errors: ensure latexmk/pdflatex are installed and on PATH; all `.tex` inputs are written to `output/tables/` prior to compilation.

## Notes

- Raw assets are immutable in `resources/`; cleaning never overwrites them.
- The pipeline excludes race/education from modeling by design to avoid unnecessary complexity and collinearity here; update `utils/cleaning.R` if you want to re-introduce them.
- Monte Carlo and Neural Network components have been removed for simplicity and runtime efficiency.

## Cleaning Outputs / Artifacts

- The main script removes previous `output/tables`, `output/figures`, and `output/artifacts` at start.
- All LaTeX logs/aux files are generated inside `output/`, not the project root. A `.gitignore` excludes common temporary files and output artifacts.
