# Smoking and Mortality (NHANES 2013–2014)

Analyze smoking and mortality using NHANES DEMO/SMQ linked to NCHS mortality. The pipeline is modular and reproducible, producing a single LaTeX report with tables and figures in `output/`.

## Repository Layout

- `config/`
  - `config.R`: Central configuration (paths, packages, model settings).
  - `requirements.txt`: R package list (informational; the main script installs as needed).
- `utils/` (utility modules)
  - `data_nhanes.R`: Download/read NHANES DEMO/SMQ and mortality raw files.
  - `cleaning.R`: Build feature matrix `X` and label `y` with imputation and standardization.
  - `io.R`: Table/figure writers and report helpers.
  - `reporting.R`: Programmatic LaTeX report generator (`output/report.tex`).
  - `helpers.R`: Plot saving, panel grid, sanitization.
- `core/` (modeling & diagnostics)
  - `modeling.R`: Weighted logistic regression (HC1 robust SEs), includes age×smoker interaction.
  - `modeling_lasso.R`: Regularized logistic regression (glmnet with CV), includes the same features.
  - `diagnostics.R`: Residual, QQ, and prediction plots.
- `scripts/`
  - `fetch_nhanes_raw.R`: Download raw NHANES and mortality assets into `resources/`.
  - `make_clean_matrices_only.R`: Construct `clean-data/X.csv`, `y.csv`, `cleaned_full.csv`, `meta.csv` and write clean-data LaTeX snippets to `output/tables/`.
- `resources/`
  - `nhanes/` and `mortality/`: Raw input files as downloaded (never overwritten by cleaning).
- `clean-data/`
  - Model-ready matrices (`X.csv`, `y.csv`) and a full cleaned frame (`cleaned_full.csv`).
- `output/`
  - `report.tex`, `report.pdf`: Single programmatic LaTeX and PDF (the only report produced).
  - `tables/`, `figures/`, `artifacts/`: Auxiliary outputs for the report.

## Requirements

- R 4.4+ with Rscript (use your explicit Rscript path).
  - Windows examples:
    - System install: `C:\\Program Files\\R\\R-4.4.2\\bin\\Rscript.exe`
    - User install: `C:\\Users\\<you>\\bin\\Rscript.exe`
- TeX Live/tinytex or equivalent LaTeX toolchain (latexmk/pdflatex).
- Internet access to download packages and NHANES/mortality assets on first run.

Required R packages (installed automatically by the main script):
- tidyverse, broom, sandwich, lmtest, car, modelsummary, tinytex, glmnet, nhanesA, pROC, PRROC, ResourceSelection, doParallel

If your R has no CRAN mirror configured, the main script sets `https://cran.rstudio.com`.

## Quick Start

1) Generate clean matrices (from downloaded NHANES CSVs if already present, or fetch on demand if your flow includes `fetch_nhanes_raw.R`):

- Windows examples:
  - System Rscript: `powershell.exe -NoProfile -Command "& 'C:\\Program Files\\R\\R-4.4.2\\bin\\Rscript.exe' 'scripts\\make_clean_matrices_only.R'"`
  - User Rscript: `powershell.exe -NoProfile -Command "& 'C:\\Users\\<you>\\bin\\Rscript.exe' 'scripts\\make_clean_matrices_only.R'"`

2) Build models and the report:

- Windows examples:
  - System Rscript: `powershell.exe -NoProfile -Command "& 'C:\\Program Files\\R\\R-4.4.2\\bin\\Rscript.exe' 'Group Final Project.R'"`
  - User Rscript: `powershell.exe -NoProfile -Command "& 'C:\\Users\\<you>\\bin\\Rscript.exe' 'Group Final Project.R'"`

Outputs:
- `output/report.pdf` (programmatically generated from utils/reporting.R; this is the only report)

## Data Pipeline (Chronological)

1. Raw ingestion (optional if CSVs already present):
   - DEMO_`<wave>`.csv (demographics) and SMQ_`<wave>`.csv (smoking) pulled via `nhanesA`.
   - Mortality public-use `.dat` read via CDC’s setup script and saved to CSV.
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
     - Residual and QQ plots, predicted risk by smoking (scaled 1–5 for presentation).
     - Classification metrics (Accuracy, LogLoss, Brier, ROC AUC) IN/OUT sample.
     - LASSO CV plot and coefficient tables.
   - Report build:
     - Generate `output/report.tex` and compile to `output/report.pdf`.

## Reproducibility

- Deterministic seeds: `CFG$seed` governs splits and CV; class weights depend only on the label prevalence.
- Centralized config: all paths, package list, and filenames live in `config/config.R`.
- Baseline and interaction: smoker baseline is set to `Never` across all models; the age×smoker interaction is included in both logistic and regularized models.
- One report: only `output/report.pdf` is produced; the script cleans previous outputs and LaTeX aux files.

## Troubleshooting

- Rscript not found: update the path in your shell command to your local R installation.
- CRAN mirror error: the main script sets a mirror if none is defined.
- LaTeX compile errors: ensure latexmk/pdflatex are installed and on PATH; all `.tex` inputs are written to `output/tables/` prior to compilation.

## Notes

- Raw assets are immutable in `resources/`; cleaning never overwrites them.
- The pipeline excludes race/education from modeling by design to reduce collinearity; update `utils/cleaning.R` to re-introduce them.
- Neural Network components have been removed; only logistic and regularized logistic models are kept.

## Cleaning Outputs / Artifacts

- The main script removes previous `output/tables`, `output/figures`, and `output/artifacts` at start.
- It also removes stale `report_final.*` / `report_testing.*` and LaTeX aux files, leaving only `report.tex` and `report.pdf` plus subfolders.
- All artifacts live under `output/`; the repo keeps `report-to-be-turned-in.tex` at the root as requested.

