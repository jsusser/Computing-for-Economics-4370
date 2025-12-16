Smoking and Mortality (NHANES 2013–2014)

Purpose
- Analyze smoking and mortality using NHANES and produce a single report PDF for submission.

Deliverable
- PDF: `output/report-to-be-turned-in.pdf`
- Source TeX: `output/report-to-be-turned-in.tex`

Quick Render (no R)
- `cd output`
- Preferred: `latexmk -pdf -interaction=nonstopmode -halt-on-error report-to-be-turned-in.tex`
- Or: `pdflatex -interaction=nonstopmode report-to-be-turned-in.tex` (run twice)
- Optional: clean aux files with `latexmk -c report-to-be-turned-in.tex`

Optional: Full Rebuild (R)
- Run the main script to regenerate tables/figures and compile the programmatic report:
  - Windows (example): `powershell.exe -NoProfile -Command "& 'C:\\Program Files\\R\\R-4.4.2\\bin\\Rscript.exe' 'Group Final Project.R'"`
- Outputs are written under `output/`.

Reproducibility
- Fixed seed (`CFG$seed`) controls train/test split and glmnet CV; CV runs non‑parallel for deterministic folds.
- Factor handling is fixed (smoker baseline `Never`; includes `smoker:age`).
- With the committed `clean-data/` inputs and current packages, results (tables/figures/PDF) are stable across runs.

Requirements
- TeX toolchain: `latexmk` (preferred) or `pdflatex` on PATH.
- R (only if doing the optional full rebuild).

Troubleshooting
- If rendering fails, ensure `latexmk`/`pdflatex` are installed and on PATH.
