# cohort-profiler

A Shiny app that takes a REDCap export, validates it against a declarative spec,
and renders a descriptive + univariate analysis report as DOCX and PDF.

The first spec is `spec/vital-hmb.yaml` (VITAL-HMB study, outcome = heavy menstrual
bleeding by alkaline hematin). The app must stay study-agnostic: study knowledge
lives in the spec, never in the R code.

---

## Non-negotiables

Read this list before writing code. These are the constraints that are expensive
to discover late.

1. **`flextable`, not `gt`.** The original report used `gt`, which is HTML-first.
   Output must be DOCX and PDF, so all tables go through `flextable` — normally
   via `gtsummary::tbl_summary() |> gtsummary::as_flex_table()`. Do not
   reintroduce `gt` anywhere in `inst/report.Rmd`.

2. **One render, then convert.** Render to DOCX with pandoc, then produce the PDF
   by converting that DOCX with headless LibreOffice:
   `soffice --headless --convert-to pdf --outdir <dir> <docx>`.
   Do not install TinyTeX, and do not add a `pdf_document` output format. The PDF
   must be a conversion of the DOCX so the two always agree.

3. **Port 7860, host 0.0.0.0.** Hugging Face Spaces requires this. Run the app
   directly (`shiny::runApp(host = "0.0.0.0", port = 7860)`); do not install or
   configure shiny-server.

4. **The container must set a UTF-8 locale.** Put
   `ENV LANG=C.UTF-8 LC_ALL=C.UTF-8` in the Dockerfile. The export is ISO-8859-1
   and contains non-ASCII characters (the degree sign in the temperature column
   header, at minimum). Under `LANG=C`, R's Latin-1 conversion fails silently
   and the CSV parses into garbage — every column key then appears ambiguous and
   the analytic sample comes back as zero rows. This is verified: the fixture
   reproduces the failure exactly. Do not remove the ENV line.

5. **Never commit real participant data.** `data-raw/synthetic_vital_hmb.csv` is
   the only dataset in this repo, and it is fully synthetic. All development,
   tests and examples use it. If you need a new field for testing, add it to
   `data-raw/make_synthetic_fixture.R` and regenerate — never paste in real rows.
   `.gitignore` blocks `*.csv` outside `data-raw/`; keep it that way.

6. **The spec is the single source of truth.** Variable names, source columns,
   labels, types, valid ranges, factor levels, domain grouping and consistency
   rules all live in `spec/*.yaml`. Do not hardcode variable lists in `R/` or in
   the Rmd. Adding a variable must mean editing only the YAML.

7. **Fail loudly on schema drift.** The original report matched columns by
   substring of the REDCap *label* and silently returned all-NA when a column was
   missing. That bug is the reason this repo exists. `validate_export()` must
   report every unmatched required column and abort. Never return a silent NA
   column.

---

## Architecture

```
app.R                     Shiny UI + server; thin — logic lives in R/
R/validate.R              validate_export(): schema gate, returns a report object
R/build_data.R            build_analysis_data(): export + spec -> tidy analysis frame
R/tables.R                descriptive and univariate table builders (flextable)
R/render.R                render_report(): Rmd -> DOCX -> PDF
inst/report.Rmd           parameterised report; params: data_file, spec_file
spec/vital-hmb.yaml       variable spec + consistency rules
data-raw/                 fixture generator + generated synthetic CSV
tests/testthat/           unit tests + full render smoke test
Dockerfile                rocker base + libreoffice-writer
README.md                 HF Spaces frontmatter (sdk: docker, app_port: 7860)
```

**Data flow.** Upload → `validate_export()` → hard failures abort with a
human-readable list; soft failures collect into a data-quality section →
`build_analysis_data()` → tidy frame (one row per participant, canonical
snake_case names, declared factor levels, out-of-range values already NA) →
`render_report()`.

The tidy frame is the only thing the report sees. Nothing downstream of
`build_analysis_data()` should know what REDCap is.

---

## The spec format

```yaml
outcome:
  name: hmb
  source: "HMB calc"
  type: binary
  labels: {0: Non-HMB, 1: HMB}

filter:
  event_column: "Event Name"
  event_value: Baseline
  require_nonmissing: ["HMB calc"]

variables:
  - name: age
    source: "agecalc"
    domain: Demographics
    label: Age (years)
    type: numeric
    range: [12, 45]
    required: true

  - name: region
    source: "B1. Which region"
    domain: Demographics
    label: Region
    type: factor
    levels: [Central, Eastern, Northern, Western, Other]

checks:
  - name: Age at menarche < current age
    expr: age_menarche < age
  - name: Systolic BP >= diastolic BP
    expr: sbp >= dbp
```

Notes on semantics:

- `source` is matched against export column names: exact match first, then unique
  substring. **A substring matching more than one column is an error**, not a
  first-match-wins situation.
- `levels` are declared, not derived. A value outside the declared levels is a
  soft failure and is reported, never silently folded into "Other".
- `range` values outside the interval become NA and are counted in the
  data-quality report.
- `type` is one of: `numeric`, `factor`, `binary`, `ordinal`, `derived`.
- Derived variables (SES index from asset PCA, MAP from BP, PHQ-9/GAD-7 item
  means) use `type: derived` with an explicit `expr` or a named function in
  `R/derive.R`. The asset-PCA in particular must take an explicit column list
  from the spec — the original derived it by regex on column names, so adding a
  checkbox silently changed the index.

---

## Validation: hard vs soft

**Hard (abort the render, list everything wrong at once — do not stop at the first error):**
- outcome column absent, or present but all missing
- record ID or event column absent
- duplicate record IDs within the analytic sample
- any `required: true` variable whose source column is unmatched
- a `source` substring matching multiple columns
- zero rows surviving the event filter

**Soft (render, and surface in the data-quality section):**
- unmatched non-required columns
- values outside `range`
- factor values outside declared `levels`
- variables above a missingness threshold (default 60%)
- consistency-rule violations

Error messages are read by non-technical staff. Write
`Column "HMB calc" not found. This looks like a raw-value export — re-export from
REDCap with labels.` Not `subscript out of bounds`.

---

## Commands

```bash
Rscript data-raw/make_synthetic_fixture.R      # regenerate the fixture
Rscript -e 'devtools::load_all(); testthat::test_dir("tests/testthat")'
Rscript -e 'shiny::runApp(port = 7860, host = "0.0.0.0")'
docker build -t cohort-profiler . && docker run -p 7860:7860 cohort-profiler
```

Dependencies are pinned with `renv`. After adding a package:
`renv::snapshot()` and commit `renv.lock`.

---

## Testing

`tests/testthat/` must cover, at minimum:

- `validate_export()` returns hard failures for: missing outcome, missing
  required column, duplicate IDs, ambiguous substring match
- `build_analysis_data()` produces the expected column set and types from the
  fixture
- out-of-range values become NA and are counted
- undeclared factor levels are reported, not silently absorbed
- **a full end-to-end render of the fixture to DOCX and PDF, asserting both files
  exist and are non-trivial in size** — this is the test that catches breakage
  from a REDCap change or a pandoc/LibreOffice issue

The render test is slow; keep it in the suite anyway. It is the only test that
exercises the whole path.

---

## Shiny specifics

- Render in `tempdir()`, never the app directory — the container filesystem is
  read-only in places and Spaces storage is ephemeral.
- `envir = new.env(parent = globalenv())` on every `rmarkdown::render()` call.
- Wrap renders in `withProgress()`. A render takes 20–40s and an unresponsive
  button reads as a crash.
- Uploads: cap at ~50 MB, accept `.csv` only, and read with
  `readr::locale(encoding = "ISO-8859-1")` — REDCap label exports are Latin-1.
- Offer DOCX and PDF as separate download buttons, plus a combined zip.
- Delete uploads and rendered files at session end
  (`session$onSessionEnded`). Participant data must not persist in the container.

---

## Statistical conventions

Preserve the original report's choices; they were deliberate.

- Continuous: median [IQR] and mean (SD), Wilcoxon rank-sum p-value.
- Categorical: n (%) to 1 decimal, chi-square, falling back to Fisher
  (`simulate.p.value = TRUE`) when any expected cell < 5.
- Univariate logistic per variable, OR with Wald 95% CI.
- Skip a variable if fewer than 30 complete cases, if a numeric variable has no
  variance, or if a categorical variable has an empty outcome margin. Several
  variables in VITAL-HMB separate the outcome perfectly (`treated_anaemia`,
  `family_bleeding`) — these must be reported as non-estimable rather than
  producing an OR of 10^8.
- Missingness: per-variable table, Little's MCAR test on numeric variables with
  <60% missing, and a chi-square probe of missingness against the outcome.
- All univariate associations are unadjusted. Multivariable and LASSO modelling
  live in a separate Python pipeline and are explicitly out of scope here.

---

## Out of scope

Do not add: multivariable models, imputation, authentication, a database,
persistent storage, or study-specific text in the R code.
