#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# run_report.R -- standalone runner for the cohort-profiler report
#
# Share THIS ONE FILE. It fetches the analysis code from GitHub, installs any
# missing R packages, checks your REDCap export against the study spec, and
# writes the report next to your data:
#
#   * a Word file (.docx), always
#   * a PDF, when LibreOffice is installed (free, libreoffice.org);
#     without it you still get the Word file, which Word itself can
#     export to PDF via File > Save As
#
# How to run
#   In RStudio:  open this file, click Source, pick your CSV when asked.
#   Terminal:    Rscript run_report.R path/to/redcap_export.csv [output_dir]
#
# Requirements: R 4.1 or newer, internet access on the first run, and pandoc
# (bundled with RStudio; plain-R users get pointed to an installer).
#
# Your data never leaves your machine -- the only download is the analysis
# code, fetched from the public repository:
REPO_ZIP <- "https://github.com/rryesuafuga/cohort-profiler/archive/refs/heads/main.zip"
# ---------------------------------------------------------------------------

message("cohort-profiler standalone runner")
message("---------------------------------")

# --- 1. R version ----------------------------------------------------------

if (getRversion() < "4.1") {
  stop("This needs R 4.1 or newer; you have ", getRversion(),
       ". Please update R from https://cran.r-project.org", call. = FALSE)
}

# --- 2. Packages -----------------------------------------------------------

needed <- c("yaml", "readr", "dplyr", "gtsummary", "flextable",
            "rmarkdown", "knitr", "naniar", "ggplot2")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
  still <- missing[!vapply(missing, requireNamespace, logical(1), quietly = TRUE)]
  if (length(still)) {
    stop("Could not install: ", paste(still, collapse = ", "),
         ". Check your internet connection and try again.", call. = FALSE)
  }
}
message("Packages: OK")

# --- 3. pandoc -------------------------------------------------------------

if (!rmarkdown::pandoc_available("2.0")) {
  stop(paste(
    "pandoc was not found. The simplest fix is to run this script from",
    "RStudio (which bundles pandoc): https://posit.co/download/rstudio-desktop/",
    "Alternatively install pandoc itself from https://pandoc.org/installing.html"),
    call. = FALSE)
}
message("pandoc: OK (", as.character(rmarkdown::pandoc_version()), ")")

# --- 4. Analysis code ------------------------------------------------------

code_dir <- file.path(tempdir(), "cohort-profiler-code")
if (!dir.exists(file.path(code_dir, "R"))) {
  message("Downloading the analysis code ...")
  zipfile <- file.path(tempdir(), "cohort-profiler.zip")
  ok <- tryCatch({
    utils::download.file(REPO_ZIP, zipfile, mode = "wb", quiet = TRUE)
    TRUE
  }, error = function(e) FALSE)
  if (!ok) {
    stop("Could not download the analysis code from GitHub. ",
         "Check your internet connection.", call. = FALSE)
  }
  utils::unzip(zipfile, exdir = tempdir())
  extracted <- list.dirs(tempdir(), recursive = FALSE)
  extracted <- extracted[grepl("cohort-profiler-", basename(extracted))]
  file.rename(extracted[[1]], code_dir)
}
for (f in list.files(file.path(code_dir, "R"), pattern = "\\.R$",
                     full.names = TRUE)) {
  source(f)
}
# Tell the render where the code lives: we are running from the user's data
# folder, so it cannot be discovered from the working directory.
options(cohortprofiler.home = code_dir)
spec_file <- file.path(code_dir, "spec", "vital-hmb.yaml")
message("Analysis code: OK")

# --- 5. LibreOffice (PDF is optional) --------------------------------------

pdf_ok <- soffice_available()
message(if (pdf_ok) "LibreOffice: OK -- you will get DOCX and PDF"
        else paste("LibreOffice: not found -- you will get the Word file only.",
                   "For a PDF too, install it free from https://libreoffice.org",
                   "and run this again (or open the Word file and Save As PDF)."))

# --- 6. The data file ------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
data_file <- if (length(args) >= 1) args[[1]] else if (interactive()) {
  message("Choose your REDCap export (.csv) in the file dialog ...")
  file.choose()
} else {
  stop("Usage: Rscript run_report.R path/to/redcap_export.csv [output_dir]",
       call. = FALSE)
}
if (!file.exists(data_file)) {
  stop('Data file not found: "', data_file, '"', call. = FALSE)
}

out_dir <- if (length(args) >= 2) args[[2]] else {
  file.path(dirname(normalizePath(data_file)),
            paste0("cohort-report-", format(Sys.Date(), "%Y-%m-%d")))
}

# --- 7. Render -------------------------------------------------------------

message("\nBuilding the report from: ", basename(data_file))
message("This takes about half a minute ...\n")

result <- tryCatch(
  render_report(
    data_file = data_file,
    spec_file = spec_file,
    out_dir = out_dir,
    basename_out = paste0(tools::file_path_sans_ext(basename(data_file)),
                          "-report"),
    formats = if (pdf_ok) c("docx", "pdf") else "docx"
  ),
  error = function(e) e
)

if (inherits(result, "error")) {
  # Validation failures arrive here with the full human-readable list.
  message("\n", conditionMessage(result))
  quit(save = "no", status = 1)
}

message("Done. Your report:")
for (p in result) message("  ", normalizePath(p))
if (interactive()) utils::browseURL(out_dir)
