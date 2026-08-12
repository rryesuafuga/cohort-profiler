# ---------------------------------------------------------------------------
# Generate run_report_standalone.R: a single file with the entire analysis
# embedded -- no GitHub download at run time.
#
# Run from the repo root after any change to R/, spec/ or inst/report.Rmd:
#   Rscript deploy/make_standalone.R
#
# The embedded copy is a snapshot: it does what the repo did on the day it
# was generated, which is exactly what you want when sharing with a
# collaborator (their results cannot change under them), and exactly what
# you must remember when the code moves on.
# ---------------------------------------------------------------------------

if (!file.exists("app.R")) {
  stop("Run this from the repo root.", call. = FALSE)
}

embed <- c(
  list.files("R", pattern = "\\.R$", full.names = TRUE),
  "spec/vital-hmb.yaml",
  "inst/report.Rmd"
)

sha <- tryCatch(
  system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE),
  error = function(e) "unknown")

out <- file("run_report_standalone.R", "w", encoding = "UTF-8")
w <- function(...) writeLines(c(...), out)

# --- header + preflight (mirrors run_report.R) ------------------------------

w(sprintf(r"---(#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# run_report_standalone.R -- cohort-profiler report, single self-contained file
#
# GENERATED from https://github.com/rryesuafuga/cohort-profiler
# at commit %s on %s by deploy/make_standalone.R. Do not edit by
# hand; regenerate instead.
#
# The entire analysis -- validation, data build, tables, report template and
# the VITAL-HMB spec -- is embedded below. Nothing is downloaded at run time.
# Output: a Word file (.docx) and a web page (.html) always, plus a PDF when
# LibreOffice is present. Still needed on the machine, because no R script
# can contain them:
#   * R 4.1+ and, on first run, internet to install missing CRAN packages
#   * pandoc (bundled with RStudio; plain-R users are pointed to an installer)
#   * LibreOffice for the PDF (optional and free; without it Word itself can
#     export the .docx to PDF via File > Save As)
#
# How to run
#   In RStudio:  open this file, click Source, pick your CSV when asked.
#   Terminal:    Rscript run_report_standalone.R export.csv [output_dir]
#
# Your data never leaves your machine.
# ---------------------------------------------------------------------------

message("cohort-profiler standalone report (embedded code, commit %s)")
message("-------------------------------------------------------------")

if (getRversion() < "4.1") {
  stop("This needs R 4.1 or newer; you have ", getRversion(),
       ". Please update R from https://cran.r-project.org", call. = FALSE)
}

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

if (!rmarkdown::pandoc_available("2.0")) {
  stop(paste(
    "pandoc was not found. The simplest fix is to run this script from",
    "RStudio (which bundles pandoc): https://posit.co/download/rstudio-desktop/",
    "Alternatively install pandoc itself from https://pandoc.org/installing.html"),
    call. = FALSE)
}
message("pandoc: OK (", as.character(rmarkdown::pandoc_version()), ")")
)---", sha, format(Sys.Date()), sha))

# --- embedded files ---------------------------------------------------------

w("", "# --- embedded analysis code, spec and template ------------------------------",
  "", ".embedded <- list()")

for (f in embed) {
  lines <- readLines(f, encoding = "UTF-8", warn = FALSE)
  w("", sprintf("# ==== %s (%d lines) ====", f, length(lines)),
    sprintf(".embedded[[%s]] <-", deparse(f)))
  # deparse() handles every quoting and escaping concern; the result is plain
  # R source that reproduces the file byte-for-byte on any locale.
  w(deparse(lines))
}

# --- runtime scaffolding ----------------------------------------------------

w(r"---(
# --- materialize the embedded files and run ---------------------------------

code_dir <- file.path(tempdir(), "cohort-profiler-embedded")
unlink(code_dir, recursive = TRUE)
for (path in names(.embedded)) {
  full <- file.path(code_dir, path)
  dir.create(dirname(full), recursive = TRUE, showWarnings = FALSE)
  writeLines(.embedded[[path]], full, useBytes = FALSE)
}
for (f in list.files(file.path(code_dir, "R"), pattern = "\\.R$",
                     full.names = TRUE)) {
  source(f)
}
options(cohortprofiler.home = code_dir)
spec_file <- file.path(code_dir, "spec", "vital-hmb.yaml")
message("Analysis code: OK (embedded)")

pdf_ok <- soffice_available()
message(if (pdf_ok) "LibreOffice: OK -- you will get DOCX, HTML and PDF"
        else paste("LibreOffice: not found -- you will get DOCX and HTML.",
                   "For a PDF too, install it free from https://libreoffice.org",
                   "and run this again (or open the Word file and Save As PDF)."))

args <- commandArgs(trailingOnly = TRUE)
data_file <- if (length(args) >= 1) args[[1]] else if (interactive()) {
  message("Choose your REDCap export (.csv) in the file dialog ...")
  file.choose()
} else {
  stop("Usage: Rscript run_report_standalone.R export.csv [output_dir]",
       call. = FALSE)
}
if (!file.exists(data_file)) {
  stop('Data file not found: "', data_file, '"', call. = FALSE)
}

out_dir <- if (length(args) >= 2) args[[2]] else {
  file.path(dirname(normalizePath(data_file)),
            paste0("cohort-report-", format(Sys.Date(), "%Y-%m-%d")))
}

message("\nBuilding the report from: ", basename(data_file))
message("This takes about half a minute ...\n")

result <- tryCatch(
  render_report(
    data_file = data_file,
    spec_file = spec_file,
    out_dir = out_dir,
    basename_out = paste0(tools::file_path_sans_ext(basename(data_file)),
                          "-report"),
    formats = if (pdf_ok) c("docx", "pdf", "html") else c("docx", "html")
  ),
  error = function(e) e
)

if (inherits(result, "error")) {
  message("\n", conditionMessage(result))
  quit(save = "no", status = 1)
}

message("Done. Your report:")
for (p in result) message("  ", normalizePath(p))
if (interactive()) utils::browseURL(out_dir)
)---")

close(out)

n <- length(readLines("run_report_standalone.R", warn = FALSE))
message(sprintf("Wrote run_report_standalone.R (%d lines, %.0f KB, commit %s)",
                n, file.size("run_report_standalone.R") / 1024, sha))
