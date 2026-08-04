# ---------------------------------------------------------------------------
# Rendering.
#
# One render, then convert. The report is knitted to DOCX with pandoc, and the
# PDF is produced by converting that DOCX with headless LibreOffice. There is
# no LaTeX in this pipeline and no second rendering path, so the two files
# cannot disagree with each other.
# ---------------------------------------------------------------------------

#' Root of the installed app, whether running from source or from a package.
app_root <- function() {
  p <- system.file(package = "cohortprofiler")
  if (nzchar(p) && file.exists(file.path(p, "report.Rmd"))) return(p)
  # Running from a source checkout: this file lives in <root>/R.
  here <- getwd()
  for (i in 1:4) {
    if (file.exists(file.path(here, "inst", "report.Rmd"))) return(here)
    here <- dirname(here)
  }
  getwd()
}

#' Path to the report template.
report_template <- function() {
  root <- app_root()
  cand <- c(file.path(root, "inst", "report.Rmd"), file.path(root, "report.Rmd"))
  hit <- cand[file.exists(cand)]
  if (!length(hit)) {
    stop("Could not find report.Rmd. Expected it at inst/report.Rmd.", call. = FALSE)
  }
  hit[[1]]
}

#' Directory holding the R sources the report needs.
r_source_dir <- function() {
  root <- app_root()
  cand <- c(file.path(root, "R"), file.path(root, "Rsrc"))
  hit <- cand[dir.exists(cand)]
  if (length(hit)) hit[[1]] else NULL
}

#' Convert a DOCX to PDF with headless LibreOffice.
#'
#' LibreOffice needs a writable profile directory. In a container the default
#' location under $HOME may not be writable, and the failure surfaces as an
#' empty error, so the profile is pointed at a temporary directory explicitly.
#'
#' @return Path to the PDF.
docx_to_pdf <- function(docx, out_dir = dirname(docx), timeout = 180) {
  soffice <- Sys.which("soffice")
  if (!nzchar(soffice)) soffice <- Sys.which("libreoffice")
  if (!nzchar(soffice)) {
    stop("LibreOffice was not found, so the PDF cannot be produced. Install libreoffice-writer.",
         call. = FALSE)
  }
  profile <- file.path(tempdir(), "lo_profile")
  dir.create(profile, showWarnings = FALSE, recursive = TRUE)

  args <- c(sprintf("-env:UserInstallation=file://%s", profile),
            "--headless", "--norestore", "--convert-to", "pdf",
            "--outdir", out_dir, docx)

  # R puts its own library directory, and the JVM's, on LD_LIBRARY_PATH. A
  # child process inherits that and LibreOffice then loads the wrong shared
  # libraries and refuses to open any document, reporting only "source file
  # could not be loaded". Clearing the variable for this call fixes it, and
  # matters in the container too, where the rocker images set it.
  res <- suppressWarnings(
    system2(soffice, args, stdout = TRUE, stderr = TRUE,
            env = "LD_LIBRARY_PATH=", timeout = timeout))

  pdf <- file.path(out_dir, sub("\\.docx$", ".pdf", basename(docx)))
  if (!file.exists(pdf)) {
    stop(sprintf("Converting the report to PDF failed.\n%s",
                 paste(res, collapse = "\n")), call. = FALSE)
  }
  pdf
}

#' Render the report for one export.
#'
#' @param data_file Path to the REDCap CSV export.
#' @param spec_file Path to the study spec YAML.
#' @param out_dir Where to write the outputs. Defaults to a fresh temp dir,
#'   because the app directory is not reliably writable in a container.
#' @param basename_out Base name for the output files, without extension.
#' @param formats Which outputs to produce: "docx", "pdf", or both.
#' @param progress Optional function taking (fraction, message) for UI updates.
#' @return A named list of file paths.
render_report <- function(data_file,
                          spec_file,
                          out_dir = NULL,
                          basename_out = "cohort-report",
                          formats = c("docx", "pdf"),
                          progress = NULL) {
  formats <- match.arg(formats, c("docx", "pdf"), several.ok = TRUE)

  # Resolve every input path before rendering: rmarkdown::render() changes the
  # working directory, after which a relative path no longer means what it did.
  data_file <- normalizePath(data_file, mustWork = TRUE)
  spec_file <- normalizePath(spec_file, mustWork = TRUE)
  r_dir     <- r_source_dir()
  if (!is.null(r_dir)) r_dir <- normalizePath(r_dir, mustWork = TRUE)

  if (is.null(out_dir)) out_dir <- file.path(tempdir(), paste0("render-", as.integer(Sys.time())))
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  say <- function(frac, msg) if (is.function(progress)) progress(frac, msg)

  # Knit a copy in the output directory: rendering in place would write
  # intermediates into the app directory.
  template <- report_template()
  rmd <- file.path(out_dir, paste0(basename_out, ".Rmd"))
  file.copy(template, rmd, overwrite = TRUE)

  say(0.1, "Checking the export against the spec")

  # The document title comes from the spec, so it is read here rather than
  # hardcoded: the Rmd's YAML header is evaluated before its setup chunk runs.
  spec <- read_spec(spec_file)
  title <- paste(c(spec$study, spec$title), collapse = " ")
  if (!nzchar(trimws(title))) title <- "Cohort Profile"

  docx <- file.path(out_dir, paste0(basename_out, ".docx"))
  say(0.2, "Building tables")

  rmarkdown::render(
    input = rmd,
    output_format = rmarkdown::word_document(toc = TRUE, toc_depth = 2),
    output_file = basename(docx),
    output_dir = out_dir,
    intermediates_dir = out_dir,
    knit_root_dir = out_dir,
    params = list(
      data_file      = data_file,
      spec_file      = spec_file,
      r_dir          = r_dir,
      title_override = title
    ),
    envir = new.env(parent = globalenv()),
    quiet = TRUE
  )
  if (!file.exists(docx)) {
    stop("The report did not produce a Word file.", call. = FALSE)
  }

  out <- list(docx = docx)
  if ("pdf" %in% formats) {
    say(0.85, "Converting to PDF")
    out$pdf <- docx_to_pdf(docx, out_dir)
  }
  if (!"docx" %in% formats) out$docx <- NULL

  say(1, "Done")
  out
}

#' Bundle rendered outputs into a zip.
zip_outputs <- function(paths, zipfile) {
  paths <- unlist(paths, use.names = FALSE)
  paths <- paths[!is.na(paths) & file.exists(paths)]
  if (!length(paths)) stop("Nothing to zip.", call. = FALSE)
  zip::zip(zipfile, files = basename(paths), root = dirname(paths[[1]]))
  zipfile
}
