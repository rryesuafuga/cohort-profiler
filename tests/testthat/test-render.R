# The slow one. It is kept in the suite anyway: it is the only test that
# exercises the whole path, and it is what catches a REDCap change, a pandoc
# upgrade or a LibreOffice problem before a study team does.

test_that("the fixture renders end to end to DOCX and PDF", {
  skip_if_not(nzchar(Sys.which("pandoc")), "pandoc is not installed")
  skip_if_not(nzchar(Sys.which("soffice")) || nzchar(Sys.which("libreoffice")),
              "LibreOffice is not installed")

  out_dir <- withr::local_tempdir()
  steps <- character(0)

  out <- render_report(
    data_file = clean_fixture_csv(),
    spec_file = vital_spec_path(),
    out_dir = out_dir,
    basename_out = "smoke",
    progress = function(frac, msg) steps <<- c(steps, msg)
  )

  expect_true(file.exists(out$docx))
  expect_true(file.exists(out$pdf))

  # A DOCX that renders but contains nothing still opens fine, so assert size.
  expect_gt(file.size(out$docx), 20000)
  expect_gt(file.size(out$pdf), 20000)

  # The PDF must really be a PDF, not a renamed failure.
  expect_identical(readBin(out$pdf, "raw", 4L), charToRaw("%PDF"))

  # The DOCX must be a real Word package.
  entries <- utils::unzip(out$docx, list = TRUE)$Name
  expect_true("word/document.xml" %in% entries)

  expect_true(length(steps) > 0)
})

test_that("rendering writes nothing into the app directory", {
  # Spaces storage is ephemeral and parts of the container filesystem are
  # read-only, so renders must stay in a temp directory.
  root <- repo_root()
  before <- list.files(root, recursive = FALSE)

  skip_if_not(nzchar(Sys.which("pandoc")), "pandoc is not installed")
  out_dir <- withr::local_tempdir()
  render_report(clean_fixture_csv(), vital_spec_path(),
                out_dir = out_dir, basename_out = "isolated", formats = "docx")

  expect_setequal(list.files(root, recursive = FALSE), before)
  expect_false(file.exists(file.path(root, "isolated.docx")))
})

test_that("a failing export aborts the render with the human-readable list", {
  out_dir <- withr::local_tempdir()
  bad <- file.path(out_dir, "bad.csv")
  # The unrepaired fixture has a duplicate record ID.
  utils::write.csv(fixture_raw(), bad, row.names = FALSE, na = "",
                   fileEncoding = "latin1")

  skip_if_not(nzchar(Sys.which("pandoc")), "pandoc is not installed")
  expect_error(
    render_report(bad, vital_spec_path(), out_dir = out_dir,
                  basename_out = "should-not-exist", formats = "docx"),
    "more than once")
  expect_false(file.exists(file.path(out_dir, "should-not-exist.docx")))
})

test_that("LibreOffice discovery reports honestly in both directions", {
  skip_if_not(nzchar(Sys.which("soffice")), "soffice not on PATH here")
  expect_true(soffice_available())
  expect_true(file.exists(find_soffice()))

  # With PATH emptied, discovery falls back to the standard install locations,
  # none of which exist on this machine — so it must say no, not guess.
  withr::local_envvar(PATH = "")
  expect_false(soffice_available())
})

test_that("a DOCX-only render neither produces nor promises a PDF", {
  skip_if_not(nzchar(Sys.which("pandoc")), "pandoc is not installed")
  out_dir <- withr::local_tempdir()
  out <- render_report(clean_fixture_csv(), vital_spec_path(),
                       out_dir = out_dir, basename_out = "docx-only",
                       formats = "docx")
  expect_true(file.exists(out$docx))
  expect_null(out$pdf)
  expect_length(list.files(out_dir, pattern = "\\.pdf$"), 0L)
})

test_that("outputs can be bundled into a zip", {
  out_dir <- withr::local_tempdir()
  a <- file.path(out_dir, "a.docx"); writeLines("a", a)
  b <- file.path(out_dir, "b.pdf");  writeLines("b", b)

  z <- file.path(out_dir, "bundle.zip")
  zip_outputs(list(docx = a, pdf = b), z)

  expect_true(file.exists(z))
  expect_setequal(utils::unzip(z, list = TRUE)$Name, c("a.docx", "b.pdf"))
})
