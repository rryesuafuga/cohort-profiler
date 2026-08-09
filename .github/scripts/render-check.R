#!/usr/bin/env Rscript
# CI smoke check, run INSIDE the built Docker image (mounted at /ci, working
# directory /app). Exercises the whole pipeline in the environment Hugging
# Face Spaces will actually run: the Latin-1 read, the validation gate, the
# pandoc render and the LibreOffice PDF conversion.
#
# The fixture plants a duplicate Record ID on purpose so validation has
# something to catch; repair it the way a study team would, then render.

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

raw <- read_export("data-raw/synthetic_vital_hmb.csv")
dup <- which(duplicated(raw[["Record ID"]]) & raw[["Event Name"]] == "Baseline")
raw[["Record ID"]][dup] <- paste0(raw[["Record ID"]][dup], "-b")

fixed <- file.path(tempdir(), "fixture-repaired.csv")
write.csv(raw, fixed, row.names = FALSE, na = "", fileEncoding = "latin1")

out <- render_report(fixed, "spec/vital-hmb.yaml",
                     out_dir = file.path(tempdir(), "render-ci"))

stopifnot(
  "DOCX missing"     = file.exists(out$docx),
  "DOCX trivially small" = file.size(out$docx) > 20000,
  "PDF missing"      = file.exists(out$pdf),
  "PDF trivially small"  = file.size(out$pdf) > 20000,
  "PDF is not a real PDF" =
    identical(readBin(out$pdf, "raw", 4L), charToRaw("%PDF"))
)

cat(sprintf("render OK: docx %.0f KB, pdf %.0f KB\n",
            file.size(out$docx) / 1024, file.size(out$pdf) / 1024))
