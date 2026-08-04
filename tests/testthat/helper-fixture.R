# Shared fixture plumbing.
#
# Tests run against data-raw/synthetic_vital_hmb.csv and nothing else. It is
# fully synthetic; real participant data must never enter this repo.

# Walk up from the working directory until the repo root is found, so tests
# work whether they are run from the root or from tests/testthat.
repo_root <- function() {
  here <- normalizePath(getwd(), mustWork = FALSE)
  for (i in 1:5) {
    if (file.exists(file.path(here, "spec", "vital-hmb.yaml"))) return(here)
    parent <- dirname(here)
    if (identical(parent, here)) break
    here <- parent
  }
  stop("Could not locate the repo root from ", getwd())
}

fixture_csv <- function() file.path(repo_root(), "data-raw", "synthetic_vital_hmb.csv")
vital_spec_path <- function() file.path(repo_root(), "spec", "vital-hmb.yaml")
vital_spec <- function() read_spec(vital_spec_path())

fixture_raw <- function() read_export(fixture_csv())

# The fixture plants a duplicate Record ID on purpose, so the hard gate has
# something to catch. Tests that need an export which passes validation repair
# it the way a study team would after reading the error.
repair_duplicate_ids <- function(raw, spec = vital_spec()) {
  id <- spec$record_id_column
  ev <- spec$filter$event_column
  dup <- which(duplicated(raw[[id]]) & raw[[ev]] == spec$filter$event_value)
  raw[[id]][dup] <- paste0(raw[[id]][dup], "-b")
  raw
}

clean_raw <- function() repair_duplicate_ids(fixture_raw())

# The export interleaves Baseline and follow-up rows, so row N of the file is
# not participant N of the analysis. Tests that plant a value in the analytic
# sample need the Baseline row numbers.
baseline_rows <- function(raw, spec = vital_spec()) {
  which(raw[[spec$filter$event_column]] == spec$filter$event_value)
}

# A repaired copy on disk, written once per test session. Latin-1 is preserved
# so the encoding path stays under test.
clean_fixture_csv <- local({
  cached <- NULL
  function() {
    if (!is.null(cached) && file.exists(cached)) return(cached)
    p <- file.path(tempdir(), "fixture-clean.csv")
    utils::write.csv(clean_raw(), p, row.names = FALSE, na = "",
                     fileEncoding = "latin1")
    cached <<- p
    p
  }
})

# Build an analysis frame once and reuse it: building is not free and most
# tests only inspect the result.
clean_analysis <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) cached <<- build_analysis_data(clean_raw(), vital_spec())
    cached
  }
})
