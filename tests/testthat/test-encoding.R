# REDCap label exports are ISO-8859-1. Reading one as UTF-8 does not raise an
# error; it silently produces column names that are not valid UTF-8, after which
# lookups miss and the analytic sample can come back empty. The fixture carries
# a degree sign in the temperature header specifically to keep this testable.

test_that("the fixture really does contain a non-ASCII header byte", {
  bytes <- readBin(fixture_csv(), "raw", 4096L)
  expect_true(as.raw(0xB0) %in% bytes)
})

test_that("reading as ISO-8859-1 gives a valid, usable temperature column", {
  raw <- fixture_raw()
  hit <- grep("Average Temperature", names(raw), fixed = TRUE, value = TRUE)

  expect_length(hit, 1L)
  expect_true(all(validUTF8(names(raw))))
  # The degree sign survives as U+00B0.
  expect_true(grepl("°", hit, fixed = TRUE))
})

test_that("reading as UTF-8 corrupts the header instead of failing loudly", {
  # This is the failure this repo has to defend against: nothing errors, the
  # column name is simply no longer a string anything can match on.
  raw <- read_export(fixture_csv(), encoding = "UTF-8")

  expect_false(all(validUTF8(names(raw))))
  # grep warns that the string is not valid UTF-8; the point is that it also
  # finds nothing, so the column is unreachable by name.
  expect_length(
    suppressWarnings(grep("Average Temperature", names(raw), fixed = TRUE)), 0L)
})

test_that("the degree-sign column carries real data through to the analysis", {
  d <- clean_analysis()
  expect_true(is.numeric(d$temperature))
  expect_lt(mean(is.na(d$temperature)), 0.1)
  expect_true(all(d$temperature >= 34 & d$temperature <= 42, na.rm = TRUE))
})
