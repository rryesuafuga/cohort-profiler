test_that("the VITAL-HMB spec loads and is internally coherent", {
  spec <- vital_spec()
  expect_s3_class(spec, "cohort_spec")
  expect_gt(length(spec$variables), 40)
  expect_identical(spec$outcome$name, "hmb")

  # Every variable has the fields the rest of the code assumes.
  for (v in spec$variables) {
    expect_true(nzchar(v$name), info = v$name)
    expect_true(nzchar(v$label), info = v$name)
    expect_true(v$type %in% c("numeric", "factor", "binary", "ordinal", "derived"),
                info = v$name)
    if (v$type != "derived") expect_true(nzchar(v$source), info = v$name)
  }
})

test_that("declared factor levels survive YAML parsing", {
  # YAML 1.1 reads bare No and Yes as booleans. If that ever slips back into
  # the spec, every binary variable silently becomes all-missing.
  spec <- vital_spec()
  for (v in spec_variables(spec, type = c("factor", "binary", "ordinal"))) {
    expect_type(v$levels, "character")
    expect_false(any(v$levels %in% c("TRUE", "FALSE")), info = v$name)
  }
  expect_true("Yes" %in% spec_variable(spec, "treated_anaemia")$levels)
})

test_that("a spec with unquoted Yes/No levels is rejected with a useful message", {
  p <- withr::local_tempfile(fileext = ".yaml")
  writeLines(c(
    "record_id_column: id",
    "outcome: {name: y, source: Y, type: binary}",
    "filter: {event_column: E, event_value: Baseline}",
    "variables:",
    "  - name: v",
    "    source: V",
    "    type: binary",
    "    levels: [No, Yes]"), p)
  expect_error(read_spec(p), "quote", ignore.case = TRUE)
})

test_that("a spec naming an unknown type is rejected", {
  p <- withr::local_tempfile(fileext = ".yaml")
  writeLines(c(
    "record_id_column: id",
    "outcome: {name: y, source: Y, type: binary}",
    "filter: {event_column: E, event_value: Baseline}",
    "variables:",
    "  - {name: v, source: V, type: quantum}"), p)
  expect_error(read_spec(p), "unknown type")
})

test_that("match_column prefers an exact match over a substring", {
  cols <- c("Mattress", "Bed with mattress", "Radio")
  # "Mattress" is a substring of nothing here but is a prefix of nothing else;
  # the exact match must win regardless.
  expect_identical(match_column("Mattress", cols)$column, "Mattress")
  expect_identical(match_column("Mattress", cols)$status, "ok")
})

test_that("match_column reports ambiguity instead of guessing", {
  cols <- c("agecalc A", "agecalc B")
  m <- match_column("agecalc", cols)
  expect_identical(m$status, "ambiguous")
  expect_length(m$candidates, 2L)

  expect_identical(match_column("nothing here", cols)$status, "missing")
})
