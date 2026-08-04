test_that("the fixture resolves every spec column against the real export", {
  spec <- vital_spec()
  v <- validate_export(fixture_raw(), spec)

  n_source_backed <- length(spec_variables(spec)) -
    length(spec_variables(spec, type = "derived"))
  expect_equal(length(v$columns), n_source_backed)
  expect_equal(v$n_analytic, 400L)
})

test_that("the degree-sign column resolves, proving the Latin-1 read worked", {
  # Under a C locale this column parses into garbage and every lookup misses.
  v <- validate_export(fixture_raw(), vital_spec())
  expect_true("temperature" %in% names(v$columns))
  expect_true(grepl("Average Temperature", v$columns[["temperature"]], fixed = TRUE))
})

test_that("a duplicate record ID in the analytic sample is a hard failure", {
  # The fixture plants exactly one duplicate.
  v <- validate_export(fixture_raw(), vital_spec())
  expect_false(v$ok)
  codes <- vapply(v$hard, `[[`, character(1), "code")
  expect_true("duplicate_ids" %in% codes)
  expect_error(stop_if_invalid(v), "more than once")
})

test_that("the repaired fixture passes validation", {
  v <- validate_export(clean_raw(), vital_spec())
  expect_true(v$ok)
  expect_length(v$hard, 0L)
})

test_that("a missing outcome column is a hard failure naming the column", {
  spec <- vital_spec()
  raw <- clean_raw()
  raw[["HMB calc"]] <- NULL
  v <- validate_export(raw, spec)

  expect_false(v$ok)
  msgs <- vapply(v$hard, `[[`, character(1), "message")
  expect_true(any(grepl("HMB calc", msgs, fixed = TRUE)))
})

test_that("a raw-value export gets a message telling staff what to re-export", {
  spec <- vital_spec()
  raw <- data.frame(record_id = "1", redcap_event_name = "baseline",
                    hmb_calc = "1", agecalc = "17",
                    check.names = FALSE, stringsAsFactors = FALSE)
  v <- validate_export(raw, spec)
  msgs <- vapply(v$hard, `[[`, character(1), "message")
  expect_true(any(grepl("re-export", msgs, ignore.case = TRUE)))
  expect_true(any(grepl("labels", msgs, ignore.case = TRUE)))
})

test_that("an outcome column present but entirely empty is a hard failure", {
  spec <- vital_spec()
  raw <- clean_raw()
  raw[["HMB calc"]] <- NA_character_
  v <- validate_export(raw, spec)

  codes <- vapply(v$hard, `[[`, character(1), "code")
  expect_true("outcome_all_missing" %in% codes)
})

test_that("a missing required column is hard but a missing optional one is soft", {
  spec <- vital_spec()

  raw <- clean_raw()
  raw[["agecalc"]] <- NULL              # age is required: true
  v <- validate_export(raw, spec)
  expect_false(v$ok)
  expect_true(any(grepl("Age", vapply(v$hard, `[[`, character(1), "message"))))

  raw2 <- clean_raw()
  raw2[["Serum Ferritin (ng/mL)"]] <- NULL   # not required
  v2 <- validate_export(raw2, spec)
  expect_true(v2$ok)
  expect_true("serum_ferritin" %in% v2$unmatched)
  expect_true(any(grepl("Serum ferritin",
                        vapply(v2$soft, `[[`, character(1), "message"))))
})

test_that("a source matching several columns is a hard failure, never a guess", {
  spec <- vital_spec()
  raw <- clean_raw()
  # Make "agecalc" match two columns and neither exactly.
  raw[["agecalc A"]] <- raw[["agecalc"]]
  raw[["agecalc B"]] <- raw[["agecalc"]]
  raw[["agecalc"]] <- NULL

  v <- validate_export(raw, spec)
  expect_false(v$ok)
  codes <- vapply(v$hard, `[[`, character(1), "code")
  expect_true("ambiguous_column" %in% codes)
  expect_true(any(grepl("agecalc A", vapply(v$hard, `[[`, character(1), "message"),
                        fixed = TRUE)))
})

test_that("zero rows surviving the event filter is a hard failure", {
  spec <- vital_spec()
  raw <- clean_raw()
  raw[["Event Name"]] <- "Month 6"
  v <- validate_export(raw, spec)

  expect_false(v$ok)
  codes <- vapply(v$hard, `[[`, character(1), "code")
  expect_true("no_matching_event" %in% codes)
})

test_that("a changed questionnaire item count is caught rather than rescaled", {
  spec <- vital_spec()
  raw <- clean_raw()
  raw[["L10. L item 10"]] <- "Not at all"   # PHQ-9 gains a tenth item

  v <- validate_export(raw, spec)
  expect_false(v$ok)
  codes <- vapply(v$hard, `[[`, character(1), "code")
  expect_true("item_count_mismatch" %in% codes)
})

test_that("every hard failure is reported at once, not just the first", {
  # Staff re-export once; they need the whole list in one go.
  spec <- vital_spec()
  raw <- fixture_raw()          # already has a duplicate ID
  raw[["agecalc"]] <- NULL      # and now a missing required column
  raw[["HMB calc"]] <- NULL     # and no outcome

  v <- validate_export(raw, spec)
  expect_false(v$ok)
  expect_gte(length(v$hard), 3L)

  msg <- tryCatch(stop_if_invalid(v), error = conditionMessage)
  expect_match(msg, "3 problems|[4-9] problems")
})
