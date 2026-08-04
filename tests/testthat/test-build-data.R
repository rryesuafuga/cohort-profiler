test_that("build_analysis_data produces one row per participant", {
  d <- clean_analysis()
  expect_equal(nrow(d), 400L)
  expect_false(anyDuplicated(d$.record_id) > 0)
})

test_that("the analysis frame has the declared columns with the declared types", {
  spec <- vital_spec()
  d <- clean_analysis()

  expect_true(spec$outcome$name %in% names(d))
  expect_s3_class(d[[spec$outcome$name]], "factor")
  expect_identical(levels(d$hmb), c("Non-HMB", "HMB"))

  for (v in spec_variables(spec)) {
    expect_true(v$name %in% names(d), info = v$name)
    x <- d[[v$name]]
    if (v$type %in% c("numeric", "ordinal", "derived")) {
      expect_true(is.numeric(x), info = v$name)
    } else {
      expect_s3_class(x, "factor")
      expect_identical(levels(x), v$levels, info = v$name)
    }
  }
})

test_that("the analysis frame carries no REDCap vocabulary", {
  # Nothing downstream of this point should know what REDCap is.
  d <- clean_analysis()
  expect_false(any(grepl("^(B|C|D|E|F|H|I|S|L|M|J|K)\\d+\\.", names(d))))
  expect_false(any(grepl("redcap", names(d), ignore.case = TRUE)))
})

test_that("out-of-range values become NA and are counted", {
  # The fixture plants BMI 6.2 and 78.4, and heart rate 12 and 240.
  d <- clean_analysis()
  q <- data_quality(d)

  expect_false(is.null(q$out_of_range))
  expect_setequal(q$out_of_range$variable, c("bmi", "heart_rate"))
  expect_equal(q$out_of_range$n[q$out_of_range$variable == "bmi"], 2L)
  expect_equal(q$out_of_range$n[q$out_of_range$variable == "heart_rate"], 2L)

  spec <- vital_spec()
  rng <- spec_variable(spec, "bmi")$range
  expect_true(all(d$bmi >= rng[[1]] & d$bmi <= rng[[2]], na.rm = TRUE))
  expect_false(any(d$heart_rate > 160, na.rm = TRUE))
})

test_that("undeclared factor levels are reported, never folded into a category", {
  spec <- vital_spec()
  raw <- clean_raw()
  # A new response appears that the spec does not declare.
  target <- head(baseline_rows(raw), 7L)
  raw[["B2. In what type of environment do you live?"]][target] <- "Peri-urban"

  d <- build_analysis_data(raw, spec)
  q <- data_quality(d)

  expect_false(is.null(q$bad_levels))
  hit <- q$bad_levels[q$bad_levels$variable == "residence", ]
  expect_equal(nrow(hit), 1L)
  expect_match(hit$value, "Peri-urban")
  expect_equal(hit$n, 7L)

  # Reported, and left missing rather than absorbed into an existing level.
  expect_false("Peri-urban" %in% levels(d$residence))
  expect_identical(levels(d$residence), c("Rural", "Semi-urban", "Urban"))
  expect_equal(sum(is.na(d$residence[1:7])), 7L)
})

test_that("non-answer strings are treated as missing, not as a category", {
  spec <- vital_spec()
  raw <- clean_raw()
  target <- head(baseline_rows(raw), 5L)
  raw[["B1. Which region do you come from?"]][target] <- "Prefer not to say"

  d <- build_analysis_data(raw, spec)
  expect_true(all(is.na(d$region[1:5])))
  expect_false("Prefer not to say" %in% levels(d$region))
  # Treated as missing, not counted as an undeclared level.
  bad <- data_quality(d)$bad_levels
  expect_false("region" %in% bad$variable)
})

test_that("declared recodes collapse levels explicitly", {
  d <- clean_analysis()
  # "Moslem" in the export is declared as "Muslim"; Married/Separated collapse.
  expect_true("Muslim" %in% levels(d$religion))
  expect_false("Moslem" %in% levels(d$religion))
  expect_gt(sum(d$religion == "Muslim", na.rm = TRUE), 0L)

  expect_identical(levels(d$marital), c("Ever-married", "Single"))
  expect_gt(sum(d$marital == "Ever-married", na.rm = TRUE), 0L)
})

test_that("ordinal variables are scored as declared", {
  d <- clean_analysis()
  expect_true(is.numeric(d$financial))
  expect_gte(min(d$financial, na.rm = TRUE), 0)
  expect_lte(max(d$financial, na.rm = TRUE), 4)
})

test_that("derived variables are computed from spec inputs", {
  d <- clean_analysis()

  # Mean arterial pressure follows its declared expression exactly.
  expect_equal(d$map, (d$sbp + 2 * d$dbp) / 3, tolerance = 1e-8)

  # The asset index is oriented so higher means wealthier, not by luck.
  expect_true(is.numeric(d$ses_index))
  expect_gt(stats::sd(d$ses_index, na.rm = TRUE), 0)

  # Item means sit inside the declared score range.
  for (nm in c("phq9_depression", "gad7_anxiety", "pms_score", "qol_score")) {
    expect_true(is.numeric(d[[nm]]), info = nm)
    expect_gte(min(d[[nm]], na.rm = TRUE), 0, label = nm)
  }
  expect_lte(max(d$phq9_depression, na.rm = TRUE), 3)
  expect_lte(max(d$qol_score, na.rm = TRUE), 4)
})

test_that("the asset index does not silently change when a checkbox is added", {
  # The original derived its asset list by regex, so a new checkbox redefined
  # socioeconomic status for the whole cohort without anyone noticing.
  spec <- vital_spec()
  base <- clean_analysis()$ses_index

  raw <- clean_raw()
  raw[["Solar water heater"]] <- rep(c("Checked", "Unchecked"), length.out = nrow(raw))
  d2 <- build_analysis_data(raw, spec)

  expect_equal(d2$ses_index, base, tolerance = 1e-8)
})

test_that("missingness is measured per variable and flagged above the threshold", {
  d <- clean_analysis()
  q <- data_quality(d)

  expect_true(all(c("variable", "n_missing", "pct_missing") %in% names(q$missingness)))
  expect_equal(q$missingness$pct_missing[q$missingness$variable == "serum_iron"], 100)
  # Serum iron is never collected in this cohort; it must be flagged, not hidden.
  expect_true("serum_iron" %in% q$high_missing$variable)
})

test_that("consistency rules are evaluated with a non-evaluable denominator", {
  d <- clean_analysis()
  ck <- data_quality(d)$checks

  expect_equal(nrow(ck), length(vital_spec()$checks))
  expect_true(all(ck$violations <= ck$evaluable))

  # Range clamping runs first, so the BMI rule sees no out-of-range values but
  # its denominator drops by the two records that were clamped.
  bmi <- ck[ck$check == "BMI within 12-60", ]
  expect_equal(bmi$violations, 0L)
  expect_equal(bmi$evaluable, 398L)
})

test_that("build_analysis_data refuses to run on an export that fails the gate", {
  # The unrepaired fixture has a duplicate ID.
  expect_error(build_analysis_data(fixture_raw(), vital_spec()),
               "cannot be analysed")
})
