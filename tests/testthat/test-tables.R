test_that("descriptive tables are flextables, not gt", {
  # Output is DOCX and PDF, so every table must go through flextable.
  d <- clean_analysis()
  ft <- descriptive_flextable(d, vital_spec(), "Demographics", title = "Table 1a")
  expect_s3_class(ft, "flextable")
  expect_false(inherits(ft, "gt_tbl"))
})

test_that("descriptive tables show spec labels, not raw variable names", {
  # Subsetting rows of a data.frame drops column attributes, so labelling at
  # the wrong moment leaves the report printing "age" instead of "Age (years)".
  d <- clean_analysis()
  ft <- descriptive_flextable(d, vital_spec(), "Demographics")
  shown <- as.character(ft$body$dataset[[1]])

  expect_true("Age (years)" %in% shown)
  expect_true("Socioeconomic status (PCA)" %in% shown)
  expect_false("age" %in% shown)
  expect_false("ses_index" %in% shown)
})

test_that("the spec decides continuous vs categorical, not the value count", {
  # financial is an ordinal scored 0-4. gtsummary would otherwise infer it as
  # categorical and print five percentage rows instead of a summary statistic.
  d <- clean_analysis()
  ft <- descriptive_flextable(d, vital_spec(), "Social history")
  shown <- as.character(ft$body$dataset[[1]])

  expect_true("Financial difficulty (0-4)" %in% shown)
  expect_false(any(shown %in% c("0", "1", "2", "3", "4")))

  # The summary cell carries median [IQR]; mean (SD).
  row <- which(shown == "Financial difficulty (0-4)")
  expect_match(as.character(ft$body$dataset[[2]][row]), "\\[.*\\];.*\\(")
})

test_that("a domain with nothing to show yields no table rather than an error", {
  d <- clean_analysis()
  d$serum_iron <- NA_real_
  expect_silent(descriptive_flextable(d, vital_spec(), "No Such Domain"))
  expect_null(descriptive_flextable(d, vital_spec(), "No Such Domain"))
})

test_that("perfectly separating predictors are non-estimable, not enormous ORs", {
  # treated_anaemia and family_bleeding separate the outcome in this cohort.
  # Fitting them anyway gives an odds ratio around 10^8, which reads as a
  # finding rather than as an artefact.
  d <- clean_analysis()
  spec <- vital_spec()

  for (nm in c("treated_anaemia", "family_bleeding")) {
    res <- univariate_fit(d, spec, nm)
    expect_true(all(is.na(res$or)), info = nm)
    expect_match(res$note[1], "not estimable")
    expect_match(res$note[1], "separated")
  }
})

test_that("a variable with no variation is non-estimable", {
  d <- clean_analysis()
  d$age <- 17
  res <- univariate_fit(d, vital_spec(), "age")
  expect_true(is.na(res$or[1]))
  expect_match(res$note[1], "no variation")
})

test_that("a variable with too few complete cases is non-estimable", {
  d <- clean_analysis()
  d$bmi[-(1:10)] <- NA_real_
  res <- univariate_fit(d, vital_spec(), "bmi")
  expect_true(is.na(res$or[1]))
  expect_match(res$note[1], "complete cases")
})

test_that("an ordinary predictor produces a finite OR with a Wald CI", {
  d <- clean_analysis()
  res <- univariate_fit(d, vital_spec(), "bleeding_days")

  expect_equal(nrow(res), 1L)
  expect_true(is.finite(res$or))
  expect_true(res$lcl < res$or && res$or < res$ucl)
  expect_true(res$p >= 0 && res$p <= 1)
  expect_identical(res$note, "")
})

test_that("a categorical predictor yields one row per non-reference level", {
  d <- clean_analysis()
  res <- univariate_fit(d, vital_spec(), "self_vol")
  # Levels are Light (reference), Normal, Heavy.
  expect_equal(nrow(res), 2L)
  expect_true(all(grepl("^Self-reported volume: ", res$term)))
})

test_that("the test chooses Fisher when an expected cell is thin", {
  d <- clean_analysis()
  # Only three positives, so every expected count in that row is below five.
  d$other_surgery <- factor(
    c(rep("Yes", 3), rep("No", nrow(d) - 3)), levels = c("No", "Yes"))
  r <- cohort_test(d, "other_surgery", "hmb")
  expect_match(r$method, "Fisher")

  # A well-populated table uses chi-square.
  r2 <- cohort_test(d, "residence", "hmb")
  expect_match(r2$method, "chi-square")

  # Continuous variables use the rank-sum test.
  r3 <- cohort_test(d, "age", "hmb")
  expect_match(r3$method, "Wilcoxon")
})

test_that("data-quality tables render when there is something to report", {
  q <- data_quality(clean_analysis())
  expect_s3_class(range_flextable(q), "flextable")
  expect_s3_class(checks_flextable(q), "flextable")
  expect_s3_class(missingness_flextable(q, vital_spec()), "flextable")
})

test_that("missingness against the outcome is probed per variable", {
  mv <- missing_vs_outcome(clean_analysis(), vital_spec())
  expect_true(is.data.frame(mv))
  expect_true(all(c("variable", "pct_missing", "p") %in% names(mv)))
  expect_true(all(mv$p >= 0 & mv$p <= 1, na.rm = TRUE))
})

test_that("Little's MCAR test runs over eligible numeric variables", {
  mc <- mcar_result(clean_analysis(), vital_spec())
  expect_true(isTRUE(mc$ok))
  expect_true(is.finite(mc$p.value))
})

test_that("p-values are formatted for a human reader", {
  expect_identical(fmt_p(0.00004), "<0.001")
  expect_identical(fmt_p(0.5), "0.500")
  expect_identical(fmt_p(NA_real_), "-")
})
