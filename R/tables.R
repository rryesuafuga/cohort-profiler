# ---------------------------------------------------------------------------
# Table builders. Everything returns a flextable, because the output formats
# are DOCX and PDF. gt is HTML-first and must not appear here.
#
# The statistical conventions are the original report's and are deliberate:
#   continuous   median [IQR] and mean (SD), Wilcoxon rank-sum
#   categorical  n (%) to one decimal, chi-square, Fisher when a cell is thin
#   univariate   logistic regression, odds ratio with a Wald 95% CI
# ---------------------------------------------------------------------------

# --- small formatting helpers ----------------------------------------------

fmt_p <- function(p) {
  ifelse(is.na(p), "-",
         ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3)))
}

fmt_est <- function(x) formatC(x, format = "f", digits = 2)

# Which outcome level counts as the event.
outcome_levels <- function(spec) {
  lv <- unname(unlist(spec$outcome$labels))
  if (is.null(lv) || length(lv) < 2) lv <- c("0", "1")
  lv
}

# Short name for the event, for table titles: "Demographics by HMB status"
# reads better than the full outcome label or the bare variable name.
outcome_positive_label <- function(spec) {
  lv <- outcome_levels(spec)
  lv[[length(lv)]]
}

#' Standard theme so every table in the report looks the same in Word.
style_table <- function(ft, title = NULL, subtitle = NULL) {
  ft <- flextable::theme_booktabs(ft)
  ft <- flextable::fontsize(ft, size = 9, part = "all")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::padding(ft, padding.top = 2, padding.bottom = 2, part = "all")
  if (!is.null(title)) {
    ft <- flextable::add_header_lines(ft, values = title)
    ft <- flextable::bold(ft, i = 1, part = "header")
  }
  if (!is.null(subtitle)) {
    ft <- flextable::add_footer_lines(ft, values = subtitle)
    ft <- flextable::fontsize(ft, size = 8, part = "footer")
    ft <- flextable::italic(ft, part = "footer")
  }
  ft <- flextable::align(ft, j = 1, align = "left", part = "all")
  flextable::autofit(ft)
}

# --- significance testing ---------------------------------------------------

#' Chi-square with a Fisher fallback, or Wilcoxon for continuous variables.
#'
#' Supplied to gtsummary as a custom test so the report's p-values follow the
#' original conventions rather than gtsummary's defaults.
cohort_test <- function(data, variable, by, ...) {
  x <- data[[variable]]
  g <- data[[by]]
  keep <- !is.na(x) & !is.na(g)
  x <- x[keep]
  g <- droplevels(as.factor(g[keep]))

  none <- dplyr::tibble(p.value = NA_real_, method = "not tested")
  if (nlevels(g) < 2L || length(x) < 3L) return(none)

  if (is.numeric(x)) {
    if (stats::sd(x, na.rm = TRUE) == 0) return(none)
    p <- suppressWarnings(stats::wilcox.test(x ~ g)$p.value)
    return(dplyr::tibble(p.value = p, method = "Wilcoxon rank-sum"))
  }

  tab <- table(droplevels(as.factor(x)), g)
  if (nrow(tab) < 2L || ncol(tab) < 2L) return(none)

  expected <- suppressWarnings(stats::chisq.test(tab)$expected)
  if (any(expected < 5)) {
    # Simulated rather than exact: some of these tables are large enough that
    # the exact computation will not finish.
    p <- stats::fisher.test(tab, simulate.p.value = TRUE, B = 10000)$p.value
    dplyr::tibble(p.value = p, method = "Fisher's exact (simulated)")
  } else {
    p <- stats::chisq.test(tab, correct = FALSE)$p.value
    dplyr::tibble(p.value = p, method = "Pearson chi-square")
  }
}

# --- descriptive tables -----------------------------------------------------

#' Variables from one domain that actually made it into the analysis frame.
domain_vars <- function(data, spec, domain) {
  v <- spec_variables(spec, domain = domain)
  nms <- vapply(v, `[[`, character(1), "name")
  nms[nms %in% names(data)]
}

#' Attach spec labels so gtsummary prints human wording, not variable names.
label_columns <- function(data, spec) {
  for (nm in names(data)) {
    v <- spec_variable(spec, nm)
    lab <- if (!is.null(v)) v$label else if (identical(nm, spec$outcome$name))
      spec$outcome$label else nm
    attr(data[[nm]], "label") <- lab
  }
  data
}

#' Descriptive table for one domain, split by the outcome.
#'
#' @return A flextable, or NULL if the domain has nothing to show.
descriptive_flextable <- function(data, spec, domain, title = NULL) {
  vars <- domain_vars(data, spec, domain)
  # A variable that is missing for everyone has nothing to describe; it is
  # already reported in the missingness section.
  vars <- vars[vapply(vars, function(nm) any(!is.na(data[[nm]])), logical(1))]
  if (!length(vars)) return(NULL)

  y <- spec$outcome$name
  d <- data[, c(vars, y), drop = FALSE]
  d <- d[!is.na(d[[y]]), , drop = FALSE]
  # Label last: subsetting rows of a data.frame drops column attributes, so
  # labelling before this point silently leaves gtsummary printing raw names.
  d <- label_columns(d, spec)

  # gtsummary infers continuous vs categorical from how many distinct values a
  # column has, which turns a 0-4 ordinal score into five percentage rows. The
  # spec declares the type, so the spec decides.
  is_continuous <- vapply(vars, function(nm) {
    v <- spec_variable(spec, nm)
    !is.null(v) && v$type %in% c("numeric", "ordinal", "derived")
  }, logical(1))
  cont <- vars[is_continuous]
  catg <- vars[!is_continuous]

  type_arg <- c(
    if (length(cont)) list(dplyr::all_of(cont) ~ "continuous"),
    if (length(catg)) list(dplyr::all_of(catg) ~ "categorical")
  )

  tbl <- gtsummary::tbl_summary(
    d,
    by = dplyr::all_of(y),
    type = type_arg,
    statistic = list(
      gtsummary::all_continuous()  ~ "{median} [{p25}, {p75}]; {mean} ({sd})",
      gtsummary::all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(gtsummary::all_categorical() ~ c(0, 1)),
    missing = "no"
  )
  tbl <- gtsummary::add_overall(tbl, last = FALSE)
  tbl <- gtsummary::add_p(tbl, test = gtsummary::everything() ~ cohort_test,
                          pvalue_fun = function(x) fmt_p(x))
  tbl <- gtsummary::modify_header(tbl, label = "**Variable**")

  ft <- gtsummary::as_flex_table(tbl)
  style_table(ft, title = title,
              subtitle = paste("Continuous: median [IQR]; mean (SD), Wilcoxon rank-sum.",
                               "Categorical: n (%), chi-square or Fisher's exact."))
}

# --- univariate logistic regression ----------------------------------------

#' Fit one unadjusted logistic model and return tidy rows.
#'
#' Returns a `note` instead of estimates when the model cannot be trusted.
#' Several VITAL-HMB variables separate the outcome perfectly; fitting them
#' anyway yields an odds ratio around 10^8, which reads as a finding rather
#' than as an artefact.
univariate_fit <- function(data, spec, varname) {
  y_name <- spec$outcome$name
  lv <- levels(data[[y_name]])
  if (is.null(lv)) lv <- outcome_levels(spec)
  y <- as.integer(data[[y_name]] == lv[length(lv)])
  x <- data[[varname]]
  v <- spec_variable(spec, varname)
  label <- if (!is.null(v)) v$label else varname

  keep <- !is.na(y) & !is.na(x)
  n <- sum(keep)
  none <- function(note) data.frame(
    variable = varname, label = label, term = label, or = NA_real_,
    lcl = NA_real_, ucl = NA_real_, p = NA_real_, n = n, note = note,
    stringsAsFactors = FALSE)

  if (n < (spec$options$min_complete_cases %||% 30L)) {
    return(none(sprintf("not estimable: %d complete cases", n)))
  }
  yy <- y[keep]
  xx <- x[keep]
  if (length(unique(yy)) < 2L) return(none("not estimable: outcome has one level"))

  if (is.numeric(xx)) {
    if (stats::sd(xx) == 0) return(none("not estimable: no variation"))
  } else {
    xx <- droplevels(as.factor(xx))
    if (nlevels(xx) < 2L) return(none("not estimable: only one category present"))
    tab <- table(xx, yy)
    if (any(tab == 0)) {
      # A zero cell means at least one category perfectly predicts the outcome.
      return(none("not estimable: outcome perfectly separated"))
    }
  }

  fit <- tryCatch(
    suppressWarnings(stats::glm(yy ~ xx, family = stats::binomial())),
    error = function(e) NULL)
  if (is.null(fit) || !fit$converged) return(none("not estimable: model did not converge"))

  co <- summary(fit)$coefficients
  co <- co[rownames(co) != "(Intercept)", , drop = FALSE]
  if (!nrow(co)) return(none("not estimable: no estimable terms"))

  # Huge coefficients or standard errors are the signature of separation that
  # slipped past the zero-cell check.
  if (any(abs(co[, 1]) > 10) || any(co[, 2] > 10)) {
    return(none("not estimable: outcome perfectly separated"))
  }

  est <- co[, 1]; se <- co[, 2]; p <- co[, 4]
  terms <- rownames(co)
  terms <- sub("^xx", "", terms)
  terms <- if (is.numeric(x)) rep(label, length(terms)) else paste0(label, ": ", terms)

  data.frame(
    variable = varname, label = label, term = terms,
    or = exp(est), lcl = exp(est - 1.96 * se), ucl = exp(est + 1.96 * se),
    p = p, n = n, note = "", stringsAsFactors = FALSE, row.names = NULL)
}

#' Univariate associations across a set of domains.
univariate_results <- function(data, spec, domains) {
  vars <- unlist(lapply(domains, function(dm) domain_vars(data, spec, dm)))
  if (!length(vars)) return(NULL)
  out <- lapply(vars, function(nm) univariate_fit(data, spec, nm))
  do.call(rbind, out)
}

#' Render univariate results as a flextable.
univariate_flextable <- function(res, title = NULL) {
  if (is.null(res) || !nrow(res)) return(NULL)
  disp <- data.frame(
    Predictor = res$term,
    OR = ifelse(is.na(res$or), "-", fmt_est(res$or)),
    `95% CI` = ifelse(is.na(res$or), "-",
                      sprintf("%s - %s", fmt_est(res$lcl), fmt_est(res$ucl))),
    p = ifelse(is.na(res$p), "-", fmt_p(res$p)),
    Note = res$note,
    check.names = FALSE, stringsAsFactors = FALSE)

  ft <- flextable::flextable(disp)
  ft <- flextable::italic(ft, i = which(nzchar(res$note)), j = 5)
  style_table(ft, title = title,
              subtitle = "Unadjusted logistic regression. OR with Wald 95% CI.")
}

# --- data quality tables ----------------------------------------------------

missingness_flextable <- function(quality, spec, top = NULL) {
  m <- quality$missingness
  if (is.null(m) || !nrow(m)) return(NULL)
  if (!is.null(top)) m <- utils::head(m, top)
  disp <- data.frame(
    Variable = m$label, Domain = m$domain,
    `N missing` = m$n_missing, `% missing` = m$pct_missing,
    check.names = FALSE, stringsAsFactors = FALSE)
  style_table(flextable::flextable(disp), title = "Missingness by variable")
}

checks_flextable <- function(quality) {
  ck <- quality$checks
  if (is.null(ck) || !nrow(ck)) return(NULL)
  disp <- data.frame(
    Check = ck$check, Evaluable = ck$evaluable,
    Violations = ck$violations, `% violating` = ck$pct,
    check.names = FALSE, stringsAsFactors = FALSE)
  style_table(flextable::flextable(disp), title = "Variable consistency checks",
              subtitle = "Records where the rule cannot be evaluated are excluded from the denominator.")
}

range_flextable <- function(quality) {
  r <- quality$out_of_range
  if (is.null(r) || !nrow(r)) return(NULL)
  disp <- data.frame(
    Variable = r$label,
    `Valid range` = sprintf("%s to %s", r$low, r$high),
    `Values dropped` = r$n,
    check.names = FALSE, stringsAsFactors = FALSE)
  style_table(flextable::flextable(disp),
              title = "Values outside the plausible range",
              subtitle = "These were set to missing before analysis.")
}

levels_flextable <- function(quality) {
  b <- quality$bad_levels
  if (is.null(b) || !nrow(b)) return(NULL)
  disp <- data.frame(
    Variable = b$label, Issue = b$issue, Values = b$value, N = b$n,
    check.names = FALSE, stringsAsFactors = FALSE)
  style_table(flextable::flextable(disp),
              title = "Responses not declared in the spec",
              subtitle = "These were left as missing rather than folded into an existing category.")
}

# --- missingness mechanism --------------------------------------------------

#' Columns that are exact linear combinations of earlier ones.
#'
#' Little's test inverts a covariance matrix, so a derived variable such as
#' mean arterial pressure -- exactly (sbp + 2*dbp)/3 -- makes it singular and
#' the test fails outright. Rank detection drops the redundant columns instead.
independent_columns <- function(data, vars) {
  m <- as.matrix(data[, vars, drop = FALSE])
  # Impute to the mean only to detect dependence; an exact linear relationship
  # survives mean imputation, and the imputed copy is not used for anything else.
  for (j in seq_len(ncol(m))) {
    mu <- mean(m[, j], na.rm = TRUE)
    m[is.na(m[, j]), j] <- mu
  }
  m <- scale(m)
  m[is.na(m)] <- 0
  q <- qr(m)
  vars[sort(q$pivot[seq_len(q$rank)])]
}

#' Little's MCAR test over numeric variables below the missingness threshold.
mcar_result <- function(data, spec) {
  thresh <- spec$options$missingness_threshold %||% 0.60
  num <- names(data)[vapply(data, is.numeric, logical(1))]
  num <- num[vapply(num, function(nm) mean(is.na(data[[nm]])) < thresh, logical(1))]
  num <- num[vapply(num, function(nm) stats::sd(data[[nm]], na.rm = TRUE) > 0, logical(1))]
  if (length(num) < 2L) {
    return(list(ok = FALSE, message = "Not enough numeric variables to test."))
  }

  keep <- independent_columns(data, num)
  dropped <- setdiff(num, keep)
  if (length(keep) < 2L) {
    return(list(ok = FALSE,
                message = "Not enough independent numeric variables to test."))
  }

  res <- tryCatch(naniar::mcar_test(data[, keep, drop = FALSE]),
                  error = function(e) NULL)
  if (is.null(res)) {
    return(list(ok = FALSE,
                message = "Little's test could not be computed on these variables."))
  }
  list(ok = TRUE, n_vars = length(keep), dropped = dropped,
       statistic = res$statistic, df = res$df, p.value = res$p.value,
       missing.patterns = res$missing.patterns)
}

#' Chi-square probe of each variable's missingness against the outcome.
#'
#' A small p-value says the data are not missing completely at random.
missing_vs_outcome <- function(data, spec) {
  y <- data[[spec$outcome$name]]
  vars <- setdiff(names(data), c(".record_id", spec$outcome$name))
  rows <- lapply(vars, function(nm) {
    miss <- is.na(data[[nm]])
    if (!any(miss) || all(miss)) return(NULL)
    tab <- table(miss, y)
    if (nrow(tab) < 2L || ncol(tab) < 2L) return(NULL)
    p <- tryCatch({
      ex <- suppressWarnings(stats::chisq.test(tab)$expected)
      if (any(ex < 5)) stats::fisher.test(tab)$p.value
      else stats::chisq.test(tab, correct = FALSE)$p.value
    }, error = function(e) NA_real_)
    v <- spec_variable(spec, nm)
    data.frame(variable = nm, label = if (!is.null(v)) v$label else nm,
               pct_missing = round(100 * mean(miss), 1), p = p,
               stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(NULL)
  out <- do.call(rbind, rows)
  out[order(out$p), , drop = FALSE]
}

missing_vs_outcome_flextable <- function(mv) {
  if (is.null(mv) || !nrow(mv)) return(NULL)
  disp <- data.frame(
    Variable = mv$label, `% missing` = mv$pct_missing, p = fmt_p(mv$p),
    check.names = FALSE, stringsAsFactors = FALSE)
  style_table(flextable::flextable(disp),
              title = "Does missingness depend on the outcome?",
              subtitle = "A small p-value suggests missingness is related to the outcome (not MCAR).")
}

# --- plots ------------------------------------------------------------------

#' Boxplots of the most strongly associated numeric variables by outcome.
outcome_plots <- function(data, spec, res, max_panels = 6L) {
  if (is.null(res)) return(NULL)
  num <- vapply(res$variable, function(nm) is.numeric(data[[nm]]), logical(1))
  cand <- res[num & !is.na(res$p), , drop = FALSE]
  if (!nrow(cand)) return(NULL)
  cand <- cand[order(cand$p), , drop = FALSE]
  vars <- utils::head(unique(cand$variable), max_panels)

  y <- spec$outcome$name
  long <- do.call(rbind, lapply(vars, function(nm) {
    v <- spec_variable(spec, nm)
    data.frame(panel = if (!is.null(v)) v$label else nm,
               value = data[[nm]], group = data[[y]],
               stringsAsFactors = FALSE)
  }))
  long <- long[!is.na(long$value) & !is.na(long$group), , drop = FALSE]
  if (!nrow(long)) return(NULL)

  ggplot2::ggplot(long, ggplot2::aes(x = group, y = value, fill = group)) +
    ggplot2::geom_boxplot(outlier.size = 0.6, alpha = 0.85) +
    ggplot2::facet_wrap(~panel, scales = "free_y") +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 9) +
    ggplot2::theme(legend.position = "none",
                   strip.text = ggplot2::element_text(face = "bold", size = 8))
}
