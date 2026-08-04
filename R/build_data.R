# ---------------------------------------------------------------------------
# Turning a validated export into the tidy analysis frame.
#
# Everything downstream of this file sees only canonical snake_case names,
# declared factor levels and values already inside their declared range.
# Nothing downstream needs to know that REDCap exists.
# ---------------------------------------------------------------------------

#' Blank out the strings that mean "no answer".
#'
#' Folding these into a factor level would invent a category nobody chose.
clean_values <- function(x, na_strings = character(0)) {
  x <- trimws(as.character(x))
  x[x %in% na_strings] <- NA_character_
  x[!nzchar(x)] <- NA_character_
  x
}

# Numeric conversion that reports what it could not parse, rather than warning
# into the void.
as_numeric_quiet <- function(x) {
  suppressWarnings(as.numeric(x))
}

#' Build the tidy analysis frame from a validated export.
#'
#' @param raw A data.frame from [read_export()].
#' @param spec A `cohort_spec`.
#' @param validation Optional `cohort_validation`; computed if absent.
#' @return A data.frame with one row per participant. Data-quality findings are
#'   attached as an attribute; read them with [data_quality()].
build_analysis_data <- function(raw, spec, validation = NULL) {
  if (is.null(validation)) validation <- validate_export(raw, spec)
  stop_if_invalid(validation)

  df <- raw[validation$rows, , drop = FALSE]
  rownames(df) <- NULL
  na_strings <- spec$na_strings

  out <- list()
  out[[".record_id"]] <- as.character(df[[validation$structural$record_id]])

  # --- outcome -------------------------------------------------------------

  labels <- spec$outcome$labels
  y_raw  <- clean_values(df[[validation$structural$outcome]], na_strings)
  if (!is.null(labels)) {
    lv <- unname(unlist(labels))
    y  <- unname(unlist(labels)[match(y_raw, names(labels))])
    y  <- factor(y, levels = lv)
  } else {
    y <- factor(y_raw)
  }
  out[[spec$outcome$name]] <- y

  # --- soft-failure accumulators -------------------------------------------

  range_hits <- list()
  level_hits <- list()

  # --- source-backed variables --------------------------------------------

  for (v in spec_variables(spec)) {
    if (v$type == "derived") next
    col <- validation$columns[[v$name]]
    if (is.null(col) || is.na(col)) next          # unmatched optional variable
    x <- clean_values(df[[col]], na_strings)

    if (v$type %in% c("numeric")) {
      num  <- as_numeric_quiet(x)
      bad  <- !is.na(x) & is.na(num)
      if (any(bad)) {
        level_hits[[length(level_hits) + 1L]] <- data.frame(
          variable = v$name, label = v$label,
          value = paste(sprintf('"%s"', utils::head(sort(unique(x[bad])), 5)),
                        collapse = ", "),
          n = sum(bad), issue = "not a number",
          stringsAsFactors = FALSE)
      }
      r <- clamp_to_range(num, v)
      if (!is.null(r$hit)) range_hits[[length(range_hits) + 1L]] <- r$hit
      out[[v$name]] <- r$value

    } else if (v$type == "ordinal") {
      x <- apply_recode(x, v)
      scores <- unlist(v$scores)
      undeclared <- setdiff(stats::na.omit(unique(x)), names(scores))
      if (length(undeclared)) {
        level_hits[[length(level_hits) + 1L]] <- data.frame(
          variable = v$name, label = v$label,
          value = paste(sprintf('"%s"', utils::head(sort(undeclared), 5)), collapse = ", "),
          n = sum(x %in% undeclared, na.rm = TRUE), issue = "undeclared level",
          stringsAsFactors = FALSE)
      }
      out[[v$name]] <- unname(scores[x])

    } else {  # factor or binary
      x <- apply_recode(x, v)
      declared <- unlist(v$levels)
      undeclared <- setdiff(stats::na.omit(unique(x)), declared)
      if (length(undeclared)) {
        # Reported, never folded into "Other": that would hide a coding change.
        level_hits[[length(level_hits) + 1L]] <- data.frame(
          variable = v$name, label = v$label,
          value = paste(sprintf('"%s"', utils::head(sort(undeclared), 5)), collapse = ", "),
          n = sum(x %in% undeclared, na.rm = TRUE), issue = "undeclared level",
          stringsAsFactors = FALSE)
      }
      out[[v$name]] <- factor(x, levels = declared)
    }
  }

  analysis <- as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE)

  # --- derived variables ---------------------------------------------------

  registry <- derive_registry()
  for (v in spec_variables(spec, type = "derived")) {
    val <- if (!is.null(v$fn)) {
      fn <- registry[[v$fn]]
      if (is.null(fn)) {
        stop(sprintf('Spec names an unknown derivation function "%s" for variable "%s".',
                     v$fn, v$name), call. = FALSE)
      }
      fn(df, v$args %||% list(), na_strings = na_strings)
    } else {
      # `expr` is evaluated against variables built so far, so a derived
      # variable can only depend on things already defined above it.
      tryCatch(
        eval(parse(text = v$expr), envir = analysis),
        error = function(e) {
          stop(sprintf('Derived variable "%s" could not be computed from `%s`: %s',
                       v$name, v$expr, conditionMessage(e)), call. = FALSE)
        })
    }
    val <- as.numeric(val)
    if (length(val) == 1L) val <- rep(val, nrow(analysis))
    r <- clamp_to_range(val, v)
    if (!is.null(r$hit)) range_hits[[length(range_hits) + 1L]] <- r$hit
    analysis[[v$name]] <- r$value
  }

  # --- data quality --------------------------------------------------------

  quality <- list(
    n_rows_file    = validation$n_rows,
    n_analytic     = nrow(analysis),
    soft           = validation$soft,
    unmatched      = validation$unmatched,
    unused_columns = validation$unused,
    out_of_range   = bind_rows_safe(range_hits),
    bad_levels     = bind_rows_safe(level_hits),
    missingness    = missingness_table(analysis, spec),
    checks         = run_checks(analysis, spec)
  )
  quality$high_missing <- quality$missingness[
    quality$missingness$pct_missing > 100 * spec$options$missingness_threshold, ,
    drop = FALSE]

  attr(analysis, "quality") <- quality
  attr(analysis, "spec")    <- spec
  class(analysis) <- c("cohort_data", class(analysis))
  analysis
}

#' Data-quality findings attached to an analysis frame.
data_quality <- function(x) attr(x, "quality")

# Clamp to the declared range, reporting how many values were dropped. Values
# outside the range become NA: an implausible number is not data.
clamp_to_range <- function(num, v) {
  if (is.null(v$range)) return(list(value = num, hit = NULL))
  lo <- v$range[[1]]; hi <- v$range[[2]]
  bad <- !is.na(num) & (num < lo | num > hi)
  hit <- if (any(bad)) {
    data.frame(variable = v$name, label = v$label, low = lo, high = hi,
               n = sum(bad), stringsAsFactors = FALSE)
  }
  num[bad] <- NA_real_
  list(value = num, hit = hit)
}

# Apply the spec's explicit level collapsing. Anything not named here and not
# in `levels` stays as-is so it can be reported as undeclared.
apply_recode <- function(x, v) {
  if (is.null(v$recode)) return(x)
  map <- unlist(v$recode)
  hit <- !is.na(x) & x %in% names(map)
  x[hit] <- unname(map[x[hit]])
  x
}

bind_rows_safe <- function(lst) {
  lst <- Filter(Negate(is.null), lst)
  if (!length(lst)) return(NULL)
  do.call(rbind, lst)
}

#' Per-variable missingness in the analysis frame.
missingness_table <- function(analysis, spec) {
  vars <- setdiff(names(analysis), ".record_id")
  lab <- vapply(vars, function(nm) {
    v <- spec_variable(spec, nm)
    if (!is.null(v)) v$label else spec$outcome$label %||% nm
  }, character(1))
  dom <- vapply(vars, function(nm) {
    v <- spec_variable(spec, nm)
    if (!is.null(v)) v$domain else "Outcome"
  }, character(1))
  n_miss <- vapply(vars, function(nm) sum(is.na(analysis[[nm]])), integer(1))
  out <- data.frame(
    variable = vars, label = unname(lab), domain = unname(dom),
    n_missing = unname(n_miss),
    pct_missing = round(100 * unname(n_miss) / nrow(analysis), 1),
    stringsAsFactors = FALSE)
  out[order(-out$pct_missing, out$variable), , drop = FALSE]
}

#' Evaluate the spec's consistency rules.
#'
#' Rows where the expression is NA are not evaluable and are excluded from the
#' denominator rather than silently counted as passes.
run_checks <- function(analysis, spec) {
  if (!length(spec$checks)) return(NULL)
  rows <- lapply(spec$checks, function(ck) {
    res <- tryCatch(eval(parse(text = ck$expr), envir = analysis),
                    error = function(e) NULL)
    if (is.null(res)) {
      return(data.frame(check = ck$name, evaluable = 0L, violations = NA_integer_,
                        pct = NA_real_, note = "could not be evaluated",
                        stringsAsFactors = FALSE))
    }
    res <- as.logical(res)
    ev  <- sum(!is.na(res))
    vi  <- sum(!is.na(res) & !res)
    data.frame(check = ck$name, evaluable = ev, violations = vi,
               pct = if (ev > 0) round(100 * vi / ev, 1) else NA_real_,
               note = "", stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}
