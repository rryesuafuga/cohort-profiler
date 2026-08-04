# ---------------------------------------------------------------------------
# Reading and interrogating a study spec.
#
# Nothing in this file knows anything about VITAL-HMB. Everything study-specific
# lives in spec/*.yaml.
# ---------------------------------------------------------------------------

#' Read a study spec from YAML.
#'
#' Applies defaults and checks the spec is internally coherent. Problems with
#' the spec itself are the study team's problem, not the data's, so they abort
#' immediately with a message naming the offending variable.
#'
#' @param path Path to a spec YAML file.
#' @return A list with class `cohort_spec`.
read_spec <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("Spec file not found: %s", path), call. = FALSE)
  }
  spec <- yaml::yaml.load_file(path)

  spec$options <- utils::modifyList(
    list(missingness_threshold = 0.60, min_complete_cases = 30L),
    spec$options %||% list()
  )
  spec$na_strings <- spec$na_strings %||% c("", "Unknown", "N/A", "NA")
  spec$checks     <- spec$checks %||% list()
  spec$variables  <- spec$variables %||% list()

  for (f in c("outcome", "filter", "record_id_column")) {
    if (is.null(spec[[f]])) {
      stop(sprintf("Spec is missing the required `%s` section.", f), call. = FALSE)
    }
  }

  # Normalise each variable, filling in defaults that the YAML may omit.
  spec$variables <- lapply(spec$variables, function(v) {
    if (is.null(v$name)) {
      stop("Every entry under `variables` needs a `name`.", call. = FALSE)
    }
    v$type     <- v$type %||% "numeric"
    v$label    <- v$label %||% v$name
    v$domain   <- v$domain %||% "Other"
    v$required <- isTRUE(v$required)

    if (!v$type %in% c("numeric", "factor", "binary", "ordinal", "derived")) {
      stop(sprintf("Variable `%s` has unknown type `%s`.", v$name, v$type),
           call. = FALSE)
    }
    if (v$type == "derived") {
      if (is.null(v$expr) && is.null(v$fn)) {
        stop(sprintf("Derived variable `%s` needs either `expr` or `fn`.", v$name),
             call. = FALSE)
      }
    } else if (is.null(v$source)) {
      stop(sprintf("Variable `%s` is type `%s` and needs a `source` column.",
                   v$name, v$type), call. = FALSE)
    }
    if (v$type == "binary") v$levels <- v$levels %||% c("No", "Yes")

    # YAML 1.1 reads bare No/Yes/On/Off as booleans, which turns a declared
    # level list into TRUE/FALSE and makes every real value look undeclared.
    # It is a silent, total data loss, so name it precisely.
    if (!is.null(v$levels)) {
      lv <- unlist(v$levels)
      if (is.logical(lv)) {
        stop(sprintf('Variable `%s`: `levels` was read as true/false. YAML treats bare No and Yes as booleans -- quote them, e.g. levels: ["No", "Yes"].',
                     v$name), call. = FALSE)
      }
      v$levels <- as.character(lv)
    }
    if (!is.null(v$recode)) {
      if (any(vapply(v$recode, is.logical, logical(1)))) {
        stop(sprintf('Variable `%s`: a `recode` value was read as true/false. Quote bare No and Yes in YAML.',
                     v$name), call. = FALSE)
      }
    }
    if (v$type == "ordinal" && is.null(v$scores)) {
      stop(sprintf("Ordinal variable `%s` needs a `scores` map.", v$name),
           call. = FALSE)
    }
    v
  })

  nms <- vapply(spec$variables, `[[`, character(1), "name")
  if (anyDuplicated(nms)) {
    stop(sprintf("Duplicate variable names in spec: %s",
                 paste(unique(nms[duplicated(nms)]), collapse = ", ")),
         call. = FALSE)
  }
  if (spec$outcome$name %in% nms) {
    stop(sprintf("`%s` is both the outcome and a variable; give one a new name.",
                 spec$outcome$name), call. = FALSE)
  }

  spec$domains <- spec$domains %||% unique(
    vapply(spec$variables, `[[`, character(1), "domain"))

  structure(spec, class = c("cohort_spec", "list"))
}

#' Variables in a spec, optionally filtered.
#'
#' @param spec A `cohort_spec`.
#' @param type Optional character vector of types to keep.
#' @param domain Optional domain to keep.
spec_variables <- function(spec, type = NULL, domain = NULL) {
  v <- spec$variables
  if (!is.null(type)) {
    v <- Filter(function(x) x$type %in% type, v)
  }
  if (!is.null(domain)) {
    v <- Filter(function(x) identical(x$domain, domain), v)
  }
  v
}

#' Look up a single variable definition by name.
spec_variable <- function(spec, name) {
  for (v in spec$variables) if (identical(v$name, name)) return(v)
  NULL
}

#' Resolve a spec `source` string against real export column names.
#'
#' Exact match wins outright. Failing that, a substring match is accepted only
#' if it is unique — a substring hitting several columns is an error, never a
#' first-match-wins guess, because that is precisely how the original report
#' silently attached itself to the wrong column.
#'
#' @param source The `source` string from the spec.
#' @param columns Character vector of column names in the export.
#' @return A list with `status` ("ok", "missing" or "ambiguous"), the matched
#'   `column` when status is "ok", and `candidates` when ambiguous.
match_column <- function(source, columns) {
  if (source %in% columns) {
    return(list(status = "ok", column = source, candidates = source))
  }
  hits <- columns[startsWith(columns, source)]
  if (length(hits) != 1L) {
    contains <- columns[vapply(columns, function(x)
      grepl(source, x, fixed = TRUE), logical(1))]
    # Prefer a unique prefix match; fall back to a unique substring match.
    hits <- if (length(hits) > 1L) hits else contains
  }
  if (length(hits) == 1L) {
    list(status = "ok", column = hits, candidates = hits)
  } else if (length(hits) == 0L) {
    list(status = "missing", column = NA_character_, candidates = character(0))
  } else {
    list(status = "ambiguous", column = NA_character_, candidates = hits)
  }
}

`%||%` <- function(x, y) if (is.null(x)) y else x
