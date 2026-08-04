# ---------------------------------------------------------------------------
# The schema gate.
#
# This file exists because the original report matched columns by substring of
# the REDCap label and returned an all-NA column when nothing matched. A whole
# analysis then ran on nothing. Every check below turns one variant of that
# silent failure into a loud one.
#
# Two severities:
#   hard  the render cannot proceed; report every problem at once, never stop
#         at the first one, because staff re-export once and want the full list
#   soft  the render proceeds and the problem is surfaced in data quality
# ---------------------------------------------------------------------------

#' Read a REDCap label export.
#'
#' REDCap label exports are ISO-8859-1. Reading one as UTF-8 does not error; it
#' silently mangles any non-ASCII header, after which every column lookup
#' misses and the analytic sample comes back empty.
#'
#' @param path Path to the CSV.
#' @return A data.frame, all columns character, names unmodified.
read_export <- function(path, encoding = "ISO-8859-1") {
  if (!file.exists(path)) {
    stop(sprintf("Data file not found: %s", path), call. = FALSE)
  }
  out <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    locale    = readr::locale(encoding = encoding),
    na        = character(),          # keep blanks; na_strings handles them
    progress  = FALSE,
    name_repair = "minimal"
  )
  as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE)
}

fail <- function(code, message, detail = NULL) {
  list(code = code, message = message, detail = detail)
}

# Does this export look like it came out of REDCap with raw values rather than
# labels? Raw exports use lowercase snake_case field names with no spaces.
looks_like_raw_export <- function(columns) {
  if (!length(columns)) return(FALSE)
  snakey <- grepl("^[a-z][a-z0-9_]*$", columns)
  mean(snakey) > 0.8
}

#' Validate an export against a spec.
#'
#' @param raw A data.frame as returned by [read_export()].
#' @param spec A `cohort_spec` from [read_spec()].
#' @return An object of class `cohort_validation`.
validate_export <- function(raw, spec) {
  hard <- list()
  soft <- list()
  cols <- names(raw)

  # --- structural columns --------------------------------------------------

  structural <- list(
    record_id = spec$record_id_column,
    event     = spec$filter$event_column,
    outcome   = spec$outcome$source
  )
  resolved_structural <- list()

  for (key in names(structural)) {
    src <- structural[[key]]
    m <- match_column(src, cols)
    resolved_structural[[key]] <- m$column
    if (m$status == "missing") {
      msg <- sprintf('Column "%s" not found in the upload.', src)
      if (key == "outcome" && looks_like_raw_export(cols)) {
        msg <- paste(msg, "This looks like a raw-value export -- re-export from",
                     "REDCap with labels.")
      } else {
        msg <- paste(msg, "Check that this is the right file and that the",
                     "column has not been renamed.")
      }
      hard <- c(hard, list(fail(paste0("missing_", key), msg)))
    } else if (m$status == "ambiguous") {
      hard <- c(hard, list(fail(
        paste0("ambiguous_", key),
        sprintf('Column "%s" matches %d columns, so we cannot tell which one is meant: %s',
                src, length(m$candidates), paste(sprintf('"%s"', m$candidates), collapse = ", ")),
        m$candidates)))
    }
  }

  # --- variable source columns --------------------------------------------

  mapping   <- character(0)
  unmatched <- character(0)

  for (v in spec_variables(spec)) {
    if (v$type == "derived") next
    m <- match_column(v$source, cols)
    if (m$status == "ok") {
      mapping[[v$name]] <- m$column
    } else if (m$status == "ambiguous") {
      # Ambiguity is always hard: picking the first match is the bug this
      # repo was built to prevent.
      hard <- c(hard, list(fail(
        "ambiguous_column",
        sprintf('Variable "%s": the text "%s" matches %d columns, so we cannot tell which one is meant: %s',
                v$label, v$source, length(m$candidates),
                paste(sprintf('"%s"', m$candidates), collapse = ", ")),
        m$candidates)))
    } else if (v$required) {
      hard <- c(hard, list(fail(
        "missing_required",
        sprintf('Required variable "%s" needs a column matching "%s", which is not in the upload.',
                v$label, v$source))))
    } else {
      unmatched <- c(unmatched, v$name)
      soft <- c(soft, list(fail(
        "missing_optional",
        sprintf('Optional variable "%s" (looking for "%s") is not in the upload and will be left out of the report.',
                v$label, v$source))))
    }
  }

  # Derived variables that name explicit source columns must find them too.
  for (v in spec_variables(spec, type = "derived")) {
    if (is.null(v$args)) next
    if (!is.null(v$args$columns)) {
      miss <- character(0)
      for (cn in v$args$columns) {
        if (match_column(cn, cols)$status != "ok") miss <- c(miss, cn)
      }
      if (length(miss)) {
        soft <- c(soft, list(fail(
          "missing_derived_input",
          sprintf('Derived variable "%s" is missing %d of its %d input columns (%s); it will be computed from the rest.',
                  v$label, length(miss), length(v$args$columns),
                  paste(sprintf('"%s"', utils::head(miss, 5)), collapse = ", ")),
          miss)))
      }
    }
    if (!is.null(v$args$pattern) && !is.null(v$args$n_items)) {
      found <- sum(grepl(v$args$pattern, cols))
      if (found != v$args$n_items) {
        # A changed item count silently rescales the score, so this is hard.
        hard <- c(hard, list(fail(
          "item_count_mismatch",
          sprintf('Derived variable "%s" expects %d items matching %s but the upload has %d. The questionnaire appears to have changed; update the spec before reporting a score.',
                  v$label, v$args$n_items, v$args$pattern, found))))
      }
    }
  }

  # Anything in the export the spec never mentions: informational only.
  claimed <- unname(mapping)
  known   <- c(claimed, unlist(resolved_structural, use.names = FALSE))
  for (v in spec_variables(spec, type = "derived")) {
    if (!is.null(v$args$columns)) {
      known <- c(known, vapply(v$args$columns, function(cn)
        match_column(cn, cols)$column, character(1)))
    }
    if (!is.null(v$args$pattern)) known <- c(known, cols[grepl(v$args$pattern, cols)])
  }
  extra <- setdiff(cols, stats::na.omit(known))

  # --- row-level structure -------------------------------------------------

  rows <- rep(TRUE, nrow(raw))
  n_analytic <- NA_integer_
  outcome_col <- resolved_structural$outcome

  if (!is.na(outcome_col)) {
    y_raw <- clean_values(raw[[outcome_col]], spec$na_strings)
    if (all(is.na(y_raw))) {
      hard <- c(hard, list(fail(
        "outcome_all_missing",
        sprintf('The outcome column "%s" is present but empty for every row. No participant has a result to analyse.',
                outcome_col))))
    }
    rows <- rows & !is.na(y_raw)
  }

  event_col <- resolved_structural$event
  if (!is.na(event_col)) {
    want <- spec$filter$event_value
    ev   <- trimws(as.character(raw[[event_col]]))
    keep <- !is.na(ev) & ev == want
    if (!any(keep)) {
      hard <- c(hard, list(fail(
        "no_matching_event",
        sprintf('No rows have "%s" in the "%s" column. Values present are: %s',
                want, event_col,
                paste(sprintf('"%s"', utils::head(unique(stats::na.omit(ev)), 8)),
                      collapse = ", ")))))
    }
    rows <- rows & keep
  }

  for (src in spec$filter$require_nonmissing %||% character(0)) {
    m <- match_column(src, cols)
    if (m$status == "ok") {
      rows <- rows & !is.na(clean_values(raw[[m$column]], spec$na_strings))
    }
  }

  if (!any(rows)) {
    # Only worth saying if we did not already say something more specific.
    if (!length(Filter(function(f) f$code %in%
                       c("no_matching_event", "outcome_all_missing"), hard))) {
      hard <- c(hard, list(fail(
        "empty_analytic_sample",
        "No participants are left after applying the event filter and requiring an outcome result. Check that the export covers the right visit.")))
    }
  } else {
    n_analytic <- sum(rows)
  }

  # --- duplicate record IDs ------------------------------------------------

  id_col <- resolved_structural$record_id
  if (!is.na(id_col) && any(rows)) {
    ids <- as.character(raw[[id_col]][rows])
    dup <- unique(ids[duplicated(ids)])
    if (length(dup)) {
      hard <- c(hard, list(fail(
        "duplicate_ids",
        sprintf('%d participant ID%s appear%s more than once in the analysis sample: %s. Each participant must appear exactly once.',
                length(dup), if (length(dup) == 1) "" else "s",
                if (length(dup) == 1) "s" else "",
                paste(utils::head(dup, 10), collapse = ", ")),
        dup)))
    }
  }

  structure(
    list(
      ok          = length(hard) == 0L,
      hard        = hard,
      soft        = soft,
      columns     = mapping,
      structural  = resolved_structural,
      unmatched   = unmatched,
      unused      = extra,
      rows        = rows,
      n_rows      = nrow(raw),
      n_analytic  = n_analytic,
      spec        = spec
    ),
    class = c("cohort_validation", "list")
  )
}

#' Abort with the full list of hard failures, formatted for a human reader.
stop_if_invalid <- function(validation) {
  if (validation$ok) return(invisible(validation))
  msgs <- vapply(validation$hard, `[[`, character(1), "message")
  stop(paste0(
    "This export cannot be analysed yet. ",
    length(msgs), if (length(msgs) == 1) " problem" else " problems",
    " found:\n\n",
    paste0("  ", seq_along(msgs), ". ", msgs, collapse = "\n\n"),
    "\n"), call. = FALSE)
}

#' @export
print.cohort_validation <- function(x, ...) {
  cat(sprintf("<cohort_validation> %s\n", if (x$ok) "PASSED" else "FAILED"))
  cat(sprintf("  rows in file:      %d\n", x$n_rows))
  cat(sprintf("  analytic sample:   %s\n",
              if (is.na(x$n_analytic)) "-" else format(x$n_analytic)))
  cat(sprintf("  columns matched:   %d\n", length(x$columns)))
  if (length(x$hard)) {
    cat("  hard failures:\n")
    for (f in x$hard) cat(sprintf("    - %s\n", f$message))
  }
  if (length(x$soft)) {
    cat(sprintf("  soft findings:     %d\n", length(x$soft)))
  }
  invisible(x)
}
