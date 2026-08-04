# ---------------------------------------------------------------------------
# Derived variables.
#
# A derived variable is declared in the spec with either an `expr` (evaluated
# against variables already built) or an `fn` naming one of the functions here.
#
# The rule for everything in this file: inputs come from the spec, never from a
# pattern guessed at run time. The original report built its asset index by
# regex over column names, so adding one checkbox to the REDCap form silently
# redefined socioeconomic status for the whole cohort.
# ---------------------------------------------------------------------------

#' Score a block of Likert items and return the per-person mean.
#'
#' Items are located by `pattern`, but the expected count is asserted upstream
#' in [validate_export()] so a changed questionnaire fails loudly.
#'
#' @param raw The filtered raw export.
#' @param args Spec `args`: `pattern`, `n_items`, `scores`, `min_answered`.
#' @return Numeric vector, NA where too few items were answered.
item_mean <- function(raw, args, na_strings = character(0), ...) {
  cols <- names(raw)[grepl(args$pattern, names(raw))]
  if (!length(cols)) return(rep(NA_real_, nrow(raw)))

  scores <- unlist(args$scores)
  min_answered <- args$min_answered %||% 1L

  scored <- vapply(cols, function(cn) {
    v <- clean_values(raw[[cn]], na_strings)
    out <- unname(scores[v])
    # A response that is not in the score map is not a 0; it is unknown.
    as.numeric(out)
  }, numeric(nrow(raw)))
  scored <- matrix(scored, nrow = nrow(raw))

  answered <- rowSums(!is.na(scored))
  means <- rowMeans(scored, na.rm = TRUE)
  means[answered < min_answered] <- NA_real_
  means
}

#' First principal component of a declared list of household assets.
#'
#' The component's sign is arbitrary in principle, so it is oriented to
#' correlate positively with the number of assets owned. Without that, a
#' rerun on slightly different data can flip the direction of every
#' socioeconomic finding in the report.
#'
#' @param raw The filtered raw export.
#' @param args Spec `args`: `columns` (explicit) and `positive` values.
#' @return Numeric vector of scores, NA for rows with no asset data.
asset_pca <- function(raw, args, na_strings = character(0), ...) {
  positive <- args$positive %||% c("Checked", "Yes")
  positive <- tolower(positive)

  present <- character(0)
  for (cn in args$columns) {
    m <- match_column(cn, names(raw))
    if (m$status == "ok") present <- c(present, m$column)
  }
  if (length(present) < 3L) return(rep(NA_real_, nrow(raw)))

  mat <- vapply(present, function(cn) {
    v <- clean_values(raw[[cn]], na_strings)
    ifelse(is.na(v), NA_real_, as.numeric(tolower(v) %in% positive))
  }, numeric(nrow(raw)))
  mat <- matrix(mat, nrow = nrow(raw), dimnames = list(NULL, present))

  # Drop assets everyone or no one owns: they carry no information and make
  # the correlation matrix singular.
  keep <- apply(mat, 2, function(z) {
    s <- stats::sd(z, na.rm = TRUE)
    !is.na(s) && s > 0
  })
  mat <- mat[, keep, drop = FALSE]
  if (ncol(mat) < 3L) return(rep(NA_real_, nrow(raw)))

  # Mean-impute so a single unanswered checkbox does not drop a participant.
  complete_enough <- rowMeans(!is.na(mat)) >= 0.5
  for (j in seq_len(ncol(mat))) {
    mu <- mean(mat[, j], na.rm = TRUE)
    mat[is.na(mat[, j]), j] <- mu
  }

  pc <- stats::prcomp(mat, center = TRUE, scale. = TRUE)
  score <- as.numeric(pc$x[, 1])

  total <- rowSums(mat)
  if (stats::sd(total) > 0 && !is.na(stats::cor(score, total)) &&
      stats::cor(score, total) < 0) {
    score <- -score
  }

  score[!complete_enough] <- NA_real_
  score
}

# Registry of functions a spec may name via `fn:`. Keeping it explicit means a
# typo in the spec is an error, not an arbitrary function call.
derive_registry <- function() {
  list(item_mean = item_mean, asset_pca = asset_pca)
}
