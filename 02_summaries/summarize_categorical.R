# summarize_categorical() ------------------------------------------------------
#
# Summarises one categorical vector into summary-schema rows.
#
# Emits one row per level in `levels`, in that order, even when the observed
# count is zero: the IPD is fully observed, so a zero proportion is a fact.
# Emits a Missing row last, only if at least one value is NA.
#
# `weights` (optional) are per-row, non-negative. Proportions are shares of
# total weight, `n` is the Kish effective sample size over all rows. NULL
# means unit weights. Proportions including Missing always sum to 1.
#
# Observed values outside `levels` are an error here, not silently dropped;
# validate_ipd() should have caught them already.

summarize_categorical <- function(x, variable, levels, weights = NULL) {
  stopifnot(is.factor(x) || is.character(x), is.character(levels), length(levels) > 0)
  w <- if (is.null(weights)) rep(1, length(x)) else check_weights(weights, length(x))

  x <- as.character(x)

  observed <- unique(x[!is.na(x)])
  unknown  <- setdiff(observed, levels)
  if (length(unknown) > 0) {
    stop(
      "`", variable, "` has value(s) not in `levels`: ", paste(unknown, collapse = ", "), ".",
      call. = FALSE
    )
  }

  mass   <- vapply(levels, function(l) sum(w[!is.na(x) & x == l]), numeric(1))
  n_miss <- sum(is.na(x))
  if (n_miss > 0) {
    levels <- c(levels, MISSING_LEVEL)
    mass   <- c(mass, sum(w[is.na(x)]))
  }

  p <- mass / sum(w)
  new_summary_row(
    variable = variable, type = "cat", level = levels,
    n = effective_n(w), est = p, sd = binomial_sd(p)
  )
}
