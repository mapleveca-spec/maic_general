# summarize_continuous() -------------------------------------------------------
#
# Summarises one continuous vector into summary-schema rows.
#
# Always emits the summary row (level = CONTINUOUS_LEVEL): n, mean, SD over
# observed values. Emits a Missing row only if at least one value is NA:
# est = share of weight on NA rows, sd = sqrt(p(1-p)).
#
# `weights` (optional) are per-row, non-negative. With weights, `n` is the
# Kish effective sample size and mean / SD / proportions are weighted.
# NULL means unit weights and gives the classic unweighted summary.
#
# The Missing row is emitted whenever NA exists, regardless of weights, so a
# weighted and an unweighted summary of the same IPD always have the same rows.

summarize_continuous <- function(x, variable, weights = NULL) {
  stopifnot(is.numeric(x))
  w <- if (is.null(weights)) rep(1, length(x)) else check_weights(weights, length(x))

  obs    <- !is.na(x)
  n_miss <- sum(!obs)

  out <- new_summary_row(
    variable = variable, type = "con", level = CONTINUOUS_LEVEL,
    n   = effective_n(w[obs]),
    est = weighted_mean(x[obs], w[obs]),
    sd  = weighted_sd(x[obs], w[obs])
  )

  if (n_miss > 0) {
    p <- sum(w[!obs]) / sum(w)
    out <- dplyr::bind_rows(out, new_summary_row(
      variable = variable, type = "con", level = MISSING_LEVEL,
      n = effective_n(w), est = p, sd = binomial_sd(p)
    ))
  }

  out
}
