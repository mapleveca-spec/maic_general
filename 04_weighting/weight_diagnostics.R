# Weight diagnostics -----------------------------------------------------------
#
# rescale_weights()    weights rescaled to sum to the number of weighted rows,
#                      so a weight of 1 means "counts as one patient". Zero
#                      weights stay zero. Presentation only: every weighted
#                      statistic in the framework is scale-invariant.
#
# weight_diagnostics() one-row tibble summarising a fit from
#                      estimate_maic_weights(): sample sizes, effective sample
#                      size, and the rescaled weight distribution.
#
#   n_ipd            IPD rows
#   n_complete       rows that entered the design matrix
#   n_excluded       rows with weight 0 because of NA in a matched variable
#   ess              Kish effective sample size of the weighted rows
#   ess_pct          ess / n_complete
#   w_min, w_q25, w_median, w_q75, w_max   rescaled weights, weighted rows only
#   top10_share      share of total weight held by the heaviest 10% of rows
#   max_moment_error residual target imbalance, from the fit

rescale_weights <- function(weights) {
  check_weights(weights, length(weights))
  pos <- weights > 0
  weights * sum(pos) / sum(weights)
}

weight_diagnostics <- function(fit) {
  w   <- fit$weights
  pos <- w > 0
  wr  <- rescale_weights(w)[pos]
  q   <- stats::quantile(wr, c(0, 0.25, 0.5, 0.75, 1), names = FALSE)

  n_top <- max(1L, ceiling(0.10 * length(wr)))
  top10 <- sum(sort(wr, decreasing = TRUE)[seq_len(n_top)]) / sum(wr)

  tibble::tibble(
    n_ipd            = length(w),
    n_complete       = sum(pos),
    n_excluded       = sum(!pos),
    ess              = effective_n(w[pos]),
    ess_pct          = effective_n(w[pos]) / sum(pos),
    w_min            = q[1],
    w_q25            = q[2],
    w_median         = q[3],
    w_q75            = q[4],
    w_max            = q[5],
    top10_share      = top10,
    max_moment_error = fit$max_moment_error
  )
}
