# estimate_weights() -----------------------------------------------------------
#
# The weighting step as one call: targets -> design matrix -> solver. Used by
# run_maic_weighting() and by every bootstrap replicate, so the two cannot
# drift apart.
#
# When the weighting set is empty (no `match` variable, and no `adjust`
# variable or include_adjust = FALSE) the analysis is a naive, unweighted
# comparison: every patient gets weight 1, nothing is excluded, ESS = n.
# That is a legitimate baseline, not an error; it is the first row of a
# sequential matching table and the reference for what weighting changed.
#
# Returns list(weighted, targets, design, fit):
#   weighted  FALSE for the naive case
#   targets   target table (zero rows when naive)
#   design    build_design_matrix() output, or NULL when naive
#   fit       estimate_maic_weights() output, or unit_weights_fit(n) when
#             naive; same fields either way so downstream code needs no branch

estimate_weights <- function(ipd, ipd_summary, sld_summary, metadata, include_adjust = FALSE,
                             na_action = c("complete_case", "error"), ...) {
  na_action <- match.arg(na_action)

  if (!any(weighting_variables(metadata, include_adjust))) {
    return(list(weighted = FALSE, targets = empty_targets(), design = NULL, fit = unit_weights_fit(nrow(ipd))))
  }

  targets <- build_match_targets(ipd_summary, sld_summary, metadata, include_adjust = include_adjust)
  design  <- build_design_matrix(ipd, targets, na_action = na_action)
  fit     <- estimate_maic_weights(design, ...)
  list(weighted = TRUE, targets = targets, design = design, fit = fit)
}

# A fit object for the naive analysis, shaped like estimate_maic_weights().
unit_weights_fit <- function(n) {
  list(
    weights          = rep(1, n),
    alpha            = stats::setNames(numeric(0), character(0)),
    ess              = n,
    n_complete       = n,
    max_moment_error = 0,
    optim            = NULL
  )
}

empty_targets <- function() {
  tibble::tibble(
    variable = character(), level = character(), moment = character(),
    term = character(), target = numeric(), sld_missing = numeric()
  )[TARGET_COLUMNS]
}
