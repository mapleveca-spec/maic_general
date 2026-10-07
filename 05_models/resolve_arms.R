# resolve_arms() ---------------------------------------------------------------
#
# Decides which IPD rows and which arm contrast an analysis uses, from the
# comparison kind and the arm arguments. One place for the rules, used by
# run_maic_analysis() and run_maic_scenarios().
#
# Anchored (published result is a contrast vs the common comparator):
#   `arm` and `reference_arm` are both required. The arm column must have
#   exactly two levels; `reference_arm` is the common comparator and the
#   estimate is "other arm vs reference_arm". All rows are kept.
# Unanchored (published result is an absolute outcome):
#   Single-arm IPD: leave `arm` NULL; all rows are kept.
#   Multi-arm IPD: `arm` and `intervention_arm` are required and the IPD is
#   subset to that arm before anything else, so the weights, balance, and
#   outcome all describe the intervention arm alone. No assumption about
#   randomisation is needed.
#
# Returns list(ipd, arm, reference_arm, anchored, contrast):
#   arm, reference_arm   what fit_outcome_model() should receive (NULL if the
#                        model has no arm term)
#   contrast             label for results, e.g. "B vs A" or "A (unanchored)"

resolve_arms <- function(ipd, anchored, arm = NULL, reference_arm = NULL, intervention_arm = NULL) {
  if (!is.null(arm)) {
    if (!arm %in% names(ipd)) stop("`arm` column `", arm, "` not found in IPD.", call. = FALSE)
    levels <- sort(unique(as.character(ipd[[arm]][!is.na(ipd[[arm]])])))
  }

  if (anchored) {
    if (is.null(arm) || is.null(reference_arm)) {
      stop("An anchored comparison needs both `arm` and `reference_arm`.", call. = FALSE)
    }
    if (length(levels) != 2) {
      stop("`arm` must have exactly two levels for an anchored comparison, found: ",
           paste(levels, collapse = ", "), ".", call. = FALSE)
    }
    if (!reference_arm %in% levels) {
      stop("`reference_arm` `", reference_arm, "` is not an arm level (", paste(levels, collapse = ", "), ").",
           call. = FALSE)
    }
    other <- setdiff(levels, reference_arm)
    return(list(ipd = ipd, arm = arm, reference_arm = reference_arm, anchored = TRUE,
                contrast = paste(other, "vs", reference_arm)))
  }

  if (is.null(arm)) {
    return(list(ipd = ipd, arm = NULL, reference_arm = NULL, anchored = FALSE, contrast = "IPD (unanchored)"))
  }
  if (is.null(intervention_arm)) {
    stop("An unanchored comparison on multi-arm IPD needs `intervention_arm` (one of: ",
         paste(levels, collapse = ", "), ").", call. = FALSE)
  }
  if (!intervention_arm %in% levels) {
    stop("`intervention_arm` `", intervention_arm, "` is not an arm level (", paste(levels, collapse = ", "), ").",
         call. = FALSE)
  }
  keep <- !is.na(ipd[[arm]]) & as.character(ipd[[arm]]) == intervention_arm
  list(ipd = ipd[keep, , drop = FALSE], arm = NULL, reference_arm = NULL, anchored = FALSE,
       contrast = paste0(intervention_arm, " (unanchored)"))
}
