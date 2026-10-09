# check_anchored_arms() --------------------------------------------------------
#
# The one place the anchored arm rules live. An anchored comparison needs an
# arm column with exactly two levels and a `reference_arm` naming the common
# comparator; the estimate is "other arm vs reference_arm". Returns the
# contrast label, e.g. "B vs A".
#
# Unanchored analyses have no arm concept at all: the IPD handed to
# run_maic_unanchored() is the analysis population as is, and the outcome
# model is intercept-only. Any arm selection happens before the framework.

check_anchored_arms <- function(ipd, arm, reference_arm) {
  if (is.null(arm) || is.null(reference_arm)) {
    stop("An anchored comparison needs both `arm` and `reference_arm`.", call. = FALSE)
  }
  if (!arm %in% names(ipd)) stop("`arm` column `", arm, "` not found in IPD.", call. = FALSE)
  levels <- sort(unique(as.character(ipd[[arm]][!is.na(ipd[[arm]])])))
  if (length(levels) != 2) {
    stop("`arm` must have exactly two levels for an anchored comparison, found: ",
         paste(levels, collapse = ", "), ".", call. = FALSE)
  }
  if (!reference_arm %in% levels) {
    stop("`reference_arm` `", reference_arm, "` is not an arm level (", paste(levels, collapse = ", "), ").",
         call. = FALSE)
  }
  paste(setdiff(levels, reference_arm), "vs", reference_arm)
}

UNANCHORED_CONTRAST <- "IPD vs comparator (unanchored)"
