# Published (SLD) outcome specification ----------------------------------------
#
# The comparator study's published result for one outcome, on the scale the
# framework compares on. Separate from the SLD covariate table because it is
# one number per outcome, not a per-variable summary.
#
#   name      outcome label, matched against the IPD outcome's name
#   scale     one of ESTIMATE_SCALES:
#               anchored contrasts   log_or, log_hr, mean_diff
#               unanchored absolutes logit_p, mean
#   estimate  point estimate on that scale
#   se        standard error on that scale; may be derived from a CI
#   anchored  derived from `scale`: TRUE for a contrast vs the common
#             comparator, FALSE for an absolute outcome
#
# define_sld_outcome() takes either `se` or a `ci_low` / `ci_high` pair.
# sld_outcome_from_proportion() is the common unanchored binary case: a
# published response rate and N give logit(p) and its delta-method SE.

define_sld_outcome <- function(name, scale, estimate, se = NULL,
                               ci_low = NULL, ci_high = NULL, conf_level = 0.95) {
  stopifnot(is.character(name), length(name) == 1, is.numeric(estimate), length(estimate) == 1)
  if (!scale %in% ESTIMATE_SCALES) {
    stop("`scale` must be one of: ", paste(ESTIMATE_SCALES, collapse = ", "), ".", call. = FALSE)
  }
  if (is.null(se)) {
    if (is.null(ci_low) || is.null(ci_high)) {
      stop("Give `se`, or both `ci_low` and `ci_high`.", call. = FALSE)
    }
    se <- se_from_ci(ci_low, ci_high, conf_level)
  }
  if (!is.numeric(se) || length(se) != 1 || se <= 0) stop("`se` must be a positive number.", call. = FALSE)

  structure(
    list(name = name, scale = scale, estimate = estimate, se = se, anchored = scale %in% ANCHORED_SCALES),
    class = "maic_sld_outcome"
  )
}

# Standard error implied by a symmetric Wald interval on the analysis scale.
# For ratios pass the log limits: se_from_ci(log(0.6), log(1.07)).
se_from_ci <- function(ci_low, ci_high, conf_level = 0.95) {
  if (any(ci_low > ci_high)) stop("`ci_low` must not exceed `ci_high`.", call. = FALSE)
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  (ci_high - ci_low) / (2 * z)
}

sld_outcome_from_proportion <- function(name, p, n) {
  stopifnot(p > 0, p < 1, n > 0)
  define_sld_outcome(name, "logit_p", estimate = stats::qlogis(p), se = 1 / sqrt(n * p * (1 - p)))
}
