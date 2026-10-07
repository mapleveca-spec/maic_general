# compare_to_sld() -------------------------------------------------------------
#
# Combines a weighted IPD estimate (fit_outcome_model()) with the comparator
# study's published result (define_sld_outcome()) into the indirect
# comparison. Both inputs must be on the same scale.
#
# Anchored (Bucher):  d = d_IPD - d_SLD, var = var_IPD + var_SLD, where both
#   contrasts are "intervention vs the common comparator". The result is
#   IPD intervention vs SLD intervention, on the input scale.
# Unanchored:         d = est_IPD - est_SLD, var = var_IPD + var_SLD.
#   logit_p difference is a log odds ratio; mean difference is a mean
#   difference. `scale` in the output names the result scale.
#
# Independence of the two studies is assumed, as in every Bucher comparison.
# Exponentiation for presentation is left to reporting.
#
# `contrast` labels what the IPD side compared (e.g. "B vs A"); the caller
# may override it when the IPD was subset to one arm.
#
# Returns a one-row tibble: outcome, anchored, contrast, scale, ipd_estimate,
# sld_estimate, estimate, se, ci_low, ci_high.

compare_to_sld <- function(ipd_fit, sld_outcome, conf_level = 0.95, contrast = ipd_fit$contrast) {
  stopifnot(inherits(sld_outcome, "maic_sld_outcome"), is.list(ipd_fit), !is.null(ipd_fit$estimate))
  ipd <- ipd_fit$estimate

  if (!identical(ipd$outcome, sld_outcome$name)) {
    stop("Outcome mismatch: IPD fit is `", ipd$outcome, "`, SLD outcome is `", sld_outcome$name, "`.", call. = FALSE)
  }
  if (!identical(ipd$scale, sld_outcome$scale)) {
    stop(
      "Scale mismatch: IPD estimate is on `", ipd$scale, "`, SLD outcome on `", sld_outcome$scale, "`.",
      call. = FALSE
    )
  }

  result_scale <- switch(ipd$scale, logit_p = "log_or", mean = "mean_diff", ipd$scale)
  est <- ipd$estimate - sld_outcome$estimate
  se  <- sqrt(ipd$se^2 + sld_outcome$se^2)
  z   <- stats::qnorm(1 - (1 - conf_level) / 2)

  tibble::tibble(
    outcome      = ipd$outcome,
    anchored     = ipd_fit$anchored,
    contrast     = contrast,
    scale        = result_scale,
    ipd_estimate = ipd$estimate,
    sld_estimate = sld_outcome$estimate,
    estimate     = est,
    se           = se,
    ci_low       = est - z * se,
    ci_high      = est + z * se
  )
}
