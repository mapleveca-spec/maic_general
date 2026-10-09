# run_maic_unanchored() / run_maic_anchored() ----------------------------------
#
# The two entry points for one complete MAIC: one IPD outcome against one
# published result. Each accepts only its own kind of published result, so
# nothing is inferred from the inputs.
#
# run_maic_unanchored, arguments: ipd, sld, metadata, outcome, sld_outcome, options.
#   The IPD is the analysis population as supplied: a single-arm study, or a
#   trial already restricted to the arm of interest. There is no arm argument.
#   The published result must be an absolute outcome (logit_p or mean). The
#   outcome model is intercept-only. include_adjust defaults to TRUE because
#   TSD 18 requires unanchored comparisons to weight on all prognostic
#   variables and effect modifiers.
#
# run_maic_anchored, arguments: the same plus arm and reference_arm.
#   The IPD has two arms; `reference_arm` is the common comparator and the
#   estimate is "other arm vs reference_arm". The published result must be a
#   contrast (log_or, log_hr, mean_diff). include_adjust defaults to FALSE:
#   effect modifiers only, with the adjust tier as a sensitivity analysis.
#
# Both call .run_maic_core(), which sequences: validate, weights
# (run_maic_weighting()), outcome model, comparison, optional bootstrap.
#
# Return value (both): the run_maic_weighting() list plus
#   kind                   "unanchored" or "anchored"
#   outcome, sld_outcome   the specs
#   contrast               label, e.g. "B vs A" or the unanchored label
#   weight_fit             estimate_maic_weights() / unit_weights_fit() result
#   fit                    fit_outcome_model() result
#   result                 one-row tibble: compare_to_sld() output with n, ess,
#                          and, if bootstrapped, boot_ci_low, boot_ci_high,
#                          n_boot_ok, n_extreme
#   bootstrap              bootstrap_comparison() result, or NULL

run_maic_unanchored <- function(ipd, sld, metadata, outcome, sld_outcome, include_adjust = TRUE, ...) {
  check_outcome_pair(outcome, sld_outcome)
  if (sld_outcome$anchored) {
    stop("`sld_outcome` is an anchored contrast (scale `", sld_outcome$scale,
         "`); use run_maic_anchored().", call. = FALSE)
  }
  .run_maic_core(ipd, sld, metadata, outcome, sld_outcome, arm = NULL, reference_arm = NULL,
                 contrast = UNANCHORED_CONTRAST, kind = "unanchored", include_adjust = include_adjust, ...)
}

run_maic_anchored <- function(ipd, sld, metadata, outcome, sld_outcome, arm, reference_arm,
                              include_adjust = FALSE, ...) {
  check_outcome_pair(outcome, sld_outcome)
  if (!sld_outcome$anchored) {
    stop("`sld_outcome` is an unanchored absolute outcome (scale `", sld_outcome$scale,
         "`); use run_maic_unanchored().", call. = FALSE)
  }
  contrast <- check_anchored_arms(ipd, arm, reference_arm)
  .run_maic_core(ipd, sld, metadata, outcome, sld_outcome, arm = arm, reference_arm = reference_arm,
                 contrast = contrast, kind = "anchored", include_adjust = include_adjust, ...)
}

.run_maic_core <- function(ipd, sld, metadata, outcome, sld_outcome, arm, reference_arm, contrast, kind,
                           include_adjust, conf_level = 0.95, n_boot = 0, seed = NULL, max_fail_rate = 0.05,
                           extreme_bound = 5, na_action = c("complete_case", "error"), prop_tol = 0.02, ...) {
  na_action <- match.arg(na_action)

  res <- run_maic_weighting(ipd, sld, metadata, include_adjust = include_adjust,
                            na_action = na_action, prop_tol = prop_tol, ...)

  fit <- fit_outcome_model(ipd, outcome, weights = res$weights, arm = arm, reference_arm = reference_arm,
                           conf_level = conf_level)
  result <- compare_to_sld(fit, sld_outcome, conf_level = conf_level, contrast = contrast)
  result <- dplyr::mutate(result, n = fit$n, ess = fit$ess)

  boot <- NULL
  if (n_boot > 0) {
    boot <- bootstrap_comparison(
      ipd, res$sld_summary, metadata, outcome, sld_outcome,
      arm = arm, reference_arm = reference_arm, include_adjust = include_adjust,
      n_boot = n_boot, seed = seed, conf_level = conf_level,
      max_fail_rate = max_fail_rate, extreme_bound = extreme_bound, na_action = na_action, ...
    )
    result <- dplyr::bind_cols(result, boot$summary[c("boot_ci_low", "boot_ci_high", "n_boot_ok", "n_extreme")])
  }

  c(res[setdiff(names(res), "fit")], list(
    kind        = kind,
    outcome     = outcome,
    sld_outcome = sld_outcome,
    contrast    = contrast,
    weight_fit  = res$fit,
    fit         = fit,
    result      = result,
    bootstrap   = boot
  ))
}
