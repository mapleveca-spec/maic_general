# run_maic_analysis() ----------------------------------------------------------
#
# One complete MAIC for one outcome: one IPD outcome spec against one
# published result. Every analysis is self-contained, with its own weights,
# balance tables, estimate, and bootstrap; run_maic_outcomes() repeats this
# over a table of outcomes.
#
# Comparison kind comes from the published result's scale (anchored contrast
# or unanchored absolute). Arms, via resolve_arms():
#   anchored    `arm` and `reference_arm` required; both arms used
#   unanchored  single-arm IPD: leave `arm` NULL;
#               multi-arm IPD: `arm` and `intervention_arm` required and the
#               IPD is subset to that arm before weighting
# include_adjust = FALSE weights on the primary set (`match`); TRUE adds the
# `adjust` tier. With n_boot > 0, bootstrap_comparison() runs as well and its
# interval columns are joined onto `result`.
#
# Sequencing only, no logic of its own. Returns the weighting result list
# (run_maic_weighting(), computed on the analysis rows) plus:
#   outcome, sld_outcome   the specs
#   arms                   resolve_arms() output (contrast label, rows used)
#   fit                    fit_outcome_model() result
#   result                 one-row tibble: compare_to_sld() output with n, ess,
#                          and, if bootstrapped, boot_ci_low, boot_ci_high,
#                          n_boot_ok, n_extreme
#   bootstrap              bootstrap_comparison() result, or NULL

run_maic_analysis <- function(ipd, sld, metadata, outcome, sld_outcome,
                              arm = NULL, reference_arm = NULL, intervention_arm = NULL,
                              include_adjust = FALSE, conf_level = 0.95,
                              n_boot = 0, seed = NULL, max_fail_rate = 0.05, extreme_bound = 5,
                              na_action = c("complete_case", "error"), prop_tol = 0.02, ...) {
  na_action <- match.arg(na_action)
  check_outcome_pair(outcome, sld_outcome)
  arms <- resolve_arms(ipd, sld_outcome$anchored, arm, reference_arm, intervention_arm)

  res <- run_maic_weighting(arms$ipd, sld, metadata, include_adjust = include_adjust,
                            na_action = na_action, prop_tol = prop_tol, ...)

  fit <- fit_outcome_model(arms$ipd, outcome, weights = res$weights,
                           arm = arms$arm, reference_arm = arms$reference_arm, conf_level = conf_level)
  result <- compare_to_sld(fit, sld_outcome, conf_level = conf_level, contrast = arms$contrast)
  result <- dplyr::mutate(result, n = fit$n, ess = fit$ess)

  boot <- NULL
  if (n_boot > 0) {
    boot <- bootstrap_comparison(
      arms$ipd, res$sld_summary, metadata, outcome, sld_outcome,
      arm = arms$arm, reference_arm = arms$reference_arm, include_adjust = include_adjust,
      n_boot = n_boot, seed = seed, conf_level = conf_level,
      max_fail_rate = max_fail_rate, extreme_bound = extreme_bound, na_action = na_action, ...
    )
    result <- dplyr::bind_cols(result, boot$summary[c("boot_ci_low", "boot_ci_high", "n_boot_ok", "n_extreme")])
  }

  c(res[setdiff(names(res), "fit")], list(
    weight_fit  = res$fit,
    outcome     = outcome,
    sld_outcome = sld_outcome,
    arms        = arms[c("arm", "reference_arm", "anchored", "contrast")],
    fit         = fit,
    result      = result,
    bootstrap   = boot
  ))
}
