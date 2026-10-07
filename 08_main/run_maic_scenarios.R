# run_maic_scenarios() ---------------------------------------------------------
#
# Executes a scenario list (06_scenarios/define_scenarios.R) for one outcome:
# one full MAIC per scenario, results stacked with one row per scenario.
#
# Arms are resolved once from the comparison kind (resolve_arms()); the same
# analysis rows are used in every scenario.
#
# include_adjust decides whether the `adjust` tier enters the weighting set.
# Its default is TRUE when any scenario varies the `adjust` flag (otherwise
# those scenarios would all be identical) and FALSE otherwise.
#
# Weighting is reused across scenarios with an identical weighting set and
# match_sd pattern.
#
# Returns list(results, runs):
#   results  tibble: scenario (index), label, flag, n_variables, variables,
#            then the one-row comparison columns, then weight_ess (the
#            scenario's weighting ESS, which differs from `ess` only when the
#            outcome has missing values)
#   runs     list of the per-scenario result lists (as run_maic_analysis(),
#            without bootstrap)

run_maic_scenarios <- function(scenarios, ipd, sld, outcome, sld_outcome,
                               arm = NULL, reference_arm = NULL, intervention_arm = NULL,
                               include_adjust = NULL, conf_level = 0.95, ...) {
  stopifnot(is.list(scenarios), length(scenarios) > 0)
  check_outcome_pair(outcome, sld_outcome)
  arms <- resolve_arms(ipd, sld_outcome$anchored, arm, reference_arm, intervention_arm)
  if (is.null(include_adjust)) {
    include_adjust <- any(vapply(scenarios, function(sc) identical(sc$flag, "adjust"), logical(1)))
  }

  weighting_cache <- new.env(parent = emptyenv())

  runs <- lapply(scenarios, function(sc) {
    in_set <- weighting_variables(sc$metadata, include_adjust)
    key <- paste(in_set, sc$metadata$match_sd & in_set, collapse = ",")
    if (is.null(weighting_cache[[key]])) {
      weighting_cache[[key]] <- run_maic_weighting(arms$ipd, sld, sc$metadata, include_adjust = include_adjust, ...)
    }
    wres <- weighting_cache[[key]]
    fit <- fit_outcome_model(arms$ipd, outcome, weights = wres$weights,
                             arm = arms$arm, reference_arm = arms$reference_arm, conf_level = conf_level)
    result <- compare_to_sld(fit, sld_outcome, conf_level = conf_level, contrast = arms$contrast)
    result <- dplyr::mutate(result, n = fit$n, ess = fit$ess)
    c(wres[setdiff(names(wres), "fit")],
      list(weight_fit = wres$fit, scenario = sc, outcome = outcome, sld_outcome = sld_outcome,
           arms = arms[c("arm", "reference_arm", "anchored", "contrast")], fit = fit, result = result))
  })

  results <- dplyr::bind_rows(lapply(seq_along(runs), function(i) {
    sc <- scenarios[[i]]
    dplyr::bind_cols(
      tibble::tibble(
        scenario    = i,
        label       = sc$label,
        flag        = sc$flag,
        n_variables = length(sc$variables),
        variables   = paste(sc$variables, collapse = ", ")
      ),
      runs[[i]]$result,
      tibble::tibble(weight_ess = runs[[i]]$diagnostics$ess)
    )
  }))

  list(results = results, runs = runs)
}
