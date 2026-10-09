# run_scenarios_unanchored() / run_scenarios_anchored() ------------------------
#
# Execute a scenario list (06_scenarios/define_scenarios.R) for one outcome:
# one MAIC per scenario, results stacked with one row per scenario. The two
# entry points mirror run_maic_unanchored() / run_maic_anchored() and accept
# only their own kind of published result.
#
# Error tolerance: a scenario that fails at any stage (infeasible target, a
# solver that does not converge, a model that cannot be fitted) does not stop
# the run. Its results row has status = "error", the error message, and the
# scenario's label and variables, with NA estimates; the remaining scenarios
# still run. `runs[[i]]` for a failed scenario holds the scenario and the
# error instead of a result list.
#
# include_adjust: NULL (default) means TRUE when any scenario varies the
# `adjust` flag and FALSE otherwise. Weighting is reused across scenarios with
# an identical weighting set and match_sd pattern.
#
# Returns list(results, runs, n_failed):
#   results  tibble: scenario (index), label, flag, n_variables, variables,
#            include_adjust, status ("ok" / "error"), error (message or NA),
#            then the one-row comparison columns, then the weight distribution
#            of that scenario (weighted, weight_ess, weight_ess_pct,
#            n_excluded, w_min, w_q25, w_median, w_q75, w_max, top10_share)
#   runs     list, one element per scenario; every element, failed or not,
#            carries the full specification of its step (kind, scenario with
#            its metadata, outcome, sld_outcome, include_adjust, arm,
#            reference_arm), so replay_scenario() can re-run it alone
#   n_failed number of scenarios with status "error"

run_scenarios_unanchored <- function(scenarios, ipd, sld, outcome, sld_outcome, include_adjust = NULL, ...) {
  check_outcome_pair(outcome, sld_outcome)
  if (sld_outcome$anchored) {
    stop("`sld_outcome` is an anchored contrast; use run_scenarios_anchored().", call. = FALSE)
  }
  .run_scenarios_core(scenarios, ipd, sld, outcome, sld_outcome, arm = NULL, reference_arm = NULL,
                      contrast = UNANCHORED_CONTRAST, kind = "unanchored", include_adjust = include_adjust, ...)
}

run_scenarios_anchored <- function(scenarios, ipd, sld, outcome, sld_outcome, arm, reference_arm,
                                   include_adjust = NULL, ...) {
  check_outcome_pair(outcome, sld_outcome)
  if (!sld_outcome$anchored) {
    stop("`sld_outcome` is an unanchored absolute outcome; use run_scenarios_unanchored().", call. = FALSE)
  }
  contrast <- check_anchored_arms(ipd, arm, reference_arm)
  .run_scenarios_core(scenarios, ipd, sld, outcome, sld_outcome, arm = arm, reference_arm = reference_arm,
                      contrast = contrast, kind = "anchored", include_adjust = include_adjust, ...)
}

.run_scenarios_core <- function(scenarios, ipd, sld, outcome, sld_outcome, arm, reference_arm, contrast, kind,
                                include_adjust = NULL, conf_level = 0.95, ...) {
  stopifnot(is.list(scenarios), length(scenarios) > 0)
  if (is.null(include_adjust)) {
    include_adjust <- any(vapply(scenarios, function(sc) identical(sc$flag, "adjust"), logical(1)))
  }
  weighting_cache <- new.env(parent = emptyenv())

  run_one <- function(sc) {
    in_set <- weighting_variables(sc$metadata, include_adjust)
    key <- paste(in_set, sc$metadata$match_sd & in_set, collapse = ",")
    if (is.null(weighting_cache[[key]])) {
      weighting_cache[[key]] <- run_maic_weighting(ipd, sld, sc$metadata, include_adjust = include_adjust, ...)
    }
    wres <- weighting_cache[[key]]
    fit <- fit_outcome_model(ipd, outcome, weights = wres$weights, arm = arm, reference_arm = reference_arm,
                             conf_level = conf_level)
    result <- compare_to_sld(fit, sld_outcome, conf_level = conf_level, contrast = contrast)
    result <- dplyr::mutate(result, n = fit$n, ess = fit$ess)
    c(wres[setdiff(names(wres), "fit")],
      list(status = "ok", error = NA_character_, weight_fit = wres$fit, fit = fit, result = result),
      spec(sc))
  }

  # Everything needed to replay this step on its own (see replay_scenario()).
  spec <- function(sc) {
    list(kind = kind, scenario = sc, outcome = outcome, sld_outcome = sld_outcome, contrast = contrast,
         include_adjust = include_adjust, arm = arm, reference_arm = reference_arm, conf_level = conf_level)
  }

  runs <- lapply(scenarios, function(sc) {
    tryCatch(run_one(sc), error = function(e) {
      c(list(status = "error", error = conditionMessage(e)), spec(sc))
    })
  })

  results <- dplyr::bind_rows(lapply(seq_along(runs), function(i) {
    r  <- runs[[i]]
    sc <- scenarios[[i]]
    header <- tibble::tibble(
      scenario       = i,
      label          = sc$label,
      flag           = sc$flag,
      n_variables    = length(sc$variables),
      variables      = paste(sc$variables, collapse = ", "),
      include_adjust = include_adjust,
      status         = r$status,
      error          = r$error
    )
    if (r$status == "ok") {
      dplyr::bind_cols(header, r$result, .weight_columns(r))
    } else {
      dplyr::bind_cols(header, .empty_result(outcome, sld_outcome, contrast), .weight_columns(NULL))
    }
  }))

  list(results = results, runs = runs, n_failed = sum(results$status == "error"))
}

.weight_columns <- function(r) {
  if (is.null(r)) {
    return(tibble::tibble(
      weighted = NA, weight_ess = NA_real_, weight_ess_pct = NA_real_, n_excluded = NA_integer_,
      w_min = NA_real_, w_q25 = NA_real_, w_median = NA_real_, w_q75 = NA_real_, w_max = NA_real_,
      top10_share = NA_real_
    ))
  }
  d <- r$diagnostics
  tibble::tibble(
    weighted = r$weighted, weight_ess = d$ess, weight_ess_pct = d$ess_pct, n_excluded = d$n_excluded,
    w_min = d$w_min, w_q25 = d$w_q25, w_median = d$w_median, w_q75 = d$w_q75, w_max = d$w_max,
    top10_share = d$top10_share
  )
}

# A comparison row with the identifying columns filled and every number NA,
# for a scenario that failed.
.empty_result <- function(outcome, sld_outcome, contrast) {
  tibble::tibble(
    outcome      = outcome$name,
    anchored     = sld_outcome$anchored,
    contrast     = contrast,
    scale        = switch(sld_outcome$scale, logit_p = "log_or", mean = "mean_diff", sld_outcome$scale),
    ipd_estimate = NA_real_,
    sld_estimate = sld_outcome$estimate,
    estimate     = NA_real_,
    se           = NA_real_,
    ci_low       = NA_real_,
    ci_high      = NA_real_,
    n            = NA_integer_,
    ess          = NA_real_
  )
}
