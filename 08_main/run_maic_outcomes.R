# run_maic_outcomes() ----------------------------------------------------------
#
# Runs run_maic_analysis() once per published result in `sld_outcomes`, with
# identical specifications, and keeps every analysis separate. Nothing is
# shared between outcomes: each has its own weights (recomputed, identical
# when the analysis rows are the same), balance tables, and bootstrap.
#
# The two tables are validated first (01_inputs/validate_outcomes.R). Arm
# arguments are passed through; `intervention_arm` is only used by unanchored
# rows and `reference_arm` only by anchored rows, so a mixed table runs with
# both supplied.
#
# Returns list(analyses, summary):
#   analyses  named list of run_maic_analysis() results, one per published
#             result, named "<outcome> [anchored|unanchored]"
#   summary   the one-row `result` tibbles stacked, for convenience; it is a
#             view of the separate analyses, not a joint analysis

run_maic_outcomes <- function(ipd, sld, metadata, outcomes, sld_outcomes,
                              arm = NULL, reference_arm = NULL, intervention_arm = NULL, ...) {
  validate_outcomes(outcomes)
  validate_sld_outcomes(sld_outcomes, outcomes)

  specs     <- outcome_specs(outcomes)
  sld_specs <- sld_outcome_specs(sld_outcomes)

  analyses <- lapply(sld_specs, function(s) {
    run_maic_analysis(
      ipd, sld, metadata, specs[[s$name]], s,
      arm = arm, reference_arm = reference_arm, intervention_arm = intervention_arm, ...
    )
  })
  names(analyses) <- vapply(sld_specs, function(s) {
    paste0(s$name, " [", if (s$anchored) "anchored" else "unanchored", "]")
  }, "")

  list(
    analyses = analyses,
    summary  = dplyr::bind_rows(lapply(analyses, `[[`, "result"))
  )
}
