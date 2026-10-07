# run_maic_weighting() ---------------------------------------------------------
#
# End-to-end pipeline from raw inputs to weights and before/after balance.
# Order: validate, summarise, balance before, targets, design, weights,
# balance after, diagnostics.
#
# include_adjust = FALSE weights on the primary set (`match`); TRUE adds the
# second tier (`adjust`). See weighting_variables().
#
# This function only sequences module calls. It owns no logic of its own, so
# scenarios can reuse any slice of it, and anything that needs a branch
# becomes a new module function rather than an argument here.
#
# Returns a named list:
#   metadata, ipd_summary, sld_summary          validated inputs and summaries
#   include_adjust                              which weighting set was used
#   targets, design, fit                        matching and weighting objects
#   weights                                     one per IPD row, 0 if excluded
#   diagnostics                                 one-row tibble
#   balance_before, balance_after               BALANCE_COLUMNS tables

run_maic_weighting <- function(ipd, sld, metadata, include_adjust = FALSE,
                               na_action = c("complete_case", "error"),
                               prop_tol = 0.02, ...) {
  na_action <- match.arg(na_action)

  validate_metadata(metadata)
  validate_sld(sld, metadata, prop_tol = prop_tol)
  validate_ipd(ipd, metadata)

  ipd_summary <- summarize_ipd(ipd, metadata)
  sld_summary <- summarize_sld(sld, metadata)
  balance_before <- create_balance_table(ipd_summary, sld_summary, metadata)

  targets <- build_match_targets(ipd_summary, sld_summary, metadata, include_adjust = include_adjust)
  design  <- build_design_matrix(ipd, targets, na_action = na_action)
  fit     <- estimate_maic_weights(design, ...)

  balance_after <- create_balance_table(
    summarize_ipd(ipd, metadata, weights = fit$weights), sld_summary, metadata
  )

  list(
    metadata       = metadata,
    ipd_summary    = ipd_summary,
    sld_summary    = sld_summary,
    include_adjust = include_adjust,
    targets        = targets,
    design         = design,
    fit            = fit,
    weights        = fit$weights,
    diagnostics    = weight_diagnostics(fit),
    balance_before = balance_before,
    balance_after  = balance_after
  )
}
