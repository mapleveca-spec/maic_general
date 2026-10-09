# Toy analysis-input lists --------------------------------------------------------
#
# Builds `analysis_inputs` lists in the shape run_analyses_unanchored() /
# run_analyses_anchored() and the step-by-step templates expect, from the toy
# data. Replace with your own prepared list for a real project.
#
# make_toy_analysis_inputs("unanchored"): three single-arm analyses. Each IPD
#   is one arm of the toy trial with the arm column removed, as a real
#   single-arm study would arrive. No match tier; usable covariates in the
#   adjust tier.
# make_toy_analysis_inputs("anchored"): two two-arm analyses with the toy
#   metadata's own tiers (effect modifiers matched, prognostic in adjust).
#
# Requires the framework to be sourced (define_outcome() etc.).

make_toy_analysis_inputs <- function(kind = c("unanchored", "anchored"), toy = make_toy_data()) {
  kind <- match.arg(kind)

  if (kind == "unanchored") {
    meta <- toy$metadata
    meta$match    <- FALSE
    meta$adjust   <- meta$variable %in% c("fac_sev_hb", "fac_tar_jnt_lead", "fac_age", "fac_bmi")
    meta$match_sd <- meta$variable == "fac_age"
    single_arm <- function(arm) toy$ipd[toy$ipd$ARM == arm, setdiff(names(toy$ipd), "ARM")]

    return(list(
      list(
        analysis_name = "Response, population A",
        population    = "Trial X arm A",
        comparator    = "Study Y single arm",
        ipd           = single_arm("A"),
        sld           = toy$sld,
        metadata      = meta,
        outcome       = define_outcome("Response", "binary", "y_resp"),
        sld_outcome   = sld_outcome_from_proportion("Response", p = 0.40, n = 100),
        adjust_order  = c("fac_age", "fac_sev_hb", "fac_tar_jnt_lead", "fac_bmi")
      ),
      list(
        analysis_name = "Response, population B",
        population    = "Trial X arm B",
        comparator    = "Study Y single arm",
        ipd           = single_arm("B"),
        sld           = toy$sld,
        metadata      = meta,
        outcome       = define_outcome("Response", "binary", "y_resp"),
        sld_outcome   = sld_outcome_from_proportion("Response", p = 0.40, n = 100)
      ),
      list(
        analysis_name = "Score change, population A",
        population    = "Trial X arm A",
        comparator    = "Study Y single arm",
        ipd           = single_arm("A"),
        sld           = toy$sld,
        metadata      = meta,
        outcome       = define_outcome("Score change", "continuous", "y_score"),
        sld_outcome   = define_sld_outcome("Score change", "mean", estimate = -6.5, se = 0.6)
      )
    ))
  }

  list(
    list(
      analysis_name = "Response, B vs A",
      population    = "Trial X",
      comparator    = "Study Y, C vs A",
      ipd           = toy$ipd,
      sld           = toy$sld,
      metadata      = toy$metadata,
      outcome       = define_outcome("Response", "binary", "y_resp"),
      sld_outcome   = define_sld_outcome("Response", "log_or", estimate = log(1.5),
                                         ci_low = log(0.9), ci_high = log(2.5)),
      arm           = "ARM",
      reference_arm = "A"
    ),
    list(
      analysis_name = "OS, B vs A",
      population    = "Trial X",
      comparator    = "Study Y, C vs A",
      ipd           = toy$ipd,
      sld           = toy$sld,
      metadata      = toy$metadata,
      outcome       = define_outcome("OS", "tte", "y_time", event = "y_event"),
      sld_outcome   = define_sld_outcome("OS", "log_hr", estimate = log(0.8), ci_low = log(0.6), ci_high = log(1.07)),
      arm           = "ARM",
      reference_arm = "A",
      adjust_order  = c("fac_egfr", "fac_bmi", "fac_prior_treatment")
    )
  )
}
