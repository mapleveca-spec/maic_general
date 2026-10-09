# Investigating a failed scenario step (unanchored) ----------------------------
#
# A scenario run never stops on a failed step: the step gets status "error"
# and the run continues. This template shows how to find the failed steps,
# replay one on its own through the main function, trace the cause with the
# module functions, and confirm a fix. Two failures are staged on the toy
# data:
#   A. an infeasible variable: fac_color_sld_extra has a level (Yellow) the
#      comparator reports but the IPD never shows, so no weights can hit it
#   B. a solver that does not converge because `maxit` is far too small
#
# Runs as is from the framework root:  Rscript templates/investigate_failed_step.R

framework_root <- "."
source(file.path(framework_root, "08_main", "source_framework.R"))
source_framework(framework_root)
source(file.path(framework_root, "sandbox", "make_toy_data.R"))
source(file.path(framework_root, "sandbox", "toy_analysis_inputs.R"))

show <- function(title, x, ...) {
  cat("\n==", title, "\n")
  print(if (is.data.frame(x)) as.data.frame(x) else x, ...)
}

# The analysis, with the problem variable put into the adjust tier --------------------
a <- make_toy_analysis_inputs("unanchored")[[1]]
a$metadata$adjust[a$metadata$variable == "fac_color_sld_extra"] <- TRUE
a$adjust_order <- c("fac_age", "fac_sev_hb", "fac_color_sld_extra", "fac_tar_jnt_lead", "fac_bmi")
ipd <- a$ipd
sld <- a$sld

# 1. Run the scenarios and read the status column ---------------------------------------
scenarios <- define_scenarios_sequential(a$metadata, a$adjust_order, include_empty = TRUE)
out <- run_scenarios_unanchored(scenarios, ipd, sld, a$outcome, a$sld_outcome)
cat("steps:", nrow(out$results), " failed:", out$n_failed, "\n")
show("Results: status and error per step; failed steps keep their label and variables",
     out$results[, c("scenario", "label", "variables", "status", "error", "estimate", "weight_ess")], right = FALSE)

# 2. List the failed steps with their replay calls ----------------------------------------
show("failed_steps(): what failed, why, and the call that reproduces it",
     failed_steps(out, ipd = "ipd", sld = "sld", run_object = "out"), right = FALSE)
first_failed <- failed_steps(out)$step[1]

# 3. Replay the step alone through the main function ----------------------------------------
# Outside a template you would run replay_scenario() directly and let it
# error, then use traceback(). Here the error is caught so the script goes on.
cat("\n== replay_scenario(): the main function raises the recorded error\n")
err <- tryCatch(replay_scenario(out, first_failed, ipd, sld), error = conditionMessage)
cat(err, "\n")
stopifnot(identical(err, out$results$error[first_failed]))

# 4. Or paste the standalone code into the console ---------------------------------------------
cat("\n== scenario_replay_code(): standalone code for the step\n")
scenario_replay_code(out, first_failed, ipd = "ipd", sld = "sld", run_object = "out")

# 5. Trace the cause with the module functions ----------------------------------------------------
# Take the step's metadata and walk the weighting chain until it breaks.
step_metadata <- out$runs[[first_failed]]$scenario$metadata
ipd_summary <- summarize_ipd(ipd, step_metadata)
sld_summary <- summarize_sld(sld, step_metadata)
aligned <- align_summaries(ipd_summary, sld_summary, step_metadata)
show("Aligned balance for the suspect variable: the IPD has 0% Yellow, the SLD has 15%",
     aligned[aligned$variable == "fac_color_sld_extra", c("variable", "level", "row_type", "ipd_est", "sld_est")],
     digits = 3)
cat("\nbuild_match_targets() is where the step fails:\n")
cat(tryCatch(
  {
    build_match_targets(ipd_summary, sld_summary, step_metadata, include_adjust = TRUE)
    "no error"
  },
  error = conditionMessage
), "\n")
# Reweighting cannot create patients in a category the IPD does not have, so the
# framework refuses rather than silently zeroing the constraint.

# 6. Fix and confirm -----------------------------------------------------------------------------------
# Options: keep the variable in the balance table but out of the weighting
# set (adjust = FALSE); or recode the levels so both sources share them, e.g.
# collapse Yellow with Red into "Other" in both IPD and SLD. Here: option 1.
fixed_metadata <- a$metadata
fixed_metadata$adjust[fixed_metadata$variable == "fac_color_sld_extra"] <- FALSE
fixed_order <- setdiff(a$adjust_order, "fac_color_sld_extra")
fixed <- run_scenarios_unanchored(define_scenarios_sequential(fixed_metadata, fixed_order, include_empty = TRUE),
                                  ipd, sld, a$outcome, a$sld_outcome)
cat("\nafter the fix, failed steps:", fixed$n_failed, "\n")
show("Fixed run", format_results(fixed$results)[, c("Model", "Status", "Estimate (95% CI)", "N", "Weighting ESS")],
     right = FALSE)

# B. A solver that does not converge -----------------------------------------------------------------
# Options passed to the scenario runner reach the solver. With maxit = 3 the
# weighted steps cannot converge; the error names the constraints and the row
# count, which is the context you need.
out_b <- run_scenarios_unanchored(define_scenarios_univariate(fixed_metadata, c("fac_age", "fac_bmi")),
                                  ipd, sld, a$outcome, a$sld_outcome, maxit = 3)
show("Non-convergence recorded per step", failed_steps(out_b)[, c("step", "label", "error")], right = FALSE)
# replay_scenario() accepts changed options, so test the remedy on one step:
retry <- replay_scenario(out_b, 1, ipd, sld, maxit = 10000)
cat("\nreplayed step 1 with maxit = 10000: estimate", round(retry$result$estimate, 3),
    "| optim convergence code", retry$weight_fit$optim$convergence, "\n")

cat("\nDone. Summary of the process:\n",
    " 1. read status/error in results;  2. failed_steps() for context and the replay call;\n",
    " 3. replay_scenario() to raise the error normally (traceback());\n",
    " 4. walk the module functions on runs[[i]]$scenario$metadata to locate the cause;\n",
    " 5. fix the specification or pass changed options, re-run, confirm n_failed is 0.\n")
