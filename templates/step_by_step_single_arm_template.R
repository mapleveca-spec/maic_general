# Step-by-step template: unanchored, single-arm IPD -------------------------------
#
# The simplest MAIC set-up: a single-arm IPD study (no arm column at all)
# compared with a published single-arm result. Every module function is
# called in turn and its output printed. Companion to
# step_by_step_template.R (anchored) and step_by_step_unanchored_template.R
# (unanchored on one arm of a two-arm IPD).
#
# Because the IPD has one arm, no arm argument is given anywhere:
# resolve_arms() keeps every row, and the outcome model is intercept-only.
#
# Filled with the toy data (arm A only, arm column removed); runs as is from
# the framework root:   Rscript templates/step_by_step_single_arm_template.R

framework_root <- "."
study_dir      <- "templates"
source(file.path(framework_root, "08_main", "source_framework.R"))
source_framework(framework_root)

show <- function(title, x, ...) {
  cat("\n==", title, "\n")
  print(if (is.data.frame(x)) as.data.frame(x) else x, ...)
}

# 0. Inputs --------------------------------------------------------------------------
# A real single-arm study has no treatment column. The toy IPD does, so arm A
# is taken and the column dropped to make the example honest.
ipd <- read.csv(file.path(study_dir, "ipd_example.csv"), stringsAsFactors = FALSE, na.strings = c("", "NA"))
ipd <- ipd[ipd$ARM == "A", setdiff(names(ipd), "ARM")]
metadata <- read_metadata_csv(file.path(study_dir, "metadata_template.csv"))
sld      <- read.csv(file.path(study_dir, "sld_template.csv"), stringsAsFactors = FALSE, na.strings = c("", "NA"))

# Unanchored: no match tier; every usable covariate in the adjust tier (TSD 18
# requires all prognostic variables and effect modifiers). Four toy variables
# cannot be weighted on (two unreported, two with a level absent on one side).
adjust_vars <- c("fac_sev_hb", "fac_tar_jnt_lead", "fac_prior_treatment", "fac_age", "fac_bmi", "fac_egfr")
metadata$match    <- FALSE
metadata$adjust   <- metadata$variable %in% adjust_vars
metadata$match_sd <- metadata$variable == "fac_age"
validate_metadata(metadata)
validate_sld(sld, metadata)
validate_ipd(ipd, metadata)
cat("IPD:", nrow(ipd), "patients, no arm column:", !"ARM" %in% names(ipd), "\n")

# 1. Outcome and published result ----------------------------------------------------
outcome     <- define_outcome("Response", "binary", "y_resp")
sld_outcome <- sld_outcome_from_proportion("Response", p = 40 / 100, n = 100)
check_outcome_pair(outcome, sld_outcome)
arms <- resolve_arms(ipd, anchored = sld_outcome$anchored)      # no arm arguments
show("Arms: unanchored, single-arm, all rows kept", arms[c("anchored", "arm", "contrast")])

# 2. Summaries and balance before weighting ----------------------------------------------
ipd_summary <- summarize_ipd(ipd, metadata)
sld_summary <- summarize_sld(sld, metadata)
balance_before <- create_balance_table(ipd_summary, sld_summary, metadata)
show("Balance before weighting", balance_before[, c("variable", "level", "row_type", "ipd_est", "sld_est", "smd")],
     digits = 3)

# 3. Naive baseline ----------------------------------------------------------------------
naive <- estimate_weights(ipd, ipd_summary, sld_summary, metadata, include_adjust = FALSE)
naive_model  <- fit_outcome_model(ipd, outcome, weights = naive$fit$weights)
naive_result <- compare_to_sld(naive_model, sld_outcome)
show("Naive (unweighted) comparison", naive_result, digits = 4)

# 4. Targets, design matrix, weights ----------------------------------------------------------
targets <- build_match_targets(ipd_summary, sld_summary, metadata, include_adjust = TRUE)
show("Targets", targets)
design <- build_design_matrix(ipd, targets)
cat("\n== Design matrix:", nrow(design$X), "complete rows x", ncol(design$X), "constraints;",
    sum(!design$complete), "excluded for NA\n")
fit <- estimate_maic_weights(design)
show("Solver output", list(alpha = round(fit$alpha, 3), optim = fit$optim, max_moment_error = fit$max_moment_error))
w <- fit$weights
show("Weight diagnostics", weight_diagnostics(fit), digits = 3)
print(plot_weights(fit, title = "MAIC weights, single-arm unanchored"))

# 5. Balance after weighting ---------------------------------------------------------------------
balance_after <- create_balance_table(summarize_ipd(ipd, metadata, weights = w), sld_summary, metadata)
show("Balance before vs after", format_balance_comparison(compare_balance_tables(balance_before, balance_after)),
     right = FALSE)

# 6. Weighted comparison --------------------------------------------------------------------------
model  <- fit_outcome_model(ipd, outcome, weights = w)
result <- compare_to_sld(model, sld_outcome)
cat("\nweighted response rate:", round(plogis(model$estimate$estimate), 3),
    "| published:", round(plogis(sld_outcome$estimate), 3), "\n")
both <- dplyr::bind_rows(
  dplyr::mutate(naive_result, n = naive_model$n, ess = naive_model$ess),
  dplyr::mutate(result,       n = model$n,       ess = model$ess)
)
show("Naive vs weighted", cbind(analysis = c("naive", "weighted"), format_results(both)), right = FALSE)

# 7. Bootstrap ----------------------------------------------------------------------------------
boot <- bootstrap_comparison(ipd, sld_summary, metadata, outcome, sld_outcome,
                             include_adjust = TRUE, n_boot = 200, seed = 2026, max_fail_rate = 0.25)
show("Bootstrap summary", boot$summary, digits = 3)
show("Bootstrap diagnostics", boot$diagnostics, digits = 3)
show("Failures by reason", boot$failure_counts)

# 8. The same in one call -----------------------------------------------------------------------
one_call <- run_maic_analysis(ipd, sld, metadata, outcome, sld_outcome, include_adjust = TRUE)
stopifnot(isTRUE(all.equal(one_call$result$estimate, result$estimate)))
cat("\nrun_maic_analysis() with no arm arguments reproduces steps 1 to 6.\n")

# 9. Scenarios over the adjust tier, with the step-by-step balance table ------------------------------
adjust_order <- c("fac_age", "fac_sev_hb", "fac_tar_jnt_lead", "fac_prior_treatment", "fac_bmi", "fac_egfr")
sequential <- run_maic_scenarios(define_scenarios_sequential(metadata, adjust_order, include_empty = TRUE),
                                 ipd, sld, outcome, sld_outcome)
univariate <- run_maic_scenarios(define_scenarios_univariate(metadata, adjust_order),
                                 ipd, sld, outcome, sld_outcome)
weight_cols <- c("Model", "Estimate (95% CI)", "N", "Weighting ESS", "ESS %", "Excluded",
                 "Weight min", "Weight median", "Weight max", "Top 10% share")
show("Sequential: naive baseline, then each variable added", format_results(sequential$results)[, weight_cols],
     right = FALSE)
show("Univariate: each variable alone", format_results(univariate$results)[, weight_cols], right = FALSE)
show("Balance path, sequential (first columns): weighted IPD and SMD at each step",
     scenario_balance_path(sequential)[, 1:9], right = FALSE)

# 10. Export everything in the fixed layout ------------------------------------------------------------
output_dir <- file.path(tempdir(), "maic_single_arm")
export_scenarios(sequential, file.path(output_dir, "sequential"))
export_scenarios(univariate, file.path(output_dir, "univariate"))
cat("\nOutputs written to", output_dir, "\n")
print(list.files(output_dir, recursive = TRUE))
