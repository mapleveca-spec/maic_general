# Step-by-step template: unanchored analysis ---------------------------------------
#
# Walks through an unanchored MAIC one module function at a time, printing
# every intermediate object. Companion to step_by_step_template.R (anchored).
#
# Set-up for this walk-through:
#   - The IPD has two arms; the comparison is unanchored, so the IPD is subset
#     to the intervention arm before anything else (resolve_arms()).
#   - No variable is in the `match` tier. Every usable covariate is in the
#     `adjust` tier, and the analysis weights with include_adjust = TRUE.
#     This is the TSD 18 requirement for unanchored comparisons: weight on all
#     prognostic variables and effect modifiers.
#   - The naive (unweighted) analysis is shown first as the baseline.
#   - Sequential and univariate scenarios over the adjust tier close the script.
#
# Filled with the toy data; runs as is from the framework root:
#   Rscript templates/step_by_step_unanchored_template.R

framework_root <- "."
study_dir      <- "templates"
source(file.path(framework_root, "08_main", "source_framework.R"))
source_framework(framework_root)

show <- function(title, x, ...) {
  cat("\n==", title, "\n")
  print(if (is.data.frame(x)) as.data.frame(x) else x, ...)
}

# 0. Inputs --------------------------------------------------------------------------
ipd_all  <- read.csv(file.path(study_dir, "ipd_example.csv"), stringsAsFactors = FALSE, na.strings = c("", "NA"))
metadata <- read_metadata_csv(file.path(study_dir, "metadata_template.csv"))
sld      <- read.csv(file.path(study_dir, "sld_template.csv"), stringsAsFactors = FALSE, na.strings = c("", "NA"))

# Move every usable covariate into the adjust tier. Four toy variables cannot
# be weighted on and stay out: fac_weight and fac_height are not reported by
# the comparator (NA in the SLD); fac_color_sld_extra has an SLD level the IPD
# lacks; fac_color_ipd_extra has an IPD level the SLD gives 0%. Section 4
# shows the error the framework raises if you try.
adjust_vars <- c("fac_sev_hb", "fac_tar_jnt_lead", "fac_prior_treatment", "fac_color_missing_both",
                 "fac_age", "fac_bmi", "fac_egfr")
metadata$match    <- FALSE
metadata$adjust   <- metadata$variable %in% adjust_vars
metadata$match_sd <- metadata$variable == "fac_age"     # keep the SD constraint on age
validate_metadata(metadata)
validate_sld(sld, metadata)
validate_ipd(ipd_all, metadata)

show("Metadata: no match tier, every usable covariate in the adjust tier",
     metadata[, c("variable", "type", "match", "adjust", "match_sd")])
show("Weighting set by specification",
     data.frame(variable = metadata$variable,
                primary  = weighting_variables(metadata),                        # all FALSE: naive
                full     = weighting_variables(metadata, include_adjust = TRUE)))

# 1. Outcome, published result, arms -------------------------------------------------
outcome     <- define_outcome("Response", "binary", "y_resp")
sld_outcome <- sld_outcome_from_proportion("Response", p = 40 / 100, n = 100)   # 40% responded on the comparator
check_outcome_pair(outcome, sld_outcome)
show("Published result: logit(p) with delta-method SE; anchored is derived from the scale", unclass(sld_outcome))

arms <- resolve_arms(ipd_all, anchored = sld_outcome$anchored, arm = "ARM", intervention_arm = "A")
show("Arms: unanchored, so the IPD is subset to the intervention arm", arms[c("anchored", "arm", "contrast")])
cat("rows:", nrow(ipd_all), "in the trial ->", nrow(arms$ipd), "in the analysis set\n")
ipd <- arms$ipd

# 2. Summaries and balance before weighting ----------------------------------------------
ipd_summary <- summarize_ipd(ipd, metadata)
sld_summary <- summarize_sld(sld, metadata)
balance_before <- create_balance_table(ipd_summary, sld_summary, metadata)
show("Balance before weighting, intervention arm vs comparator population",
     balance_before[, c("variable", "level", "row_type", "ipd_est", "sld_est", "smd")], digits = 3)

# 3. Naive baseline: include_adjust = FALSE leaves the weighting set empty -----------------
naive <- estimate_weights(ipd, ipd_summary, sld_summary, metadata, include_adjust = FALSE)
show("Naive weighting object: unit weights, nothing excluded, no targets",
     list(weighted = naive$weighted, n_targets = nrow(naive$targets), diagnostics = weight_diagnostics(naive$fit)))
naive_model  <- fit_outcome_model(ipd, outcome, weights = naive$fit$weights)
naive_result <- compare_to_sld(naive_model, sld_outcome, contrast = arms$contrast)
show("Naive comparison: weighted proportion (here unweighted) vs published proportion, as a log OR",
     naive_result, digits = 4)

# 4. Targets for the full set, and what infeasible variables look like ------------------------
targets <- build_match_targets(ipd_summary, sld_summary, metadata, include_adjust = TRUE)
show("Targets: mean (+ variance for age) per continuous, K-1 proportions per categorical", targets)

bad <- metadata
bad$adjust[bad$variable == "fac_color_sld_extra"] <- TRUE
msg <- tryCatch(build_match_targets(ipd_summary, sld_summary, bad, include_adjust = TRUE), error = conditionMessage)
cat("\n== Trying to weight on fac_color_sld_extra fails, by design:\n", msg, "\n")
bad <- metadata
bad$adjust[bad$variable == "fac_weight"] <- TRUE
msg <- tryCatch(build_match_targets(ipd_summary, sld_summary, bad, include_adjust = TRUE), error = conditionMessage)
cat("\n== Trying to weight on an unreported variable fails, by design:\n", msg, "\n")

# 5. Design matrix and weights ----------------------------------------------------------------
design <- build_design_matrix(ipd, targets)
cat("\n== Design matrix:", nrow(design$X), "complete rows x", ncol(design$X), "constraints;",
    sum(!design$complete), "rows excluded for NA in a weighted variable\n")
print(round(head(design$X, 4), 3))

fit <- estimate_maic_weights(design)
show("Solver output", list(alpha = round(fit$alpha, 3), optim = fit$optim, max_moment_error = fit$max_moment_error))
w <- fit$weights
show("Weight diagnostics: many constraints on a small arm cost a lot of ESS", weight_diagnostics(fit), digits = 3)
print(plot_weights(fit, title = "MAIC weights, unanchored, intervention arm"))

# 6. Balance after weighting -------------------------------------------------------------------
balance_after <- create_balance_table(summarize_ipd(ipd, metadata, weights = w), sld_summary, metadata)
show("Balance before vs after: every weighted term reaches SMD 0",
     format_balance_comparison(compare_balance_tables(balance_before, balance_after)), right = FALSE)

# 7. Weighted outcome model and comparison --------------------------------------------------------
model  <- fit_outcome_model(ipd, outcome, weights = w)
show("Weighted model: intercept-only logistic regression; the intercept is logit(weighted response rate)",
     summary(model$model)$coefficients)
cat("weighted response rate:", round(plogis(model$estimate$estimate), 3),
    "| published:", round(plogis(sld_outcome$estimate), 3), "\n")
result <- compare_to_sld(model, sld_outcome, contrast = arms$contrast)
show("Unanchored comparison: difference of logits is a log odds ratio", result, digits = 4)

both <- dplyr::bind_rows(
  dplyr::mutate(naive_result, n = naive_model$n, ess = naive_model$ess),
  dplyr::mutate(result,       n = model$n,       ess = model$ess)
)
show("Naive vs weighted", cbind(analysis = c("naive", "weighted"), format_results(both)), right = FALSE)

# 8. Bootstrap --------------------------------------------------------------------------------
# Ten constraints on 30 patients: expect some replicates to be infeasible or
# not converge, so the failure allowance is raised for the toy data.
boot <- bootstrap_comparison(ipd, sld_summary, metadata, outcome, sld_outcome,
                             include_adjust = TRUE, n_boot = 200, seed = 2026, max_fail_rate = 0.25)
show("Bootstrap summary", boot$summary, digits = 3)
show("Bootstrap diagnostics", boot$diagnostics, digits = 3)
show("Failures by reason", boot$failure_counts)

# 9. Scenarios over the adjust tier --------------------------------------------------------------
# Sequential: start naive, add one variable at a time in this order.
adjust_order <- c("fac_age", "fac_sev_hb", "fac_tar_jnt_lead", "fac_prior_treatment",
                  "fac_color_missing_both", "fac_bmi", "fac_egfr")
sequential <- run_maic_scenarios(
  define_scenarios_sequential(metadata, adjust_order, include_empty = TRUE),
  ipd_all, sld, outcome, sld_outcome, arm = "ARM", intervention_arm = "A"
)
show("Sequential: naive baseline, then each variable added to the weighting set",
     format_results(sequential$results)[, c("Model", "Estimate (95% CI)", "N", "Weighting ESS")], right = FALSE)

univariate <- run_maic_scenarios(
  define_scenarios_univariate(metadata, adjust_order),
  ipd_all, sld, outcome, sld_outcome, arm = "ARM", intervention_arm = "A"
)
show("Univariate: each variable weighted on alone",
     format_results(univariate$results)[, c("Model", "Estimate (95% CI)", "N", "Weighting ESS")], right = FALSE)

cat("\nDone. run_maic_analysis(ipd_all, sld, metadata, outcome, sld_outcome, arm = \"ARM\",",
    "intervention_arm = \"A\", include_adjust = TRUE) runs steps 1 to 8 in one call.\n")
