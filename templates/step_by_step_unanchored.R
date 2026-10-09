# Step by step: unanchored MAIC -------------------------------------------------
#
# Walks through one unanchored MAIC by calling each module function in turn
# and printing what it produces. The pipeline run_maic_unanchored() makes
# exactly these calls, in this order, with no extra logic.
#
# Input: one element of an `analysis_inputs` list (see
# sandbox/toy_analysis_inputs.R for the shape):
#   ipd          the analysis population as is: a single-arm study, or a
#                trial already restricted to the arm of interest. No arm
#                column is needed and none is used.
#   sld          comparator baseline table
#   metadata     covariate metadata; for unanchored, no `match` tier and
#                every usable covariate in the `adjust` tier
#   outcome      define_outcome()
#   sld_outcome  an absolute published outcome (logit_p or mean)
#
# Runs as is from the framework root:  Rscript templates/step_by_step_unanchored.R

framework_root <- "."
source(file.path(framework_root, "08_main", "source_framework.R"))
source_framework(framework_root)
source(file.path(framework_root, "sandbox", "make_toy_data.R"))
source(file.path(framework_root, "sandbox", "toy_analysis_inputs.R"))

show <- function(title, x, ...) {
  cat("\n==", title, "\n")
  print(if (is.data.frame(x)) as.data.frame(x) else x, ...)
}

# Replace with your own prepared input ---------------------------------------------
a <- make_toy_analysis_inputs("unanchored")[[1]]
ipd <- a$ipd
sld <- a$sld
metadata <- a$metadata
outcome <- a$outcome
sld_outcome <- a$sld_outcome
cat("Analysis:", a$analysis_name, "|", nrow(ipd), "patients | arm column present:", "ARM" %in% names(ipd), "\n")

# 1. Validate ------------------------------------------------------------------------
validate_metadata(metadata)
validate_sld(sld, metadata)
validate_ipd(ipd, metadata)
check_outcome_pair(outcome, sld_outcome)
stopifnot(!sld_outcome$anchored)
show("Metadata: no match tier; adjust tier is the weighting set (include_adjust = TRUE)",
     metadata[, c("variable", "type", "match", "adjust", "match_sd")])
show("Published result: anchored is derived from the scale", unclass(sld_outcome))

# 2. Summaries and balance before weighting ---------------------------------------------
ipd_summary <- summarize_ipd(ipd, metadata)
sld_summary <- summarize_sld(sld, metadata)
show("IPD summary: one row per variable and level, Missing rows where NA exists", ipd_summary, digits = 3)
show("SLD summary: same schema", sld_summary, digits = 3)
balance_before <- create_balance_table(ipd_summary, sld_summary, metadata)
show("Balance before weighting", balance_before[, c("variable", "level", "row_type", "ipd_est", "sld_est", "smd")],
     digits = 3)

# 3. Naive baseline: empty weighting set, unit weights ----------------------------------------
naive <- estimate_weights(ipd, ipd_summary, sld_summary, metadata, include_adjust = FALSE)
naive_model  <- fit_outcome_model(ipd, outcome, weights = naive$fit$weights)
naive_result <- compare_to_sld(naive_model, sld_outcome, contrast = UNANCHORED_CONTRAST)
show("Naive comparison (unweighted)", naive_result, digits = 4)

# 4. Targets ----------------------------------------------------------------------------
targets <- build_match_targets(ipd_summary, sld_summary, metadata, include_adjust = TRUE)
show("Targets: one constraint per row; reference levels dropped; Missing rows never targets", targets)

# 5. Design matrix and weights ----------------------------------------------------------------
design <- build_design_matrix(ipd, targets)
cat("\n== Design matrix:", nrow(design$X), "complete rows x", ncol(design$X), "constraints;",
    sum(!design$complete), "rows excluded for NA in a weighted variable\n")
print(round(head(design$X, 4), 3))
fit <- estimate_maic_weights(design)
show("Solver: alpha, optim record, residual moment error",
     list(alpha = round(fit$alpha, 3), optim = fit$optim, max_moment_error = fit$max_moment_error))
w <- fit$weights
show("Weight diagnostics", weight_diagnostics(fit), digits = 3)
ipd$weight <- rescale_weights(w)
show("Heaviest and lightest patients (rescaled: 1 = one patient)",
     rbind(head(ipd[order(-ipd$weight), c(metadata$variable[metadata$adjust], "weight")], 5),
           head(ipd[order(ipd$weight), c(metadata$variable[metadata$adjust], "weight")], 5)), digits = 3)
print(plot_weights(fit))

# 6. Balance after weighting ---------------------------------------------------------------------
balance_after <- create_balance_table(summarize_ipd(ipd, metadata, weights = w), sld_summary, metadata)
show("Balance before vs after: weighted terms reach SMD 0",
     format_balance_comparison(compare_balance_tables(balance_before, balance_after)), right = FALSE)

# 7. Weighted outcome model and comparison --------------------------------------------------------
model  <- fit_outcome_model(ipd, outcome, weights = w)
show("Weighted model: intercept-only; the intercept is the weighted outcome on the analysis scale",
     summary(model$model)$coefficients)
result <- compare_to_sld(model, sld_outcome, contrast = UNANCHORED_CONTRAST)
show("Unanchored comparison: IPD minus published, variances added", result, digits = 4)
both <- dplyr::bind_rows(
  dplyr::mutate(naive_result, n = naive_model$n, ess = naive_model$ess),
  dplyr::mutate(result,       n = model$n,       ess = model$ess)
)
show("Naive vs weighted", cbind(analysis = c("naive", "weighted"), format_results(both)), right = FALSE)

# 8. Bootstrap --------------------------------------------------------------------------------
boot <- bootstrap_comparison(ipd, sld_summary, metadata, outcome, sld_outcome,
                             include_adjust = TRUE, n_boot = 200, seed = 2026, max_fail_rate = 0.25)
show("Bootstrap summary: percentile interval, extreme-replicate count", boot$summary, digits = 3)
show("Bootstrap diagnostics", boot$diagnostics, digits = 3)
show("Failures by reason", boot$failure_counts)

# 9. The same in one call -----------------------------------------------------------------------
one_call <- run_maic_unanchored(ipd, sld, metadata, outcome, sld_outcome)
stopifnot(isTRUE(all.equal(one_call$result$estimate, result$estimate)))
cat("\nrun_maic_unanchored() reproduces steps 1 to 7.\n")

# 10. Scenarios: sequential from the naive model, and univariate -------------------------------------
order <- if (is.null(a$adjust_order)) metadata$variable[metadata$adjust] else a$adjust_order
sequential <- run_scenarios_unanchored(define_scenarios_sequential(metadata, order, include_empty = TRUE),
                                       ipd, sld, outcome, sld_outcome)
univariate <- run_scenarios_unanchored(define_scenarios_univariate(metadata, order), ipd, sld, outcome, sld_outcome)
cols <- c("Model", "Status", "Estimate (95% CI)", "N", "Weighting ESS", "ESS %", "Excluded",
          "Weight min", "Weight median", "Weight max", "Top 10% share")
show("Sequential: each row is a model; status and error columns record any failed step",
     format_results(sequential$results)[, cols], right = FALSE)
show("Univariate", format_results(univariate$results)[, cols], right = FALSE)
show("Balance path, sequential (first columns): weighted IPD and SMD at each step",
     scenario_balance_path(sequential)[, 1:9], right = FALSE)

# 11. Export in the fixed layout ----------------------------------------------------------------------
output_dir <- file.path(tempdir(), "maic_step_unanchored")
export_scenarios(sequential, file.path(output_dir, "sequential"))
export_scenarios(univariate, file.path(output_dir, "univariate"))
cat("\nOutputs written to", output_dir, "\n")
print(list.files(output_dir, recursive = TRUE))
