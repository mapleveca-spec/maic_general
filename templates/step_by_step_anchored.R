# Step by step: anchored MAIC --------------------------------------------------
#
# Walks through one anchored MAIC by calling each module function in turn and
# printing what it produces. The pipeline run_maic_anchored() makes exactly
# these calls, in this order, with no extra logic.
#
# Input: one element of an `analysis_inputs` list (see
# sandbox/toy_analysis_inputs.R for the shape):
#   ipd            two-arm trial IPD
#   sld            comparator baseline table
#   metadata       covariate metadata; `match` = effect modifiers (always
#                  weighted on), `adjust` = prognostic variables (sensitivity)
#   outcome        define_outcome()
#   sld_outcome    a published contrast vs the common comparator
#                  (log_or, log_hr, mean_diff)
#   arm            IPD treatment column
#   reference_arm  the common comparator; the estimate is "other arm vs it"
#
# Runs as is from the framework root:  Rscript templates/step_by_step_anchored.R

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
a <- make_toy_analysis_inputs("anchored")[[1]]
ipd <- a$ipd
sld <- a$sld
metadata <- a$metadata
outcome <- a$outcome
sld_outcome <- a$sld_outcome
arm <- a$arm
reference_arm <- a$reference_arm
cat("Analysis:", a$analysis_name, "|", nrow(ipd), "patients | arms:", paste(sort(unique(ipd[[arm]])), collapse = ", "),
    "| reference:", reference_arm, "\n")

# 1. Validate, including the arm rules ------------------------------------------------
validate_metadata(metadata)
validate_sld(sld, metadata)
validate_ipd(ipd, metadata)
check_outcome_pair(outcome, sld_outcome)
stopifnot(sld_outcome$anchored)
contrast <- check_anchored_arms(ipd, arm, reference_arm)
cat("contrast:", contrast, "\n")
show("Metadata tiers", metadata[, c("variable", "type", "match", "adjust", "match_sd")])
show("Weighting set by specification",
     data.frame(variable = metadata$variable,
                primary  = weighting_variables(metadata),
                full     = weighting_variables(metadata, include_adjust = TRUE)))

# 2. Summaries and balance before weighting ---------------------------------------------
ipd_summary <- summarize_ipd(ipd, metadata)
sld_summary <- summarize_sld(sld, metadata)
balance_before <- create_balance_table(ipd_summary, sld_summary, metadata)
show("Balance before weighting (whole trial vs comparator population)",
     balance_before[, c("variable", "level", "row_type", "ipd_est", "sld_est", "smd")], digits = 3)

# 3. Naive baseline: the plain arm-only regression -------------------------------------------
naive_meta <- metadata
naive_meta$match <- FALSE
naive_meta$adjust <- FALSE
naive_meta$match_sd <- FALSE
naive <- estimate_weights(ipd, ipd_summary, sld_summary, naive_meta)
naive_model  <- fit_outcome_model(ipd, outcome, weights = naive$fit$weights, arm = arm, reference_arm = reference_arm)
naive_result <- compare_to_sld(naive_model, sld_outcome, contrast = contrast)
show("Naive comparison: unweighted arm contrast minus published contrast (Bucher)", naive_result, digits = 4)

# 4. Targets for the primary set (effect modifiers) -----------------------------------------------
targets <- build_match_targets(ipd_summary, sld_summary, metadata, include_adjust = FALSE)
show("Targets", targets)

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
print(plot_weights(fit))

# 6. Balance after weighting ---------------------------------------------------------------------
balance_after <- create_balance_table(summarize_ipd(ipd, metadata, weights = w), sld_summary, metadata)
show("Balance before vs after: matched terms reach SMD 0, unmatched ones do not",
     format_balance_comparison(compare_balance_tables(balance_before, balance_after)), right = FALSE)

# 7. Weighted outcome model and comparison --------------------------------------------------------
model <- fit_outcome_model(ipd, outcome, weights = w, arm = arm, reference_arm = reference_arm)
show("Weighted model: outcome ~ arm, nothing else", summary(model$model)$coefficients)
result <- compare_to_sld(model, sld_outcome, contrast = contrast)
show("Bucher comparison: IPD contrast minus published contrast, variances added", result, digits = 4)
both <- dplyr::bind_rows(
  dplyr::mutate(naive_result, n = naive_model$n, ess = naive_model$ess),
  dplyr::mutate(result,       n = model$n,       ess = model$ess)
)
show("Naive vs weighted", cbind(analysis = c("naive", "weighted"), format_results(both)), right = FALSE)

# 8. Bootstrap (stratified by arm) --------------------------------------------------------------
boot <- bootstrap_comparison(ipd, sld_summary, metadata, outcome, sld_outcome,
                             arm = arm, reference_arm = reference_arm, n_boot = 200, seed = 2026)
show("Bootstrap summary", boot$summary, digits = 3)
show("Bootstrap diagnostics", boot$diagnostics, digits = 3)
show("Failures by reason", boot$failure_counts)

# 9. The same in one call -----------------------------------------------------------------------
one_call <- run_maic_anchored(ipd, sld, metadata, outcome, sld_outcome, arm = arm, reference_arm = reference_arm)
stopifnot(isTRUE(all.equal(one_call$result$estimate, result$estimate)))
cat("\nrun_maic_anchored() reproduces steps 1 to 7.\n")

# 10. Scenarios: prognostic variables added to the effect modifiers one by one -----------------------
order <- if (is.null(a$adjust_order)) metadata$variable[metadata$adjust] else a$adjust_order
sequential <- run_scenarios_anchored(define_scenarios_sequential(metadata, order, include_empty = TRUE),
                                     ipd, sld, outcome, sld_outcome, arm = arm, reference_arm = reference_arm)
univariate <- run_scenarios_anchored(define_scenarios_univariate(metadata, order),
                                     ipd, sld, outcome, sld_outcome, arm = arm, reference_arm = reference_arm)
cols <- c("Model", "Status", "Estimate (95% CI)", "N", "Weighting ESS", "ESS %", "Excluded",
          "Weight min", "Weight median", "Weight max", "Top 10% share")
show("Sequential: '(none)' is the primary set alone; status/error columns record any failed step",
     format_results(sequential$results)[, cols], right = FALSE)
show("Univariate", format_results(univariate$results)[, cols], right = FALSE)
show("Balance path, sequential (first columns)", scenario_balance_path(sequential)[, 1:9], right = FALSE)

# 11. Export in the fixed layout ----------------------------------------------------------------------
output_dir <- file.path(tempdir(), "maic_step_anchored")
export_scenarios(sequential, file.path(output_dir, "sequential"))
export_scenarios(univariate, file.path(output_dir, "univariate"))
cat("\nOutputs written to", output_dir, "\n")
print(list.files(output_dir, recursive = TRUE))
