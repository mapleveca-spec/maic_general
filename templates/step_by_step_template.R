# Step-by-step template ----------------------------------------------------------
#
# Walks through one MAIC by calling each module function in turn and printing
# what it produces, so you can see every intermediate object: the summaries,
# the aligned balance table, the target moments, the design matrix, the
# solver output, the weights, the weighted balance, the outcome model, the
# comparison, the bootstrap, and the scenario definitions. The pipelines in
# 08_main do exactly these calls, in this order, with no extra logic.
#
# Filled with the toy data; runs as is from the framework root:
#   Rscript templates/step_by_step_template.R
# Run it line by line in RStudio to inspect objects as you go.
#
# Example: anchored comparison of binary response, B vs A in the IPD against a
# published odds ratio, weighting on the primary set (effect modifiers).

framework_root <- "."
study_dir      <- "templates"
source(file.path(framework_root, "08_main", "source_framework.R"))
source_framework(framework_root)

show <- function(title, x, ...) {
  cat("\n==", title, "\n")
  print(if (is.data.frame(x)) as.data.frame(x) else x, ...)
}

# 0. Inputs --------------------------------------------------------------------------
ipd      <- read.csv(file.path(study_dir, "ipd_example.csv"), stringsAsFactors = FALSE, na.strings = c("", "NA"))
metadata <- read_metadata_csv(file.path(study_dir, "metadata_template.csv"))
sld      <- read.csv(file.path(study_dir, "sld_template.csv"), stringsAsFactors = FALSE, na.strings = c("", "NA"))

validate_metadata(metadata)
validate_sld(sld, metadata)
validate_ipd(ipd, metadata)

metadata_shown <- metadata
metadata_shown$level_order <- vapply(metadata$level_order, function(x) paste(x, collapse = "|"), "")
show("Metadata: the flags that drive everything", metadata_shown)
show("Which variables are in the weighting set?",
     data.frame(variable = metadata$variable,
                primary  = weighting_variables(metadata),
                full     = weighting_variables(metadata, include_adjust = TRUE)))

# 1. Arms and the comparison kind ------------------------------------------------------
outcome     <- define_outcome("Response", "binary", "y_resp")
sld_outcome <- define_sld_outcome("Response", "log_or", estimate = log(1.5), ci_low = log(0.9), ci_high = log(2.5))
check_outcome_pair(outcome, sld_outcome)
arms <- resolve_arms(ipd, anchored = sld_outcome$anchored, arm = "ARM", reference_arm = "A")
show("Arms: comparison kind, rows used, contrast", arms[c("anchored", "arm", "reference_arm", "contrast")])
cat("rows in analysis set:", nrow(arms$ipd), "\n")
ipd <- arms$ipd

# 2. Summaries: both sources in one schema ---------------------------------------------
ipd_summary <- summarize_ipd(ipd, metadata)
sld_summary <- summarize_sld(sld, metadata)
show("IPD summary (unweighted): one row per variable and level, Missing rows where NA exists", ipd_summary, digits = 3)
show("SLD summary: same schema, straight from the published table", sld_summary, digits = 3)

# 3. Balance before weighting ----------------------------------------------------------
aligned <- align_summaries(ipd_summary, sld_summary, metadata)
show("Aligned: union of levels, gaps filled with 0 where the side reported the variable",
     aligned[, c("variable", "level", "row_type", "ipd_est", "sld_est")], digits = 3)
balance_before <- add_smd(aligned)
show("Balance before weighting (SMD = IPD minus SLD over pooled SD)",
     balance_before[, c("variable", "level", "row_type", "ipd_est", "sld_est", "smd")], digits = 3)

# 4. Target moments -----------------------------------------------------------------
targets <- build_match_targets(ipd_summary, sld_summary, metadata, include_adjust = FALSE)
show("Targets: one constraint per row; reference levels dropped; Missing rows never targets", targets)

# 5. Design matrix -----------------------------------------------------------------
design <- build_design_matrix(ipd, targets)
cat("\n== Design matrix: patients x constraints, centred on the targets\n")
cat("dimensions:", nrow(design$X), "complete rows x", ncol(design$X), "constraints;",
    sum(!design$complete), "rows excluded for NA in a matched variable\n")
print(round(head(design$X, 5), 3))
show("Column means before weighting (= IPD minus SLD moment on complete cases)", round(colMeans(design$X), 4))

# 6. Solve for the weights -------------------------------------------------------------
fit <- estimate_maic_weights(design)
show("Solver: coefficients alpha (weight_i = exp(X_i alpha)), convergence, residual moment error",
     list(alpha = round(fit$alpha, 4), optim = fit$optim, max_moment_error = fit$max_moment_error))
w <- fit$weights
cat("\nweighted column means (should be ~0):\n")
print(round(colSums(design$X * w[design$complete]) / sum(w[design$complete]), 6))

# 7. Weights and their diagnostics -----------------------------------------------------
show("Weight diagnostics", weight_diagnostics(fit), digits = 3)
ipd$weight <- rescale_weights(w)
show("Patients with the largest and smallest weights (rescaled: 1 = one patient)",
     rbind(head(ipd[order(-ipd$weight), c("USUBJID", "ARM", metadata$variable[metadata$match], "weight")], 5),
           head(ipd[order(ipd$weight), c("USUBJID", "ARM", metadata$variable[metadata$match], "weight")], 5)),
     digits = 3)
weights_plot <- plot_weights(fit)
print(weights_plot)   # opens in the plot pane when run interactively

# 8. Balance after weighting -----------------------------------------------------------
ipd_summary_w  <- summarize_ipd(ipd, metadata, weights = w)
balance_after  <- create_balance_table(ipd_summary_w, sld_summary, metadata)
comparison     <- compare_balance_tables(balance_before, balance_after)
show("Balance before vs after: matched terms go to SMD 0, unmatched ones do not",
     format_balance_comparison(comparison), right = FALSE)

# 9. Outcome model on the weighted IPD ---------------------------------------------------
model <- fit_outcome_model(ipd, outcome, weights = w, arm = arms$arm, reference_arm = arms$reference_arm)
show("Outcome model: weighted regression of outcome on arm, nothing else", summary(model$model)$coefficients)
show("IPD estimate on the analysis scale, with robust SE", model$estimate, digits = 4)
cat("contrast:", model$contrast, "| n:", model$n, "| ESS:", round(model$ess, 1), "\n")

# 10. Indirect comparison ----------------------------------------------------------------
result <- compare_to_sld(model, sld_outcome)
show("Bucher comparison: IPD contrast minus published contrast, variances added", result, digits = 4)
show("Formatted", format_results(dplyr::mutate(result, n = model$n, ess = model$ess)), right = FALSE)

# 11. Bootstrap ------------------------------------------------------------------------
boot <- bootstrap_comparison(ipd, sld_summary, metadata, outcome, sld_outcome,
                             arm = arms$arm, reference_arm = arms$reference_arm, n_boot = 200, seed = 2026)
show("Bootstrap summary: percentile interval over all finite replicates", boot$summary, digits = 3)
show("Bootstrap diagnostics: SE, median, replicate ESS spread", boot$diagnostics, digits = 3)
show("Failures by reason", boot$failure_counts)
show("Replicate estimate quantiles (log OR)", round(quantile(boot$replicates, c(0, .025, .25, .5, .75, .975, 1)), 3))

# 12. Scenario definitions -------------------------------------------------------------------
scenarios <- define_scenarios_sequential(metadata, metadata$variable[metadata$adjust], include_empty = TRUE)
flag_on <- function(s, flag) paste(s$metadata$variable[s$metadata[[flag]]], collapse = ", ")
show("Sequential scenarios: which variables are on in each",
     data.frame(label     = vapply(scenarios, `[[`, "", "label"),
                adjust_on = vapply(scenarios, flag_on, "", flag = "adjust"),
                match_on  = vapply(scenarios, flag_on, "", flag = "match")),
     right = FALSE)
sens <- run_maic_scenarios(scenarios, ipd, sld, outcome, sld_outcome,
                           arm = arms$arm, reference_arm = arms$reference_arm)
show("Scenario results: one row per model, with that model's weight distribution",
     format_results(sens$results)[, c("Model", "Estimate (95% CI)", "N", "Weighting ESS", "ESS %", "Excluded",
                                      "Weight min", "Weight median", "Weight max", "Top 10% share")], right = FALSE)

cat("\nDone. The same chain, without the printing, is run_maic_analysis().\n")
