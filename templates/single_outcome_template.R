# Single-outcome template: binary response ---------------------------------------
#
# A complete MAIC for one outcome, specified inline without the outcome tables.
# Use this when a study has one primary endpoint, or to look at one outcome in
# depth. The multi-outcome route is new_study_template.R.
#
# Filled with the toy data, so it runs as is from the framework root:
#   Rscript templates/single_outcome_template.R
#
# The outcome here is binary response. For a continuous or time-to-event
# outcome change define_outcome() and the published-result scale; see the
# table in the README ("Outcome tables") for which scale goes with which.

framework_root <- "."
study_dir      <- "templates"
output_dir     <- file.path(tempdir(), "maic_output_response")
dir.create(output_dir, showWarnings = FALSE)

source(file.path(framework_root, "08_main", "source_framework.R"))
source_framework(framework_root)

# Covariate inputs (see new_study_template.R for the field-by-field notes) -----------
ipd      <- read.csv(file.path(study_dir, "ipd_example.csv"), stringsAsFactors = FALSE, na.strings = c("", "NA"))
metadata <- read_metadata_csv(file.path(study_dir, "metadata_template.csv"))
sld      <- read.csv(file.path(study_dir, "sld_template.csv"), stringsAsFactors = FALSE, na.strings = c("", "NA"))

validate_metadata(metadata)
validate_sld(sld, metadata)
validate_ipd(ipd, metadata)

# The outcome ------------------------------------------------------------------------
# IPD column y_resp is 0/1. The name is a label that must match the published
# result's name below.
response <- define_outcome("Response", type = "binary", var = "y_resp")

# Published results for this outcome --------------------------------------------------
# Anchored: the comparator trial reported OR 1.50 (95% CI 0.90 to 2.50) for its
# intervention vs the common comparator. Enter on the log scale; the CI gives the SE.
response_anchored <- define_sld_outcome(
  "Response", scale = "log_or",
  estimate = log(1.50), ci_low = log(0.90), ci_high = log(2.50)
)
# Unanchored: 40 of 100 patients responded on the comparator intervention.
response_unanchored <- sld_outcome_from_proportion("Response", p = 40 / 100, n = 100)

# Arms --------------------------------------------------------------------------------
arm              <- "ARM"   # IPD treatment column
reference_arm    <- "A"     # common comparator for the anchored comparison: estimate is B vs A
intervention_arm <- "A"     # arm compared in the unanchored comparison; IPD is subset to it

# Anchored analysis, primary weighting set, with bootstrap ---------------------------------
anchored <- run_maic_analysis(
  ipd, sld, metadata, response, response_anchored,
  arm = arm, reference_arm = reference_arm,
  include_adjust = FALSE,
  n_boot = 200, seed = 2026            # raise to 1000+ for the final run
)

# Anchored analysis, full weighting set (effect modifiers + prognostic variables) ------------
# Nine moments on 60 toy patients: a few bootstrap resamples cannot be weighted
# (a level vanishes, or the solver stalls), so the allowed failure rate is
# raised here. On trial-sized data the default 5% is appropriate; a high
# failure count is itself a finding about the specification.
anchored_full <- run_maic_analysis(
  ipd, sld, metadata, response, response_anchored,
  arm = arm, reference_arm = reference_arm,
  include_adjust = TRUE,
  n_boot = 200, seed = 2026, max_fail_rate = 0.10
)

# Unanchored analysis on the intervention arm --------------------------------------------
# TSD 18: unanchored comparisons must weight on all prognostic variables and
# effect modifiers, so include_adjust = TRUE.
unanchored <- run_maic_analysis(
  ipd, sld, metadata, response, response_unanchored,
  arm = arm, intervention_arm = intervention_arm,
  include_adjust = TRUE,
  n_boot = 200, seed = 2026, max_fail_rate = 0.10   # 30 toy patients, see note above
)

# Sensitivity on the anchored comparison: add prognostic variables one at a time ---------
prognostic_order <- metadata$variable[metadata$adjust]   # or your own prespecified order
sequential <- run_maic_scenarios(
  define_scenarios_sequential(metadata, prognostic_order, include_empty = TRUE),
  ipd, sld, response, response_anchored, arm = arm, reference_arm = reference_arm
)
univariate <- run_maic_scenarios(
  define_scenarios_univariate(metadata, prognostic_order),
  ipd, sld, response, response_anchored, arm = arm, reference_arm = reference_arm
)

# Tables and figures -----------------------------------------------------------------------
analysis_labels <- c("anchored, primary set", "anchored, full set", "unanchored, full set")
results <- dplyr::bind_cols(
  tibble::tibble(analysis = analysis_labels),
  format_results(dplyr::bind_rows(anchored$result, anchored_full$result, unanchored$result))
)
balance_of <- function(a) format_balance_comparison(compare_balance_tables(a$balance_before, a$balance_after))
balance_anchored   <- balance_of(anchored)
balance_unanchored <- balance_of(unanchored)
diagnostics <- dplyr::bind_rows(
  cbind(analysis = "anchored, primary set", anchored$diagnostics),
  cbind(analysis = "anchored, full set",    anchored_full$diagnostics),
  cbind(analysis = "unanchored, full set",  unanchored$diagnostics)
)
boot_table <- function(label, a) {
  cbind(analysis = label, a$bootstrap$summary, a$bootstrap$diagnostics[-(1:2)])
}
bootstrap <- dplyr::bind_rows(
  boot_table("anchored, primary set", anchored),
  boot_table("anchored, full set",    anchored_full),
  boot_table("unanchored, full set",  unanchored)
)

write.csv(results,            file.path(output_dir, "results_response.csv"),      row.names = FALSE)
write.csv(balance_anchored,   file.path(output_dir, "balance_anchored.csv"),      row.names = FALSE)
write.csv(balance_unanchored, file.path(output_dir, "balance_unanchored.csv"),    row.names = FALSE)
write.csv(diagnostics,        file.path(output_dir, "weight_diagnostics.csv"),    row.names = FALSE)
write.csv(bootstrap,          file.path(output_dir, "bootstrap_diagnostics.csv"), row.names = FALSE)
write.csv(format_results(sequential$results), file.path(output_dir, "sequential.csv"), row.names = FALSE)
write.csv(format_results(univariate$results), file.path(output_dir, "univariate.csv"), row.names = FALSE)
ggplot2::ggsave(file.path(output_dir, "weights_anchored.png"),
                plot_weights(anchored$weight_fit, title = "MAIC weights: anchored, primary set"),
                width = 7, height = 4.5, dpi = 150)
ggplot2::ggsave(file.path(output_dir, "weights_unanchored.png"),
                plot_weights(unanchored$weight_fit, title = "MAIC weights: unanchored, intervention arm"),
                width = 7, height = 4.5, dpi = 150)
saveRDS(list(anchored = anchored, anchored_full = anchored_full, unanchored = unanchored,
             sequential = sequential, univariate = univariate),
        file.path(output_dir, "maic_response.rds"))

cat("== Response: indirect comparisons\n")
show_cols <- c("analysis", "Contrast", "Measure", "Estimate (95% CI)", "Bootstrap 95% CI", "N", "ESS")
print(as.data.frame(results[, show_cols]), right = FALSE)
cat("\n== Bootstrap stability\n")
print(as.data.frame(bootstrap[, c("analysis", "n_boot_ok", "n_extreme", "ess_median", "ess_min")]), digits = 3)
cat("\n== Sequential sensitivity, anchored\n")
print(as.data.frame(format_results(sequential$results)[, c("Model", "Estimate (95% CI)", "N", "Weighting ESS")]),
      right = FALSE)
cat("\nOutputs written to", output_dir, "\n")
