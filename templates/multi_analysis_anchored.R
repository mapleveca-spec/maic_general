# Multiple anchored analyses from a prepared input list ------------------------
#
# For every element of `analysis_inputs`: a sequential scenario starting from
# the primary weighting set (effect modifiers) and adding adjust-tier
# prognostic variables one by one, and a univariate scenario. Each analysis
# is independent. Everything is exported in one fixed layout and the results
# are stacked across analyses.
#
# Input element (see sandbox/toy_analysis_inputs.R):
#   analysis_name, population, comparator, ipd (two arms), sld, metadata,
#   outcome, sld_outcome (log_or, log_hr or mean_diff), arm, reference_arm,
#   adjust_order (optional)
#
# Error handling as in multi_analysis_unanchored.R: failed steps are rows
# with status "error"; analyses that cannot start are listed in `errors`.
#
# Runs as is from the framework root:  Rscript templates/multi_analysis_anchored.R

framework_root <- "."
output_dir     <- file.path(tempdir(), "maic_multi_anchored")
source(file.path(framework_root, "08_main", "source_framework.R"))
source_framework(framework_root)
source(file.path(framework_root, "sandbox", "make_toy_data.R"))
source(file.path(framework_root, "sandbox", "toy_analysis_inputs.R"))

# Replace with your own prepared list -------------------------------------------------
analysis_inputs <- make_toy_analysis_inputs("anchored")

# Run ----------------------------------------------------------------------------------
out <- run_analyses_anchored(analysis_inputs, output_dir = output_dir)

write.csv(format_results(out$sequential), file.path(output_dir, "all_sequential.csv"), row.names = FALSE)
write.csv(format_results(out$univariate), file.path(output_dir, "all_univariate.csv"), row.names = FALSE)
write.csv(out$errors, file.path(output_dir, "errors.csv"), row.names = FALSE)
saveRDS(out, file.path(output_dir, "all_runs.rds"))

# Print ----------------------------------------------------------------------------------
cols <- c("analysis", "Model", "Status", "Contrast", "Estimate (95% CI)", "N", "Weighting ESS", "ESS %", "Weight max")
stacked <- function(results) {
  dplyr::bind_cols(results[c("analysis", "population", "comparator")], format_results(results))
}

cat("== Sequential scenarios, all analyses ('(none)' = effect modifiers only)\n")
print(as.data.frame(stacked(out$sequential)[, cols]), right = FALSE)
cat("\n== Univariate scenarios, all analyses\n")
print(as.data.frame(stacked(out$univariate)[, cols]), right = FALSE)
cat("\n== Analyses that could not start\n")
print(as.data.frame(out$errors), right = FALSE)
cat("\nOutputs written to", output_dir, "\n")
print(list.files(output_dir, recursive = FALSE))
