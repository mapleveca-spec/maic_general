# Multiple unanchored analyses from a prepared input list ----------------------
#
# For every element of `analysis_inputs`: a sequential scenario starting from
# the unweighted model and adding adjust-tier variables one by one, and a
# univariate scenario. Each analysis is independent. Everything is exported
# in one fixed layout and the results are stacked across analyses.
#
# Input element (see sandbox/toy_analysis_inputs.R):
#   analysis_name, population, comparator, ipd (the analysis population as
#   is; no arm column is used), sld, metadata, outcome, sld_outcome
#   (logit_p or mean), adjust_order (optional)
#
# Error handling: a failed scenario step is recorded in that analysis's
# results with status "error", the message, and the variables in play; an
# analysis that cannot start is recorded in `errors` with its stage. Nothing
# stops the other analyses. The last two inputs below are broken on purpose
# to show both.
#
# Output layout under output_dir:
#   <analysis_name>/sequential/  results.csv, results_numeric.csv,
#                                balance_path.csv, weights/*.png, manifest.csv
#   <analysis_name>/univariate/  same
#   all_sequential.csv, all_univariate.csv, errors.csv, all_runs.rds
#
# Runs as is from the framework root:  Rscript templates/multi_analysis_unanchored.R

framework_root <- "."
output_dir     <- file.path(tempdir(), "maic_multi_unanchored")
source(file.path(framework_root, "08_main", "source_framework.R"))
source_framework(framework_root)
source(file.path(framework_root, "sandbox", "make_toy_data.R"))
source(file.path(framework_root, "sandbox", "toy_analysis_inputs.R"))

# Replace with your own prepared list -------------------------------------------------
analysis_inputs <- make_toy_analysis_inputs("unanchored")

# Two deliberately broken inputs, to show how failures are reported.
broken_step <- analysis_inputs[[1]]
broken_step$analysis_name <- "Response, population A, infeasible variable"
# fac_color_sld_extra has an SLD level the IPD lacks, so its step must fail
broken_step$metadata$adjust[broken_step$metadata$variable == "fac_color_sld_extra"] <- TRUE
broken_step$adjust_order <- c("fac_age", "fac_sev_hb", "fac_color_sld_extra", "fac_bmi")

broken_input <- analysis_inputs[[2]]
broken_input$analysis_name <- "Response, population B, wrong published scale"
broken_input$sld_outcome <- define_sld_outcome("Response", "log_or", estimate = 0.4, se = 0.26)  # anchored, not allowed

analysis_inputs <- c(analysis_inputs, list(broken_step, broken_input))

# Run ----------------------------------------------------------------------------------
out <- run_analyses_unanchored(analysis_inputs, output_dir = output_dir)

write.csv(format_results(out$sequential), file.path(output_dir, "all_sequential.csv"), row.names = FALSE)
write.csv(format_results(out$univariate), file.path(output_dir, "all_univariate.csv"), row.names = FALSE)
write.csv(out$errors, file.path(output_dir, "errors.csv"), row.names = FALSE)
saveRDS(out, file.path(output_dir, "all_runs.rds"))

# Print ----------------------------------------------------------------------------------
cols <- c("analysis", "Model", "Status", "Estimate (95% CI)", "N", "Weighting ESS", "ESS %", "Weight max",
          "Top 10% share")
stacked <- function(results) {
  dplyr::bind_cols(results[c("analysis", "population", "comparator")], format_results(results))
}

cat("== Sequential scenarios, all analyses\n")
print(as.data.frame(stacked(out$sequential)[, cols]), right = FALSE)
cat("\n== Univariate scenarios, all analyses\n")
print(as.data.frame(stacked(out$univariate)[, cols]), right = FALSE)
cat("\n== Failed steps (status = error) with their context\n")
failed <- out$sequential[out$sequential$status == "error", c("analysis", "label", "variables", "error")]
print(as.data.frame(failed), right = FALSE)
cat("\n== Analyses that could not start\n")
print(as.data.frame(out$errors), right = FALSE)
cat("\nOutputs written to", output_dir, "\n")
