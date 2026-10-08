# Multi-analysis template: several unanchored MAICs from a prepared input list ----
#
# Use when a project has several analyses (populations, comparators, outcomes)
# that each need the same treatment: a sequential scenario starting from the
# unweighted model and adding adjust-tier variables one by one, and a
# univariate scenario. Every analysis is independent; results are written in
# one fixed layout and stacked across analyses for the report.
#
# Input: a list, one element per analysis, each a list with
#   analysis_name  label, also used as the folder name
#   population     free text, carried into the stacked tables
#   comparator     free text, carried into the stacked tables
#   ipd            single-arm IPD for this analysis (or multi-arm; see arm below)
#   sld            comparator baseline table
#   metadata       covariate metadata; for an unanchored MAIC put every usable
#                  covariate in the `adjust` tier and leave `match` FALSE
#   outcome        define_outcome()
#   sld_outcome    define_sld_outcome() / sld_outcome_from_proportion(), on an
#                  unanchored scale (logit_p or mean)
#   adjust_order   (optional) order in which adjust variables enter the
#                  sequential scenario; default is metadata row order
#   arm, intervention_arm  (optional) only if the IPD has several arms
#
# Output layout:
#   <output_dir>/
#     <analysis_name>/sequential/   results.csv, results_numeric.csv,
#                                   balance_path.csv, weights/*.png, manifest.csv
#     <analysis_name>/univariate/   same
#     all_sequential.csv            every analysis's sequential table stacked
#     all_univariate.csv            every analysis's univariate table stacked
#     all_runs.rds                  every run object, for anything else
#
# Filled with analyses built from the toy data, so it runs as is:
#   Rscript templates/multi_analysis_template.R

framework_root <- "."
output_dir     <- file.path(tempdir(), "maic_multi")
source(file.path(framework_root, "08_main", "source_framework.R"))
source_framework(framework_root)

# Build the input list ----------------------------------------------------------------------
# Replace this block with your own prepared list. Here three analyses are
# carved from the toy data: two populations (arm A, arm B) and two outcomes.
toy <- readRDS("sandbox/toy_data.rds")
meta_unanchored <- toy$metadata
meta_unanchored$match    <- FALSE
meta_unanchored$adjust   <- meta_unanchored$variable %in% c("fac_sev_hb", "fac_tar_jnt_lead", "fac_age", "fac_bmi")
meta_unanchored$match_sd <- meta_unanchored$variable == "fac_age"

analysis_inputs <- list(
  resp_armA = list(
    analysis_name = "Response, population A",
    population    = "Trial X arm A",
    comparator    = "Study Y single arm",
    ipd           = toy$ipd[toy$ipd$ARM == "A", ],
    sld           = toy$sld,
    metadata      = meta_unanchored,
    outcome       = define_outcome("Response", "binary", "y_resp"),
    sld_outcome   = sld_outcome_from_proportion("Response", p = 0.40, n = 100),
    adjust_order  = c("fac_age", "fac_sev_hb", "fac_tar_jnt_lead", "fac_bmi")
  ),
  resp_armB = list(
    analysis_name = "Response, population B",
    population    = "Trial X arm B",
    comparator    = "Study Y single arm",
    ipd           = toy$ipd[toy$ipd$ARM == "B", ],
    sld           = toy$sld,
    metadata      = meta_unanchored,
    outcome       = define_outcome("Response", "binary", "y_resp"),
    sld_outcome   = sld_outcome_from_proportion("Response", p = 0.40, n = 100)
  ),
  score_armA = list(
    analysis_name = "Score change, population A",
    population    = "Trial X arm A",
    comparator    = "Study Y single arm",
    ipd           = toy$ipd[toy$ipd$ARM == "A", ],
    sld           = toy$sld,
    metadata      = meta_unanchored,
    outcome       = define_outcome("Score change", "continuous", "y_score"),
    sld_outcome   = define_sld_outcome("Score change", "mean", estimate = -6.5, se = 0.6)
  )
)

# Run one analysis: sequential + univariate scenarios, exported ------------------------------
run_one <- function(a) {
  validate_metadata(a$metadata)
  validate_sld(a$sld, a$metadata)
  validate_ipd(a$ipd, a$metadata)
  check_outcome_pair(a$outcome, a$sld_outcome)
  stopifnot(!a$sld_outcome$anchored)

  order <- if (is.null(a$adjust_order)) a$metadata$variable[a$metadata$adjust] else a$adjust_order
  run_scenarios <- function(scenarios) {
    run_maic_scenarios(scenarios, a$ipd, a$sld, a$outcome, a$sld_outcome,
                       arm = a$arm, intervention_arm = a$intervention_arm)
  }
  sequential <- run_scenarios(define_scenarios_sequential(a$metadata, order, include_empty = TRUE))
  univariate <- run_scenarios(define_scenarios_univariate(a$metadata, order))

  dir <- file.path(output_dir, slugify(a$analysis_name))
  export_scenarios(sequential, file.path(dir, "sequential"))
  export_scenarios(univariate, file.path(dir, "univariate"))

  tag <- function(results) {
    dplyr::bind_cols(
      tibble::tibble(analysis = a$analysis_name, population = a$population, comparator = a$comparator),
      results
    )
  }
  list(sequential = sequential, univariate = univariate,
       sequential_table = tag(sequential$results), univariate_table = tag(univariate$results))
}

runs <- lapply(analysis_inputs, run_one)

# Stack across analyses -------------------------------------------------------------------------
all_sequential <- dplyr::bind_rows(lapply(runs, `[[`, "sequential_table"))
all_univariate <- dplyr::bind_rows(lapply(runs, `[[`, "univariate_table"))
format_stacked <- function(stacked) {
  dplyr::bind_cols(stacked[c("analysis", "population", "comparator")], format_results(stacked))
}
write.csv(format_stacked(all_sequential), file.path(output_dir, "all_sequential.csv"), row.names = FALSE)
write.csv(format_stacked(all_univariate), file.path(output_dir, "all_univariate.csv"), row.names = FALSE)
saveRDS(runs, file.path(output_dir, "all_runs.rds"))

# Print -------------------------------------------------------------------------------------------
show_cols <- c("analysis", "Model", "Estimate (95% CI)", "N", "Weighting ESS", "ESS %", "Weight max", "Top 10% share")
cat("== Sequential scenarios, all analyses\n")
print(as.data.frame(format_stacked(all_sequential)[, show_cols]), right = FALSE)
cat("\n== Univariate scenarios, all analyses\n")
print(as.data.frame(format_stacked(all_univariate)[, show_cols]), right = FALSE)
cat("\n== Balance path, first analysis, sequential (first columns)\n")
path <- scenario_balance_path(runs[[1]]$sequential)
print(as.data.frame(path[, 1:7]), right = FALSE)
cat("\nOutputs written to", output_dir, "\n")
print(list.files(output_dir, recursive = TRUE))
