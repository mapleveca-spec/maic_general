# New study template -----------------------------------------------------------
#
# Copy this file and the four CSV templates into your study folder, replace the
# contents, and run top to bottom. Every step is annotated with what the input
# must look like. The templates are filled with the toy data, so this script
# runs as is from the framework root:   Rscript templates/new_study_template.R
#
# Inputs you prepare:
#   1. ipd            one row per patient (CSV, SAS, whatever you have)
#   2. metadata       one row per covariate, from metadata_template.csv
#   3. sld            the comparator's baseline table in long format
#   4. outcomes       which IPD columns are outcomes
#   5. sld_outcomes   the comparator's published results, one row per comparison
#
# Each outcome is analysed independently: run_maic_analysis() does one
# outcome; run_maic_outcomes() repeats it over the published-results table
# with identical specifications and keeps the analyses separate.

framework_root <- "."            # path to the maic_general folder
study_dir      <- "templates"    # path to your study's input files
output_dir     <- file.path(tempdir(), "maic_output")   # change to your results folder
dir.create(output_dir, showWarnings = FALSE)

source(file.path(framework_root, "08_main", "source_framework.R"))
source_framework(framework_root)

# 1. IPD -------------------------------------------------------------------------
# Requirements: continuous covariates numeric; categorical covariates factor or
# character with values exactly as they appear in metadata$level_order;
# missing values coded NA (never the string "Missing"); an arm column with two
# levels if any comparison is anchored; outcome columns as named in `outcomes`.
ipd <- read.csv(file.path(study_dir, "ipd_example.csv"), stringsAsFactors = FALSE, na.strings = c("", "NA"))

# Recode here if your source uses different labels than level_order, for
# example numeric severity codes 1/2/3 to Mild/Moderate/Severe.

# 2. Metadata --------------------------------------------------------------------
# One row per covariate. level_order is "Level1|Level2|..." in the order you
# want displayed, and must list every level that appears in the IPD or the
# comparator's table. Tiers: match = effect modifiers (always weighted on);
# adjust = prognostic variables (weighted on with include_adjust = TRUE).
metadata <- read_metadata_csv(file.path(study_dir, "metadata_template.csv"))

# 3. SLD (comparator baseline table) --------------------------------------------
# Long format, one row per variable and level, study N on every row.
#   "Age, mean (SD): 50.0 (10.0)"      -> var_level = NA,  sld_est = 50,  sld_sd = 10
#   "Severe, n (%): 65 (65%)"          -> var_level = "Severe", sld_est = 0.65, sld_sd blank
#   "Missing: 10 (10%)"                -> var_level = "Missing", sld_est = 0.10, sld_sd blank
#   variable not reported              -> one row, var_level = NA, sld_est = NA, sld_sd = NA
# sld_sd is only needed for continuous summary rows; the framework derives
# sqrt(p(1-p)) for every proportion row. If the paper gives counts,
# sld_est = count / N. If it gives a median and range for a continuous
# variable, convert to mean and SD before entry and record the method in your
# analysis plan.
sld <- read.csv(file.path(study_dir, "sld_template.csv"), stringsAsFactors = FALSE, na.strings = c("", "NA"))

# 4. Outcomes ----------------------------------------------------------------------
# name, type (binary / continuous / tte), var, event (tte only, else blank).
outcomes <- read.csv(file.path(study_dir, "outcomes_template.csv"), stringsAsFactors = FALSE, na.strings = c("", "NA"))

# 5. Published results -------------------------------------------------------------
# One row per comparison, on the analysis scale. `anchored` says whether the
# published number is a contrast vs the common comparator (TRUE) or an
# absolute outcome in the comparator arm (FALSE). Typical conversions:
#   anchored HR 0.80 (0.60, 1.07)  -> scale log_hr,  estimate log(0.80), se se_from_ci(log(0.60), log(1.07))
#   anchored OR 1.50 (0.90, 2.50)  -> scale log_or,  estimate log(1.50), se se_from_ci(log(0.90), log(2.50))
#   anchored mean diff -2.0 (SE .9)-> scale mean_diff, estimate -2.0, se 0.9
#   unanchored response 40/100     -> scale logit_p, estimate qlogis(.40), se 1/sqrt(100*.4*.6)
#   unanchored mean 6.5 (SD 6, N 100) -> scale mean, estimate 6.5, se 6/sqrt(100)
sld_outcomes <- read.csv(file.path(study_dir, "sld_outcomes_template.csv"), stringsAsFactors = FALSE)

# Validate everything first so all input problems surface together ---------------
validate_metadata(metadata)
validate_sld(sld, metadata)
validate_ipd(ipd, metadata)
validate_outcomes(outcomes)
validate_sld_outcomes(sld_outcomes, outcomes)

# Arms -------------------------------------------------------------------------------
# arm:              IPD column with treatment assignment (NULL for single-arm IPD)
# reference_arm:    the common comparator, used by anchored comparisons; the
#                   IPD estimate is "other arm vs reference_arm"
# intervention_arm: the arm compared in unanchored comparisons; the IPD is
#                   subset to it before weighting
arm              <- "ARM"
reference_arm    <- "A"
intervention_arm <- "A"

# Primary analyses: weight on effect modifiers only, one analysis per outcome -------
primary <- run_maic_outcomes(
  ipd, sld, metadata, outcomes, sld_outcomes,
  arm = arm, reference_arm = reference_arm, intervention_arm = intervention_arm,
  include_adjust = FALSE,
  n_boot = 0, seed = 2026          # e.g. n_boot = 1000 for the final run
)

# Full specification: effect modifiers plus prognostic variables ------------------
full <- run_maic_outcomes(
  ipd, sld, metadata, outcomes, sld_outcomes,
  arm = arm, reference_arm = reference_arm, intervention_arm = intervention_arm,
  include_adjust = TRUE,
  n_boot = 0, seed = 2026
)

# One outcome in depth: its own balance tables, weight diagnostics, weight plot ---------
os <- primary$analyses[["OS [anchored]"]]
balance_os <- format_balance_comparison(compare_balance_tables(os$balance_before, os$balance_after))
ggplot2::ggsave(file.path(output_dir, "weights_OS_anchored.png"), plot_weights(os$weight_fit),
                width = 7, height = 4.5, dpi = 150)

# Sensitivity for one outcome: add prognostic variables one at a time -----------------
# The order of this vector is the order variables enter the weighting set.
# Default: adjust-tier variables in metadata row order. For a prespecified
# order, write the vector out, e.g. c("fac_egfr", "fac_bmi", "fac_prior_treatment").
# Only adjust-tier variables are allowed; variables left out are excluded
# from every scenario.
prognostic_order <- metadata$variable[metadata$adjust]
sequential <- run_maic_scenarios(
  define_scenarios_sequential(metadata, prognostic_order, include_empty = TRUE),
  ipd, sld, os$outcome, os$sld_outcome, arm = arm, reference_arm = reference_arm
)
univariate <- run_maic_scenarios(
  define_scenarios_univariate(metadata, prognostic_order),
  ipd, sld, os$outcome, os$sld_outcome, arm = arm, reference_arm = reference_arm
)

# Tables -----------------------------------------------------------------------------
results_primary <- format_results(primary$summary)
results_full    <- format_results(full$summary)
sequential_tab  <- format_results(sequential$results)
univariate_tab  <- format_results(univariate$results)
diagnostics     <- dplyr::bind_rows(
  lapply(names(primary$analyses), function(k) cbind(analysis = k, primary$analyses[[k]]$diagnostics))
)

write.csv(balance_os,      file.path(output_dir, "balance_OS_anchored.csv"), row.names = FALSE)
write.csv(results_primary, file.path(output_dir, "results_primary.csv"),     row.names = FALSE)
write.csv(results_full,    file.path(output_dir, "results_full.csv"),        row.names = FALSE)
write.csv(sequential_tab,  file.path(output_dir, "sequential_OS.csv"),       row.names = FALSE)
write.csv(univariate_tab,  file.path(output_dir, "univariate_OS.csv"),       row.names = FALSE)
write.csv(diagnostics,     file.path(output_dir, "weight_diagnostics.csv"),  row.names = FALSE)
saveRDS(list(primary = primary, full = full, sequential = sequential, univariate = univariate),
        file.path(output_dir, "maic_results.rds"))

cat("== Weight diagnostics per analysis (primary)\n")
print(as.data.frame(diagnostics[, c("analysis", "n_ipd", "n_complete", "ess", "ess_pct", "w_max")]), digits = 3)
cat("\n== Results, primary\n")
print(as.data.frame(results_primary), right = FALSE)
cat("\n== Results, full\n")
print(as.data.frame(results_full), right = FALSE)
cat("\n== Sequential, OS\n")
print(as.data.frame(sequential_tab), right = FALSE)
cat("\nOutputs written to", output_dir, "\n")
