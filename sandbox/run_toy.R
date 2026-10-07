# Runs the full MAIC on the toy data and prints the key outputs.
# Execute from the framework root:  Rscript sandbox/run_toy.R

source("08_main/source_framework.R")
source_framework(".")

toy <- readRDS("sandbox/toy_data.rds")

# Set to e.g. 500 for bootstrap intervals (a couple of minutes per outcome).
n_boot <- 0

res <- run_maic_outcomes(
  ipd = toy$ipd, sld = toy$sld, metadata = toy$metadata,
  outcomes = toy$outcomes, sld_outcomes = toy$sld_outcomes,
  arm = "ARM", reference_arm = "A", intervention_arm = "A",
  include_adjust = FALSE,   # primary weighting set (`match`); TRUE adds the `adjust` tier
  n_boot = n_boot, seed = 2026
)

one <- res$analyses[["Response [anchored]"]]

cat("== Match targets (anchored analyses use the full IPD)\n")
print(as.data.frame(one$targets))

cat("\n== Weight diagnostics\n")
print(as.data.frame(one$diagnostics), digits = 3)

cat("\n== Balance before vs after weighting\n")
print(as.data.frame(format_balance_comparison(compare_balance_tables(one$balance_before, one$balance_after))),
      right = FALSE)

cat("\n== Indirect comparisons, one independent analysis per published result\n")
print(as.data.frame(format_results(res$summary)), right = FALSE)
