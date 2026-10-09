# Runs one anchored and one unanchored MAIC on the toy data and prints the key
# outputs. Execute from the framework root:  Rscript sandbox/run_toy.R

source("08_main/source_framework.R")
source_framework(".")

toy <- readRDS("sandbox/toy_data.rds")

# Set to e.g. 500 for bootstrap intervals (a couple of minutes per analysis).
n_boot <- 0

# Anchored: two-arm IPD, published odds ratio vs the common comparator.
anchored <- run_maic_anchored(
  toy$ipd, toy$sld, toy$metadata,
  outcome     = define_outcome("Response", "binary", "y_resp"),
  sld_outcome = define_sld_outcome("Response", "log_or", estimate = log(1.5), ci_low = log(0.9), ci_high = log(2.5)),
  arm = "ARM", reference_arm = "A",
  n_boot = n_boot, seed = 2026
)

# Unanchored: single-arm IPD (arm A, arm column dropped), published response rate.
arm_a <- toy$ipd[toy$ipd$ARM == "A", setdiff(names(toy$ipd), "ARM")]
meta_una <- toy$metadata
meta_una$match    <- FALSE
meta_una$adjust   <- meta_una$variable %in% c("fac_sev_hb", "fac_tar_jnt_lead", "fac_age", "fac_bmi")
meta_una$match_sd <- meta_una$variable == "fac_age"
unanchored <- run_maic_unanchored(
  arm_a, toy$sld, meta_una,
  outcome     = define_outcome("Response", "binary", "y_resp"),
  sld_outcome = sld_outcome_from_proportion("Response", p = 0.40, n = 100),
  n_boot = n_boot, seed = 2026
)

cat("== Anchored: match targets\n")
print(as.data.frame(anchored$targets))
cat("\n== Anchored: weight diagnostics\n")
print(as.data.frame(anchored$diagnostics), digits = 3)
cat("\n== Anchored: balance before vs after weighting\n")
print(as.data.frame(format_balance_comparison(compare_balance_tables(anchored$balance_before, anchored$balance_after))),
      right = FALSE)
cat("\n== Results\n")
print(as.data.frame(format_results(dplyr::bind_rows(anchored$result, unanchored$result))), right = FALSE)

ggplot2::ggsave("sandbox/weights_toy.png", plot_weights(anchored$weight_fit), width = 7, height = 4.5, dpi = 110)
cat("\nWeight histogram written to sandbox/weights_toy.png\n")
