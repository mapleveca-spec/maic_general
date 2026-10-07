resp     <- define_outcome("Response", "binary", "y_resp")
os       <- define_outcome("OS", "tte", "y_time", event = "y_event")
resp_anc <- define_sld_outcome("Response", "log_or", estimate = log(1.5), se = 0.26)
resp_una <- sld_outcome_from_proportion("Response", p = 0.40, n = 100)
os_anc   <- define_sld_outcome("OS", "log_hr", estimate = log(0.8), se = 0.15)

anc <- run_maic_analysis(toy$ipd, toy$sld, toy$metadata, resp, resp_anc, arm = "ARM", reference_arm = "A")

test_that("anchored analysis: components, contrast, one-row result", {
  expect_true(all(c("weights", "diagnostics", "balance_before", "balance_after", "weight_fit",
                    "outcome", "sld_outcome", "arms", "fit", "result", "bootstrap") %in% names(anc)))
  expect_equal(nrow(anc$result), 1)
  expect_true(anc$result$anchored)
  expect_equal(anc$result$contrast, "B vs A")
  expect_equal(anc$arms$reference_arm, "A")
  expect_equal(anc$result$scale, "log_or")
  expect_equal(anc$result$n, anc$fit$n)
  expect_null(anc$bootstrap)
  expect_length(anc$weights, nrow(toy$ipd))
})

test_that("anchored analysis reproduces the manual chain", {
  w <- run_maic_weighting(toy$ipd, toy$sld, toy$metadata)
  expect_equal(anc$weights, w$weights)
  f <- fit_outcome_model(toy$ipd, resp, weights = w$weights, arm = "ARM", reference_arm = "A")
  expect_equal(anc$result$estimate, compare_to_sld(f, resp_anc)$estimate)
})

test_that("reference_arm flips the anchored sign; arm arguments are required", {
  b <- run_maic_analysis(toy$ipd, toy$sld, toy$metadata, resp, resp_anc, arm = "ARM", reference_arm = "B")
  expect_equal(b$result$ipd_estimate, -anc$result$ipd_estimate)
  expect_equal(b$result$contrast, "A vs B")
  expect_error(run_maic_analysis(toy$ipd, toy$sld, toy$metadata, resp, resp_anc),
               "needs both `arm` and `reference_arm`")
  expect_error(run_maic_analysis(toy$ipd, toy$sld, toy$metadata, resp, resp_anc, arm = "ARM"),
               "needs both `arm` and `reference_arm`")
})

test_that("unanchored on multi-arm IPD subsets to the intervention arm before weighting", {
  expect_error(run_maic_analysis(toy$ipd, toy$sld, toy$metadata, resp, resp_una, arm = "ARM"),
               "needs `intervention_arm`")
  u <- run_maic_analysis(toy$ipd, toy$sld, toy$metadata, resp, resp_una, arm = "ARM", intervention_arm = "A")
  n_a <- sum(toy$ipd$ARM == "A")
  expect_length(u$weights, n_a)
  expect_equal(u$diagnostics$n_ipd, n_a)
  expect_equal(u$result$contrast, "A (unanchored)")
  expect_false(u$result$anchored)
  expect_equal(u$result$scale, "log_or")
  # identical to running on the pre-subset IPD with no arm
  direct <- run_maic_analysis(toy$ipd[toy$ipd$ARM == "A", ], toy$sld, toy$metadata, resp, resp_una)
  expect_equal(u$result$estimate, direct$result$estimate)
  expect_equal(direct$result$contrast, "IPD (unanchored)")
})

test_that("unanchored on single-arm IPD needs no arm arguments", {
  single <- toy$ipd[toy$ipd$ARM == "B", ]
  u <- run_maic_analysis(single, toy$sld, toy$metadata, resp, resp_una)
  expect_equal(nrow(u$result), 1)
  expect_null(u$arms$arm)
})

test_that("mismatched outcome pair fails before any work", {
  expect_error(run_maic_analysis(toy$ipd, toy$sld, toy$metadata, resp, os_anc, arm = "ARM", reference_arm = "A"),
               "Outcome mismatch")
  expect_error(run_maic_analysis(toy$ipd, toy$sld, toy$metadata, os, define_sld_outcome("OS", "mean", 0, se = 1)),
               "Unanchored tte")
})

test_that("include_adjust and weighting options pass through", {
  full <- run_maic_analysis(toy$ipd, toy$sld, toy$metadata, os, os_anc, arm = "ARM", reference_arm = "A",
                            include_adjust = TRUE)
  expect_true(full$include_adjust)
  expect_gt(nrow(full$targets), nrow(anc$targets))
  expect_error(run_maic_analysis(toy$ipd, toy$sld, toy$metadata, os, os_anc, arm = "ARM", reference_arm = "A",
                                 include_adjust = TRUE, na_action = "error"), "NA in matched variable")
})

test_that("bootstrap columns join the one-row result when requested", {
  r <- run_maic_analysis(toy$ipd, toy$sld, toy$metadata, resp, resp_anc, arm = "ARM", reference_arm = "A",
                         n_boot = 20, seed = 2, max_fail_rate = 1)
  expect_true(all(c("boot_ci_low", "boot_ci_high", "n_boot_ok", "n_extreme") %in% names(r$result)))
  expect_equal(r$result$estimate, anc$result$estimate)
  expect_equal(nrow(r$bootstrap$summary), 1)
})
