resp     <- define_outcome("Response", "binary", "y_resp")
os       <- define_outcome("OS", "tte", "y_time", event = "y_event")
resp_anc <- define_sld_outcome("Response", "log_or", estimate = log(1.5), se = 0.26)
resp_una <- sld_outcome_from_proportion("Response", p = 0.40, n = 100)
os_anc   <- define_sld_outcome("OS", "log_hr", estimate = log(0.8), se = 0.15)
arm_a    <- toy$ipd[toy$ipd$ARM == "A", setdiff(names(toy$ipd), "ARM")]   # cleaned single-arm IPD

# check_anchored_arms ----------------------------------------------------------

test_that("anchored arm rules: both arguments, two levels, reference present", {
  expect_equal(check_anchored_arms(toy$ipd, "ARM", "A"), "B vs A")
  expect_equal(check_anchored_arms(toy$ipd, "ARM", "B"), "A vs B")
  expect_error(check_anchored_arms(toy$ipd, NULL, "A"), "needs both")
  expect_error(check_anchored_arms(toy$ipd, "ARM", NULL), "needs both")
  expect_error(check_anchored_arms(toy$ipd, "nope", "A"), "column `nope` not found")
  expect_error(check_anchored_arms(toy$ipd, "ARM", "Z"), "not an arm level \\(A, B\\)")
  d <- toy$ipd
  d$ARM <- factor(rep(c("A", "B", "C"), length.out = nrow(d)))
  expect_error(check_anchored_arms(d, "ARM", "A"), "exactly two levels.*A, B, C")
})

# run_maic_anchored ------------------------------------------------------------

anc <- run_maic_anchored(toy$ipd, toy$sld, toy$metadata, resp, resp_anc, arm = "ARM", reference_arm = "A")

test_that("anchored: components, contrast, one-row result, primary set by default", {
  expect_equal(anc$kind, "anchored")
  expect_true(all(c("weights", "diagnostics", "balance_before", "balance_after", "weight_fit",
                    "outcome", "sld_outcome", "contrast", "fit", "result", "bootstrap") %in% names(anc)))
  expect_equal(nrow(anc$result), 1)
  expect_true(anc$result$anchored)
  expect_equal(anc$result$contrast, "B vs A")
  expect_false(anc$include_adjust)
  expect_null(anc$bootstrap)
  w <- run_maic_weighting(toy$ipd, toy$sld, toy$metadata)
  expect_equal(anc$weights, w$weights)
  f <- fit_outcome_model(toy$ipd, resp, weights = w$weights, arm = "ARM", reference_arm = "A")
  expect_equal(anc$result$estimate, compare_to_sld(f, resp_anc)$estimate)
})

test_that("anchored: reference_arm flips the sign; arms are required; wrong kind is refused", {
  b <- run_maic_anchored(toy$ipd, toy$sld, toy$metadata, resp, resp_anc, arm = "ARM", reference_arm = "B")
  expect_equal(b$result$ipd_estimate, -anc$result$ipd_estimate)
  expect_equal(b$result$contrast, "A vs B")
  expect_error(run_maic_anchored(toy$ipd, toy$sld, toy$metadata, resp, resp_anc, arm = "ARM", reference_arm = NULL),
               "needs both")
  expect_error(run_maic_anchored(toy$ipd, toy$sld, toy$metadata, resp, resp_una, arm = "ARM", reference_arm = "A"),
               "unanchored absolute outcome.*use run_maic_unanchored")
  expect_error(run_maic_anchored(toy$ipd, toy$sld, toy$metadata, resp, os_anc, arm = "ARM", reference_arm = "A"),
               "Outcome mismatch")
})

# run_maic_unanchored ----------------------------------------------------------

una <- run_maic_unanchored(arm_a, toy$sld, toy$metadata, resp, resp_una)

test_that("unanchored: no arm concept, IPD used as supplied, full weighting set by default", {
  expect_equal(una$kind, "unanchored")
  expect_false(una$result$anchored)
  expect_equal(una$result$contrast, UNANCHORED_CONTRAST)
  expect_true(una$include_adjust)
  expect_length(una$weights, nrow(arm_a))
  expect_equal(una$diagnostics$n_ipd, nrow(arm_a))
  expect_equal(una$result$scale, "log_or")
  expect_equal(length(coef(una$fit$model)), 1)   # intercept only
})

test_that("unanchored: an IPD with an arm column is used whole, nothing is filtered", {
  both_arms <- run_maic_unanchored(toy$ipd, toy$sld, toy$metadata, resp, resp_una)
  expect_length(both_arms$weights, nrow(toy$ipd))
  expect_false("arm" %in% names(formals(run_maic_unanchored)))
})

test_that("unanchored: wrong kind and mismatches are refused", {
  expect_error(run_maic_unanchored(arm_a, toy$sld, toy$metadata, resp, resp_anc),
               "anchored contrast.*use run_maic_anchored")
  expect_error(run_maic_unanchored(arm_a, toy$sld, toy$metadata, os, define_sld_outcome("OS", "mean", 0, se = 1)),
               "Unanchored tte")
})

# shared options ---------------------------------------------------------------

test_that("include_adjust and weighting options pass through", {
  full <- run_maic_anchored(toy$ipd, toy$sld, toy$metadata, os, os_anc, arm = "ARM", reference_arm = "A",
                            include_adjust = TRUE)
  expect_true(full$include_adjust)
  expect_gt(nrow(full$targets), nrow(anc$targets))
  expect_error(run_maic_anchored(toy$ipd, toy$sld, toy$metadata, os, os_anc, arm = "ARM", reference_arm = "A",
                                 include_adjust = TRUE, na_action = "error"), "NA in matched variable")
  primary_una <- run_maic_unanchored(arm_a, toy$sld, toy$metadata, resp, resp_una, include_adjust = FALSE)
  expect_lt(nrow(primary_una$targets), nrow(una$targets))
})

test_that("bootstrap columns join the one-row result when requested", {
  r <- run_maic_anchored(toy$ipd, toy$sld, toy$metadata, resp, resp_anc, arm = "ARM", reference_arm = "A",
                         n_boot = 20, seed = 2, max_fail_rate = 1)
  expect_true(all(c("boot_ci_low", "boot_ci_high", "n_boot_ok", "n_extreme") %in% names(r$result)))
  expect_equal(r$result$estimate, anc$result$estimate)
  u <- run_maic_unanchored(arm_a, toy$sld, toy$metadata, resp, resp_una, n_boot = 20, seed = 2, max_fail_rate = 1)
  expect_equal(nrow(u$bootstrap$summary), 1)
})
