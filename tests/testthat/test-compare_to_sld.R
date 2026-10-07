res  <- run_maic_weighting(toy$ipd, toy$sld, toy$metadata)
w    <- res$weights
resp <- define_outcome("Response", "binary", "y_resp")
os   <- define_outcome("OS", "tte", "y_time", event = "y_event")

# define_sld_outcome -----------------------------------------------------------

test_that("se is taken directly or derived from a CI; anchored follows from the scale", {
  a <- define_sld_outcome("Response", "log_or", estimate = 0.5, se = 0.2)
  z <- qnorm(0.975)
  b <- define_sld_outcome("Response", "log_or", estimate = 0.5, ci_low = 0.5 - z * 0.2, ci_high = 0.5 + z * 0.2)
  expect_equal(a$se, b$se, tolerance = 1e-4)
  expect_s3_class(a, "maic_sld_outcome")
  expect_true(a$anchored)
  expect_false(define_sld_outcome("Response", "logit_p", 0, se = 1)$anchored)
  expect_false(define_sld_outcome("x", "mean", 0, se = 1)$anchored)
  expect_true(define_sld_outcome("x", "mean_diff", 0, se = 1)$anchored)
})

test_that("spec validation", {
  expect_error(define_sld_outcome("x", "odds", 1, se = 1), "must be one of")
  expect_error(define_sld_outcome("x", "log_or", 1), "Give `se`")
  expect_error(define_sld_outcome("x", "log_or", 1, ci_low = 2, ci_high = 1), "must not exceed")
  expect_error(define_sld_outcome("x", "log_or", 1, se = 0), "positive")
})

test_that("proportion helper gives logit and delta-method se", {
  s <- sld_outcome_from_proportion("Response", p = 0.4, n = 100)
  expect_equal(s$scale, "logit_p")
  expect_equal(s$estimate, qlogis(0.4))
  expect_equal(s$se, 1 / sqrt(100 * 0.4 * 0.6))
})

# check_outcome_pair -----------------------------------------------------------

test_that("check_outcome_pair enforces name, scale, and the tte rule", {
  expect_true(check_outcome_pair(resp, define_sld_outcome("Response", "log_or", 0, se = 1)))
  expect_true(check_outcome_pair(resp, define_sld_outcome("Response", "logit_p", 0, se = 1)))
  expect_error(check_outcome_pair(resp, define_sld_outcome("OS", "log_or", 0, se = 1)), "Outcome mismatch")
  expect_error(check_outcome_pair(resp, define_sld_outcome("Response", "log_hr", 0, se = 1)),
               "`Response` \\(anchored\\) must be on scale `log_or`, got `log_hr`")
  expect_error(check_outcome_pair(resp, define_sld_outcome("Response", "mean", 0, se = 1)),
               "\\(unanchored\\) must be on scale `logit_p`")
  expect_error(check_outcome_pair(os, define_sld_outcome("OS", "mean", 0, se = 1)), "Unanchored tte")
})

# anchored (Bucher) ------------------------------------------------------------

test_that("anchored comparison is the Bucher difference with summed variances", {
  f <- fit_outcome_model(toy$ipd, resp, weights = w, arm = "ARM", reference_arm = "A")
  s <- define_sld_outcome("Response", "log_or", estimate = 0.3, se = 0.25)
  out <- compare_to_sld(f, s)
  expect_equal(nrow(out), 1)
  expect_true(out$anchored)
  expect_equal(out$contrast, "B vs A")
  expect_equal(out$scale, "log_or")
  expect_equal(out$estimate, f$estimate$estimate - 0.3)
  expect_equal(out$se, sqrt(f$estimate$se^2 + 0.25^2))
  expect_equal(out$ci_high - out$estimate, qnorm(0.975) * out$se)
})

test_that("anchored tte on log_hr scale", {
  f <- fit_outcome_model(toy$ipd, os, weights = w, arm = "ARM", reference_arm = "A")
  s <- define_sld_outcome("OS", "log_hr", estimate = log(0.8), ci_low = log(0.6), ci_high = log(1.07))
  out <- compare_to_sld(f, s)
  expect_equal(out$scale, "log_hr")
  expect_equal(out$ipd_estimate, f$estimate$estimate)
  expect_equal(out$sld_estimate, log(0.8))
})

# unanchored -------------------------------------------------------------------

test_that("unanchored binary gives a log odds ratio from two logits; contrast can be relabelled", {
  f <- fit_outcome_model(toy$ipd, resp, weights = w)
  s <- sld_outcome_from_proportion("Response", p = 0.4, n = 100)
  out <- compare_to_sld(f, s, contrast = "A (unanchored)")
  expect_false(out$anchored)
  expect_equal(out$scale, "log_or")
  expect_equal(out$contrast, "A (unanchored)")
  p_ipd <- plogis(f$estimate$estimate)
  expect_equal(exp(out$estimate), (p_ipd / (1 - p_ipd)) / (0.4 / 0.6))
})

test_that("unanchored continuous gives a mean difference", {
  f <- fit_outcome_model(toy$ipd, define_outcome("Age as outcome", "continuous", "fac_age"), weights = w)
  s <- define_sld_outcome("Age as outcome", "mean", estimate = 48, se = 1)
  out <- compare_to_sld(f, s)
  expect_equal(out$scale, "mean_diff")
  expect_equal(out$estimate, 50 - 48, tolerance = 1e-4)
})

# guards -----------------------------------------------------------------------

test_that("outcome and scale mismatches are refused", {
  f <- fit_outcome_model(toy$ipd, resp, weights = w, arm = "ARM", reference_arm = "A")
  expect_error(compare_to_sld(f, define_sld_outcome("OS", "log_or", 0, se = 1)), "Outcome mismatch")
  expect_error(compare_to_sld(f, define_sld_outcome("Response", "log_hr", 0, se = 1)), "Scale mismatch")
  fu <- fit_outcome_model(toy$ipd, resp, weights = w)
  expect_error(compare_to_sld(fu, define_sld_outcome("Response", "log_or", 0, se = 1)), "Scale mismatch")
})

test_that("conf_level is honoured", {
  f <- fit_outcome_model(toy$ipd, resp, weights = w, arm = "ARM", reference_arm = "A")
  s <- define_sld_outcome("Response", "log_or", 0.3, se = 0.25)
  out <- compare_to_sld(f, s, conf_level = 0.8)
  expect_equal(out$ci_high - out$estimate, qnorm(0.9) * out$se)
})
