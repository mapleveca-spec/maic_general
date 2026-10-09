res <- run_maic_weighting(toy$ipd, toy$sld, toy$metadata)
w   <- res$weights

resp <- define_outcome("Response", "binary", "y_resp")
os   <- define_outcome("OS", "tte", "y_time", event = "y_event")
age  <- define_outcome("Age as outcome", "continuous", "fac_age")

# define_outcome ---------------------------------------------------------------

test_that("define_outcome validates its spec", {
  expect_s3_class(resp, "maic_outcome")
  expect_error(define_outcome("x", "ordinal", "y"), "must be one of")
  expect_error(define_outcome("x", "tte", "t"), "needs an `event`")
  expect_error(define_outcome("x", "binary", "y", event = "e"), "only used for tte")
  expect_equal(outcome_columns(os), c("y_time", "y_event"))
})

# anchored, unit weights: must reproduce the plain arm-only models -------------

test_that("anchored binary with unit weights equals an unweighted arm-only glm", {
  f <- fit_outcome_model(toy$ipd, resp, arm = "ARM", reference_arm = "A")
  ref <- glm(y_resp ~ ARM, data = toy$ipd, family = binomial())
  expect_equal(f$estimate$estimate, unname(coef(ref)["ARMB"]))
  expect_equal(f$estimate$term, "ARMB")
  expect_equal(f$contrast, "B vs A")
  expect_equal(f$estimate$scale, "log_or")
  expect_equal(f$n, nrow(toy$ipd))
  expect_equal(f$ess, nrow(toy$ipd))
  expect_equal(length(coef(f$model)), 2)  # intercept + arm, no covariates
})

test_that("anchored continuous with unit weights equals lm", {
  score <- define_outcome("Score change", "continuous", "y_score")
  f <- fit_outcome_model(toy$ipd, score, arm = "ARM", reference_arm = "A")
  ref <- lm(y_score ~ ARM, data = toy$ipd)
  expect_equal(f$estimate$estimate, unname(coef(ref)["ARMB"]))
  expect_equal(f$estimate$scale, "mean_diff")
})

test_that("anchored tte with unit weights equals coxph", {
  f <- fit_outcome_model(toy$ipd, os, arm = "ARM", reference_arm = "A")
  ref <- survival::coxph(survival::Surv(y_time, y_event) ~ ARM, data = toy$ipd)
  expect_equal(f$estimate$estimate, unname(coef(ref)["ARMB"]))
  expect_equal(f$estimate$scale, "log_hr")
})

test_that("reference_arm is required with arm and flips the sign of the contrast", {
  expect_error(fit_outcome_model(toy$ipd, resp, arm = "ARM"), "`reference_arm` is required")
  a <- fit_outcome_model(toy$ipd, resp, arm = "ARM", reference_arm = "A")
  b <- fit_outcome_model(toy$ipd, resp, arm = "ARM", reference_arm = "B")
  expect_equal(a$estimate$estimate, -b$estimate$estimate)
  expect_equal(b$estimate$term, "ARMA")
  expect_equal(b$contrast, "A vs B")
})

test_that("every outcome type in the toy IPD is fittable anchored", {
  specs <- list(resp, os, define_outcome("Score change", "continuous", "y_score"))
  expect_setequal(vapply(specs, `[[`, "", "type"), OUTCOME_TYPES)
  for (spec in specs) {
    f <- fit_outcome_model(toy$ipd, spec, weights = w, arm = "ARM", reference_arm = "A")
    expect_true(is.finite(f$estimate$estimate), label = spec$name)
  }
})

# unanchored -------------------------------------------------------------------

test_that("unanchored binary is the logit of the weighted proportion", {
  f <- fit_outcome_model(toy$ipd, resp, weights = w)
  p <- sum(w * toy$ipd$y_resp) / sum(w)
  expect_equal(plogis(f$estimate$estimate), p)
  expect_equal(f$estimate$scale, "logit_p")
  expect_equal(f$contrast, "IPD (unanchored)")
  expect_equal(f$n, sum(w > 0))
})

test_that("unanchored continuous is the weighted mean", {
  f <- fit_outcome_model(toy$ipd, age, weights = w)
  expect_equal(f$estimate$estimate, 50, tolerance = 1e-4)  # fac_age was matched to 50
  expect_equal(f$estimate$scale, "mean")
})

test_that("unanchored tte is refused", {
  expect_error(fit_outcome_model(toy$ipd, os, weights = w), "pseudo-IPD")
})

# weights ----------------------------------------------------------------------

test_that("estimates and SEs are invariant to the scale of the weights", {
  a <- fit_outcome_model(toy$ipd, resp, weights = w, arm = "ARM", reference_arm = "A")
  b <- fit_outcome_model(toy$ipd, resp, weights = 7 * w, arm = "ARM", reference_arm = "A")
  expect_equal(a$estimate, b$estimate)
  c1 <- fit_outcome_model(toy$ipd, os, weights = w, arm = "ARM", reference_arm = "A")
  c2 <- fit_outcome_model(toy$ipd, os, weights = 7 * w, arm = "ARM", reference_arm = "A")
  expect_equal(c1$estimate, c2$estimate)
})

test_that("zero-weight rows are dropped and ess matches the weights used", {
  w0 <- w
  w0[1:5] <- 0
  f <- fit_outcome_model(toy$ipd, resp, weights = w0, arm = "ARM", reference_arm = "A")
  expect_equal(f$n, sum(w0 > 0))
  expect_equal(f$ess, effective_n(w0[w0 > 0]), tolerance = 1e-8)
})

test_that("CI is symmetric around the estimate at the requested level", {
  f <- fit_outcome_model(toy$ipd, resp, weights = w, arm = "ARM", reference_arm = "A", conf_level = 0.9)
  e <- f$estimate
  expect_equal(e$ci_high - e$estimate, e$estimate - e$ci_low)
  expect_equal(e$ci_high - e$estimate, qnorm(0.95) * e$se)
})

# errors -----------------------------------------------------------------------

test_that("missing columns and bad arms are reported", {
  expect_error(fit_outcome_model(toy$ipd, define_outcome("x", "binary", "nope")), "absent from IPD: nope")
  expect_error(fit_outcome_model(toy$ipd, resp, arm = "ARM", reference_arm = "Z"), "not an arm level")
  d <- toy$ipd
  d$ARM <- factor(rep(c("A", "B", "C"), length.out = nrow(d)))
  expect_error(fit_outcome_model(d, resp, arm = "ARM", reference_arm = "A"), "exactly two levels")
})
