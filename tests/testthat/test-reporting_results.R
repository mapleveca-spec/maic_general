out <- run_maic_outcomes(toy$ipd, toy$sld, toy$metadata, toy$outcomes, toy$sld_outcomes,
                         arm = "ARM", reference_arm = "A", intervention_arm = "A")
results <- out$summary

# back_transform_results -------------------------------------------------------

test_that("log scales are exponentiated, mean differences are not", {
  bt <- back_transform_results(results)
  is_log <- bt$scale %in% c("log_or", "log_hr")
  expect_equal(bt$est[is_log], exp(bt$estimate[is_log]))
  expect_equal(bt$lo[is_log], exp(bt$ci_low[is_log]))
  expect_equal(bt$est[!is_log], bt$estimate[!is_log])
  expect_equal(bt$measure[bt$scale == "log_or"], rep("Odds ratio", sum(bt$scale == "log_or")))
  expect_equal(bt$measure[bt$scale == "mean_diff"], rep("Mean difference", sum(bt$scale == "mean_diff")))
  expect_false("boot_lo" %in% names(bt))
})

test_that("unknown scales are refused", {
  r <- results
  r$scale[1] <- "logit_p"
  expect_error(back_transform_results(r), "Unknown result scale.*logit_p")
})

# format_results ---------------------------------------------------------------

test_that("one display row per result with the expected columns, including the contrast", {
  tab <- format_results(results)
  expect_named(tab, c("Outcome", "Comparison", "Contrast", "Measure", "Estimate (95% CI)", "N", "ESS"))
  expect_equal(nrow(tab), nrow(results))
  expect_equal(tab$Comparison, ifelse(results$anchored, "Anchored", "Unanchored"))
  expect_equal(tab$Contrast, results$contrast)
  expect_match(tab$`Estimate (95% CI)`, "^-?[0-9]+\\.[0-9]{2} \\(-?[0-9]+\\.[0-9]{2}, -?[0-9]+\\.[0-9]{2}\\)$")
})

test_that("format_results works on a single analysis result too", {
  one <- out$analyses[["OS [anchored]"]]$result
  tab <- format_results(one, digits = 3)
  expect_equal(nrow(tab), 1)
  expect_equal(tab$`Estimate (95% CI)`,
               sprintf("%.3f (%.3f, %.3f)", exp(one$estimate), exp(one$ci_low), exp(one$ci_high)))
})

test_that("bootstrap columns appear only when present", {
  resp <- define_outcome("Response", "binary", "y_resp")
  rb <- run_maic_analysis(toy$ipd, toy$sld, toy$metadata, resp, define_sld_outcome("Response", "log_or", 0.4, se = .26),
                          arm = "ARM", reference_arm = "A", n_boot = 10, seed = 5, max_fail_rate = 1)
  tab <- format_results(rb$result)
  expect_true("Bootstrap 95% CI" %in% names(tab))
  expect_match(tab$`Bootstrap 95% CI`[1], "^\\(")
  expect_false("Bootstrap 95% CI" %in% names(format_results(results)))
})

test_that("scenario results carry Model, Variables, and Weighting ESS", {
  os <- define_outcome("OS", "tte", "y_time", event = "y_event")
  sc <- define_scenarios_sequential(toy$metadata, c("fac_prior_treatment", "fac_bmi"))
  o  <- run_maic_scenarios(sc, toy$ipd, toy$sld, os, define_sld_outcome("OS", "log_hr", log(.8), se = .15),
                           arm = "ARM", reference_arm = "A")
  tab <- format_results(o$results)
  expect_equal(names(tab)[1:2], c("Model", "Variables"))
  expect_equal(names(tab)[length(tab)], "Weighting ESS")
  expect_equal(tab$Model, c("+ fac_prior_treatment", "+ fac_bmi"))
  expect_equal(nrow(tab), 2)
})

test_that("format_estimate_ci handles NA", {
  expect_equal(format_estimate_ci(c(1.5, NA), c(1, NA), c(2.25, NA)), c("1.50 (1.00, 2.25)", "NR"))
})
