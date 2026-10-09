resp     <- define_outcome("Response", "binary", "y_resp")
resp_anc <- define_sld_outcome("Response", "log_or", estimate = log(1.5), se = 0.26)
resp_una <- sld_outcome_from_proportion("Response", p = 0.40, n = 100)
arm_a    <- toy$ipd[toy$ipd$ARM == "A", setdiff(names(toy$ipd), "ARM")]
order_adj <- c("fac_prior_treatment", "fac_bmi", "fac_egfr")
seq_adj  <- define_scenarios_sequential(toy$metadata, order_adj, include_empty = TRUE)

anc <- run_scenarios_anchored(seq_adj, toy$ipd, toy$sld, resp, resp_anc, arm = "ARM", reference_arm = "A")

test_that("anchored scenarios: one row per scenario with status, result, and weight columns", {
  expect_named(anc, c("results", "runs", "n_failed"))
  expect_equal(nrow(anc$results), length(seq_adj))
  expect_equal(names(anc$results)[1:7], c("scenario", "label", "flag", "n_variables", "variables", "status", "error"))
  expect_true(all(anc$results$status == "ok"))
  expect_true(all(is.na(anc$results$error)))
  expect_equal(anc$n_failed, 0)
  expect_true(all(anc$results$contrast == "B vs A"))
  expect_true(all(c("weight_ess", "w_max", "top10_share") %in% names(anc$results)))
  expect_true(all(vapply(anc$runs, `[[`, logical(1), "include_adjust")))
})

test_that("each anchored scenario row matches a direct run", {
  direct <- run_maic_anchored(toy$ipd, toy$sld, seq_adj[[3]]$metadata, resp, resp_anc,
                              arm = "ARM", reference_arm = "A", include_adjust = TRUE)
  expect_equal(anc$results$estimate[3], direct$result$estimate)
  expect_equal(anc$results$w_max[3], direct$diagnostics$w_max)
  expect_equal(anc$results$weight_ess[1], run_maic_weighting(toy$ipd, toy$sld, toy$metadata)$diagnostics$ess)
})

test_that("unanchored scenarios take the IPD as supplied, no arm arguments exist", {
  m <- toy$metadata
  m$match <- FALSE
  m$match_sd <- m$variable == "fac_age"
  m$adjust <- m$variable %in% c("fac_sev_hb", "fac_age", "fac_bmi")
  sc <- define_scenarios_sequential(m, c("fac_age", "fac_sev_hb", "fac_bmi"), include_empty = TRUE)
  una <- run_scenarios_unanchored(sc, arm_a, toy$sld, resp, resp_una)
  expect_false("arm" %in% names(formals(run_scenarios_unanchored)))
  expect_equal(nrow(una$results), 4)
  expect_false(una$runs[[1]]$weighted)
  expect_equal(una$results$weight_ess[1], nrow(arm_a))
  expect_true(all(una$results$contrast == UNANCHORED_CONTRAST))
  expect_true(all(diff(una$results$weight_ess) < 0))
})

test_that("wrong kind of published result is refused up front", {
  expect_error(run_scenarios_unanchored(seq_adj, arm_a, toy$sld, resp, resp_anc), "use run_scenarios_anchored")
  expect_error(run_scenarios_anchored(seq_adj, toy$ipd, toy$sld, resp, resp_una, arm = "ARM", reference_arm = "A"),
               "use run_scenarios_unanchored")
  expect_error(run_scenarios_anchored(seq_adj, toy$ipd, toy$sld, resp, resp_anc, arm = "ARM", reference_arm = NULL),
               "needs both")
})

# error tolerance ----------------------------------------------------------------

test_that("a failing scenario is recorded with its context and the rest still run", {
  m <- toy$metadata
  m$match <- FALSE
  m$match_sd <- FALSE
  # fac_color_sld_extra has an SLD level absent from the IPD: step 3 must fail
  m$adjust <- m$variable %in% c("fac_sev_hb", "fac_age", "fac_color_sld_extra", "fac_bmi")
  sc <- define_scenarios_sequential(m, c("fac_sev_hb", "fac_age", "fac_color_sld_extra", "fac_bmi"),
                                    include_empty = TRUE)
  out <- run_scenarios_unanchored(sc, arm_a, toy$sld, resp, resp_una)

  expect_equal(nrow(out$results), 5)
  expect_equal(out$results$status, c("ok", "ok", "ok", "error", "error"))
  expect_equal(out$n_failed, 2)
  expect_match(out$results$error[4], "fac_color_sld_extra.*absent from IPD")
  expect_equal(out$results$variables[4], "fac_sev_hb, fac_age, fac_color_sld_extra")
  expect_true(all(is.na(out$results$estimate[4:5])))
  expect_false(is.na(out$results$estimate[3]))
  expect_equal(out$results$scale[4], "log_or")             # identifying columns still filled
  expect_equal(out$results$contrast[4], UNANCHORED_CONTRAST)
  expect_true(all(is.na(out$results$w_max[4:5])))
  expect_equal(out$runs[[4]]$status, "error")
  expect_equal(out$runs[[4]]$scenario$label, "+ fac_color_sld_extra")
  expect_null(out$runs[[4]]$result)
})

test_that("a failed solver names the constraints it was solving", {
  m <- toy$metadata
  m$match_sd <- FALSE
  sc <- define_scenarios_univariate(m, "fac_egfr")
  out <- run_scenarios_anchored(sc, toy$ipd, toy$sld, resp, resp_anc, arm = "ARM", reference_arm = "A", maxit = 1)
  expect_equal(out$results$status, "error")
  expect_match(out$results$error, "did not converge.*constraints on .* rows \\[.*fac_egfr.*\\]")
})

test_that("formatted scenario table carries Status and Error columns and NR for failed numbers", {
  m <- toy$metadata
  m$match <- FALSE
  m$match_sd <- FALSE
  m$adjust <- m$variable %in% c("fac_age", "fac_color_sld_extra")
  out <- run_scenarios_unanchored(define_scenarios_univariate(m, c("fac_age", "fac_color_sld_extra")),
                                  arm_a, toy$sld, resp, resp_una)
  tab <- format_results(out$results)
  expect_true(all(c("Status", "Error") %in% names(tab)))
  expect_equal(tab$Status, c("ok", "error"))
  expect_equal(tab$Error[1], "")
  expect_match(tab$Error[2], "absent from IPD")
  expect_equal(tab$`Estimate (95% CI)`[2], "NR")
  expect_equal(tab$`Weight max`[2], "NR")
  expect_equal(tab$`Top 10% share`[2], "NR")
})
