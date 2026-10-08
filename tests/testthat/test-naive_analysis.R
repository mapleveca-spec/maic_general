naive_meta <- toy$metadata
naive_meta$match    <- FALSE
naive_meta$adjust   <- FALSE
naive_meta$match_sd <- FALSE

resp     <- define_outcome("Response", "binary", "y_resp")
resp_anc <- define_sld_outcome("Response", "log_or", estimate = log(1.5), se = 0.26)

# estimate_weights -------------------------------------------------------------

test_that("an empty weighting set gives unit weights, not an error", {
  ipd_s <- summarize_ipd(toy$ipd, toy$metadata)
  sld_s <- summarize_sld(toy$sld, toy$metadata)
  w <- estimate_weights(toy$ipd, ipd_s, sld_s, naive_meta)
  expect_false(w$weighted)
  expect_equal(w$fit$weights, rep(1, nrow(toy$ipd)))
  expect_equal(w$fit$ess, nrow(toy$ipd))
  expect_equal(w$fit$n_complete, nrow(toy$ipd))
  expect_equal(nrow(w$targets), 0)
  expect_identical(names(w$targets), TARGET_COLUMNS)
  expect_null(w$design)
  expect_named(w$fit, names(unit_weights_fit(3)))
})

test_that("a non-empty set still runs the full chain through estimate_weights", {
  ipd_s <- summarize_ipd(toy$ipd, toy$metadata)
  sld_s <- summarize_sld(toy$sld, toy$metadata)
  w <- estimate_weights(toy$ipd, ipd_s, sld_s, toy$metadata)
  expect_true(w$weighted)
  direct <- estimate_maic_weights(build_design_matrix(toy$ipd, build_match_targets(ipd_s, sld_s, toy$metadata)))
  expect_equal(w$fit$weights, direct$weights)
})

test_that("adjust variables alone are naive unless include_adjust = TRUE", {
  m <- toy$metadata
  m$match <- FALSE
  m$match_sd <- FALSE
  ipd_s <- summarize_ipd(toy$ipd, m)
  sld_s <- summarize_sld(toy$sld, m)
  expect_false(estimate_weights(toy$ipd, ipd_s, sld_s, m)$weighted)
  expect_true(estimate_weights(toy$ipd, ipd_s, sld_s, m, include_adjust = TRUE)$weighted)
})

# pipelines --------------------------------------------------------------------

test_that("naive run_maic_weighting: balance unchanged, diagnostics reflect unit weights", {
  res <- run_maic_weighting(toy$ipd, toy$sld, naive_meta)
  expect_false(res$weighted)
  expect_equal(res$balance_after, res$balance_before)
  expect_equal(res$diagnostics$ess_pct, 1)
  expect_equal(res$diagnostics$n_excluded, 0)
  expect_equal(res$diagnostics$w_max, 1)
  expect_equal(res$diagnostics$max_moment_error, 0)
})

test_that("naive run_maic_analysis equals the unweighted model, and bootstraps", {
  naive <- run_maic_analysis(toy$ipd, toy$sld, naive_meta, resp, resp_anc, arm = "ARM", reference_arm = "A",
                             n_boot = 20, seed = 1)
  ref <- glm(y_resp ~ ARM, data = toy$ipd, family = binomial())
  expect_equal(naive$result$ipd_estimate, unname(coef(ref)["ARMB"]))
  expect_equal(naive$result$n, nrow(toy$ipd))
  expect_equal(naive$result$ess, nrow(toy$ipd))
  expect_false(naive$weighted)
  expect_equal(naive$bootstrap$n_failed, 0)
  expect_true(all(naive$bootstrap$ess == nrow(toy$ipd)))
})

test_that("sequential match scenarios can start from the naive analysis", {
  m <- toy$metadata
  m$adjust <- FALSE   # keep the adjust tier out so the empty scenario is truly unweighted
  sc <- define_scenarios_sequential(m, c("fac_sev_hb", "fac_tar_jnt_lead", "fac_age"), flag = "match",
                                    include_empty = TRUE)
  out <- run_maic_scenarios(sc, toy$ipd, toy$sld, resp, resp_anc, arm = "ARM", reference_arm = "A")
  expect_equal(out$results$label[1], "(none)")
  expect_false(out$runs[[1]]$weighted)
  expect_equal(out$results$weight_ess[1], nrow(toy$ipd))
  expect_true(all(out$results$weight_ess[-1] < nrow(toy$ipd)))
  expect_equal(out$results$n_variables, 0:3)
})

test_that("plot_weights and weight_diagnostics accept the naive fit", {
  fit <- unit_weights_fit(12)
  d <- weight_diagnostics(fit)
  expect_equal(d$ess, 12)
  expect_equal(d$top10_share, 2 / 12)
  expect_s3_class(plot_weights(fit), "ggplot")
})
