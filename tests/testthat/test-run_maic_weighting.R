res <- run_maic_weighting(toy$ipd, toy$sld, toy$metadata)

test_that("result has every documented component", {
  expect_named(res, c(
    "metadata", "ipd_summary", "sld_summary", "include_adjust", "targets", "design", "fit",
    "weights", "diagnostics", "balance_before", "balance_after"
  ))
})

test_that("pipeline reproduces the manual chain exactly", {
  ipd_s <- summarize_ipd(toy$ipd, toy$metadata)
  sld_s <- summarize_sld(toy$sld, toy$metadata)
  expect_equal(res$ipd_summary, ipd_s)
  expect_equal(res$balance_before, create_balance_table(ipd_s, sld_s, toy$metadata))
  t <- build_match_targets(ipd_s, sld_s, toy$metadata)
  expect_equal(res$targets, t)
  fit <- estimate_maic_weights(build_design_matrix(toy$ipd, t))
  expect_equal(res$weights, fit$weights)
  expect_equal(res$diagnostics, weight_diagnostics(fit))
})

test_that("before and after share keys; matched terms are balanced after", {
  keys <- c("display_order", "variable", "type", "level", "row_type")
  expect_identical(res$balance_before[keys], res$balance_after[keys])
  matched <- res$balance_after$variable %in% toy$metadata$variable[toy$metadata$match] &
    res$balance_after$row_type != "missing"
  expect_true(all(abs(res$balance_after$smd[matched]) < 1e-3))
})

test_that("validation errors surface from the pipeline", {
  bad_meta <- toy$metadata
  bad_meta$type[1] <- "categorical"
  expect_error(run_maic_weighting(toy$ipd, toy$sld, bad_meta), "Invalid metadata")

  bad_sld <- toy$sld[toy$sld$var_name != "fac_age", ]
  expect_error(run_maic_weighting(toy$ipd, bad_sld, toy$metadata), "Invalid SLD")

  bad_ipd <- toy$ipd
  bad_ipd$fac_age <- NULL
  expect_error(run_maic_weighting(bad_ipd, toy$sld, toy$metadata), "Invalid IPD")
})

test_that("na_action and solver options pass through", {
  # NA sits in adjust-tier variables, so only the full weighting set triggers it
  expect_silent(run_maic_weighting(toy$ipd, toy$sld, toy$metadata, na_action = "error"))
  expect_error(run_maic_weighting(toy$ipd, toy$sld, toy$metadata, include_adjust = TRUE, na_action = "error"),
               "NA in matched variable")
  expect_error(run_maic_weighting(toy$ipd, toy$sld, toy$metadata, maxit = 1),
               "did not converge|reproduce")
})
