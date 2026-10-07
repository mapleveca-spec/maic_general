# define_scenarios_* -----------------------------------------------------------

# Prognostic (adjust-tier) variables in the toy metadata, in a chosen order.
order_adj <- c("fac_prior_treatment", "fac_bmi", "fac_egfr")
primary   <- toy$metadata$variable[toy$metadata$match]

test_that("sequential: scenario k switches on the first k adjust variables, primary set untouched", {
  sc <- define_scenarios_sequential(toy$metadata, order_adj)
  expect_length(sc, 3)
  for (k in 1:3) {
    m <- sc[[k]]$metadata
    expect_equal(sc[[k]]$variables, order_adj[1:k])
    expect_setequal(m$variable[m$adjust], order_adj[1:k])
    expect_setequal(m$variable[m$match], primary)
    expect_equal(sc[[k]]$flag, "adjust")
    expect_silent(validate_metadata(m))
  }
  expect_equal(vapply(sc, `[[`, character(1), "label"), c("+ fac_prior_treatment", "+ fac_bmi", "+ fac_egfr"))
})

test_that("sequential: include_empty prepends the primary-set-only scenario", {
  sc <- define_scenarios_sequential(toy$metadata, order_adj, include_empty = TRUE)
  expect_length(sc, 4)
  expect_equal(sc[[1]]$label, "(none)")
  expect_false(any(sc[[1]]$metadata$adjust))
  expect_setequal(sc[[1]]$metadata$variable[sc[[1]]$metadata$match], primary)
})

test_that("univariate: one adjust variable on per scenario", {
  sc <- define_scenarios_univariate(toy$metadata, order_adj)
  expect_length(sc, 3)
  for (k in 1:3) {
    expect_equal(sc[[k]]$metadata$variable[sc[[k]]$metadata$adjust], order_adj[k])
    expect_equal(sc[[k]]$label, order_adj[k])
  }
})

test_that("match flag: match_sd follows the variable out of the weighting set", {
  sc <- define_scenarios_univariate(toy$metadata, c("fac_sev_hb", "fac_age"), flag = "match")
  m1 <- sc[[1]]$metadata   # fac_sev_hb only; fac_age had match_sd = TRUE
  expect_false(m1$match_sd[m1$variable == "fac_age"])
  expect_equal(m1$variable[m1$match], "fac_sev_hb")
  m2 <- sc[[2]]$metadata
  expect_true(m2$match_sd[m2$variable == "fac_age"])  # kept when fac_age is matched
  for (s in sc) expect_silent(validate_metadata(s$metadata))
})

test_that("scenario definitions leave other columns untouched", {
  sc <- define_scenarios_sequential(toy$metadata, order_adj)
  keep <- setdiff(names(toy$metadata), c("adjust", "match_sd"))
  expect_identical(sc[[2]]$metadata[keep], toy$metadata[keep])
})

test_that("bad variable lists are rejected, including variables from the other tier", {
  expect_error(define_scenarios_sequential(toy$metadata, c("fac_bmi", "nope")), "not in metadata: nope")
  expect_error(define_scenarios_univariate(toy$metadata, c("fac_bmi", "fac_bmi")), "must be unique")
  expect_error(define_scenarios_sequential(toy$metadata, "fac_bmi", flag = "show_balance"))
  expect_error(define_scenarios_sequential(toy$metadata, c("fac_age", "fac_bmi")),
               "already in the `match` tier: fac_age")
  expect_error(define_scenarios_univariate(toy$metadata, "fac_bmi", flag = "match"),
               "already in the `adjust` tier: fac_bmi")
})

# run_maic_scenarios -----------------------------------------------------------

resp     <- define_outcome("Response", "binary", "y_resp")
resp_anc <- define_sld_outcome("Response", "log_or", estimate = log(1.5), se = 0.26)
resp_una <- sld_outcome_from_proportion("Response", p = 0.40, n = 100)
seq_adj  <- define_scenarios_sequential(toy$metadata, order_adj, include_empty = TRUE)
out <- run_maic_scenarios(seq_adj, toy$ipd, toy$sld, resp, resp_anc, arm = "ARM", reference_arm = "A")

test_that("one row per scenario, scenario columns first, one outcome throughout", {
  expect_equal(nrow(out$results), length(seq_adj))
  expect_equal(names(out$results)[1:5], c("scenario", "label", "flag", "n_variables", "variables"))
  expect_equal(out$results$n_variables, 0:3)
  expect_true(all(out$results$outcome == "Response"))
  expect_true(all(out$results$contrast == "B vs A"))
  expect_length(out$runs, length(seq_adj))
})

test_that("adjust scenarios weight on primary + adjust tier, so each has its own weights", {
  expect_true(all(vapply(out$runs, `[[`, logical(1), "include_adjust")))
  n_targets <- vapply(out$runs, function(r) nrow(r$targets), integer(1))
  expect_true(all(diff(n_targets) > 0))
  expect_equal(length(unique(out$results$weight_ess)), length(seq_adj))
  expect_equal(out$results$weight_ess[1], run_maic_weighting(toy$ipd, toy$sld, toy$metadata)$diagnostics$ess)
})

test_that("each scenario row matches a direct single-outcome run", {
  expect_equal(out$results$label, c("(none)", "+ fac_prior_treatment", "+ fac_bmi", "+ fac_egfr"))
  direct <- run_maic_analysis(toy$ipd, toy$sld, seq_adj[[3]]$metadata, resp, resp_anc,
                              arm = "ARM", reference_arm = "A", include_adjust = TRUE)
  expect_equal(out$results$estimate[3], direct$result$estimate)
  expect_equal(out$results$n[3], direct$result$n)
})

test_that("match scenarios vary the primary set; include_adjust defaults to FALSE for them", {
  uni <- define_scenarios_univariate(toy$metadata, c("fac_sev_hb", "fac_tar_jnt_lead", "fac_age"), flag = "match")
  o2  <- run_maic_scenarios(uni, toy$ipd, toy$sld, resp, resp_anc, arm = "ARM", reference_arm = "A")
  expect_false(any(vapply(o2$runs, `[[`, logical(1), "include_adjust")))
  expect_length(unique(o2$results$weight_ess), 3)
  expect_true(all(o2$results$weight_ess > out$results$weight_ess[1]))  # one constraint set each: more ESS
})

test_that("unanchored scenarios subset to the intervention arm; anchored need both arm arguments", {
  o3 <- run_maic_scenarios(seq_adj[2:3], toy$ipd, toy$sld, resp, resp_una, arm = "ARM", intervention_arm = "A")
  expect_equal(nrow(o3$results), 2)
  expect_true(all(o3$results$contrast == "A (unanchored)"))
  expect_true(all(vapply(o3$runs, function(r) r$diagnostics$n_ipd, numeric(1)) == sum(toy$ipd$ARM == "A")))
  expect_error(run_maic_scenarios(seq_adj[2:3], toy$ipd, toy$sld, resp, resp_anc),
               "needs both `arm` and `reference_arm`")
  expect_error(run_maic_scenarios(seq_adj[2:3], toy$ipd, toy$sld, resp, resp_una, arm = "ARM"),
               "needs `intervention_arm`")
})
