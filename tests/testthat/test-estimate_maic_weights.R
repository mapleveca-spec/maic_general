ipd_s   <- summarize_ipd(toy$ipd, toy$metadata)
sld_s   <- summarize_sld(toy$sld, toy$metadata)
targets <- build_match_targets(ipd_s, sld_s, toy$metadata, include_adjust = TRUE)
design  <- build_design_matrix(toy$ipd, targets)
fit     <- estimate_maic_weights(design)

test_that("weights have IPD length, zero on excluded rows, positive elsewhere", {
  expect_length(fit$weights, nrow(toy$ipd))
  expect_true(all(fit$weights[!design$complete] == 0))
  expect_true(all(fit$weights[design$complete] > 0))
  expect_equal(fit$n_complete, sum(design$complete))
})

test_that("weighted column means of the design matrix are zero", {
  w <- fit$weights[design$complete]
  wm <- colSums(design$X * w) / sum(w)
  expect_true(all(abs(wm) / apply(design$X, 2, sd) < 1e-4))  # relative to each column's scale
  expect_lt(fit$max_moment_error, 1e-4)
})

test_that("alpha reproduces the weights on the original scale", {
  w <- as.numeric(exp(design$X %*% fit$alpha))
  expect_equal(w / sum(w), fit$weights[design$complete] / sum(fit$weights))
  expect_identical(names(fit$alpha), colnames(design$X))
})

test_that("weighted IPD summary hits every mean and proportion target", {
  w_s <- summarize_ipd(toy$ipd, toy$metadata, weights = fit$weights)
  for (i in which(targets$moment != "variance")) {
    est <- w_s$est[w_s$variable == targets$variable[i] & w_s$level == targets$level[i]]
    expect_equal(est, targets$target[i], tolerance = 1e-4, label = targets$term[i])
  }
})

test_that("match_sd makes the weighted variance equal the SLD variance", {
  w <- fit$weights
  x <- toy$ipd$fac_age
  m <- sum(w * x) / sum(w)
  pop_var <- sum(w * (x - m)^2) / sum(w)        # weights define a distribution
  expect_equal(pop_var, 10^2, tolerance = 1e-4)

  # fac_bmi is matched on mean only, so its weighted SD is free to differ
  cc <- !is.na(toy$ipd$fac_bmi)
  xb <- toy$ipd$fac_bmi[cc]
  wb <- w[cc]
  mb <- sum(wb * xb) / sum(wb)
  expect_false(isTRUE(all.equal(sum(wb * (xb - mb)^2) / sum(wb), 3^2, tolerance = 1e-3)))
})

test_that("post-weighting balance table has SMD 0 on matched terms, SLD side unchanged", {
  before <- create_balance_table(ipd_s, sld_s, toy$metadata)
  after  <- create_balance_table(summarize_ipd(toy$ipd, toy$metadata, weights = fit$weights), sld_s, toy$metadata)
  expect_identical(before[c("sld_n", "sld_est", "sld_sd")], after[c("sld_n", "sld_est", "sld_sd")])

  matched_var <- toy$metadata$variable[weighting_variables(toy$metadata, TRUE)]
  on_target <- after$variable %in% matched_var & after$row_type %in% c("summary", "level")
  expect_true(all(abs(after$smd[on_target]) < 1e-3))

  # The reference level is matched implicitly
  expect_lt(abs(after$smd[after$variable == "fac_sev_hb" & after$level == "Mild"]), 1e-3)

  # Unmatched variables still show imbalance
  expect_gt(abs(after$smd[after$variable == "fac_color_sld_extra" & after$level == "Yellow"]), 0.1)
})

test_that("ESS is below the complete-case count and equals the Kish formula", {
  w <- fit$weights[design$complete]
  expect_equal(fit$ess, sum(w)^2 / sum(w^2))
  expect_lt(fit$ess, fit$n_complete)
})

test_that("already-balanced data gives equal weights", {
  t0 <- targets
  cc <- toy$ipd[design$complete, ]
  for (i in seq_len(nrow(t0))) {
    x <- cc[[t0$variable[i]]]
    t0$target[i] <- switch(
      t0$moment[i],
      mean       = mean(x),
      variance   = mean(x^2),
      proportion = mean(x == t0$level[i])
    )
  }
  f0 <- estimate_maic_weights(build_design_matrix(toy$ipd, t0))
  w <- f0$weights[design$complete]
  expect_true(all(abs(w / mean(w) - 1) < 1e-3))
  expect_equal(f0$ess, f0$n_complete, tolerance = 1e-4)
})

test_that("unreachable target fails loudly instead of returning bad weights", {
  t_bad <- targets
  t_bad$target[t_bad$term == "fac_tar_jnt_lead:Yes"] <- 0.999  # feasible range but extreme
  expect_error(estimate_maic_weights(build_design_matrix(toy$ipd, t_bad), maxit = 5), "did not converge|reproduce")
})
