ipd_s   <- summarize_ipd(toy$ipd, toy$metadata)
sld_s   <- summarize_sld(toy$sld, toy$metadata)
targets <- build_match_targets(ipd_s, sld_s, toy$metadata)
design  <- build_design_matrix(toy$ipd, targets)
fit     <- estimate_maic_weights(design)

# rescale_weights --------------------------------------------------------------

test_that("rescaled weights sum to the number of weighted rows and keep zeros", {
  wr <- rescale_weights(fit$weights)
  expect_equal(sum(wr), sum(fit$weights > 0))
  expect_identical(wr == 0, fit$weights == 0)
  expect_equal(wr / sum(wr), fit$weights / sum(fit$weights))
})

test_that("rescaling is idempotent and leaves unit weights alone", {
  expect_equal(rescale_weights(rep(1, 7)), rep(1, 7))
  wr <- rescale_weights(fit$weights)
  expect_equal(rescale_weights(wr), wr)
})

test_that("rescaling does not change weighted summaries", {
  a <- summarize_ipd(toy$ipd, toy$metadata, weights = fit$weights)
  b <- summarize_ipd(toy$ipd, toy$metadata, weights = rescale_weights(fit$weights))
  expect_equal(a, b)
})

# weight_diagnostics -----------------------------------------------------------

diag <- weight_diagnostics(fit)

test_that("diagnostics is one row with consistent counts", {
  expect_equal(nrow(diag), 1)
  expect_equal(diag$n_ipd, 60)
  expect_equal(diag$n_complete + diag$n_excluded, diag$n_ipd)
  expect_equal(diag$n_complete, fit$n_complete)
})

test_that("ESS agrees with the fit and is a fraction of complete cases", {
  expect_equal(diag$ess, fit$ess)
  expect_equal(diag$ess_pct, fit$ess / fit$n_complete)
  expect_true(diag$ess_pct > 0 && diag$ess_pct < 1)
})

test_that("weight quantiles are on the rescaled scale and ordered", {
  wr <- rescale_weights(fit$weights)[fit$weights > 0]
  expect_equal(diag$w_min, min(wr))
  expect_equal(diag$w_max, max(wr))
  expect_equal(diag$w_median, median(wr))
  expect_true(diag$w_min <= diag$w_q25 && diag$w_q25 <= diag$w_median &&
                diag$w_median <= diag$w_q75 && diag$w_q75 <= diag$w_max)
})

test_that("top10_share is between 0.10 and 1 and matches a direct calculation", {
  wr <- sort(rescale_weights(fit$weights)[fit$weights > 0], decreasing = TRUE)
  k  <- ceiling(0.10 * length(wr))
  expect_equal(diag$top10_share, sum(wr[1:k]) / sum(wr))
  expect_true(diag$top10_share >= 0.10 && diag$top10_share <= 1)
})

test_that("uniform weights give ess_pct 1 and a flat distribution", {
  flat <- list(weights = c(rep(1, 40), rep(0, 20)), max_moment_error = 0)
  d <- weight_diagnostics(flat)
  expect_equal(d$ess_pct, 1)
  expect_equal(d$n_excluded, 20)
  expect_equal(c(d$w_min, d$w_median, d$w_max), c(1, 1, 1))
  expect_equal(d$top10_share, 0.10)
})

test_that("max_moment_error is passed through", {
  expect_equal(diag$max_moment_error, fit$max_moment_error)
})
