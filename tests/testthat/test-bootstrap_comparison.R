resp     <- define_outcome("Response", "binary", "y_resp")
os       <- define_outcome("OS", "tte", "y_time", event = "y_event")
resp_anc <- define_sld_outcome("Response", "log_or", estimate = log(1.5), se = 0.26)
sld_s    <- summarize_sld(toy$sld, toy$metadata)
point    <- run_maic_analysis(toy$ipd, toy$sld, toy$metadata, resp, resp_anc, arm = "ARM", reference_arm = "A")

boot <- bootstrap_comparison(toy$ipd, sld_s, toy$metadata, resp, resp_anc,
                             arm = "ARM", reference_arm = "A", n_boot = 60, seed = 1, max_fail_rate = 0.5)

test_that("shape: one-row summary and diagnostics, a replicate vector", {
  expect_named(boot, c("summary", "diagnostics", "replicates", "ess", "n_failed", "failure_counts", "failures"))
  expect_equal(nrow(boot$summary), 1)
  expect_equal(boot$summary$outcome, "Response")
  expect_true(boot$summary$anchored)
  expect_equal(boot$summary$scale, "log_or")
  expect_equal(length(boot$replicates) + boot$n_failed, 60)
  expect_equal(boot$summary$n_boot_ok, length(boot$replicates))
  expect_length(boot$ess, length(boot$replicates))
})

test_that("nothing finite is excluded; failures counted by reason", {
  expect_true(all(is.finite(boot$replicates)))
  expect_equal(sum(boot$failure_counts), boot$n_failed)
  expect_named(boot$failure_counts, FAILURE_REASONS)
})

test_that("percentile interval over all replicates brackets the point estimate", {
  expect_equal(boot$summary$boot_ci_low, unname(quantile(boot$replicates, 0.025)))
  expect_equal(boot$summary$boot_ci_high, unname(quantile(boot$replicates, 0.975)))
  expect_true(point$result$estimate > boot$summary$boot_ci_low && point$result$estimate < boot$summary$boot_ci_high)
})

test_that("extreme replicates are counted on log scales only, never dropped", {
  expect_equal(boot$summary$n_extreme, sum(abs(boot$replicates) > 5))
  expect_equal(boot$summary$extreme_share, boot$summary$n_extreme / boot$summary$n_boot_ok)
  tight <- bootstrap_comparison(toy$ipd, sld_s, toy$metadata, resp, resp_anc, arm = "ARM", reference_arm = "A",
                                n_boot = 10, seed = 1, max_fail_rate = 1, extreme_bound = 1e-6)
  expect_equal(tight$summary$n_extreme, tight$summary$n_boot_ok)
  expect_equal(length(tight$replicates), tight$summary$n_boot_ok)

  score <- define_outcome("Score change", "continuous", "y_score")
  score_sld <- define_sld_outcome("Score change", "mean_diff", -2, se = .9)
  md <- bootstrap_comparison(toy$ipd, sld_s, toy$metadata, score, score_sld, arm = "ARM", reference_arm = "A",
                             n_boot = 10, seed = 1, max_fail_rate = 1, extreme_bound = 1e-6)
  expect_equal(md$summary$n_extreme, 0L)
})

test_that("diagnostics carry the bootstrap SE and replicate ESS spread", {
  expect_equal(boot$diagnostics$boot_se, sd(boot$replicates))
  expect_equal(boot$diagnostics$boot_median, median(boot$replicates))
  expect_equal(boot$diagnostics$ess_median, median(boot$ess))
  expect_true(boot$diagnostics$ess_min <= boot$diagnostics$ess_max)
})

test_that("failure messages are classified by reason", {
  msgs <- c("Degenerate replicate: non-finite estimate for x.",
            "MAIC weight estimation did not converge (optim code 1).",
            "`fac_color_sld_extra`: SLD has level(s) absent from IPD, cannot match: Yellow.")
  expect_equal(.count_failures(msgs), c(infeasible = 1L, nonconverged = 1L, degenerate = 1L))
  expect_equal(.count_failures(character()), c(infeasible = 0L, nonconverged = 0L, degenerate = 0L))
})

test_that("seed makes the bootstrap reproducible; different seeds differ", {
  args <- list(toy$ipd, sld_s, toy$metadata, resp, resp_anc, arm = "ARM", reference_arm = "A", n_boot = 10,
               max_fail_rate = 1)
  b1 <- do.call(bootstrap_comparison, c(args, seed = 7))
  b2 <- do.call(bootstrap_comparison, c(args, seed = 7))
  b3 <- do.call(bootstrap_comparison, c(args, seed = 8))
  expect_identical(b1$replicates, b2$replicates)
  expect_false(identical(b1$replicates, b3$replicates))
})

test_that("unanchored bootstrap runs on the rows given without arm", {
  arm_a <- toy$ipd[toy$ipd$ARM == "A", ]
  b <- bootstrap_comparison(arm_a, sld_s, toy$metadata, resp, sld_outcome_from_proportion("Response", .4, 100),
                            n_boot = 10, seed = 3, max_fail_rate = 1)
  expect_false(b$summary$anchored)
  expect_equal(b$summary$scale, "log_or")
})

test_that("too many failures is an error naming the counts and the first cause", {
  m <- toy$metadata
  m$match <- m$variable == "fac_color_sld_extra"  # SLD level Yellow absent from IPD: every replicate fails
  m$match_sd <- FALSE
  expect_error(
    bootstrap_comparison(toy$ipd, sld_s, m, resp, resp_anc, arm = "ARM", reference_arm = "A", n_boot = 5, seed = 1),
    "5 of 5 bootstrap replicates failed.*infeasible = 5.*absent from IPD"
  )
})

test_that("a mismatched pair is refused up front", {
  expect_error(bootstrap_comparison(toy$ipd, sld_s, toy$metadata, os, resp_anc, arm = "ARM", reference_arm = "A",
                                    n_boot = 5), "Outcome mismatch")
})
