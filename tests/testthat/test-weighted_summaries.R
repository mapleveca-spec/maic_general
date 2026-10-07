# weighted_stats ---------------------------------------------------------------

test_that("unit weights reproduce unweighted statistics", {
  x <- c(2, 4, 4, 4, 5, 5, 7, 9)
  w <- rep(1, length(x))
  expect_equal(effective_n(w), 8)
  expect_equal(weighted_mean(x, w), mean(x))
  expect_equal(weighted_sd(x, w), sd(x))
})

test_that("weighted mean and ESS match hand calculations", {
  x <- c(1, 2, 3)
  w <- c(1, 1, 2)
  expect_equal(weighted_mean(x, w), (1 + 2 + 6) / 4)
  expect_equal(effective_n(w), 16 / 6)
})

test_that("weighted sd equals frequency-weight sd when weights are integer counts", {
  x <- c(1, 2, 3)
  w <- c(1, 1, 2)
  # Reliability-weight form does NOT equal the expanded-sample sd in general;
  # check against the formula directly instead.
  m <- weighted_mean(x, w)
  denom <- sum(w) - sum(w^2) / sum(w)
  expect_equal(weighted_sd(x, w), sqrt(sum(w * (x - m)^2) / denom))
})

test_that("degenerate inputs give NA, not errors", {
  expect_true(is.na(weighted_sd(5, 1)))
  expect_true(is.na(weighted_mean(numeric(0), numeric(0))))
  expect_equal(effective_n(numeric(0)), 0)
  expect_true(is.na(weighted_sd(c(1, 2), c(1, 0))))  # one effective obs
})

test_that("check_weights rejects bad weights", {
  expect_error(check_weights(c(1, 2), 3), "length 3")
  expect_error(check_weights(c(1, NA), 2), "contains NA")
  expect_error(check_weights(c(1, -1), 2), "negative")
  expect_error(check_weights(c(0, 0), 2), "positive sum")
  expect_error(check_weights("a", 1), "numeric")
})

# summarize_continuous with weights ---------------------------------------------

test_that("continuous: NULL weights and unit weights are identical", {
  x <- c(1, NA, 3, 4)
  expect_equal(summarize_continuous(x, "v"), summarize_continuous(x, "v", weights = rep(1, 4)))
})

test_that("continuous: weights shift mean, ESS, and missing share", {
  x <- c(10, NA, 30)
  w <- c(3, 1, 1)
  out <- summarize_continuous(x, "v", weights = w)
  expect_equal(out$est[1], (30 + 30) / 4)
  expect_equal(out$n[1], effective_n(c(3, 1)))
  expect_equal(out$est[2], 1 / 5)        # weight on NA row / total weight
  expect_equal(out$n[2], effective_n(w))
})

test_that("continuous: Missing row presence does not depend on weights", {
  x <- c(1, NA, 3)
  out <- summarize_continuous(x, "v", weights = c(1, 0, 1))
  expect_equal(out$level, c(CONTINUOUS_LEVEL, MISSING_LEVEL))
  expect_equal(out$est[2], 0)
})

# summarize_categorical with weights --------------------------------------------

test_that("categorical: NULL weights and unit weights are identical", {
  x <- c("a", "b", NA, "a")
  expect_equal(
    summarize_categorical(x, "v", c("a", "b")),
    summarize_categorical(x, "v", c("a", "b"), weights = rep(1, 4))
  )
})

test_that("categorical: proportions are weight shares and still sum to 1", {
  x <- c("a", "b", NA, "a")
  w <- c(1, 2, 1, 4)
  out <- summarize_categorical(x, "v", c("a", "b"), weights = w)
  expect_equal(out$est, c(5, 2, 1) / 8)
  expect_equal(sum(out$est), 1)
  expect_equal(out$n, rep(effective_n(w), 3))
})

# summarize_ipd with weights ----------------------------------------------------

test_that("summarize_ipd: weighted and unweighted tables have identical keys", {
  w <- runif(nrow(toy$ipd), 0.2, 3)
  a <- summarize_ipd(toy$ipd, toy$metadata)
  b <- summarize_ipd(toy$ipd, toy$metadata, weights = w)
  expect_identical(a[c("variable", "type", "level")], b[c("variable", "type", "level")])
  expect_false(isTRUE(all.equal(a$est, b$est)))
})

test_that("summarize_ipd: weights length is checked once up front", {
  expect_error(summarize_ipd(toy$ipd, toy$metadata, weights = 1:3), "length 60")
})

test_that("before/after balance tables share rows and differ only in IPD columns", {
  w <- runif(nrow(toy$ipd), 0.2, 3)
  sld_s  <- summarize_sld(toy$sld, toy$metadata)
  before <- create_balance_table(summarize_ipd(toy$ipd, toy$metadata), sld_s, toy$metadata)
  after  <- create_balance_table(summarize_ipd(toy$ipd, toy$metadata, weights = w), sld_s, toy$metadata)
  keys <- c("display_order", "variable", "type", "level", "row_type")
  expect_identical(before[keys], after[keys])
  expect_identical(before[c("sld_n", "sld_est", "sld_sd")], after[c("sld_n", "sld_est", "sld_sd")])
  expect_false(isTRUE(all.equal(before$smd, after$smd)))
})
