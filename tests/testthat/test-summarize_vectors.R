# summarize_continuous ---------------------------------------------------------

test_that("continuous without missing gives one row with observed n", {
  out <- summarize_continuous(c(1, 2, 3, 4), "v")
  expect_identical(names(out), SUMMARY_COLUMNS)
  expect_equal(nrow(out), 1)
  expect_equal(out$level, CONTINUOUS_LEVEL)
  expect_equal(out$n, 4)
  expect_equal(out$est, 2.5)
  expect_equal(out$sd, sd(1:4))
})

test_that("continuous with missing adds a Missing row with full-N denominator", {
  out <- summarize_continuous(c(1, NA, 3, NA), "v")
  expect_equal(nrow(out), 2)
  expect_equal(out$level, c(CONTINUOUS_LEVEL, MISSING_LEVEL))
  expect_equal(out$n, c(2, 4))
  expect_equal(out$est, c(2, 0.5))
  expect_equal(out$sd[2], sqrt(0.25))
})

test_that("continuous all-missing has NA summary and 100% missing", {
  out <- summarize_continuous(c(NA_real_, NA_real_), "v")
  expect_equal(out$n, c(0, 2))
  expect_true(is.na(out$est[1]))
  expect_equal(out$est[2], 1)
})

test_that("continuous rejects non-numeric", {
  expect_error(summarize_continuous(c("a", "b"), "v"))
})

# summarize_categorical --------------------------------------------------------

test_that("categorical emits every level in given order, zero counts kept", {
  x <- factor(c("b", "b", "a"), levels = c("a", "b"))
  out <- summarize_categorical(x, "v", levels = c("b", "a", "c"))
  expect_identical(names(out), SUMMARY_COLUMNS)
  expect_equal(out$level, c("b", "a", "c"))
  expect_equal(out$n, c(3, 3, 3))
  expect_equal(out$est, c(2 / 3, 1 / 3, 0))
  expect_equal(out$sd, sqrt(out$est * (1 - out$est)))
})

test_that("categorical adds Missing last only when NA present, proportions sum to 1", {
  out <- summarize_categorical(c("a", NA, "b", NA), "v", levels = c("a", "b"))
  expect_equal(out$level, c("a", "b", MISSING_LEVEL))
  expect_equal(out$est, c(0.25, 0.25, 0.5))
  expect_equal(sum(out$est), 1)

  out <- summarize_categorical(c("a", "b"), "v", levels = c("a", "b"))
  expect_false(MISSING_LEVEL %in% out$level)
})

test_that("categorical accepts character and factor identically", {
  f <- factor(c("a", "b", NA), levels = c("a", "b"))
  expect_equal(
    summarize_categorical(f, "v", c("a", "b")),
    summarize_categorical(as.character(f), "v", c("a", "b"))
  )
})

test_that("categorical rejects values outside levels", {
  expect_error(summarize_categorical(c("a", "z"), "v", levels = c("a", "b")), "not in `levels`: z")
})

test_that("categorical on toy edge cases", {
  m <- toy$metadata
  lv <- function(v) m$level_order[[which(m$variable == v)]]

  out <- summarize_categorical(toy$ipd$fac_color_missing_both, "fac_color_missing_both", lv("fac_color_missing_both"))
  expect_equal(out$level, c("Blue", "Red", MISSING_LEVEL))
  expect_equal(out$est[3], 6 / 60)

  # SLD-only level Yellow is in level_order, so IPD emits it with proportion 0
  out <- summarize_categorical(toy$ipd$fac_color_sld_extra, "fac_color_sld_extra", lv("fac_color_sld_extra"))
  expect_equal(out$level, c("Blue", "Red", "Yellow"))
  expect_equal(out$est[3], 0)
})
