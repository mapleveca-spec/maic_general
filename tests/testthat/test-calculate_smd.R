# smd() ------------------------------------------------------------------------

test_that("continuous SMD uses pooled SD with IPD minus SLD sign", {
  expect_equal(smd(52, 10, 50, 10), 0.2)
  expect_equal(smd(50, 10, 52, 10), -0.2)
  expect_equal(smd(1, 3, 0, 4), 1 / sqrt((9 + 16) / 2))
})

test_that("proportion SMD equals the textbook formula when sd = sqrt(p(1-p))", {
  p1 <- 0.3
  p2 <- 0.5
  expected <- (p1 - p2) / sqrt((p1 * (1 - p1) + p2 * (1 - p2)) / 2)
  expect_equal(smd(p1, sqrt(p1 * (1 - p1)), p2, sqrt(p2 * (1 - p2))), expected)
})

test_that("identical constants give 0, not NaN", {
  expect_equal(smd(0, 0, 0, 0), 0)
  expect_equal(smd(1, 0, 1, 0), 0)
})

test_that("different constants give signed Inf", {
  expect_equal(smd(1, 0, 0, 0), Inf)
  expect_equal(smd(0, 0, 1, 0), -Inf)
})

test_that("NA propagates", {
  expect_true(is.na(smd(NA, NA, 50, 10)))
  expect_true(is.na(smd(50, 10, NA, NA)))
})

test_that("smd is vectorised", {
  out <- smd(c(52, 0), c(10, 0), c(50, 0), c(10, 0))
  expect_equal(out, c(0.2, 0))
})

# add_smd() --------------------------------------------------------------------

balance <- add_smd(align_summaries(
  summarize_ipd(toy$ipd, toy$metadata),
  summarize_sld(toy$sld, toy$metadata),
  toy$metadata
))
rows <- function(v) balance[balance$variable == v, ]

test_that("add_smd appends exactly one column and keeps row count", {
  expect_identical(names(balance), BALANCE_COLUMNS)
  expect_equal(nrow(balance), 26)
})

test_that("add_smd refuses a table missing aligned columns", {
  expect_error(add_smd(balance[, -2]))
})

test_that("summary rows use means and SDs", {
  r <- rows("fac_age")
  expect_equal(r$smd, (r$ipd_est - 50) / sqrt((r$ipd_sd^2 + 100) / 2))
})

test_that("level and missing rows use proportions", {
  r <- rows("fac_color_missing_both")
  m <- r[r$row_type == "missing", ]
  expect_equal(m$smd, 0)  # 0.10 vs 0.10
  b <- r[r$level == "Blue", ]
  expect_equal(b$smd, (b$ipd_est - 0.5) / sqrt((b$ipd_est * (1 - b$ipd_est) + 0.25) / 2))
})

test_that("SLD-only and IPD-only levels get finite SMDs", {
  r <- rows("fac_color_sld_extra")
  expect_true(is.finite(r$smd[r$level == "Yellow"]))
  expect_true(r$smd[r$level == "Yellow"] < 0)  # IPD 0 vs SLD 0.15
  r <- rows("fac_color_ipd_extra")
  expect_true(r$smd[r$level == "Red"] > 0)     # IPD 0.47 vs SLD 0
})

test_that("unreported SLD variables have NA SMD on every row", {
  expect_true(all(is.na(rows("fac_weight")$smd)))
  expect_true(all(is.na(rows("fac_height")$smd)))
})

test_that("every reported row has a non-NA SMD", {
  reported <- !is.na(balance$sld_est)
  expect_false(anyNA(balance$smd[reported]))
})
