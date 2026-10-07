aligned <- align_summaries(
  summarize_ipd(toy$ipd, toy$metadata),
  summarize_sld(toy$sld, toy$metadata),
  toy$metadata
)
rows <- function(v) aligned[aligned$variable == v, ]

test_that("aligned table has the schema, every metadata variable, display order", {
  expect_identical(names(aligned), ALIGNED_COLUMNS)
  expect_identical(unique(aligned$variable), toy$metadata$variable[order(toy$metadata$display_order)])
  expect_true(all(aligned$row_type %in% ROW_TYPES))
  expect_false(anyNA(aligned$level))
})

test_that("row_type is consistent with type and level", {
  expect_true(all(aligned$row_type[aligned$level == MISSING_LEVEL] == "missing"))
  expect_true(all(aligned$level[aligned$row_type == "summary"] == CONTINUOUS_LEVEL))
  expect_true(all(aligned$type[aligned$row_type == "summary"] == "con"))
  expect_true(all(aligned$type[aligned$row_type == "level"] == "cat"))
})

test_that("Missing row appears iff either side has missingness", {
  with_missing <- unique(aligned$variable[aligned$row_type == "missing"])
  expect_setequal(with_missing, c(
    "fac_prior_treatment", "fac_color_sld_extra", "fac_color_missing_both",
    "fac_bmi", "fac_height", "fac_egfr"
  ))
  expect_false(MISSING_LEVEL %in% rows("fac_age")$level)
  expect_false(MISSING_LEVEL %in% rows("fac_weight")$level)
})

test_that("Missing is always the last level within a variable", {
  last <- tapply(aligned$level, aligned$variable, function(l) l[length(l)])
  has  <- tapply(aligned$level, aligned$variable, function(l) MISSING_LEVEL %in% l)
  expect_true(all(last[has] == MISSING_LEVEL))
})

test_that("categorical levels follow level_order, union across sides", {
  expect_equal(rows("fac_color_sld_extra")$level, c("Blue", "Red", "Yellow", MISSING_LEVEL))
  expect_equal(rows("fac_color_ipd_extra")$level, c("Blue", "Red"))
  expect_equal(rows("fac_sev_hb")$level, c("Mild", "Moderate", "Severe"))
})

test_that("level absent on one side is filled with 0 when that side reported the variable", {
  r <- rows("fac_color_ipd_extra")
  expect_equal(r$sld_est[r$level == "Red"], 0)
  expect_equal(r$sld_sd[r$level == "Red"], 0)
  expect_equal(r$sld_n[r$level == "Red"], 100)

  r <- rows("fac_color_sld_extra")
  expect_equal(r$ipd_est[r$level == MISSING_LEVEL], 0)
  expect_equal(r$ipd_n[r$level == MISSING_LEVEL], 60)

  r <- rows("fac_prior_treatment")
  expect_equal(r$sld_est[r$level == MISSING_LEVEL], 0)
})

test_that("continuous Missing row on one side only is filled with 0 on the other", {
  r <- rows("fac_bmi")  # IPD has missing, SLD reported with no Missing row
  expect_equal(r$sld_est[r$row_type == "missing"], 0)
  expect_equal(r$sld_n[r$row_type == "missing"], 100)

  r <- rows("fac_egfr")  # SLD has Missing row, IPD complete
  expect_equal(r$ipd_est[r$row_type == "missing"], 0)
  expect_equal(r$ipd_n[r$row_type == "missing"], 60)
  expect_equal(r$sld_est[r$row_type == "missing"], 0.12)
})

test_that("unreported SLD variable stays NA, including its Missing row", {
  r <- rows("fac_height")  # IPD has missing, SLD unreported
  expect_true(is.na(r$sld_est[r$row_type == "summary"]))
  expect_true(is.na(r$sld_est[r$row_type == "missing"]))
  expect_equal(r$sld_n, c(100, 100))

  r <- rows("fac_weight")
  expect_equal(nrow(r), 1)
  expect_true(is.na(r$sld_est))
})

test_that("existing values pass through unchanged", {
  r <- rows("fac_age")
  expect_equal(r$ipd_est, mean(toy$ipd$fac_age))
  expect_equal(r$sld_est, 50)
  expect_equal(r$sld_sd, 10)
})

test_that("categorical proportions still sum to 1 on each reported side", {
  cat_rows <- aligned[aligned$type == "cat", ]
  expect_true(all(abs(tapply(cat_rows$ipd_est, cat_rows$variable, sum) - 1) < 1e-12))
  expect_true(all(abs(tapply(cat_rows$sld_est, cat_rows$variable, sum) - 1) < 1e-12))
})
