# summarize_ipd ----------------------------------------------------------------

test_that("summarize_ipd returns the summary schema with all metadata variables", {
  out <- summarize_ipd(toy$ipd, toy$metadata)
  expect_identical(names(out), SUMMARY_COLUMNS)
  expect_setequal(unique(out$variable), toy$metadata$variable)
  expect_identical(unique(out$variable), toy$metadata$variable)  # metadata order
})

test_that("summarize_ipd emits Missing rows exactly for IPD variables with NA", {
  out <- summarize_ipd(toy$ipd, toy$metadata)
  with_missing <- unique(out$variable[out$level == MISSING_LEVEL])
  expected <- names(toy$ipd)[colSums(is.na(toy$ipd)) > 0]
  expected <- intersect(expected, toy$metadata$variable)
  expect_setequal(with_missing, expected)
})

test_that("summarize_ipd row shapes match toy edge cases", {
  out <- summarize_ipd(toy$ipd, toy$metadata)
  rows <- function(v) out[out$variable == v, ]

  expect_equal(rows("fac_age")$level, CONTINUOUS_LEVEL)
  expect_equal(rows("fac_bmi")$level, c(CONTINUOUS_LEVEL, MISSING_LEVEL))
  expect_equal(rows("fac_bmi")$n, c(53, 60))
  expect_equal(rows("fac_sev_hb")$level, c("Mild", "Moderate", "Severe"))
  expect_equal(rows("fac_color_sld_extra")$level, c("Blue", "Red", "Yellow"))
  expect_equal(rows("fac_color_sld_extra")$est[3], 0)
  expect_equal(rows("fac_color_missing_both")$level, c("Blue", "Red", MISSING_LEVEL))
})

test_that("summarize_ipd categorical proportions sum to 1 per variable", {
  out <- summarize_ipd(toy$ipd, toy$metadata)
  cat_out <- out[out$type == "cat", ]
  sums <- tapply(cat_out$est, cat_out$variable, sum)
  expect_true(all(abs(sums - 1) < 1e-12))
})

test_that("summarize_ipd is a pure stack of the vector summarisers", {
  out <- summarize_ipd(toy$ipd, toy$metadata)
  expect_equal(
    out[out$variable == "fac_age", ],
    summarize_continuous(toy$ipd$fac_age, "fac_age")
  )
})

# summarize_sld ----------------------------------------------------------------

test_that("summarize_sld maps columns and recodes the continuous level", {
  out <- summarize_sld(toy$sld, toy$metadata)
  expect_identical(names(out), SUMMARY_COLUMNS)
  expect_equal(nrow(out), nrow(toy$sld))
  con_levels <- out$level[out$type == "con"]
  expect_true(all(con_levels %in% c(CONTINUOUS_LEVEL, MISSING_LEVEL)))
  expect_equal(sum(con_levels == CONTINUOUS_LEVEL), sum(toy$metadata$type == "con"))
  expect_false(anyNA(out$level))
  expect_equal(out$est[out$variable == "fac_age"], 50)
  expect_true(is.na(out$est[out$variable == "fac_weight"]))
})

test_that("summarize_sld passes Missing rows through and keeps SLD-only levels", {
  out <- summarize_sld(toy$sld, toy$metadata)
  expect_equal(
    out$level[out$variable == "fac_color_sld_extra"],
    c("Blue", "Red", "Yellow", MISSING_LEVEL)
  )
  expect_equal(out$level[out$variable == "fac_color_ipd_extra"], "Blue")
})

test_that("summarize_sld derives the binomial sd for proportion rows, so entered values are optional", {
  s <- toy$sld
  is_prop <- s$var_type == "cat" | s$var_level %in% MISSING_LEVEL
  s$sld_sd[is_prop] <- NA
  expect_silent(validate_sld(s, toy$metadata))
  out <- summarize_sld(s, toy$metadata)
  expect_equal(out, summarize_sld(toy$sld, toy$metadata))
  prop <- out[out$type == "cat" | out$level == MISSING_LEVEL, ]
  expect_equal(prop$sd, sqrt(prop$est * (1 - prop$est)))
  # a wrong entered value is ignored, not propagated
  s2 <- toy$sld
  s2$sld_sd[s2$var_type == "cat"] <- 99
  expect_equal(summarize_sld(s2, toy$metadata), out)
})

test_that("summarize_sld passes a continuous Missing row through", {
  out <- summarize_sld(toy$sld, toy$metadata)
  rows <- out[out$variable == "fac_egfr", ]
  expect_equal(rows$level, c(CONTINUOUS_LEVEL, MISSING_LEVEL))
  expect_equal(rows$est, c(85, 0.12))
})

test_that("summarize_sld drops variables not in metadata and follows metadata order", {
  s <- rbind(toy$sld[rev(seq_len(nrow(toy$sld))), ], data.frame(
    var_name = "fac_extra", var_type = "con", var_level = NA_character_,
    sld_n = 100, sld_est = 1, sld_sd = 1
  ))
  out <- summarize_sld(s, toy$metadata)
  expect_false("fac_extra" %in% out$variable)
  expect_identical(unique(out$variable), toy$metadata$variable)
})

# both sides -------------------------------------------------------------------

test_that("IPD and SLD summaries share keys for every variable", {
  ipd_s <- summarize_ipd(toy$ipd, toy$metadata)
  sld_s <- summarize_sld(toy$sld, toy$metadata)
  expect_identical(unique(ipd_s$variable), unique(sld_s$variable))
  for (v in toy$metadata$variable[toy$metadata$type == "con"]) {
    expect_true(CONTINUOUS_LEVEL %in% ipd_s$level[ipd_s$variable == v])
    expect_true(CONTINUOUS_LEVEL %in% sld_s$level[sld_s$variable == v])
  }
})
