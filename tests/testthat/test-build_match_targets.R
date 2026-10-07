ipd_s <- summarize_ipd(toy$ipd, toy$metadata)
sld_s <- summarize_sld(toy$sld, toy$metadata)
primary <- build_match_targets(ipd_s, sld_s, toy$metadata)
full    <- build_match_targets(ipd_s, sld_s, toy$metadata, include_adjust = TRUE)

test_that("weighting_variables selects the primary set, or both tiers", {
  m <- toy$metadata
  expect_equal(m$variable[weighting_variables(m)], c("fac_sev_hb", "fac_tar_jnt_lead", "fac_age"))
  expect_equal(m$variable[weighting_variables(m, TRUE)],
               c("fac_sev_hb", "fac_tar_jnt_lead", "fac_prior_treatment", "fac_age", "fac_bmi", "fac_egfr"))
})

test_that("primary targets cover exactly the match = TRUE variables", {
  expect_identical(names(primary), TARGET_COLUMNS)
  expect_setequal(unique(primary$variable), toy$metadata$variable[toy$metadata$match])
  expect_true(all(primary$moment %in% MOMENTS))
  expect_false(anyDuplicated(primary$term) > 0)
})

test_that("include_adjust adds the adjust tier on top of the primary set", {
  expect_setequal(unique(full$variable), toy$metadata$variable[toy$metadata$match | toy$metadata$adjust])
  expect_true(all(primary$term %in% full$term))
  expect_equal(nrow(full), nrow(primary) + 4)  # prior_treatment (2), bmi (1), egfr (1)
})

test_that("continuous variables give one mean constraint by default", {
  r <- full[full$variable == "fac_bmi", ]
  expect_equal(nrow(r), 1)
  expect_equal(r$moment, "mean")
  expect_equal(r$term, "fac_bmi")
  expect_equal(r$target, 30)
})

test_that("match_sd adds a variance constraint with the SLD second moment as target", {
  r <- primary[primary$variable == "fac_age", ]
  expect_equal(r$moment, c("mean", "variance"))
  expect_equal(r$term, c("fac_age", "fac_age:variance"))
  expect_equal(r$target, c(50, 50^2 + 10^2))
  expect_equal(r$level, c(CONTINUOUS_LEVEL, CONTINUOUS_LEVEL))
})

test_that("categorical drops the first level_order level as reference", {
  r <- primary[primary$variable == "fac_sev_hb", ]
  expect_equal(r$level, c("Moderate", "Severe"))
  expect_equal(r$term, c("fac_sev_hb:Moderate", "fac_sev_hb:Severe"))
  expect_equal(r$target, c(0.30, 0.65))
  expect_equal(nrow(primary[primary$variable == "fac_tar_jnt_lead", ]), 1)
})

test_that("Missing rows never become targets; IPD-only missingness is irrelevant", {
  expect_false(MISSING_LEVEL %in% full$level)
  r <- full[full$variable == "fac_prior_treatment", ]
  expect_equal(r$level, c("Extended half-life", "Other"))
  expect_equal(r$sld_missing, c(0, 0))
})

test_that("SLD missing mass is rescaled out and recorded", {
  m <- toy$metadata
  m$match <- m$variable == "fac_color_missing_both"   # Blue .5, Red .4, Missing .1
  m$adjust <- FALSE
  m$match_sd <- FALSE
  r <- build_match_targets(ipd_s, sld_s, m)
  expect_equal(r$level, "Red")
  expect_equal(r$target, 0.4 / 0.9)
  expect_equal(r$sld_missing, 0.1)
})

test_that("variables follow display_order", {
  m <- toy$metadata
  m$display_order <- rev(m$display_order)
  r <- build_match_targets(ipd_s, sld_s, m)
  expect_identical(unique(r$variable), rev(toy$metadata$variable[toy$metadata$match]))
})

test_that("unreported SLD variable cannot be matched", {
  m <- toy$metadata
  m$match[m$variable == "fac_weight"] <- TRUE
  expect_error(build_match_targets(ipd_s, sld_s, m), "unreported variable\\(s\\): fac_weight")
})

test_that("SLD level absent from IPD is infeasible", {
  m <- toy$metadata
  m$match <- m$variable == "fac_color_sld_extra"   # Yellow: IPD 0, SLD .15
  m$match_sd <- FALSE
  expect_error(build_match_targets(ipd_s, sld_s, m), "absent from IPD.*Yellow")
})

test_that("zero SLD proportion for an IPD level is rejected", {
  m <- toy$metadata
  m$match <- m$variable == "fac_color_ipd_extra"    # Red: IPD .47, SLD 0
  m$match_sd <- FALSE
  expect_error(build_match_targets(ipd_s, sld_s, m), "zero their weights: Red")
})

test_that("an empty weighting set is an error that names the tiers considered", {
  m <- toy$metadata
  m$match <- FALSE
  m$match_sd <- FALSE
  expect_error(build_match_targets(ipd_s, sld_s, m), "match = TRUE\\.$")
  m$adjust <- FALSE
  expect_error(build_match_targets(ipd_s, sld_s, m, include_adjust = TRUE), "match = TRUE or adjust = TRUE")
})
