test_that("every toy metadata variable has an IPD column", {
  expect_true(all(toy$metadata$variable %in% names(toy$ipd)))
})

test_that("toy IPD is valid and returned unchanged", {
  expect_identical(validate_ipd(toy$ipd, toy$metadata), toy$ipd)
})

test_that("metadata variable absent from IPD fails", {
  d <- toy$ipd
  d$fac_bmi <- NULL
  expect_error(validate_ipd(d, toy$metadata), "absent from IPD: fac_bmi")
})

test_that("continuous variable must be numeric", {
  d <- toy$ipd
  d$fac_age <- as.character(d$fac_age)
  expect_error(validate_ipd(d, toy$metadata), "Continuous `fac_age` must be numeric")
})

test_that("categorical variable must be factor or character", {
  d <- toy$ipd
  d$fac_tar_jnt_lead <- as.integer(d$fac_tar_jnt_lead)
  expect_error(validate_ipd(d, toy$metadata), "must be factor or character")
})

test_that("character categorical is accepted", {
  d <- toy$ipd
  d$fac_sev_hb <- as.character(d$fac_sev_hb)
  expect_silent(validate_ipd(d, toy$metadata))
})

test_that("IPD level not in level_order is rejected", {
  d <- toy$ipd
  d$fac_color_ipd_extra <- as.character(d$fac_color_ipd_extra)
  d$fac_color_ipd_extra[1] <- "Green"
  expect_error(validate_ipd(d, toy$metadata), "not in metadata\\$level_order: Green")
})

test_that("literal Missing value is rejected, NA is accepted", {
  d <- toy$ipd
  d$fac_prior_treatment <- as.character(d$fac_prior_treatment)
  d$fac_prior_treatment[1] <- MISSING_LEVEL
  expect_error(validate_ipd(d, toy$metadata), "reserved value `Missing`")

  expect_true(anyNA(toy$ipd$fac_prior_treatment))
  expect_silent(validate_ipd(toy$ipd, toy$metadata))
})

test_that("unused factor levels not in level_order do not fail", {
  d <- toy$ipd
  d$fac_tar_jnt_lead <- factor(d$fac_tar_jnt_lead, levels = c("No", "Yes", "Unused"))
  expect_silent(validate_ipd(d, toy$metadata))
})

test_that("all problems are reported together", {
  d <- toy$ipd
  d$fac_age <- as.character(d$fac_age)
  d$fac_bmi <- NULL
  expect_error(validate_ipd(d, toy$metadata), "2 problem\\(s\\)")
})
