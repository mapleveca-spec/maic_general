test_that("toy SLD is valid and returned unchanged", {
  expect_identical(validate_sld(toy$sld, toy$metadata), toy$sld)
})

test_that("missing required column fails", {
  s <- toy$sld
  s$sld_sd <- NULL
  expect_error(validate_sld(s, toy$metadata), "missing required column.*sld_sd")
})

test_that("study N must be constant and positive", {
  s <- toy$sld
  s$sld_n[1] <- 99
  expect_error(validate_sld(s, toy$metadata), "identical on every row")
})

test_that("continuous variable needs one row with NA level", {
  s <- toy$sld
  s$var_level[s$var_name == "fac_age"] <- "x"
  expect_error(validate_sld(s, toy$metadata), "exactly one summary row.*found 0")
  expect_error(validate_sld(s, toy$metadata), "unexpected var_level: x")

  s <- toy$sld
  s <- rbind(s, s[s$var_name == "fac_age", ])
  expect_error(validate_sld(s, toy$metadata), "exactly one summary row.*found 2")
})

test_that("continuous may carry one Missing row with a proportion", {
  expect_true(any(toy$sld$var_name == "fac_egfr" & toy$sld$var_level == MISSING_LEVEL))
  expect_silent(validate_sld(toy$sld, toy$metadata))

  s <- toy$sld
  s <- rbind(s, s[s$var_name == "fac_egfr" & s$var_level %in% MISSING_LEVEL, ])
  expect_error(validate_sld(s, toy$metadata), "more than one `Missing` row")

  s <- toy$sld
  s$sld_est[s$var_name == "fac_egfr" & s$var_level %in% MISSING_LEVEL] <- 1.5
  expect_error(validate_sld(s, toy$metadata), "proportion in \\[0, 1\\]")

  s <- toy$sld
  s$var_level[s$var_name == "fac_egfr" & s$var_level %in% MISSING_LEVEL] <- "Low"
  expect_error(validate_sld(s, toy$metadata), "unexpected var_level: Low")
})

test_that("continuous est and sd are NA together or not at all", {
  s <- toy$sld
  s$sld_sd[s$var_name == "fac_age"] <- NA
  expect_error(validate_sld(s, toy$metadata), "summary row must have both or neither")
})

test_that("categorical proportions must be in [0,1] and sum to 1 within tolerance", {
  s <- toy$sld
  s$sld_est[s$var_name == "fac_color_sld_extra" & s$var_level == "Blue"] <- 0.55
  expect_error(validate_sld(s, toy$metadata), "proportions sum to 1.1")
  expect_silent(validate_sld(s, toy$metadata, prop_tol = 0.2))

  s <- toy$sld
  s$sld_est[s$var_name == "fac_tar_jnt_lead"] <- c(1.5, -0.5)
  expect_error(validate_sld(s, toy$metadata), "outside \\[0, 1\\]")
})

test_that("duplicate categorical level is rejected", {
  s <- toy$sld
  s$var_level[s$var_name == "fac_tar_jnt_lead"] <- "Yes"
  expect_error(validate_sld(s, toy$metadata), "duplicate level")
})

test_that("every metadata variable must be present in SLD", {
  s <- toy$sld[toy$sld$var_name != "fac_weight", ]
  expect_error(validate_sld(s, toy$metadata), "absent from SLD: fac_weight")
})

test_that("extra SLD variables not in metadata are ignored", {
  s <- rbind(toy$sld, data.frame(
    var_name = "fac_extra", var_type = "con", var_level = NA_character_,
    sld_n = 100, sld_est = 1, sld_sd = 1
  ))
  expect_silent(validate_sld(s, toy$metadata))
})

test_that("var_type must agree with metadata", {
  s <- toy$sld
  s$var_type[s$var_name == "fac_age"] <- "cat"
  s$var_level[s$var_name == "fac_age"] <- "a"
  s$sld_est[s$var_name == "fac_age"] <- 1
  expect_error(validate_sld(s, toy$metadata), "`con` in metadata but `cat` in SLD")
})

test_that("SLD levels must be in level_order, except the reserved Missing level", {
  s <- toy$sld
  s$var_level[s$var_name == "fac_color_sld_extra" & s$var_level == "Yellow"] <- "Green"
  expect_error(validate_sld(s, toy$metadata), "not in metadata\\$level_order: Green")

  # Missing rows exist in the toy SLD and are accepted without being in level_order.
  expect_true(MISSING_LEVEL %in% toy$sld$var_level)
  expect_silent(validate_sld(toy$sld, toy$metadata))
})
