test_that("toy metadata is valid and returned unchanged", {
  expect_identical(validate_metadata(toy$metadata), toy$metadata)
})

test_that("missing required column fails", {
  m <- toy$metadata
  m$match <- NULL
  expect_error(validate_metadata(m), "missing required column.*match")
})

test_that("invalid type is rejected", {
  m <- toy$metadata
  m$type[1] <- "continuous"
  expect_error(validate_metadata(m), "`type` has invalid value")
})

test_that("NA in logical flag is rejected", {
  m <- toy$metadata
  m$adjust[2] <- NA
  expect_error(validate_metadata(m), "`adjust` contains NA")
})

test_that("duplicate variable and display_order are rejected together", {
  m <- toy$metadata
  m$variable[2] <- m$variable[1]
  m$display_order[2] <- m$display_order[1]
  expect_error(validate_metadata(m), "2 problem\\(s\\)")
})

test_that("categorical needs level_order, continuous must not have one", {
  m <- toy$metadata
  m$level_order[1] <- list(NULL)
  expect_error(validate_metadata(m), "level_order.*empty for categorical")

  m <- toy$metadata
  m$level_order[[7]] <- c("a", "b")
  expect_error(validate_metadata(m), "must be empty for continuous")
})

test_that("match_sd is only allowed on matched continuous variables", {
  m <- toy$metadata
  m$match_sd[m$variable == "fac_sev_hb"] <- TRUE
  expect_error(validate_metadata(m), "match_sd. must be FALSE for categorical.*fac_sev_hb")

  m <- toy$metadata
  m$match_sd[m$variable == "fac_weight"] <- TRUE   # fac_weight is in neither tier
  expect_error(validate_metadata(m), "match_sd. requires .match. or .adjust. = TRUE for: fac_weight")

  m <- toy$metadata
  m$match_sd[m$variable == "fac_bmi"] <- TRUE      # adjust tier, con: allowed
  expect_silent(validate_metadata(m))
})

test_that("a variable cannot be in both weighting tiers", {
  m <- toy$metadata
  m$adjust[m$variable == "fac_age"] <- TRUE        # already match = TRUE
  expect_error(validate_metadata(m), "both `match` and `adjust` are TRUE for: fac_age")
})

test_that("reserved Missing level cannot appear in level_order", {
  m <- toy$metadata
  m$level_order[[2]] <- c("No", "Yes", MISSING_LEVEL)
  expect_error(validate_metadata(m), "reserved level")
})
