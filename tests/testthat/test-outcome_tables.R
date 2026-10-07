# validate_outcomes ------------------------------------------------------------

test_that("toy outcomes table is valid and returned unchanged", {
  expect_identical(validate_outcomes(toy$outcomes), toy$outcomes)
})

test_that("outcomes: structural errors", {
  expect_error(validate_outcomes(toy$outcomes[, -3]), "missing required column.*var")
  expect_error(validate_outcomes(toy$outcomes[0, ]), "no rows")
})

test_that("outcomes: duplicate names, bad types, event rules", {
  o <- toy$outcomes
  o$name[2] <- o$name[1]
  expect_error(validate_outcomes(o), "duplicates: Response")

  o <- toy$outcomes
  o$type[1] <- "ordinal"
  expect_error(validate_outcomes(o), "invalid value\\(s\\): ordinal")

  o <- toy$outcomes
  o$event[o$type == "tte"] <- NA
  expect_error(validate_outcomes(o), "tte outcome\\(s\\) need an `event`.*OS")

  o <- toy$outcomes
  o$event[o$type == "binary"] <- "y_event"
  expect_error(validate_outcomes(o), "must be NA for non-tte.*Response")
})

# validate_sld_outcomes --------------------------------------------------------

test_that("toy sld_outcomes table is valid and returned unchanged", {
  expect_identical(validate_sld_outcomes(toy$sld_outcomes, toy$outcomes), toy$sld_outcomes)
})

test_that("sld_outcomes: unknown name, duplicate key, bad se", {
  s <- toy$sld_outcomes
  s$name[1] <- "PFS"
  expect_error(validate_sld_outcomes(s, toy$outcomes), "not in outcomes: PFS")

  s <- toy$sld_outcomes
  s <- rbind(s, s[1, ])
  expect_error(validate_sld_outcomes(s, toy$outcomes), "Duplicate \\(name, anchored\\).*Response TRUE")

  s <- toy$sld_outcomes
  s$se[1] <- 0
  expect_error(validate_sld_outcomes(s, toy$outcomes), "`se` must be numeric and positive")
})

test_that("sld_outcomes: scale must match the outcome type and comparison kind", {
  s <- toy$sld_outcomes
  s$scale[s$name == "OS"] <- "log_or"
  expect_error(validate_sld_outcomes(s, toy$outcomes),
               "`OS` \\(anchored = TRUE\\) must be on scale `log_hr`, got `log_or`")

  s <- toy$sld_outcomes
  s$scale[s$name == "Response" & !s$anchored] <- "log_or"
  expect_error(validate_sld_outcomes(s, toy$outcomes), "anchored = FALSE\\) must be on scale `logit_p`")

  s <- rbind(toy$sld_outcomes, data.frame(name = "OS", anchored = FALSE, scale = "log_hr", estimate = 0, se = 1))
  expect_error(validate_sld_outcomes(s, toy$outcomes), "unanchored tte comparison is not supported")
})

# converters -------------------------------------------------------------------

test_that("outcome_specs gives one validated spec per row, keyed by name", {
  specs <- outcome_specs(toy$outcomes)
  expect_named(specs, toy$outcomes$name)
  expect_true(all(vapply(specs, inherits, logical(1), "maic_outcome")))
  expect_equal(specs$OS$event, "y_event")
  expect_null(specs$Response$event)
})

test_that("sld_outcome_specs gives one spec per row with anchored derived from the scale", {
  specs <- sld_outcome_specs(toy$sld_outcomes)
  expect_length(specs, nrow(toy$sld_outcomes))
  expect_true(all(vapply(specs, inherits, logical(1), "maic_sld_outcome")))
  expect_equal(vapply(specs, `[[`, logical(1), "anchored"), toy$sld_outcomes$anchored)
  expect_equal(specs[[1]]$estimate, toy$sld_outcomes$estimate[1])

  s <- toy$sld_outcomes
  s$anchored[1] <- FALSE   # disagrees with log_or
  expect_error(sld_outcome_specs(s), "`anchored` = FALSE disagrees with scale `log_or`")
})

test_that("every spec pair passes check_outcome_pair", {
  specs <- outcome_specs(toy$outcomes)
  for (s in sld_outcome_specs(toy$sld_outcomes)) expect_true(check_outcome_pair(specs[[s$name]], s))
})

test_that("estimate_scale is the single source of truth for scales", {
  expect_equal(estimate_scale("binary", TRUE), "log_or")
  expect_equal(estimate_scale("binary", FALSE), "logit_p")
  expect_equal(estimate_scale("tte", TRUE), "log_hr")
  expect_true(is.na(estimate_scale("tte", FALSE)))
  expect_equal(estimate_scale("continuous", FALSE), "mean")
  expect_setequal(ESTIMATE_SCALES, c(ANCHORED_SCALES, UNANCHORED_SCALES))
})
