out <- run_maic_outcomes(toy$ipd, toy$sld, toy$metadata, toy$outcomes, toy$sld_outcomes,
                         arm = "ARM", reference_arm = "A", intervention_arm = "A")

test_that("one independent analysis per published result, named by outcome and kind", {
  expect_named(out, c("analyses", "summary"))
  expect_length(out$analyses, nrow(toy$sld_outcomes))
  expect_equal(names(out$analyses), c("Response [anchored]", "OS [anchored]", "Score change [anchored]",
                                      "Response [unanchored]", "Score change [unanchored]"))
  expect_equal(nrow(out$summary), nrow(toy$sld_outcomes))
  expect_equal(out$summary$outcome, toy$sld_outcomes$name)
  expect_equal(out$summary$anchored, toy$sld_outcomes$anchored)
})

test_that("each analysis equals a direct single-outcome run", {
  specs <- outcome_specs(toy$outcomes)
  for (s in sld_outcome_specs(toy$sld_outcomes)) {
    key <- paste0(s$name, " [", if (s$anchored) "anchored" else "unanchored", "]")
    direct <- run_maic_analysis(toy$ipd, toy$sld, toy$metadata, specs[[s$name]], s,
                                arm = "ARM", reference_arm = "A", intervention_arm = "A")
    expect_equal(out$analyses[[key]]$result, direct$result, label = key)
  }
})

test_that("anchored and unanchored analyses use different rows and contrasts", {
  expect_equal(out$analyses[["Response [anchored]"]]$result$contrast, "B vs A")
  expect_equal(out$analyses[["Response [unanchored]"]]$result$contrast, "A (unanchored)")
  expect_length(out$analyses[["Response [anchored]"]]$weights, nrow(toy$ipd))
  expect_length(out$analyses[["Response [unanchored]"]]$weights, sum(toy$ipd$ARM == "A"))
})

test_that("summary is a stack of the separate one-row results", {
  stacked <- dplyr::bind_rows(lapply(out$analyses, `[[`, "result"))
  expect_equal(out$summary, stacked)
  expect_true(all(c("contrast", "n", "ess") %in% names(out$summary)))
})

test_that("table validation happens first; arm requirements come from the rows present", {
  bad <- toy$sld_outcomes
  bad$scale[1] <- "log_hr"
  expect_error(run_maic_outcomes(toy$ipd, toy$sld, toy$metadata, toy$outcomes, bad, arm = "ARM", reference_arm = "A"),
               "Invalid sld_outcomes")
  anchored_only <- toy$sld_outcomes[toy$sld_outcomes$anchored, ]
  r <- run_maic_outcomes(toy$ipd, toy$sld, toy$metadata, toy$outcomes, anchored_only, arm = "ARM", reference_arm = "A")
  expect_equal(nrow(r$summary), 3)
  expect_error(run_maic_outcomes(toy$ipd, toy$sld, toy$metadata, toy$outcomes, toy$sld_outcomes,
                                 arm = "ARM", reference_arm = "A"), "needs `intervention_arm`")
})

test_that("options pass through to every analysis", {
  r <- run_maic_outcomes(toy$ipd, toy$sld, toy$metadata, toy$outcomes, toy$sld_outcomes[1:2, ],
                         arm = "ARM", reference_arm = "A", include_adjust = TRUE)
  expect_true(all(vapply(r$analyses, `[[`, logical(1), "include_adjust")))
})
