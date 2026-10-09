resp     <- define_outcome("Response", "binary", "y_resp")
resp_una <- sld_outcome_from_proportion("Response", p = 0.40, n = 100)
resp_anc <- define_sld_outcome("Response", "log_or", estimate = log(1.5), se = 0.26)
arm_a    <- toy$ipd[toy$ipd$ARM == "A", setdiff(names(toy$ipd), "ARM")]

m <- toy$metadata
m$match <- FALSE
m$match_sd <- FALSE
m$adjust <- m$variable %in% c("fac_sev_hb", "fac_age", "fac_color_sld_extra", "fac_bmi")
sc  <- define_scenarios_sequential(m, c("fac_sev_hb", "fac_age", "fac_color_sld_extra", "fac_bmi"),
                                   include_empty = TRUE)
out <- run_scenarios_unanchored(sc, arm_a, toy$sld, resp, resp_una)

test_that("every run element carries the specification needed to replay it", {
  for (r in out$runs) {
    expect_true(all(c("kind", "scenario", "outcome", "sld_outcome", "include_adjust", "arm", "reference_arm",
                      "conf_level") %in% names(r)))
    expect_equal(r$kind, "unanchored")
    expect_true(r$include_adjust)
    expect_null(r$arm)
  }
  expect_true(all(out$results$include_adjust))
})

test_that("replaying a failed step through the main function raises the recorded error", {
  expect_equal(out$results$status[4], "error")
  expect_error(replay_scenario(out, 4, arm_a, toy$sld), "fac_color_sld_extra.*absent from IPD")
  err <- tryCatch(replay_scenario(out, 4, arm_a, toy$sld), error = conditionMessage)
  expect_equal(err, out$results$error[4])
})

test_that("replaying a successful step reproduces its estimate; options can be changed", {
  rep3 <- replay_scenario(out, 3, arm_a, toy$sld)
  expect_equal(rep3$result$estimate, out$results$estimate[3])
  expect_equal(rep3$include_adjust, TRUE)
  rep3b <- replay_scenario(out, 3, arm_a, toy$sld, include_adjust = FALSE)   # override: naive
  expect_false(rep3b$weighted)
})

test_that("scenario_replay_code prints runnable code that reproduces the step", {
  code <- utils::capture.output(scenario_replay_code(out, 3, ipd = "arm_a", sld = "toy$sld"))
  code <- paste(code, collapse = "\n")
  expect_match(code, "# Scenario 3: \\+ fac_age")
  expect_match(code, "weighting set: fac_sev_hb, fac_age")
  expect_match(code, "step_metadata <- out\\$runs\\[\\[3\\]\\]\\$scenario\\$metadata")
  expect_match(code,
               "run_maic_unanchored\\(arm_a, toy\\$sld, step_metadata, outcome, sld_outcome, include_adjust = TRUE\\)")
  replayed <- eval(parse(text = code))
  expect_equal(replayed$result$estimate, out$results$estimate[3])

  failed_code <- paste(utils::capture.output(scenario_replay_code(out, 4, "arm_a", "toy$sld")), collapse = "\n")
  expect_match(failed_code, "# Error recorded: .*absent from IPD")
})

test_that("anchored replay code and replay carry the arms", {
  anc <- run_scenarios_anchored(define_scenarios_univariate(toy$metadata, "fac_bmi"), toy$ipd, toy$sld, resp, resp_anc,
                                arm = "ARM", reference_arm = "A")
  code <- paste(utils::capture.output(scenario_replay_code(anc, 1, "toy$ipd", "toy$sld", run_object = "anc")),
                collapse = "\n")
  expect_match(code, "run_maic_anchored\\(toy\\$ipd, toy\\$sld, step_metadata, outcome, sld_outcome, ")
  expect_match(code, "arm = \"ARM\", reference_arm = \"A\", include_adjust = TRUE\\)")   # adjust-flag list
  expect_equal(eval(parse(text = code))$result$estimate, anc$results$estimate[1])
  expect_equal(replay_scenario(anc, 1, toy$ipd, toy$sld)$result$estimate, anc$results$estimate[1])
})

test_that("failed_steps lists failed steps with context and the replay call", {
  f <- failed_steps(out, ipd = "arm_a", sld = "toy$sld")
  expect_equal(f$step, c(4L, 5L))
  expect_equal(f$label[1], "+ fac_color_sld_extra")
  expect_match(f$error[1], "absent from IPD")
  expect_match(f$replay[1], "^run_maic_unanchored\\(arm_a, toy\\$sld, step_metadata")
  expect_equal(nrow(failed_steps(run_scenarios_unanchored(sc[1:2], arm_a, toy$sld, resp, resp_una))), 0)
})

test_that("export manifest carries the replay call per step", {
  dir <- tempfile("replay")
  man <- export_scenarios(out, dir, dpi = 60)
  expect_true("replay" %in% names(man))
  expect_match(man$replay[4], "^run_maic_unanchored\\(")
})

test_that("step index is checked", {
  expect_error(replay_scenario(out, 99, arm_a, toy$sld))
  expect_error(scenario_replay_code(out, 0))
})
