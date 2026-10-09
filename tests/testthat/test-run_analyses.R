inputs_una <- make_toy_analysis_inputs("unanchored", toy)
inputs_anc <- make_toy_analysis_inputs("anchored", toy)

out_una <- run_analyses_unanchored(inputs_una)

test_that("unanchored: one independent analysis per input, each with sequential and univariate", {
  expect_named(out_una, c("analyses", "sequential", "univariate", "errors"))
  expect_equal(names(out_una$analyses), vapply(inputs_una, `[[`, "", "analysis_name"))
  expect_equal(nrow(out_una$errors), 0)
  a1 <- out_una$analyses[[1]]
  expect_named(a1, c("sequential", "univariate"))
  expect_equal(nrow(a1$sequential$results), length(inputs_una[[1]]$adjust_order) + 1)
  expect_equal(a1$sequential$results$label[1], "(none)")
  expect_equal(nrow(a1$univariate$results), length(inputs_una[[1]]$adjust_order))
})

test_that("unanchored: stacked tables carry analysis, population, comparator", {
  expect_equal(names(out_una$sequential)[1:3], c("analysis", "population", "comparator"))
  expect_setequal(unique(out_una$sequential$analysis), names(out_una$analyses))
  expect_equal(nrow(out_una$sequential), sum(vapply(out_una$analyses, function(a) nrow(a$sequential$results), 1L)))
  expect_true(all(out_una$sequential$contrast == UNANCHORED_CONTRAST))
})

test_that("unanchored: adjust_order is honoured and defaults to metadata order", {
  expect_equal(out_una$analyses[[1]]$sequential$results$variables[2], "fac_age")
  default_order <- inputs_una[[2]]$metadata$variable[inputs_una[[2]]$metadata$adjust]
  expect_equal(out_una$analyses[[2]]$sequential$results$variables[2], default_order[1])
})

test_that("unanchored: each analysis equals a direct run on its own inputs", {
  a <- inputs_una[[3]]
  direct <- run_scenarios_unanchored(define_scenarios_univariate(a$metadata, a$metadata$variable[a$metadata$adjust]),
                                     a$ipd, a$sld, a$outcome, a$sld_outcome)
  expect_equal(out_una$analyses[[3]]$univariate$results$estimate, direct$results$estimate)
})

test_that("anchored: per-analysis arms, contrasts labelled", {
  out <- run_analyses_anchored(inputs_anc)
  expect_equal(nrow(out$errors), 0)
  expect_true(all(out$sequential$contrast == "B vs A"))
  expect_equal(out$analyses[[2]]$sequential$results$variables[2], "fac_egfr")   # adjust_order
})

test_that("an analysis that cannot start is recorded with its stage; the others run", {
  bad <- inputs_una
  bad[[2]]$sld_outcome <- define_sld_outcome("Response", "log_or", 0.4, se = 0.26)   # anchored result
  bad[[3]]$ipd$fac_age <- NULL                                                       # invalid IPD
  out <- run_analyses_unanchored(bad)
  expect_equal(nrow(out$errors), 2)
  expect_equal(out$errors$analysis, c("Response, population B", "Score change, population A"))
  expect_equal(out$errors$stage, c("inputs", "inputs"))
  expect_match(out$errors$message[1], "use run_analyses_anchored")
  expect_match(out$errors$message[2], "absent from IPD: fac_age")
  expect_equal(names(out$analyses)[vapply(out$analyses, function(a) is.null(a$error), logical(1))],
               "Response, population A")
  expect_equal(unique(out$sequential$analysis), "Response, population A")
})

test_that("anchored runner refuses an unanchored input and records it", {
  mixed <- inputs_anc
  mixed[[1]]$sld_outcome <- sld_outcome_from_proportion("Response", 0.4, 100)
  out <- run_analyses_anchored(mixed)
  expect_equal(out$errors$analysis, "Response, B vs A")
  expect_match(out$errors$message, "use run_analyses_unanchored")
  expect_equal(nrow(out$errors), 1)
})

test_that("inputs need unique names; output_dir triggers the export layout", {
  dup <- inputs_una[c(1, 1)]
  expect_error(run_analyses_unanchored(dup), "unique `analysis_name`")
  dir <- tempfile("multi")
  out <- run_analyses_unanchored(inputs_una[1], output_dir = dir)
  expect_equal(nrow(out$errors), 0)
  expect_true(file.exists(file.path(dir, "response_population_a", "sequential", "results.csv")))
  expect_true(file.exists(file.path(dir, "response_population_a", "univariate", "balance_path.csv")))
  expect_true(file.exists(file.path(dir, "response_population_a", "sequential", "weights", "01_none.png")))
})
