resp     <- define_outcome("Response", "binary", "y_resp")
resp_una <- sld_outcome_from_proportion("Response", p = 0.40, n = 100)
m <- toy$metadata
m$match <- FALSE
m$adjust <- m$variable %in% c("fac_sev_hb", "fac_age", "fac_bmi")
m$match_sd <- m$variable == "fac_age"
sc  <- define_scenarios_sequential(m, c("fac_age", "fac_sev_hb", "fac_bmi"), include_empty = TRUE)
out <- run_scenarios_unanchored(sc, toy$ipd[toy$ipd$ARM == "A", ], toy$sld, resp, resp_una)

test_that("slugify makes ordered, file-safe names", {
  expect_equal(slugify(c("+ fac_age", "(none)", "Score change", "  A / B  ")),
               c("fac_age", "none", "score_change", "a_b"))
  expect_equal(slugify(""), "none")
})

test_that("balance path: one row per balance row; SLD, unweighted pair, then an IPD | SMD pair per step", {
  path <- scenario_balance_path(out)
  expect_equal(nrow(path), nrow(out$runs[[1]]$balance_before))
  expected <- c("Variable", "Level", "SLD", "Unweighted | IPD", "Unweighted | SMD",
                as.vector(rbind(paste(out$results$label, "| IPD"), paste(out$results$label, "| SMD"))))
  expect_equal(names(path), expected)
  # the naive step reproduces the unweighted pair
  expect_equal(path[["(none) | IPD"]], path[["Unweighted | IPD"]])
  expect_equal(path[["(none) | SMD"]], path[["Unweighted | SMD"]])
})

test_that("balance path: matched terms reach the SLD value and SMD 0 once added", {
  path <- scenario_balance_path(out)
  age <- path[path$Variable == "fac_age" & path$Level == "", ]
  expect_equal(age$SLD, "50.0 (10.0)")
  expect_match(age[["+ fac_age | IPD"]], "^50\\.0 \\(")
  expect_equal(age[["+ fac_age | SMD"]], "0.000")
  expect_equal(age[["+ fac_bmi | SMD"]], "0.000")
  expect_false(age[["Unweighted | SMD"]] == "0.000")
  bmi <- path[path$Variable == "fac_bmi" & path$Level == "", ]
  expect_false(bmi[["+ fac_age | SMD"]] == "0.000")
  expect_equal(bmi[["+ fac_bmi | SMD"]], "0.000")
  expect_match(bmi[["+ fac_bmi | IPD"]], "^30\\.0 \\(")
})

test_that("balance path refuses runs with different balance rows", {
  fake <- out
  fake$runs[[2]]$balance_after <- fake$runs[[2]]$balance_after[-1, ]
  expect_error(scenario_balance_path(fake), "do not share balance rows")
})

test_that("export_scenarios writes the fixed layout and a manifest", {
  dir <- file.path(tempfile("scen"), "sequential")
  man <- export_scenarios(out, dir, dpi = 60)
  expect_equal(nrow(man), length(sc))
  expect_true(all(file.exists(man$weights)))
  expect_equal(basename(man$weights), c("01_none.png", "02_fac_age.png", "03_fac_sev_hb.png", "04_fac_bmi.png"))
  expect_equal(man$weighted, c(FALSE, TRUE, TRUE, TRUE))
  for (f in c("results.csv", "results_numeric.csv", "balance_path.csv", "manifest.csv")) {
    expect_true(file.exists(file.path(dir, f)), label = f)
  }
  res <- read.csv(file.path(dir, "results.csv"), check.names = FALSE)
  expect_equal(nrow(res), length(sc))
  expect_true("Weight max" %in% names(res))
  path <- read.csv(file.path(dir, "balance_path.csv"), check.names = FALSE)
  expect_equal(ncol(path), 5 + 2 * length(sc))
})
