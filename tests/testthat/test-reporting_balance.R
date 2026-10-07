res <- run_maic_weighting(toy$ipd, toy$sld, toy$metadata)
cmp <- compare_balance_tables(res$balance_before, res$balance_after)

# compare_balance_tables -------------------------------------------------------

test_that("comparison keeps keys and SLD once, IPD twice with suffixes", {
  expect_equal(nrow(cmp), nrow(res$balance_before))
  expect_identical(cmp[BALANCE_KEYS], res$balance_before[BALANCE_KEYS])
  expect_equal(cmp$sld_est, res$balance_before$sld_est)
  expect_equal(cmp$ipd_est_before, res$balance_before$ipd_est)
  expect_equal(cmp$ipd_est_after, res$balance_after$ipd_est)
  expect_equal(cmp$smd_before, res$balance_before$smd)
  expect_equal(cmp$smd_after, res$balance_after$smd)
})

test_that("comparison refuses mismatched rows or differing SLD sides", {
  expect_error(compare_balance_tables(res$balance_before, res$balance_after[-1, ]), "identical rows")
  a <- res$balance_after
  a$sld_est[1] <- a$sld_est[1] + 0.1
  expect_error(compare_balance_tables(res$balance_before, a), "SLD columns differ")
})

# format_cell / format_smd -----------------------------------------------------

test_that("cells are formatted by row_type", {
  expect_equal(format_cell("summary", 49.4033, 10.997), "49.4 (11.0)")
  expect_equal(format_cell("level", 0.4667, 0.499), "46.7%")
  expect_equal(format_cell("missing", 0.0833, 0.276), "8.3%")
  expect_equal(format_cell(c("summary", "level"), c(NA, 0.5), c(NA, 0.5)), c("NR", "50.0%"))
  expect_equal(format_cell("summary", 1.25, 0.5, digits = 2), "1.25 (0.50)")
})

test_that("SMDs format with digits, NA, and infinities", {
  expect_equal(format_smd(c(0.3381, -0.5537, NA, Inf, -Inf)), c("0.338", "-0.554", "NR", "Inf", "-Inf"))
  expect_equal(format_smd(0.12345, digits = 2), "0.12")
})

# table formatters -------------------------------------------------------------

test_that("format_balance_table has one row per balance row and labels each variable once", {
  tab <- format_balance_table(res$balance_before)
  expect_named(tab, c("Variable", "Level", "SLD", "IPD", "SMD"))
  expect_equal(nrow(tab), nrow(res$balance_before))
  expect_equal(sum(nzchar(tab$Variable)), length(unique(res$balance_before$variable)))
  expect_equal(tab$Variable[1], res$balance_before$variable[1])
  expect_equal(tab$Variable[2], "")
})

test_that("format_balance_comparison renders unreported SLD and matched zeros as expected", {
  tab <- format_balance_comparison(cmp)
  expect_named(tab, c("Variable", "Level", "SLD", "IPD before", "IPD after", "SMD before", "SMD after"))
  weight <- tab[cmp$variable == "fac_weight", ]
  expect_equal(weight$SLD, "NR")
  expect_equal(weight$`SMD after`, "NR")
  age <- tab[cmp$variable == "fac_age", ]
  expect_equal(age$SLD, "50.0 (10.0)")
  expect_equal(age$`SMD after`, "0.000")
  expect_equal(age$Level, CONTINUOUS_LEVEL)
  miss <- tab[cmp$variable == "fac_bmi" & cmp$row_type == "missing", ]
  expect_equal(miss$Level, MISSING_LEVEL)
  expect_match(miss$`IPD before`, "%$")
})
