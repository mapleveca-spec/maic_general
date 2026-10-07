ipd_s <- summarize_ipd(toy$ipd, toy$metadata)
sld_s <- summarize_sld(toy$sld, toy$metadata)

test_that("create_balance_table equals align + smd when all variables are shown", {
  expect_equal(
    create_balance_table(ipd_s, sld_s, toy$metadata),
    add_smd(align_summaries(ipd_s, sld_s, toy$metadata))
  )
})

test_that("show_balance = FALSE drops the variable, nothing else changes", {
  m <- toy$metadata
  m$show_balance[m$variable == "fac_bmi"] <- FALSE
  out <- create_balance_table(ipd_s, sld_s, m)
  expect_false("fac_bmi" %in% out$variable)
  expect_identical(names(out), BALANCE_COLUMNS)
  full <- create_balance_table(ipd_s, sld_s, toy$metadata)
  expect_equal(out, full[full$variable != "fac_bmi", ], ignore_attr = TRUE)
})

test_that("display_order governs row order, not metadata row order", {
  m <- toy$metadata
  m$display_order <- rev(m$display_order)
  out <- create_balance_table(ipd_s, sld_s, m)
  expect_identical(unique(out$variable), rev(toy$metadata$variable))
  expect_false(is.unsorted(out$display_order))
})

test_that("summaries with extra variables are ignored, only metadata drives rows", {
  extra <- rbind(ipd_s, summarize_continuous(toy$ipd$y_time, "y_time"))
  out <- create_balance_table(extra, sld_s, toy$metadata)
  expect_false("y_time" %in% out$variable)
})

test_that("fails loudly when nothing is shown", {
  m <- toy$metadata
  m$show_balance <- FALSE
  expect_error(create_balance_table(ipd_s, sld_s, m), "show_balance = TRUE")
})
