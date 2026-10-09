# define_scenarios_* -----------------------------------------------------------

# Prognostic (adjust-tier) variables in the toy metadata, in a chosen order.
order_adj <- c("fac_prior_treatment", "fac_bmi", "fac_egfr")
primary   <- toy$metadata$variable[toy$metadata$match]

test_that("sequential: scenario k switches on the first k adjust variables, primary set untouched", {
  sc <- define_scenarios_sequential(toy$metadata, order_adj)
  expect_length(sc, 3)
  for (k in 1:3) {
    m <- sc[[k]]$metadata
    expect_equal(sc[[k]]$variables, order_adj[1:k])
    expect_setequal(m$variable[m$adjust], order_adj[1:k])
    expect_setequal(m$variable[m$match], primary)
    expect_equal(sc[[k]]$flag, "adjust")
    expect_silent(validate_metadata(m))
  }
  expect_equal(vapply(sc, `[[`, character(1), "label"), c("+ fac_prior_treatment", "+ fac_bmi", "+ fac_egfr"))
})

test_that("sequential: include_empty prepends the primary-set-only scenario", {
  sc <- define_scenarios_sequential(toy$metadata, order_adj, include_empty = TRUE)
  expect_length(sc, 4)
  expect_equal(sc[[1]]$label, "(none)")
  expect_false(any(sc[[1]]$metadata$adjust))
  expect_setequal(sc[[1]]$metadata$variable[sc[[1]]$metadata$match], primary)
})

test_that("univariate: one adjust variable on per scenario", {
  sc <- define_scenarios_univariate(toy$metadata, order_adj)
  expect_length(sc, 3)
  for (k in 1:3) {
    expect_equal(sc[[k]]$metadata$variable[sc[[k]]$metadata$adjust], order_adj[k])
    expect_equal(sc[[k]]$label, order_adj[k])
  }
})

test_that("match flag: match_sd follows the variable out of the weighting set", {
  sc <- define_scenarios_univariate(toy$metadata, c("fac_sev_hb", "fac_age"), flag = "match")
  m1 <- sc[[1]]$metadata   # fac_sev_hb only; fac_age had match_sd = TRUE
  expect_false(m1$match_sd[m1$variable == "fac_age"])
  expect_equal(m1$variable[m1$match], "fac_sev_hb")
  m2 <- sc[[2]]$metadata
  expect_true(m2$match_sd[m2$variable == "fac_age"])  # kept when fac_age is matched
  for (s in sc) expect_silent(validate_metadata(s$metadata))
})

test_that("scenario definitions leave other columns untouched", {
  sc <- define_scenarios_sequential(toy$metadata, order_adj)
  keep <- setdiff(names(toy$metadata), c("adjust", "match_sd"))
  expect_identical(sc[[2]]$metadata[keep], toy$metadata[keep])
})

test_that("bad variable lists are rejected, including variables from the other tier", {
  expect_error(define_scenarios_sequential(toy$metadata, c("fac_bmi", "nope")), "not in metadata: nope")
  expect_error(define_scenarios_univariate(toy$metadata, c("fac_bmi", "fac_bmi")), "must be unique")
  expect_error(define_scenarios_sequential(toy$metadata, "fac_bmi", flag = "show_balance"))
  expect_error(define_scenarios_sequential(toy$metadata, c("fac_age", "fac_bmi")),
               "already in the `match` tier: fac_age")
  expect_error(define_scenarios_univariate(toy$metadata, "fac_bmi", flag = "match"),
               "already in the `adjust` tier: fac_bmi")
})
