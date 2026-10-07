ipd_s   <- summarize_ipd(toy$ipd, toy$metadata)
sld_s   <- summarize_sld(toy$sld, toy$metadata)
targets <- build_match_targets(ipd_s, sld_s, toy$metadata, include_adjust = TRUE)
dm      <- build_design_matrix(toy$ipd, targets)

test_that("shape: one column per target, one row per complete case", {
  expect_identical(colnames(dm$X), targets$term)
  expect_length(dm$complete, nrow(toy$ipd))
  expect_equal(nrow(dm$X), sum(dm$complete))
  expect_false(anyNA(dm$X))
})

test_that("complete cases are rows without NA in any matched variable", {
  matched <- toy$metadata$variable[weighting_variables(toy$metadata, TRUE)]
  expect_identical(dm$complete, complete.cases(toy$ipd[matched]))
  # NA sits in fac_prior_treatment (5) and fac_bmi (7); union may overlap
  expect_equal(sum(!dm$complete), sum(is.na(toy$ipd$fac_prior_treatment) | is.na(toy$ipd$fac_bmi)))
})

test_that("mean columns are x minus target", {
  cc <- toy$ipd[dm$complete, ]
  expect_equal(unname(dm$X[, "fac_age"]), cc$fac_age - 50)
  expect_equal(unname(dm$X[, "fac_egfr"]), cc$fac_egfr - 85)
})

test_that("variance columns are x^2 minus the second-moment target", {
  cc <- toy$ipd[dm$complete, ]
  expect_equal(unname(dm$X[, "fac_age:variance"]), cc$fac_age^2 - (50^2 + 10^2))
})

test_that("variance target outside the range of x^2 is rejected", {
  t2 <- targets
  t2$target[t2$term == "fac_age:variance"] <- 1e9
  expect_error(build_design_matrix(toy$ipd, t2), "outside the IPD observed range for: fac_age:variance")
})

test_that("proportion columns are indicator minus target", {
  cc <- toy$ipd[dm$complete, ]
  expect_equal(unname(dm$X[, "fac_sev_hb:Severe"]), as.numeric(cc$fac_sev_hb == "Severe") - 0.65)
  expect_equal(unname(dm$X[, "fac_prior_treatment:Other"]), as.numeric(cc$fac_prior_treatment == "Other") - 0.15)
})

test_that("unweighted column means equal IPD minus SLD moments on complete cases", {
  cc <- toy$ipd[dm$complete, ]
  expect_equal(unname(colMeans(dm$X)["fac_age"]), mean(cc$fac_age) - 50)
  expect_equal(unname(colMeans(dm$X)["fac_tar_jnt_lead:Yes"]), mean(cc$fac_tar_jnt_lead == "Yes") - 0.5)
})

test_that("na_action = 'error' names the variables with NA", {
  expect_error(build_design_matrix(toy$ipd, targets, na_action = "error"),
               "NA in matched variable\\(s\\): fac_prior_treatment, fac_bmi")
})

test_that("no NA means complete is all TRUE and both na_actions agree", {
  m <- toy$metadata
  m$match <- m$variable %in% c("fac_age", "fac_sev_hb")
  t2 <- build_match_targets(ipd_s, sld_s, m)
  a <- build_design_matrix(toy$ipd, t2, "complete_case")
  b <- build_design_matrix(toy$ipd, t2, "error")
  expect_true(all(a$complete))
  expect_identical(a, b)
})

test_that("mean target outside IPD range is rejected", {
  t2 <- targets
  t2$target[t2$term == "fac_age"] <- 1000
  expect_error(build_design_matrix(toy$ipd, t2), "outside the IPD observed range for: fac_age")
})

test_that("missing IPD column and unknown moment are rejected", {
  expect_error(build_design_matrix(toy$ipd[, -which(names(toy$ipd) == "fac_age")], targets),
               "absent from IPD: fac_age")
  t2 <- targets
  t2$moment[1] <- "kurtosis"
  expect_error(build_design_matrix(toy$ipd, t2), "Unsupported moment `kurtosis`")
})
