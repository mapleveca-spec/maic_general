test_that("anchored: both arm arguments required, two levels, contrast labelled other vs reference", {
  a <- resolve_arms(toy$ipd, anchored = TRUE, arm = "ARM", reference_arm = "A")
  expect_equal(nrow(a$ipd), nrow(toy$ipd))
  expect_equal(a$arm, "ARM")
  expect_equal(a$reference_arm, "A")
  expect_true(a$anchored)
  expect_equal(a$contrast, "B vs A")
  expect_equal(resolve_arms(toy$ipd, TRUE, "ARM", "B")$contrast, "A vs B")

  expect_error(resolve_arms(toy$ipd, TRUE), "needs both `arm` and `reference_arm`")
  expect_error(resolve_arms(toy$ipd, TRUE, arm = "ARM"), "needs both `arm` and `reference_arm`")
  expect_error(resolve_arms(toy$ipd, TRUE, "ARM", "Z"), "not an arm level \\(A, B\\)")
  expect_error(resolve_arms(toy$ipd, TRUE, "nope", "A"), "column `nope` not found")
  d <- toy$ipd
  d$ARM <- factor(rep(c("A", "B", "C"), length.out = nrow(d)))
  expect_error(resolve_arms(d, TRUE, "ARM", "A"), "exactly two levels.*A, B, C")
})

test_that("unanchored, single-arm IPD: no arm, all rows", {
  u <- resolve_arms(toy$ipd, anchored = FALSE)
  expect_equal(nrow(u$ipd), nrow(toy$ipd))
  expect_null(u$arm)
  expect_false(u$anchored)
  expect_equal(u$contrast, "IPD (unanchored)")
})

test_that("unanchored, multi-arm IPD: intervention_arm required and subsets the rows", {
  expect_error(resolve_arms(toy$ipd, FALSE, arm = "ARM"), "needs `intervention_arm` \\(one of: A, B\\)")
  expect_error(resolve_arms(toy$ipd, FALSE, arm = "ARM", intervention_arm = "Z"), "not an arm level")
  u <- resolve_arms(toy$ipd, FALSE, arm = "ARM", intervention_arm = "A")
  expect_equal(nrow(u$ipd), sum(toy$ipd$ARM == "A"))
  expect_true(all(u$ipd$ARM == "A"))
  expect_null(u$arm)
  expect_equal(u$contrast, "A (unanchored)")
})
