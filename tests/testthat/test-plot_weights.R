res <- run_maic_weighting(toy$ipd, toy$sld, toy$metadata, include_adjust = TRUE)

test_that("plot_weights returns a ggplot built from the rescaled positive weights", {
  p <- plot_weights(res$fit)
  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), sum(res$weights > 0))
  expect_equal(sum(p$data$weight), sum(res$weights > 0), tolerance = 1e-8)   # rescaled: mean weight 1
  expect_match(p$labels$subtitle, "48 weighted patients \\(12 excluded\\)")
  expect_match(p$labels$subtitle, sprintf("ESS = %.1f", res$diagnostics$ess))
})

test_that("plot_weights accepts a bare weight vector and renders without error", {
  p <- plot_weights(res$weights, bins = 10, title = "custom")
  expect_equal(p$labels$title, "custom")
  f <- tempfile(fileext = ".png")
  ggplot2::ggsave(f, p, width = 6, height = 4, dpi = 72)
  expect_true(file.exists(f) && file.size(f) > 0)
})

test_that("plot_weights rejects invalid weights", {
  expect_error(plot_weights(c(1, -1)), "negative")
  expect_error(plot_weights(c(0, 0)), "positive sum")
})
