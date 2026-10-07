template_dir <- file.path(framework_root, "templates")

test_that("metadata_from_table round-trips the toy metadata through a flat file", {
  flat <- toy$metadata
  flat$level_order <- vapply(flat$level_order, function(x) if (is.null(x)) "" else paste(x, collapse = "|"), "")
  m <- metadata_from_table(as.data.frame(flat))
  expect_equal(m, toy$metadata)
})

test_that("read_metadata_csv reads the template and validates it", {
  m <- read_metadata_csv(file.path(template_dir, "metadata_template.csv"))
  expect_equal(m, toy$metadata)
})

test_that("flags accept common spellings and reject others", {
  flat <- as.data.frame(toy$metadata)
  flat$level_order <- vapply(toy$metadata$level_order, function(x) if (is.null(x)) "" else paste(x, collapse = "|"), "")
  flat$match <- ifelse(flat$match, "yes", "no")
  flat$adjust <- ifelse(flat$adjust, 1, 0)
  expect_equal(metadata_from_table(flat)$match, toy$metadata$match)
  expect_equal(metadata_from_table(flat)$adjust, toy$metadata$adjust)
  flat$match[1] <- "maybe"
  expect_error(metadata_from_table(flat), "`match` has value\\(s\\) that are not TRUE/FALSE: maybe")
})

test_that("level_order splitting trims whitespace and honours sep", {
  flat <- as.data.frame(toy$metadata)
  joined <- function(x) if (is.null(x)) "" else paste(x, collapse = " ; ")
  flat$level_order <- vapply(toy$metadata$level_order, joined, "")
  m <- metadata_from_table(flat, sep = ";")
  expect_equal(m$level_order, toy$metadata$level_order)
})

test_that("invalid metadata in a flat file fails through the validator", {
  flat <- as.data.frame(toy$metadata)
  flat$level_order <- ""
  expect_error(metadata_from_table(flat), "Invalid metadata")
  expect_error(metadata_from_table(flat[, -2]), "missing column\\(s\\): type")
})

test_that("se_from_ci inverts a Wald interval", {
  expect_equal(se_from_ci(log(0.6), log(1.07)), (log(1.07) - log(0.6)) / (2 * qnorm(0.975)))
  expect_equal(se_from_ci(-1, 1, conf_level = 0.8), 1 / qnorm(0.9))
  expect_error(se_from_ci(1, 0), "must not exceed")
})

test_that("the CSV templates reproduce the toy inputs", {
  sld <- read.csv(file.path(template_dir, "sld_template.csv"), stringsAsFactors = FALSE, na.strings = c("", "NA"))
  expect_silent(validate_sld(sld, toy$metadata))
  out <- read.csv(file.path(template_dir, "outcomes_template.csv"), stringsAsFactors = FALSE, na.strings = c("", "NA"))
  expect_silent(validate_outcomes(out))
  so <- read.csv(file.path(template_dir, "sld_outcomes_template.csv"), stringsAsFactors = FALSE)
  expect_silent(validate_sld_outcomes(so, out))
  ipd <- read.csv(file.path(template_dir, "ipd_example.csv"), stringsAsFactors = FALSE, na.strings = c("", "NA"))
  expect_silent(validate_ipd(ipd, toy$metadata))
})
