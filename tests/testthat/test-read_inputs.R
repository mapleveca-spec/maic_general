flatten_levels <- function(lo, sep = "|") vapply(lo, function(x) if (is.null(x)) "" else paste(x, collapse = sep), "")

test_that("metadata_from_table round-trips the toy metadata through a flat table", {
  flat <- toy$metadata
  flat$level_order <- flatten_levels(toy$metadata$level_order)
  m <- metadata_from_table(as.data.frame(flat))
  expect_equal(m, toy$metadata)
})

test_that("read_metadata_csv reads a CSV written the way an analyst would store it", {
  flat <- as.data.frame(toy$metadata)
  flat$level_order <- flatten_levels(toy$metadata$level_order)
  f <- tempfile(fileext = ".csv")
  write.csv(flat, f, row.names = FALSE, na = "")
  m <- read_metadata_csv(f)
  expect_equal(m, toy$metadata)
})

test_that("flags accept common spellings and reject others", {
  flat <- as.data.frame(toy$metadata)
  flat$level_order <- flatten_levels(toy$metadata$level_order)
  flat$match <- ifelse(flat$match, "yes", "no")
  flat$adjust <- ifelse(flat$adjust, 1, 0)
  expect_equal(metadata_from_table(flat)$match, toy$metadata$match)
  expect_equal(metadata_from_table(flat)$adjust, toy$metadata$adjust)
  flat$match[1] <- "maybe"
  expect_error(metadata_from_table(flat), "`match` has value\\(s\\) that are not TRUE/FALSE: maybe")
})

test_that("level_order splitting trims whitespace and honours sep", {
  flat <- as.data.frame(toy$metadata)
  flat$level_order <- flatten_levels(toy$metadata$level_order, sep = " ; ")
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

test_that("toy SLD and IPD survive a CSV round trip and still validate", {
  f <- tempfile(fileext = ".csv")
  write.csv(toy$sld, f, row.names = FALSE, na = "")
  sld <- read.csv(f, stringsAsFactors = FALSE, na.strings = c("", "NA"))
  expect_silent(validate_sld(sld, toy$metadata))
  write.csv(toy$ipd, f, row.names = FALSE, na = "")
  ipd <- read.csv(f, stringsAsFactors = FALSE, na.strings = c("", "NA"))
  expect_silent(validate_ipd(ipd, toy$metadata))
})
