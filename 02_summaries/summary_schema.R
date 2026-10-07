# Summary schema ---------------------------------------------------------------
#
# Internal long-format schema produced by both the IPD and SLD summarisers.
# The balance layer joins two tables of this shape, so neither side needs to
# know where the other came from.
#
#   variable  character  metadata$variable
#   type      character  "con" or "cat"
#   level     character  "" for a continuous summary row, category otherwise,
#                        MISSING_LEVEL for a missingness row
#   n         numeric    denominator of `est`: observed count for a continuous
#                        summary row, full N for categorical and Missing rows
#   est       numeric    mean (continuous) or proportion (categorical / Missing)
#   sd        numeric    SD (continuous) or sqrt(p(1-p)) (categorical / Missing)

SUMMARY_COLUMNS <- c("variable", "type", "level", "n", "est", "sd")

CONTINUOUS_LEVEL <- ""

new_summary_row <- function(variable, type, level, n, est, sd) {
  tibble::tibble(
    variable = variable, type = type, level = level,
    n = as.numeric(n), est = as.numeric(est), sd = as.numeric(sd)
  )
}

binomial_sd <- function(p) sqrt(p * (1 - p))
