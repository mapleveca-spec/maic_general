# Balance schema ---------------------------------------------------------------
#
# One row per (variable, level), both sources side by side. Produced by
# align_summaries(); calculate_smd() appends `smd` later.
#
#   display_order numeric    from metadata
#   variable      character  metadata$variable
#   type          character  "con" or "cat"
#   level         character  CONTINUOUS_LEVEL, a category, or MISSING_LEVEL
#   row_type      character  one of ROW_TYPES, see below
#   ipd_n, ipd_est, ipd_sd   IPD side, summary-schema meaning of n / est / sd
#   sld_n, sld_est, sld_sd   SLD side, same meaning
#
# row_type is the stable key downstream code filters on. Matching drops
# "missing" rows; reporting formats "summary" rows as mean (SD) and the others
# as %. Nobody should string-match on `level`.

ROW_TYPES <- c("summary", "level", "missing")

ALIGNED_COLUMNS <- c(
  "display_order", "variable", "type", "level", "row_type",
  "ipd_n", "ipd_est", "ipd_sd",
  "sld_n", "sld_est", "sld_sd"
)

BALANCE_COLUMNS <- c(ALIGNED_COLUMNS, "smd")

row_type_of <- function(type, level) {
  dplyr::case_when(
    level == MISSING_LEVEL    ~ "missing",
    type == "con"             ~ "summary",
    TRUE                      ~ "level"
  )
}
