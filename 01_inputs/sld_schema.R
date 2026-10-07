# SLD schema -------------------------------------------------------------------
#
# Contract for the summary-level data (SLD) table. Long format: one row per
# (variable, level). No logic here, only constants shared across modules.
#
# Row rules:
#   Continuous:  exactly one summary row with var_level = NA (est = mean,
#                sd = SD; both NA if the study did not report the variable),
#                plus an optional row with var_level = MISSING_LEVEL whose
#                est is the proportion missing.
#   Categorical: one row per level, est = proportion, optionally including a
#                MISSING_LEVEL row. Proportions sum to 1.

SLD_COLUMNS <- c(
  "var_name",  # character: matches metadata$variable
  "var_type",  # character: one of VARIABLE_TYPES, must agree with metadata$type
  "var_level", # character: category for cat rows, NA for con rows
  "sld_n",     # numeric:   study N, identical on every row
  "sld_est",   # numeric:   mean (con) or proportion (cat); NA if not reported
  "sld_sd"     # numeric:   SD for the con summary row (NA if not reported).
  #                         Ignored on proportion rows (cat levels, Missing):
  #                         the framework derives sqrt(p(1-p)); leave blank.
)

# Reserved level name for missingness. It is never part of metadata$level_order
# and always sorts last within a variable.
MISSING_LEVEL <- "Missing"
