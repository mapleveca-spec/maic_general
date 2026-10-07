# Metadata schema -------------------------------------------------------------
#
# The metadata table is the single source of truth that drives every module.
# This file defines the contract: column names, allowed values, and column
# types. It contains no logic so that other modules can reference the same
# constants without pulling in validation code.

METADATA_COLUMNS <- c(
  "variable",      # character: column name in the IPD / key in the SLD
  "type",          # character: one of VARIABLE_TYPES
  "show_balance",  # logical:   include in balance tables
  "match",         # logical:   primary weighting set, always matched (effect modifiers)
  "adjust",        # logical:   second weighting tier (prognostic variables), matched only
  #                             when include_adjust = TRUE; a variable is in at most one tier
  "match_sd",      # logical:   also match the SD (con only; requires match or adjust)
  "display_order", # numeric:   row ordering of variables in outputs
  "level_order"    # list:      character vector of category order (categorical only)
)

VARIABLE_TYPES <- c("con", "cat")  # continuous, categorical

METADATA_LOGICAL_COLUMNS <- c("show_balance", "match", "match_sd", "adjust")
