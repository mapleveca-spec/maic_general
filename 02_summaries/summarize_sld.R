# summarize_sld() --------------------------------------------------------------
#
# Adapter from the SLD input contract (sld_schema.R) to the internal summary
# schema (summary_schema.R). It computes nothing: the SLD already holds
# summary statistics.
#
# What it does:
# - Keeps only metadata variables, in metadata order. Extra SLD variables are
#   ignored, matching validate_sld().
# - Renames columns to the summary schema.
# - Recodes the continuous NA level to CONTINUOUS_LEVEL so both summary
#   tables use the same key.
# - Derives sd = sqrt(p(1-p)) for every proportion row (categorical levels and
#   Missing rows), exactly as summarize_categorical() does for the IPD, so an
#   entered sld_sd on those rows is ignored and may be left blank.
# - Passes Missing rows through untouched. Levels absent from the SLD are not
#   added here; alignment in the balance layer owns that.
# - Assumes inputs have passed validate_metadata() and validate_sld().

summarize_sld <- function(sld, metadata) {
  keep <- sld[sld$var_name %in% metadata$variable, , drop = FALSE]
  keep <- keep[order(match(keep$var_name, metadata$variable)), , drop = FALSE]

  is_proportion <- keep$var_type == "cat" | keep$var_level %in% MISSING_LEVEL
  sd <- keep$sld_sd
  sd[is_proportion] <- binomial_sd(keep$sld_est[is_proportion])

  new_summary_row(
    variable = keep$var_name,
    type     = keep$var_type,
    level    = ifelse(is.na(keep$var_level), CONTINUOUS_LEVEL, keep$var_level),
    n        = keep$sld_n,
    est      = keep$sld_est,
    sd       = sd
  )
}
