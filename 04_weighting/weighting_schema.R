# Weighting schema ------------------------------------------------------------
#
# Target table: one row per moment constraint the weights must satisfy.
#
#   variable     character  metadata$variable
#   level        character  category for a proportion constraint, "" otherwise
#   moment       character  one of MOMENTS:
#                           mean        weighted mean of x        = SLD mean
#                           variance    weighted mean of x^2      = SLD mean^2 + sd^2
#                                       (with the mean matched, this pins the variance)
#                           proportion  weighted mean of 1(x==l)  = SLD proportion
#   term         character  unique name for the corresponding design column
#   target       numeric    value the weighted IPD moment must equal
#   sld_missing  numeric    SLD missing proportion that was removed before
#                           rescaling (0 when none); traceability only
#
MOMENTS <- c("mean", "variance", "proportion")

# Which metadata rows enter the weighting set. The primary set is `match`
# (always). With include_adjust = TRUE the second tier, `adjust`, is added:
# the fuller specification NICE TSD 18 asks for in unanchored comparisons and
# as a sensitivity analysis in anchored ones.
weighting_variables <- function(metadata, include_adjust = FALSE) {
  metadata$match | (include_adjust & metadata$adjust)
}

TARGET_COLUMNS <- c("variable", "level", "moment", "term", "target", "sld_missing")

make_term <- function(variable, level, moment = "mean") {
  dplyr::case_when(
    moment == "variance" ~ paste0(variable, ":variance"),
    level == CONTINUOUS_LEVEL ~ variable,
    TRUE ~ paste0(variable, ":", level)
  )
}
