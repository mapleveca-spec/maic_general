# Outcome table schemas --------------------------------------------------------
#
# Two tables describe the outcome analyses for run_maic_outcomes(), which runs
# one independent analysis per published result. The single-outcome core,
# run_maic_analysis(), takes one spec of each kind instead (05_models).
#
# outcomes: one row per IPD outcome.
#   name   character  label, unique
#   type   character  one of OUTCOME_TYPES
#   var    character  IPD column (0/1, numeric, or time)
#   event  character  IPD event column for tte rows, NA otherwise
#
# sld_outcomes: one row per published comparator result.
#   name      character  must match an outcomes$name
#   anchored  logical    TRUE for a contrast vs the common comparator,
#                        FALSE for an absolute outcome
#   scale     character  must equal estimate_scale(type, anchored)
#   estimate  numeric
#   se        numeric    positive
# At most one row per (name, anchored).

OUTCOME_TYPES <- c("binary", "continuous", "tte")

ANCHORED_SCALES   <- c("log_or", "log_hr", "mean_diff")
UNANCHORED_SCALES <- c("logit_p", "mean")
ESTIMATE_SCALES   <- c(ANCHORED_SCALES, UNANCHORED_SCALES)

OUTCOME_TABLE_COLUMNS     <- c("name", "type", "var", "event")
SLD_OUTCOME_TABLE_COLUMNS <- c("name", "anchored", "scale", "estimate", "se")

# The scale on which the framework estimates an outcome of a given type for a
# given comparison kind. Input validation and model fitting both use this, so
# a published result can be checked against it before any model runs.
#   anchored    binary log_or, continuous mean_diff, tte log_hr
#   unanchored  binary logit_p, continuous mean, tte unsupported (NA)
estimate_scale <- function(type, anchored) {
  if (anchored) {
    switch(type, binary = "log_or", continuous = "mean_diff", tte = "log_hr")
  } else {
    switch(type, binary = "logit_p", continuous = "mean", tte = NA_character_)
  }
}
