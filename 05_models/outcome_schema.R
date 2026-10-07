# Outcome specification --------------------------------------------------------
#
# One analysis = one IPD outcome + one published result. The IPD outcome is
# described by define_outcome(); the published result by define_sld_outcome()
# (sld_outcome.R). Whether the comparison is anchored follows from the
# published result's scale, so it is never specified separately.
#
#   name   label used in results
#   type   one of OUTCOME_TYPES (01_inputs/outcome_table_schema.R)
#   var    IPD column: 0/1 for binary, numeric for continuous, time for tte
#   event  IPD column with 0/1 event indicator (tte only)

define_outcome <- function(name, type, var, event = NULL) {
  stopifnot(is.character(name), length(name) == 1, nzchar(name))
  if (!type %in% OUTCOME_TYPES) {
    stop("`type` must be one of: ", paste(OUTCOME_TYPES, collapse = ", "), ".", call. = FALSE)
  }
  stopifnot(is.character(var), length(var) == 1)
  if (type == "tte" && (is.null(event) || !is.character(event) || length(event) != 1)) {
    stop("tte outcome `", name, "` needs an `event` column.", call. = FALSE)
  }
  if (type != "tte" && !is.null(event)) {
    stop("`event` is only used for tte outcomes.", call. = FALSE)
  }
  structure(list(name = name, type = type, var = var, event = event), class = "maic_outcome")
}

outcome_columns <- function(outcome) c(outcome$var, outcome$event)

# Checks that a published result can be compared with an IPD outcome: same
# name, a scale the framework produces for that outcome type and comparison
# kind, and no unanchored time-to-event.
check_outcome_pair <- function(outcome, sld_outcome) {
  stopifnot(inherits(outcome, "maic_outcome"), inherits(sld_outcome, "maic_sld_outcome"))
  if (!identical(outcome$name, sld_outcome$name)) {
    stop("Outcome mismatch: IPD outcome is `", outcome$name, "`, published result is `", sld_outcome$name, "`.",
         call. = FALSE)
  }
  if (outcome$type == "tte" && !sld_outcome$anchored) {
    stop("Unanchored tte comparison is not supported: it needs pseudo-IPD from the SLD.", call. = FALSE)
  }
  expected <- estimate_scale(outcome$type, sld_outcome$anchored)
  if (!identical(sld_outcome$scale, expected)) {
    stop("`", outcome$name, "` (", if (sld_outcome$anchored) "anchored" else "unanchored", ") must be on scale `",
         expected, "`, got `", sld_outcome$scale, "`.", call. = FALSE)
  }
  invisible(TRUE)
}
