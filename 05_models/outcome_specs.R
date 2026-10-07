# Table -> spec converters -----------------------------------------------------
#
# Turn validated outcome tables (01_inputs/outcome_table_schema.R) into the
# spec objects the single-outcome functions take. One row in, one spec out.
#
# outcome_specs()      named list of maic_outcome, keyed by outcome name.
# sld_outcome_specs()  list of maic_sld_outcome, one per published result, in
#                      table order. `anchored` is derived from the scale and
#                      checked against the table's column.

outcome_specs <- function(outcomes) {
  specs <- lapply(seq_len(nrow(outcomes)), function(i) {
    ev <- outcomes$event[i]
    define_outcome(
      name  = outcomes$name[i],
      type  = outcomes$type[i],
      var   = outcomes$var[i],
      event = if (is.na(ev)) NULL else as.character(ev)
    )
  })
  stats::setNames(specs, outcomes$name)
}

sld_outcome_specs <- function(sld_outcomes) {
  lapply(seq_len(nrow(sld_outcomes)), function(i) {
    r <- sld_outcomes[i, ]
    spec <- define_sld_outcome(r$name, r$scale, estimate = r$estimate, se = r$se)
    if (!identical(spec$anchored, as.logical(r$anchored))) {
      stop("`", r$name, "`: `anchored` = ", r$anchored, " disagrees with scale `", r$scale, "`.", call. = FALSE)
    }
    spec
  })
}
