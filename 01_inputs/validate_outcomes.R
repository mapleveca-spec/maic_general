# validate_outcomes() / validate_sld_outcomes() --------------------------------
#
# Pure validators for the two outcome tables (see outcome_schema.R). Same
# contract as the other validators: collect every problem, never transform,
# return the input invisibly.
#
# validate_sld_outcomes() needs the validated `outcomes` table to check that
# each published result is on the scale the framework will produce for that
# outcome type and comparison kind.

validate_outcomes <- function(outcomes) {
  .check_table(outcomes, "outcomes", OUTCOME_TABLE_COLUMNS)
  pc <- problem_collector("outcomes")

  nm <- outcomes$name
  if (!is.character(nm) || anyNA(nm) || any(!nzchar(nm))) pc$add("`name` must be character with no missing values.")
  dup <- unique(nm[duplicated(nm)])
  if (length(dup) > 0) pc$add(paste0("`name` has duplicates: ", paste(dup, collapse = ", "), "."))

  bad_type <- unique(outcomes$type[!outcomes$type %in% OUTCOME_TYPES])
  if (length(bad_type) > 0) {
    pc$add(paste0("`type` has invalid value(s): ", paste(bad_type, collapse = ", "), "."))
  }
  if (!is.character(outcomes$var) || anyNA(outcomes$var)) pc$add("`var` must be character with no missing values.")

  is_tte <- outcomes$type %in% "tte"
  ev <- as.character(outcomes$event)
  if (any(is_tte & is.na(ev))) {
    pc$add(paste0("tte outcome(s) need an `event` column: ", paste(nm[is_tte & is.na(ev)], collapse = ", "), "."))
  }
  if (any(!is_tte & !is.na(ev))) {
    pc$add(paste0("`event` must be NA for non-tte outcome(s): ", paste(nm[!is_tte & !is.na(ev)], collapse = ", "), "."))
  }

  pc$report()
  invisible(outcomes)
}

validate_sld_outcomes <- function(sld_outcomes, outcomes) {
  .check_table(sld_outcomes, "sld_outcomes", SLD_OUTCOME_TABLE_COLUMNS)
  pc <- problem_collector("sld_outcomes")

  if (!is.logical(sld_outcomes$anchored) || anyNA(sld_outcomes$anchored)) {
    pc$add("`anchored` must be logical with no NA.")
  }
  if (!is.numeric(sld_outcomes$estimate) || anyNA(sld_outcomes$estimate)) pc$add("`estimate` must be numeric, no NA.")
  if (!is.numeric(sld_outcomes$se) || anyNA(sld_outcomes$se) || any(sld_outcomes$se <= 0, na.rm = TRUE)) {
    pc$add("`se` must be numeric and positive.")
  }

  unknown <- setdiff(sld_outcomes$name, outcomes$name)
  if (length(unknown) > 0) {
    pc$add(paste0("`name` not in outcomes: ", paste(unknown, collapse = ", "), "."))
  }

  key <- paste(sld_outcomes$name, sld_outcomes$anchored)
  dup <- unique(key[duplicated(key)])
  if (length(dup) > 0) pc$add(paste0("Duplicate (name, anchored) row(s): ", paste(dup, collapse = "; "), "."))

  if (is.logical(sld_outcomes$anchored)) {
    for (i in seq_len(nrow(sld_outcomes))) {
      .check_sld_outcome_scale(sld_outcomes[i, ], outcomes, pc)
    }
  }

  pc$report()
  invisible(sld_outcomes)
}

.check_sld_outcome_scale <- function(row, outcomes, pc) {
  type <- outcomes$type[outcomes$name == row$name]
  if (length(type) != 1 || is.na(row$anchored)) return(invisible())
  if (type == "tte" && !row$anchored) {
    pc$add(paste0("`", row$name, "`: unanchored tte comparison is not supported."))
    return(invisible())
  }
  expected <- estimate_scale(type, row$anchored)
  if (!identical(row$scale, expected)) {
    pc$add(paste0(
      "`", row$name, "` (anchored = ", row$anchored, ") must be on scale `", expected, "`, got `", row$scale, "`."
    ))
  }
}

.check_table <- function(x, what, required) {
  if (!is.data.frame(x)) stop("`", what, "` must be a data.frame, got <", class(x)[1], ">.", call. = FALSE)
  missing_cols <- setdiff(required, names(x))
  if (length(missing_cols) > 0) {
    stop("`", what, "` is missing required column(s): ", paste(missing_cols, collapse = ", "), ".", call. = FALSE)
  }
  if (nrow(x) == 0) stop("`", what, "` has no rows.", call. = FALSE)
  invisible(x)
}
