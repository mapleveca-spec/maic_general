# validate_metadata() ----------------------------------------------------------
#
# Validates a metadata table against the schema in metadata_schema.R.
#
# Design:
# - Pure validation. Never transforms, reorders, or fills the metadata.
# - Collects every problem before failing, so the analyst fixes the table in
#   one pass instead of one error at a time.
# - Validates the metadata in isolation. Checking metadata against the actual
#   IPD / SLD columns is a separate concern and lives with the data validators.
#
# Returns the metadata invisibly, unchanged, so it can be used in a pipeline.

validate_metadata <- function(metadata) {
  if (!is.data.frame(metadata)) {
    stop("`metadata` must be a data.frame, got <", class(metadata)[1], ">.", call. = FALSE)
  }
  missing_cols <- setdiff(METADATA_COLUMNS, names(metadata))
  if (length(missing_cols) > 0) {
    stop("`metadata` is missing required column(s): ", paste(missing_cols, collapse = ", "), ".", call. = FALSE)
  }
  if (nrow(metadata) == 0) stop("`metadata` has no rows.", call. = FALSE)

  pc <- problem_collector("metadata")
  .check_meta_variable(metadata, pc)
  .check_meta_type(metadata, pc)
  .check_meta_flags(metadata, pc)
  .check_meta_weighting_tiers(metadata, pc)
  .check_meta_display_order(metadata, pc)
  .check_meta_level_order(metadata, pc)
  pc$report()

  invisible(metadata)
}

.check_meta_variable <- function(metadata, pc) {
  v <- metadata$variable
  if (!is.character(v)) pc$add("`variable` must be character.")
  if (anyNA(v) || any(!nzchar(v), na.rm = TRUE)) pc$add("`variable` contains missing or empty values.")
  dup <- unique(v[duplicated(v)])
  if (length(dup) > 0) pc$add(paste0("`variable` has duplicates: ", paste(dup, collapse = ", "), "."))
}

.check_meta_type <- function(metadata, pc) {
  bad <- unique(metadata$type[!metadata$type %in% VARIABLE_TYPES])
  if (length(bad) > 0) {
    pc$add(paste0(
      "`type` has invalid value(s): ", paste(bad, collapse = ", "),
      ". Allowed: ", paste(VARIABLE_TYPES, collapse = ", "), "."
    ))
  }
}

.check_meta_flags <- function(metadata, pc) {
  for (col in METADATA_LOGICAL_COLUMNS) {
    x <- metadata[[col]]
    if (!is.logical(x)) {
      pc$add(paste0("`", col, "` must be logical."))
    } else if (anyNA(x)) {
      pc$add(paste0("`", col, "` contains NA; use TRUE or FALSE."))
    }
  }
}

.check_meta_weighting_tiers <- function(metadata, pc) {
  flags <- metadata[c("match", "adjust", "match_sd")]
  if (!all(vapply(flags, is.logical, logical(1)))) return(invisible())

  both <- metadata$variable[metadata$match %in% TRUE & metadata$adjust %in% TRUE]
  if (length(both) > 0) {
    pc$add(paste0("A variable belongs to one weighting tier only; both `match` and `adjust` are TRUE for: ",
                  paste(both, collapse = ", "), "."))
  }
  sd_on_cat <- metadata$variable[metadata$match_sd %in% TRUE & metadata$type %in% "cat"]
  if (length(sd_on_cat) > 0) {
    pc$add(paste0("`match_sd` must be FALSE for categorical variable(s): ", paste(sd_on_cat, collapse = ", "), "."))
  }
  in_set <- metadata$match %in% TRUE | metadata$adjust %in% TRUE
  sd_no_mean <- metadata$variable[metadata$match_sd %in% TRUE & !in_set]
  if (length(sd_no_mean) > 0) {
    pc$add(paste0("`match_sd` requires `match` or `adjust` = TRUE for: ", paste(sd_no_mean, collapse = ", "), "."))
  }
}

.check_meta_display_order <- function(metadata, pc) {
  d <- metadata$display_order
  if (!is.numeric(d)) {
    pc$add("`display_order` must be numeric.")
    return(invisible())
  }
  if (anyNA(d)) pc$add("`display_order` contains NA.")
  dup <- unique(d[duplicated(d)])
  if (length(dup) > 0) pc$add(paste0("`display_order` has duplicates: ", paste(dup, collapse = ", "), "."))
}

.check_meta_level_order <- function(metadata, pc) {
  lo <- metadata$level_order
  if (!is.list(lo)) {
    pc$add("`level_order` must be a list column (one character vector per row).")
    return(invisible())
  }
  for (i in seq_len(nrow(metadata))) {
    .check_one_level_order(lo[[i]], metadata$variable[i], metadata$type[i], pc)
  }
}

.check_one_level_order <- function(levels, var, type, pc) {
  has_levels <- length(levels) > 0 && !all(is.na(levels))

  if (!identical(type, "cat")) {
    if (has_levels) pc$add(paste0("`level_order` must be empty for continuous variable `", var, "`."))
    return(invisible())
  }
  if (!has_levels) {
    pc$add(paste0("`level_order` is empty for categorical variable `", var, "`."))
    return(invisible())
  }
  if (!is.character(levels)) pc$add(paste0("`level_order` for `", var, "` must be a character vector."))
  if (anyNA(levels) || any(!nzchar(levels), na.rm = TRUE)) {
    pc$add(paste0("`level_order` for `", var, "` contains missing or empty levels."))
  }
  if (anyDuplicated(levels) > 0) pc$add(paste0("`level_order` for `", var, "` contains duplicate levels."))
  if (MISSING_LEVEL %in% levels) {
    pc$add(paste0("`level_order` for `", var, "` uses reserved level `", MISSING_LEVEL, "`."))
  }
}
