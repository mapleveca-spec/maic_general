# validate_sld() ---------------------------------------------------------------
#
# Validates an SLD table against sld_schema.R and checks it is consistent with
# already-validated metadata.
#
# Design:
# - Pure validation. Never transforms or reorders the SLD.
# - Collects every problem before failing.
# - Metadata is the single source of truth: every metadata variable must appear
#   in the SLD (an unreported variable is an explicit row with NA est/sd), and
#   every categorical level except MISSING_LEVEL must be in level_order.
#   Variables in the SLD but not in the metadata are ignored, not rejected.
# - Categorical proportions within a variable must sum to 1 within `prop_tol`.
#   The tolerance absorbs rounding in published tables; it is not a license to
#   renormalise. The framework never rescales proportions downstream.
#
# Returns the SLD invisibly, unchanged.

validate_sld <- function(sld, metadata, prop_tol = 0.02) {
  if (!is.data.frame(sld)) {
    stop("`sld` must be a data.frame, got <", class(sld)[1], ">.", call. = FALSE)
  }
  missing_cols <- setdiff(SLD_COLUMNS, names(sld))
  if (length(missing_cols) > 0) {
    stop("`sld` is missing required column(s): ", paste(missing_cols, collapse = ", "), ".", call. = FALSE)
  }
  if (nrow(sld) == 0) stop("`sld` has no rows.", call. = FALSE)

  pc <- problem_collector("SLD")

  # Column types must be right before anything else can be checked.
  .check_sld_column_types(sld, pc)
  pc$report()

  .check_sld_study_n(sld, pc)
  if (any(sld$sld_sd < 0, na.rm = TRUE)) pc$add("`sld_sd` contains negative values.")
  for (var in unique(sld$var_name)) {
    .check_sld_variable(sld[sld$var_name == var, , drop = FALSE], var, prop_tol, pc)
  }
  .check_sld_vs_metadata(sld, metadata, pc)
  pc$report()

  invisible(sld)
}

.check_sld_column_types <- function(sld, pc) {
  if (!is.character(sld$var_name) || anyNA(sld$var_name) || any(!nzchar(sld$var_name))) {
    pc$add("`var_name` must be character with no missing or empty values.")
  }
  bad_type <- unique(sld$var_type[!sld$var_type %in% VARIABLE_TYPES])
  if (length(bad_type) > 0) {
    pc$add(paste0("`var_type` has invalid value(s): ", paste(bad_type, collapse = ", "), "."))
  }
  if (!is.character(sld$var_level) && !all(is.na(sld$var_level))) pc$add("`var_level` must be character.")
  for (col in c("sld_n", "sld_est", "sld_sd")) {
    if (!is.numeric(sld[[col]])) pc$add(paste0("`", col, "` must be numeric."))
  }
}

.check_sld_study_n <- function(sld, pc) {
  if (anyNA(sld$sld_n) || any(sld$sld_n <= 0)) pc$add("`sld_n` must be positive with no NA.")
  if (length(unique(sld$sld_n)) > 1) pc$add("`sld_n` must be identical on every row (single study N).")
}

.check_sld_variable <- function(rows, var, prop_tol, pc) {
  types <- unique(rows$var_type)
  if (length(types) > 1) {
    pc$add(paste0("`", var, "` has more than one var_type: ", paste(types, collapse = ", "), "."))
    return(invisible())
  }
  if (types == "con") .check_sld_continuous(rows, var, pc) else .check_sld_categorical(rows, var, prop_tol, pc)
}

.check_sld_continuous <- function(rows, var, pc) {
  is_summary <- is.na(rows$var_level)
  is_missing <- !is_summary & rows$var_level == MISSING_LEVEL
  other      <- rows$var_level[!is_summary & !is_missing]

  if (sum(is_summary) != 1) {
    pc$add(paste0(
      "Continuous `", var, "` must have exactly one summary row (NA var_level), found ", sum(is_summary), "."
    ))
  }
  if (sum(is_missing) > 1) pc$add(paste0("Continuous `", var, "` has more than one `", MISSING_LEVEL, "` row."))
  if (length(other) > 0) {
    pc$add(paste0("Continuous `", var, "` has unexpected var_level: ", paste(other, collapse = ", "), "."))
  }
  if (sum(is_summary) == 1) {
    s <- rows[is_summary, ]
    if (xor(is.na(s$sld_est), is.na(s$sld_sd))) {
      pc$add(paste0("Continuous `", var, "` summary row must have both or neither of sld_est and sld_sd as NA."))
    }
  }
  if (sum(is_missing) == 1) {
    m <- rows[is_missing, ]
    if (is.na(m$sld_est) || m$sld_est < 0 || m$sld_est > 1) {
      pc$add(paste0("Continuous `", var, "` `", MISSING_LEVEL, "` row must have a proportion in [0, 1]."))
    }
  }
}

.check_sld_categorical <- function(rows, var, prop_tol, pc) {
  lv <- rows$var_level
  if (anyNA(lv) || any(!nzchar(lv), na.rm = TRUE)) {
    pc$add(paste0("Categorical `", var, "` has missing or empty var_level."))
  }
  dup <- unique(lv[duplicated(lv)])
  if (length(dup) > 0) {
    pc$add(paste0("Categorical `", var, "` has duplicate level(s): ", paste(dup, collapse = ", "), "."))
  }
  if (anyNA(rows$sld_est)) {
    pc$add(paste0("Categorical `", var, "` has NA sld_est; omit the level or give a proportion."))
    return(invisible())
  }
  if (any(rows$sld_est < 0 | rows$sld_est > 1)) {
    pc$add(paste0("Categorical `", var, "` has proportions outside [0, 1]."))
  }
  total <- sum(rows$sld_est)
  if (abs(total - 1) > prop_tol) {
    pc$add(paste0("Categorical `", var, "` proportions sum to ", format(total), ", expected 1 (tol ", prop_tol, ")."))
  }
}

.check_sld_vs_metadata <- function(sld, metadata, pc) {
  not_in_sld <- setdiff(metadata$variable, sld$var_name)
  if (length(not_in_sld) > 0) {
    pc$add(paste0(
      "Metadata variable(s) absent from SLD: ", paste(not_in_sld, collapse = ", "),
      ". Add a row with NA sld_est/sld_sd for unreported variables."
    ))
  }

  for (var in intersect(metadata$variable, unique(sld$var_name))) {
    meta_type <- metadata$type[metadata$variable == var]
    sld_type  <- unique(sld$var_type[sld$var_name == var])
    if (length(sld_type) == 1 && !identical(sld_type, meta_type)) {
      pc$add(paste0("`", var, "` is `", meta_type, "` in metadata but `", sld_type, "` in SLD."))
    }
    if (identical(meta_type, "cat") && identical(sld_type, "cat")) {
      allowed <- c(metadata$level_order[[which(metadata$variable == var)]], MISSING_LEVEL)
      unknown <- setdiff(sld$var_level[sld$var_name == var], allowed)
      unknown <- unknown[!is.na(unknown)]
      if (length(unknown) > 0) {
        pc$add(paste0(
          "`", var, "` has SLD level(s) not in metadata$level_order: ", paste(unknown, collapse = ", "), "."
        ))
      }
    }
  }
}
