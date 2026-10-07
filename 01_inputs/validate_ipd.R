# validate_ipd() ---------------------------------------------------------------
#
# Validates individual patient data (IPD) against already-validated metadata.
#
# Design:
# - Pure validation. Never transforms, recodes, or drops columns.
# - Collects every problem before failing.
# - Only metadata variables are checked. Other columns (IDs, arms, outcomes)
#   are not the concern of this validator.
# - Continuous variables must be numeric. Categorical variables must be factor
#   or character, every observed level must be in metadata$level_order, and
#   missingness must be NA, never the literal MISSING_LEVEL string.
#
# Returns the IPD invisibly, unchanged.

validate_ipd <- function(ipd, metadata) {
  if (!is.data.frame(ipd)) {
    stop("`ipd` must be a data.frame, got <", class(ipd)[1], ">.", call. = FALSE)
  }
  if (nrow(ipd) == 0) stop("`ipd` has no rows.", call. = FALSE)

  pc <- problem_collector("IPD")

  absent <- setdiff(metadata$variable, names(ipd))
  if (length(absent) > 0) {
    pc$add(paste0("Metadata variable(s) absent from IPD: ", paste(absent, collapse = ", "), "."))
  }

  for (var in setdiff(metadata$variable, absent)) {
    i <- which(metadata$variable == var)
    if (metadata$type[i] == "con") {
      .check_ipd_continuous(ipd[[var]], var, pc)
    } else {
      .check_ipd_categorical(ipd[[var]], var, metadata$level_order[[i]], pc)
    }
  }
  pc$report()

  invisible(ipd)
}

.check_ipd_continuous <- function(x, var, pc) {
  if (!is.numeric(x)) pc$add(paste0("Continuous `", var, "` must be numeric, got <", class(x)[1], ">."))
}

.check_ipd_categorical <- function(x, var, allowed, pc) {
  if (!is.factor(x) && !is.character(x)) {
    pc$add(paste0("Categorical `", var, "` must be factor or character, got <", class(x)[1], ">."))
    return(invisible())
  }
  observed <- unique(as.character(x))
  observed <- observed[!is.na(observed)]

  if (MISSING_LEVEL %in% observed) {
    pc$add(paste0(
      "Categorical `", var, "` contains the reserved value `", MISSING_LEVEL, "`; code missingness as NA."
    ))
  }
  unknown <- setdiff(observed, c(allowed, MISSING_LEVEL))
  if (length(unknown) > 0) {
    pc$add(paste0(
      "Categorical `", var, "` has IPD level(s) not in metadata$level_order: ", paste(unknown, collapse = ", "), "."
    ))
  }
}
