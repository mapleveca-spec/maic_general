# build_design_matrix() --------------------------------------------------------
#
# Builds the centred design matrix for method-of-moments weighting:
# one row per usable IPD patient, one column per target constraint, cell =
# patient's moment value minus the SLD target. A weighted column mean of zero
# everywhere means every target is met.
#
# Column per moment:
#   mean        x
#   variance    x^2   (target is the SLD second moment, see weighting_schema.R)
#   proportion  1 if x == level else 0
# Columns are named by targets$term and keep the targets' row order.
#
# Rows with NA in any matched variable:
#   na_action = "complete_case"  rows are excluded from X and flagged FALSE in
#                                `complete`; the caller assigns them weight 0.
#   na_action = "error"          stop, naming the variables with NA.
# Imputation is out of scope; do it upstream if it is wanted.
#
# Feasibility that needs raw IPD lives here: a mean target outside the
# observed range of the complete cases cannot be reached by reweighting.
#
# Returns list(X = numeric matrix, complete = logical of length nrow(ipd)).

build_design_matrix <- function(ipd, targets, na_action = c("complete_case", "error")) {
  na_action <- match.arg(na_action)
  stopifnot(all(TARGET_COLUMNS %in% names(targets)), nrow(targets) > 0)

  vars <- unique(targets$variable)
  absent <- setdiff(vars, names(ipd))
  if (length(absent) > 0) {
    stop("Target variable(s) absent from IPD: ", paste(absent, collapse = ", "), ".", call. = FALSE)
  }

  na_by_var <- vapply(vars, function(v) anyNA(ipd[[v]]), logical(1))
  if (na_action == "error" && any(na_by_var)) {
    stop("NA in matched variable(s): ", paste(vars[na_by_var], collapse = ", "),
         ". Use na_action = \"complete_case\" or impute upstream.", call. = FALSE)
  }

  complete <- stats::complete.cases(ipd[vars])
  if (!any(complete)) stop("No IPD row is complete on all matched variables.", call. = FALSE)
  cc <- ipd[complete, vars, drop = FALSE]

  cols <- lapply(seq_len(nrow(targets)), function(i) {
    .moment_column(cc[[targets$variable[i]]], targets$moment[i], targets$level[i], targets$term[i])
  })
  X <- do.call(cbind, cols)
  colnames(X) <- targets$term

  .check_mean_targets_in_range(X, targets)

  X <- sweep(X, 2, targets$target, "-")
  list(X = X, complete = complete)
}

.moment_column <- function(x, moment, level, term) {
  switch(
    moment,
    mean       = as.numeric(x),
    variance   = as.numeric(x)^2,
    proportion = as.numeric(as.character(x) == level),
    stop("Unsupported moment `", moment, "` for term `", term, "`.", call. = FALSE)
  )
}

.check_mean_targets_in_range <- function(X, targets) {
  # A weighted mean of any column must lie within that column's observed range.
  is_mean <- targets$moment %in% c("mean", "variance")
  if (!any(is_mean)) return(invisible())
  lo <- apply(X[, is_mean, drop = FALSE], 2, min)
  hi <- apply(X[, is_mean, drop = FALSE], 2, max)
  out_of_range <- targets$target[is_mean] < lo | targets$target[is_mean] > hi
  if (any(out_of_range)) {
    bad <- targets$term[is_mean][out_of_range]
    stop("Mean target outside the IPD observed range for: ", paste(bad, collapse = ", "),
         ". Reweighting cannot reach it.", call. = FALSE)
  }
  invisible()
}
