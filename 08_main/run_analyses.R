# run_analyses_unanchored() / run_analyses_anchored() --------------------------
#
# Several analyses from a prepared input list, each treated the same way:
# a sequential scenario starting from the unweighted model and adding the
# adjust-tier variables one by one, and a univariate scenario. Every analysis
# is independent. Results are exported in one fixed layout (export_scenarios())
# and stacked across analyses.
#
# `inputs` is a list, one element per analysis, each a list with
#   analysis_name  label, also the folder name (slugified)
#   population     free text, carried into the stacked tables
#   comparator     free text, carried into the stacked tables
#   ipd            the analysis population as is (unanchored: no arm column
#                  is used; anchored: two arms)
#   sld            comparator baseline table
#   metadata       covariate metadata
#   outcome        define_outcome()
#   sld_outcome    define_sld_outcome() / sld_outcome_from_proportion()
#   adjust_order   (optional) order in which adjust-tier variables enter the
#                  sequential scenario; default metadata row order
#   arm, reference_arm   anchored only, per analysis
#
# Error tolerance at two levels: a failing scenario step is recorded inside
# that analysis's results (see run_scenarios_*()); an analysis that cannot
# start at all (invalid inputs, wrong kind of published result, bad arms) is
# recorded in `errors` with its name and stage, and the other analyses run.
#
# Returns list(analyses, sequential, univariate, errors):
#   analyses    named list; each is list(sequential, univariate) of
#               run_scenarios_*() results, or list(error = message)
#   sequential  all sequential results stacked, with analysis, population,
#               comparator columns in front
#   univariate  same for univariate
#   errors      tibble: analysis, stage, message (zero rows if none)

run_analyses_unanchored <- function(inputs, output_dir = NULL, ...) {
  .run_analyses_core(inputs, output_dir, kind = "unanchored", ...)
}

run_analyses_anchored <- function(inputs, output_dir = NULL, ...) {
  .run_analyses_core(inputs, output_dir, kind = "anchored", ...)
}

.run_analyses_core <- function(inputs, output_dir, kind, ...) {
  stopifnot(is.list(inputs), length(inputs) > 0)
  names(inputs) <- vapply(inputs, function(a) if (is.null(a$analysis_name)) "" else a$analysis_name, "")
  if (any(!nzchar(names(inputs))) || anyDuplicated(names(inputs)) > 0) {
    stop("Every input needs a unique `analysis_name`.", call. = FALSE)
  }

  analyses <- lapply(inputs, function(a) {
    tryCatch(.run_analysis_scenarios(a, kind, output_dir, ...), error = function(e) {
      list(error = conditionMessage(e), stage = or_default(attr(e, "stage"), "unknown"))
    })
  })

  tag <- function(name, results) {
    a <- inputs[[name]]
    dplyr::bind_cols(
      tibble::tibble(analysis = name, population = or_default(a$population, NA_character_),
                     comparator = or_default(a$comparator, NA_character_)),
      results
    )
  }
  ok <- names(analyses)[vapply(analyses, function(x) is.null(x$error), logical(1))]
  failed <- setdiff(names(analyses), ok)

  list(
    analyses   = analyses,
    sequential = dplyr::bind_rows(lapply(ok, function(n) tag(n, analyses[[n]]$sequential$results))),
    univariate = dplyr::bind_rows(lapply(ok, function(n) tag(n, analyses[[n]]$univariate$results))),
    errors     = tibble::tibble(
      analysis = failed,
      stage    = unname(vapply(failed, function(n) analyses[[n]]$stage, "")),
      message  = unname(vapply(failed, function(n) analyses[[n]]$error, ""))
    )
  )
}

or_default <- function(x, default) if (is.null(x)) default else x

.stage_error <- function(stage, expr) {
  tryCatch(expr, error = function(e) {
    err <- simpleError(conditionMessage(e))
    attr(err, "stage") <- stage
    stop(err)
  })
}

.run_analysis_scenarios <- function(a, kind, output_dir, ...) {
  .stage_error("inputs", {
    required <- c("analysis_name", "ipd", "sld", "metadata", "outcome", "sld_outcome")
    missing <- setdiff(required, names(a))
    if (length(missing) > 0) stop("input is missing: ", paste(missing, collapse = ", "), ".", call. = FALSE)
    validate_metadata(a$metadata)
    validate_sld(a$sld, a$metadata)
    validate_ipd(a$ipd, a$metadata)
    check_outcome_pair(a$outcome, a$sld_outcome)
    if (kind == "unanchored" && a$sld_outcome$anchored) {
      stop("published result is anchored; use run_analyses_anchored().", call. = FALSE)
    }
    if (kind == "anchored" && !a$sld_outcome$anchored) {
      stop("published result is unanchored; use run_analyses_unanchored().", call. = FALSE)
    }
  })

  order <- or_default(a$adjust_order, a$metadata$variable[a$metadata$adjust])
  scen_seq <- .stage_error("scenarios", define_scenarios_sequential(a$metadata, order, include_empty = TRUE))
  scen_uni <- .stage_error("scenarios", define_scenarios_univariate(a$metadata, order))

  run <- function(scenarios) {
    if (kind == "unanchored") {
      run_scenarios_unanchored(scenarios, a$ipd, a$sld, a$outcome, a$sld_outcome, ...)
    } else {
      run_scenarios_anchored(scenarios, a$ipd, a$sld, a$outcome, a$sld_outcome,
                             arm = a$arm, reference_arm = a$reference_arm, ...)
    }
  }
  sequential <- .stage_error("sequential", run(scen_seq))
  univariate <- .stage_error("univariate", run(scen_uni))

  if (!is.null(output_dir)) {
    dir <- file.path(output_dir, slugify(a$analysis_name))
    .stage_error("export", {
      export_scenarios(sequential, file.path(dir, "sequential"))
      export_scenarios(univariate, file.path(dir, "univariate"))
    })
  }
  list(sequential = sequential, univariate = univariate)
}
