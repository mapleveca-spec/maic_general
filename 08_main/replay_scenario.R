# replay_scenario() / scenario_replay_code() -----------------------------------
#
# Debugging aids for a scenario run. A failed step is recorded, not thrown,
# so to investigate it you want to re-run exactly that step on its own
# through the main function and get the ordinary error and traceback.
#
# replay_scenario, arguments: scenario_result, step, ipd, sld, options.
#   Re-runs step `step` with the metadata, include_adjust, arms, outcome and
#   published result that the scenario run used, via run_maic_unanchored() or
#   run_maic_anchored(). Errors propagate normally. Extra arguments (n_boot,
#   maxit, na_action, ...) pass through, so you can also re-run with changed
#   settings. The IPD and SLD are not stored in the run (they are the same
#   for every step), so you pass them.
#
# scenario_replay_code, arguments: scenario_result, step, and the names of your
#   data objects (ipd = "ipd", sld = "sld") and run object (run_object = "out").
#   Prints, and returns invisibly, standalone R code that reproduces the step:
#   the step's metadata pulled from the run object, then the main-function
#   call. Paste it into the console or a script. `ipd` / `sld` are the names
#   of your data objects as they should appear in the code.
#
# failed_steps(scenario_result) lists the failed steps with label, variables,
#   error, and the replay call, for a quick overview.

replay_scenario <- function(scenario_result, step, ipd, sld, ...) {
  r <- .get_run(scenario_result, step)
  args <- list(ipd, sld, r$scenario$metadata, r$outcome, r$sld_outcome,
               include_adjust = r$include_adjust, conf_level = r$conf_level)
  if (r$kind == "anchored") args <- c(args, list(arm = r$arm, reference_arm = r$reference_arm))
  args <- utils::modifyList(args, list(...))   # anything passed here overrides the recorded setting
  do.call(if (r$kind == "unanchored") run_maic_unanchored else run_maic_anchored, args)
}

scenario_replay_code <- function(scenario_result, step, ipd = "ipd", sld = "sld", run_object = "out") {
  r <- .get_run(scenario_result, step)
  on <- r$scenario$metadata$variable[weighting_variables(r$scenario$metadata, r$include_adjust)]
  lines <- c(
    sprintf("# Scenario %d: %s (%s = %s); weighting set: %s", step, r$scenario$label, r$scenario$flag,
            if (length(r$scenario$variables)) paste(r$scenario$variables, collapse = ", ") else "none",
            if (length(on)) paste(on, collapse = ", ") else "none (naive)"),
    if (identical(r$status, "error")) paste0("# Error recorded: ", r$error),
    sprintf("step_metadata <- %s$runs[[%d]]$scenario$metadata", run_object, step),
    sprintf("outcome       <- %s$runs[[%d]]$outcome", run_object, step),
    sprintf("sld_outcome   <- %s$runs[[%d]]$sld_outcome", run_object, step),
    if (r$kind == "unanchored") {
      sprintf("run_maic_unanchored(%s, %s, step_metadata, outcome, sld_outcome, include_adjust = %s)",
              ipd, sld, r$include_adjust)
    } else {
      sprintf(paste0("run_maic_anchored(%s, %s, step_metadata, outcome, sld_outcome, ",
                     "arm = \"%s\", reference_arm = \"%s\", include_adjust = %s)"),
              ipd, sld, r$arm, r$reference_arm, r$include_adjust)
    }
  )
  code <- paste(lines, collapse = "\n")
  cat(code, "\n")
  invisible(code)
}

failed_steps <- function(scenario_result, ipd = "ipd", sld = "sld", run_object = "out") {
  idx <- which(vapply(scenario_result$runs, function(r) identical(r$status, "error"), logical(1)))
  tibble::tibble(
    step      = idx,
    label     = vapply(idx, function(i) scenario_result$runs[[i]]$scenario$label, ""),
    variables = vapply(idx, function(i) paste(scenario_result$runs[[i]]$scenario$variables, collapse = ", "), ""),
    error     = vapply(idx, function(i) scenario_result$runs[[i]]$error, ""),
    replay    = vapply(idx, function(i) {
      utils::capture.output(code <- scenario_replay_code(scenario_result, i, ipd, sld, run_object))
      sub(".*\n", "", code)   # the main-function call, last line of the code
    }, "")
  )
}

.get_run <- function(scenario_result, step) {
  stopifnot(is.list(scenario_result$runs), length(step) == 1, step >= 1, step <= length(scenario_result$runs))
  scenario_result$runs[[step]]
}
