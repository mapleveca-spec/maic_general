# problem_collector() ----------------------------------------------------------
#
# Shared by the validators: accumulates problem messages and raises them all
# at once, so an analyst fixes an input table in one pass.
#
# Returns a list of three functions. `add` records one message, `any` says
# whether anything has been recorded, and `report` stops with every recorded
# message under a heading naming the table (`what`), or does nothing if the
# collector is empty.

problem_collector <- function(what) {
  env <- new.env(parent = emptyenv())
  env$problems <- character()

  add <- function(msg) {
    env$problems <- c(env$problems, msg)
    invisible(NULL)
  }
  any_problems <- function() length(env$problems) > 0
  report <- function() {
    if (!any_problems()) return(invisible(NULL))
    stop(
      "Invalid ", what, " (", length(env$problems), " problem(s)):\n",
      paste0("  - ", env$problems, collapse = "\n"),
      call. = FALSE
    )
  }

  list(add = add, any = any_problems, report = report)
}
