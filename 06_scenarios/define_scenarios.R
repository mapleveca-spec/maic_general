# Scenario definitions ---------------------------------------------------------
#
# A scenario is a metadata table with a particular pattern of one weighting
# flag switched on, plus a label. Nothing here runs an analysis; the runner in
# 08_main executes a scenario list.
#
# Both flags are weighting tiers (see 01_inputs/metadata_schema.R):
#   flag = "adjust"  varies the second tier on top of a fixed primary set:
#                    "add prognostic variables one by one to the effect
#                    modifiers". The runner must weight with
#                    include_adjust = TRUE (its default for such lists).
#   flag = "match"   varies the primary set itself.
#
# Variables listed in `variables` must not already sit in the other tier, so
# a scenario never moves a variable between tiers. Variables not listed have
# the varied flag set FALSE in every scenario. match_sd is kept only where the
# variable remains in some tier, so the metadata stays valid.
#
# define_scenarios_sequential()  scenario k switches on variables[1:k].
#                                include_empty = TRUE adds a scenario with
#                                nothing on in the varied tier (for "adjust":
#                                the primary set alone).
# define_scenarios_univariate()  scenario k switches on variables[k] only.
#
# Each scenario: list(label, flag, variables, metadata).

SCENARIO_FLAGS <- c("adjust", "match")

define_scenarios_sequential <- function(metadata, variables, flag = c("adjust", "match"),
                                        include_empty = FALSE) {
  flag <- match.arg(flag)
  .check_scenario_variables(metadata, variables, flag)
  sets <- lapply(seq_along(variables), function(k) variables[seq_len(k)])
  if (include_empty) sets <- c(list(character()), sets)
  lapply(sets, function(on) .make_scenario(metadata, on, flag, label = .sequential_label(on)))
}

define_scenarios_univariate <- function(metadata, variables, flag = c("adjust", "match")) {
  flag <- match.arg(flag)
  .check_scenario_variables(metadata, variables, flag)
  lapply(variables, function(v) .make_scenario(metadata, v, flag, label = v))
}

.make_scenario <- function(metadata, on, flag, label) {
  m <- metadata
  m[[flag]] <- m$variable %in% on
  m$match_sd <- m$match_sd & (m$match | m$adjust)
  list(label = label, flag = flag, variables = on, metadata = m)
}

.sequential_label <- function(on) {
  if (length(on) == 0) return("(none)")
  paste0("+ ", on[length(on)])
}

.check_scenario_variables <- function(metadata, variables, flag) {
  stopifnot(is.character(variables), length(variables) > 0)
  unknown <- setdiff(variables, metadata$variable)
  if (length(unknown) > 0) {
    stop("Scenario variable(s) not in metadata: ", paste(unknown, collapse = ", "), ".", call. = FALSE)
  }
  if (anyDuplicated(variables) > 0) stop("Scenario `variables` must be unique.", call. = FALSE)

  other <- setdiff(SCENARIO_FLAGS, flag)
  clash <- variables[metadata[[other]][match(variables, metadata$variable)]]
  if (length(clash) > 0) {
    stop("Scenario variable(s) already in the `", other, "` tier: ", paste(clash, collapse = ", "),
         ". A scenario varies one tier and must not move variables between tiers.", call. = FALSE)
  }
  invisible(variables)
}
