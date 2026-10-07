# build_match_targets() --------------------------------------------------------
#
# Builds the target-moment table for weighting from the IPD and SLD summaries,
# for the weighting set (weighting_variables(): `match`, plus `adjust` when
# include_adjust = TRUE).
#
# Per variable:
#   con  one "mean" constraint, target = SLD mean; plus, if match_sd, one
#        "variance" constraint, target = SLD mean^2 + sd^2 (second moment).
#   cat  Missing row dropped; if the SLD had missing mass, observed-level
#        proportions are rescaled to sum to 1 and the removed mass is kept in
#        `sld_missing`. The first level in level_order is the reference and is
#        dropped, giving K - 1 "proportion" constraints.
#
# Feasibility is checked here because the aligned table already has both
# sides; it is cheap and failing early beats a solver that silently diverges:
#   - SLD not reported                        -> error
#   - cat: target > 0 where IPD proportion = 0 -> error (no rows to upweight)
#   - cat: target = 0 where IPD proportion > 0 -> error (would zero out rows;
#          drop the level or stop matching the variable instead)
#   Continuous range feasibility (target inside the IPD's observed range)
#   needs raw IPD, not a summary, so it belongs to the design-matrix step.
#
# Returns a tibble in TARGET_COLUMNS order, variables in display_order.

build_match_targets <- function(ipd_summary, sld_summary, metadata, include_adjust = FALSE) {
  matched <- metadata[weighting_variables(metadata, include_adjust), , drop = FALSE]
  if (nrow(matched) == 0) {
    stop("The weighting set is empty: no metadata variable has match = TRUE",
         if (include_adjust) " or adjust = TRUE", ".", call. = FALSE)
  }

  aligned <- align_summaries(ipd_summary, sld_summary, matched)

  unreported <- unique(aligned$variable[aligned$row_type == "summary" & is.na(aligned$sld_est)])
  unreported <- union(unreported, unique(aligned$variable[aligned$row_type == "level" & is.na(aligned$sld_est)]))
  if (length(unreported) > 0) {
    stop("Cannot match on SLD-unreported variable(s): ", paste(unreported, collapse = ", "), ".", call. = FALSE)
  }

  rows <- lapply(split(aligned, factor(aligned$variable, levels = unique(aligned$variable))), function(v) {
    if (v$type[1] == "con") .targets_continuous(v, matched) else .targets_categorical(v, matched)
  })

  out <- dplyr::bind_rows(rows)
  rownames(out) <- NULL
  out[TARGET_COLUMNS]
}

.targets_continuous <- function(v, metadata) {
  s   <- v[v$row_type == "summary", ]
  var <- s$variable
  out <- tibble::tibble(
    variable = var, level = CONTINUOUS_LEVEL, moment = "mean",
    term = make_term(var, CONTINUOUS_LEVEL, "mean"), target = s$sld_est, sld_missing = 0
  )

  if (isTRUE(metadata$match_sd[metadata$variable == var])) {
    out <- dplyr::bind_rows(out, tibble::tibble(
      variable = var, level = CONTINUOUS_LEVEL, moment = "variance",
      term = make_term(var, CONTINUOUS_LEVEL, "variance"),
      target = s$sld_est^2 + s$sld_sd^2, sld_missing = 0
    ))
  }
  out
}

.targets_categorical <- function(v, metadata) {
  var <- v$variable[1]
  missing_mass <- sum(v$sld_est[v$row_type == "missing"])
  obs <- v[v$row_type == "level", ]

  target <- obs$sld_est / (1 - missing_mass)
  ipd_p  <- obs$ipd_est

  bad_up   <- target > 0 & ipd_p == 0
  bad_down <- target == 0 & ipd_p > 0
  if (any(bad_up)) {
    stop("`", var, "`: SLD has level(s) absent from IPD, cannot match: ",
         paste(obs$level[bad_up], collapse = ", "), ".", call. = FALSE)
  }
  if (any(bad_down)) {
    stop("`", var, "`: SLD proportion is 0 for level(s) present in IPD, matching would zero their weights: ",
         paste(obs$level[bad_down], collapse = ", "), ".", call. = FALSE)
  }

  reference <- metadata$level_order[[which(metadata$variable == var)]][1]
  keep <- obs$level != reference

  tibble::tibble(
    variable = var, level = obs$level[keep], moment = "proportion",
    term = make_term(var, obs$level[keep]), target = target[keep], sld_missing = missing_mass
  )
}
