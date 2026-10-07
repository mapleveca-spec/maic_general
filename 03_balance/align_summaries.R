# align_summaries() ------------------------------------------------------------
#
# Joins an IPD summary and an SLD summary (both in the summary schema) into
# one aligned table, one row per (variable, level), for every metadata variable.
#
# Level set per variable:
#   con: CONTINUOUS_LEVEL, then MISSING_LEVEL if either side has it.
#   cat: metadata$level_order, then MISSING_LEVEL if either side has it.
# Nothing is dropped, nothing is renormalised.
#
# Fill rule for a row one side lacks:
#   - If that side reported the variable (any non-NA est), a missing level is
#     a true zero: est = 0, sd = 0, n = that side's N.
#   - If that side did not report the variable at all, est / sd stay NA.
#     Unknown is not zero.
#   - n for a filled row is max(n) over that side's rows for the variable,
#     which equals the full N under the summary-schema definition of n.
#
# Assumes both summaries were built from the same validated metadata.

align_summaries <- function(ipd_summary, sld_summary, metadata) {
  rows <- lapply(seq_len(nrow(metadata)), function(i) {
    var  <- metadata$variable[i]
    type <- metadata$type[i]

    ipd_v <- ipd_summary[ipd_summary$variable == var, , drop = FALSE]
    sld_v <- sld_summary[sld_summary$variable == var, , drop = FALSE]

    base_levels <- if (type == "con") CONTINUOUS_LEVEL else metadata$level_order[[i]]
    has_missing <- MISSING_LEVEL %in% c(ipd_v$level, sld_v$level)
    levels <- c(base_levels, if (has_missing) MISSING_LEVEL)

    tibble::tibble(
      display_order = metadata$display_order[i],
      variable      = var,
      type          = type,
      level         = levels,
      row_type      = row_type_of(type, levels),
      .fill_side(ipd_v, levels, prefix = "ipd"),
      .fill_side(sld_v, levels, prefix = "sld")
    )
  })

  out <- dplyr::bind_rows(rows)
  # order() is stable, so levels keep their within-variable order.
  out <- out[order(out$display_order), , drop = FALSE]
  out[ALIGNED_COLUMNS]
}

# Returns a 3-column tibble (<prefix>_n, <prefix>_est, <prefix>_sd) aligned to
# `levels`, applying the fill rule above.
.fill_side <- function(side, levels, prefix) {
  idx <- match(levels, side$level)
  n   <- side$n[idx]
  est <- side$est[idx]
  sd  <- side$sd[idx]

  absent   <- is.na(idx)
  reported <- any(!is.na(side$est))
  if (any(absent) && reported) {
    n[absent]   <- max(side$n, na.rm = TRUE)
    est[absent] <- 0
    sd[absent]  <- 0
  } else if (any(absent)) {
    n[absent] <- if (nrow(side) > 0) max(side$n, na.rm = TRUE) else NA_real_
  }

  out <- tibble::tibble(n = n, est = est, sd = sd)
  names(out) <- paste0(prefix, "_", names(out))
  out
}
