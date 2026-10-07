# compare_balance_tables() -----------------------------------------------------
#
# Joins a pre-weighting and a post-weighting balance table (BALANCE_COLUMNS)
# into one numeric table, one row per (variable, level). The SLD side is
# taken from `before` and checked to be identical in `after`.
#
# Columns: display_order, variable, type, level, row_type,
#          sld_n, sld_est, sld_sd,
#          ipd_n_before, ipd_est_before, ipd_sd_before, smd_before,
#          ipd_n_after,  ipd_est_after,  ipd_sd_after,  smd_after
#
# Numeric only; format_balance_comparison() turns it into display strings.

BALANCE_KEYS <- c("display_order", "variable", "type", "level", "row_type")

compare_balance_tables <- function(before, after) {
  stopifnot(all(BALANCE_COLUMNS %in% names(before)), all(BALANCE_COLUMNS %in% names(after)))
  if (!identical(before[BALANCE_KEYS], after[BALANCE_KEYS])) {
    stop("`before` and `after` must have identical rows (same variables, levels, order).", call. = FALSE)
  }
  sld_cols <- c("sld_n", "sld_est", "sld_sd")
  if (!isTRUE(all.equal(before[sld_cols], after[sld_cols]))) {
    stop("SLD columns differ between `before` and `after`; they must come from the same SLD summary.", call. = FALSE)
  }

  ipd_cols <- c("ipd_n", "ipd_est", "ipd_sd", "smd")
  b <- before[ipd_cols]
  a <- after[ipd_cols]
  names(b) <- paste0(ipd_cols, "_before")
  names(a) <- paste0(ipd_cols, "_after")

  dplyr::bind_cols(before[c(BALANCE_KEYS, sld_cols)], b, a)
}
