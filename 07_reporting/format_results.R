# Results formatting -----------------------------------------------------------
#
# back_transform_results()  numeric: adds `measure` and natural-scale columns
#                           est, lo, hi (and boot_lo, boot_hi if present).
#                           log_or -> odds ratio, log_hr -> hazard ratio
#                           (exponentiated); mean_diff left as is.
# format_results()          display table: one string per estimate and
#                           interval, plus N and ESS. Works on a single
#                           analysis `result`, on run_maic_outcomes()$summary,
#                           and on run_maic_scenarios()$results, where the
#                           scenario columns and the weight distribution
#                           (ESS, exclusions, min / quartiles / max, top-10%
#                           share) are carried through.
# format_weight_summary()   the weight distribution as one string per row.
#
# The scale column of a results table is always one of RESULT_SCALES, since
# compare_to_sld() converts unanchored logits and means to a contrast.

RESULT_SCALES <- c(log_or = "Odds ratio", log_hr = "Hazard ratio", mean_diff = "Mean difference")

back_transform_results <- function(results) {
  stopifnot(all(c("scale", "estimate", "ci_low", "ci_high") %in% names(results)))
  bad <- setdiff(unique(results$scale), names(RESULT_SCALES))
  if (length(bad) > 0) stop("Unknown result scale(s): ", paste(bad, collapse = ", "), ".", call. = FALSE)

  f <- function(x) ifelse(results$scale %in% c("log_or", "log_hr"), exp(x), x)
  out <- results
  out$measure <- unname(RESULT_SCALES[results$scale])
  out$est <- f(results$estimate)
  out$lo  <- f(results$ci_low)
  out$hi  <- f(results$ci_high)
  if ("boot_ci_low" %in% names(results)) {
    out$boot_lo <- f(results$boot_ci_low)
    out$boot_hi <- f(results$boot_ci_high)
  }
  out
}

format_results <- function(results, digits = 2, na_label = "NR") {
  bt <- back_transform_results(results)

  tab <- tibble::tibble(
    Outcome    = bt$outcome,
    Comparison = ifelse(bt$anchored, "Anchored", "Unanchored"),
    Contrast   = if ("contrast" %in% names(bt)) bt$contrast else NA_character_,
    Measure    = bt$measure,
    `Estimate (95% CI)` = format_estimate_ci(bt$est, bt$lo, bt$hi, digits, na_label)
  )
  if ("boot_lo" %in% names(bt)) {
    tab$`Bootstrap 95% CI` <- paste0("(", .num(bt$boot_lo, digits), ", ", .num(bt$boot_hi, digits), ")")
    tab$`Bootstrap 95% CI`[is.na(bt$boot_lo)] <- na_label
  }
  tab$N   <- bt$n
  tab$ESS <- round(bt$ess, 1)

  if ("label" %in% names(bt)) {
    tab <- dplyr::bind_cols(
      tibble::tibble(Model = bt$label, Variables = bt$variables),
      tab,
      tibble::tibble(
        `Weighting ESS`   = round(bt$weight_ess, 1),
        `ESS %`           = round(100 * bt$weight_ess_pct),
        `Excluded`        = bt$n_excluded,
        `Weight min`      = .num(bt$w_min, 2),
        `Weight Q1`       = .num(bt$w_q25, 2),
        `Weight median`   = .num(bt$w_median, 2),
        `Weight Q3`       = .num(bt$w_q75, 2),
        `Weight max`      = .num(bt$w_max, 2),
        `Top 10% share`   = paste0(round(100 * bt$top10_share), "%")
      )
    )
  }
  tab
}

# Compact one-line weight summary for narrow tables:
# "min 0.04 | Q1 0.28 | median 0.48 | Q3 1.28 | max 5.37".
format_weight_summary <- function(results) {
  stopifnot(all(c("w_min", "w_q25", "w_median", "w_q75", "w_max") %in% names(results)))
  paste0("min ", .num(results$w_min, 2), " | Q1 ", .num(results$w_q25, 2), " | median ", .num(results$w_median, 2),
         " | Q3 ", .num(results$w_q75, 2), " | max ", .num(results$w_max, 2))
}

format_estimate_ci <- function(est, lo, hi, digits = 2, na_label = "NR") {
  out <- paste0(.num(est, digits), " (", .num(lo, digits), ", ", .num(hi, digits), ")")
  out[is.na(est)] <- na_label
  out
}
