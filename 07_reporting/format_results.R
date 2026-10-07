# Results formatting -----------------------------------------------------------
#
# back_transform_results()  numeric: adds `measure` and natural-scale columns
#                           est, lo, hi (and boot_lo, boot_hi if present).
#                           log_or -> odds ratio, log_hr -> hazard ratio
#                           (exponentiated); mean_diff left as is.
# format_results()          display table: one string per estimate and
#                           interval, plus N and ESS. Works on
#                           run_maic_analysis()$results and on
#                           run_maic_scenarios()$results (scenario columns
#                           are carried through when present).
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
      tibble::tibble(`Weighting ESS` = round(bt$weight_ess, 1))
    )
  }
  tab
}

format_estimate_ci <- function(est, lo, hi, digits = 2, na_label = "NR") {
  out <- paste0(.num(est, digits), " (", .num(lo, digits), ", ", .num(hi, digits), ")")
  out[is.na(est)] <- na_label
  out
}
