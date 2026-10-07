# Balance table formatting -----------------------------------------------------
#
# Turns numeric balance tables into display strings. Formatting is driven by
# row_type, never by inspecting `level`:
#   summary  mean (SD)             e.g. "49.4 (11.0)"
#   level    percentage            e.g. "46.7%"
#   missing  percentage            e.g. "8.3%"
# NA estimates render as `na_label` ("NR", not reported). SMDs render with
# `smd_digits`; infinite SMDs render as "Inf" / "-Inf".
#
# format_balance_table()       one balance table -> SLD, IPD, SMD strings
# format_balance_comparison()  compare_balance_tables() output -> SLD,
#                              IPD before, IPD after, SMD before, SMD after
#
# Variable labels are shown once per variable in `Variable`, with the level
# in `Level`; a continuous summary row has an empty Level and a Missing row
# shows the reserved label. Reporting layers (Word, HTML) consume these
# character tables as they are.

format_balance_table <- function(balance, digits = 1, smd_digits = 3, na_label = "NR") {
  tibble::tibble(
    Variable = .first_of_group(balance$variable),
    Level    = balance$level,
    SLD      = format_cell(balance$row_type, balance$sld_est, balance$sld_sd, digits, na_label),
    IPD      = format_cell(balance$row_type, balance$ipd_est, balance$ipd_sd, digits, na_label),
    SMD      = format_smd(balance$smd, smd_digits, na_label)
  )
}

format_balance_comparison <- function(comparison, digits = 1, smd_digits = 3, na_label = "NR") {
  cm <- comparison
  tibble::tibble(
    Variable     = .first_of_group(cm$variable),
    Level        = cm$level,
    SLD          = format_cell(cm$row_type, cm$sld_est, cm$sld_sd, digits, na_label),
    `IPD before` = format_cell(cm$row_type, cm$ipd_est_before, cm$ipd_sd_before, digits, na_label),
    `IPD after`  = format_cell(cm$row_type, cm$ipd_est_after, cm$ipd_sd_after, digits, na_label),
    `SMD before` = format_smd(cm$smd_before, smd_digits, na_label),
    `SMD after`  = format_smd(cm$smd_after, smd_digits, na_label)
  )
}

# One display string per row, chosen by row_type.
format_cell <- function(row_type, est, sd, digits = 1, na_label = "NR") {
  out <- ifelse(
    row_type == "summary",
    paste0(.num(est, digits), " (", .num(sd, digits), ")"),
    paste0(.num(100 * est, digits), "%")
  )
  out[is.na(est)] <- na_label
  out
}

format_smd <- function(smd, digits = 3, na_label = "NR") {
  out <- .num(smd, digits)
  out[is.infinite(smd)] <- ifelse(smd[is.infinite(smd)] > 0, "Inf", "-Inf")
  out[is.na(smd)] <- na_label
  out
}

# formatC() keeps the sign of a negative number that rounds to zero ("-0.000");
# strip it so a matched term reads 0.000.
.num <- function(x, digits) sub("^-(0(\\.0+)?)$", "\\1", formatC(x, format = "f", digits = digits))

.first_of_group <- function(x) ifelse(duplicated(x), "", x)
