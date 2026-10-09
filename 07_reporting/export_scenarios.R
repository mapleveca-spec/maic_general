# Scenario export --------------------------------------------------------------
#
# Turns a run_scenarios_*() result into files laid out the same way for every
# analysis, so a project with many analyses is navigable:
#
#   <dir>/
#     results.csv            formatted scenario table, one row per model, with
#                            status, error, and that model's weight distribution
#     results_numeric.csv    the unformatted results tibble
#     balance_path.csv       one table: how balance changes step by step,
#                            from the balance-path function below
#     weights/01_none.png    weight histogram per successful step, numbered
#     weights/02_fac_age.png
#     manifest.csv           step, label, variables, status, error, plot path
#
# Failed scenarios have no weights or balance: their histogram is skipped and
# their balance-path columns read `failed_label`.
#
# scenario_balance_path()  the step-by-step balance table (see below)
# export_scenarios()       writes the layout above; returns the manifest
# slugify()                file-safe name from a label

slugify <- function(x) {
  s <- tolower(gsub("[^A-Za-z0-9]+", "_", trimws(as.character(x))))
  s <- gsub("^_+|_+$", "", s)
  ifelse(nzchar(s), s, "none")
}

# One table showing how balance changes across the scenarios of a run.
# Rows are the balance rows (variable, level). Columns:
#   Variable, Level, SLD,
#   "Unweighted | IPD", "Unweighted | SMD",
#   then for each scenario in order: "<label> | IPD", "<label> | SMD"
# where IPD is the (weighted) IPD summary cell, mean (SD) or %, and SMD is the
# standardised mean difference against the SLD after that scenario's
# weighting. A naive scenario reproduces the unweighted pair, which shows the
# starting position in the same table. A failed scenario's pair reads
# `failed_label`.
scenario_balance_path <- function(scenario_result, digits = 1, smd_digits = 3, na_label = "NR",
                                  failed_label = "failed") {
  runs <- scenario_result$runs
  stopifnot(length(runs) > 0)
  ok <- which(vapply(runs, function(r) identical(r$status, "ok"), logical(1)))
  if (length(ok) == 0) stop("Every scenario failed; there is no balance to tabulate.", call. = FALSE)

  base <- runs[[ok[1]]]$balance_before
  keys <- base[c("variable", "level", "row_type")]
  for (i in ok) {
    if (!identical(runs[[i]]$balance_after[c("variable", "level", "row_type")], keys)) {
      stop("Scenarios do not share balance rows; they must come from the same metadata and IPD.", call. = FALSE)
    }
  }

  pair <- function(b, name) {
    out <- if (is.null(b)) {
      tibble::tibble(rep(failed_label, nrow(base)), rep(failed_label, nrow(base)), .name_repair = "minimal")
    } else {
      tibble::tibble(
        format_cell(b$row_type, b$ipd_est, b$ipd_sd, digits, na_label),
        format_smd(b$smd, smd_digits, na_label),
        .name_repair = "minimal"
      )
    }
    names(out) <- paste(name, c("IPD", "SMD"), sep = " | ")
    out
  }

  steps <- lapply(runs, function(r) pair(if (identical(r$status, "ok")) r$balance_after else NULL, r$scenario$label))
  dplyr::bind_cols(
    tibble::tibble(
      Variable = ifelse(duplicated(base$variable), "", base$variable),
      Level    = base$level,
      SLD      = format_cell(base$row_type, base$sld_est, base$sld_sd, digits, na_label)
    ),
    pair(base, "Unweighted"),
    steps,
    .name_repair = "minimal"
  )
}

export_scenarios <- function(scenario_result, dir, plot_width = 7, plot_height = 4.5, dpi = 150, ...) {
  runs <- scenario_result$runs
  stopifnot(length(runs) > 0)
  dir.create(file.path(dir, "weights"), recursive = TRUE, showWarnings = FALSE)

  step   <- seq_along(runs)
  label  <- vapply(runs, function(r) r$scenario$label, "")
  status <- vapply(runs, function(r) r$status, "")
  stem   <- sprintf("%02d_%s", step, slugify(sub("^\\+\\s*", "", label)))

  weight_files <- ifelse(status == "ok", file.path(dir, "weights", paste0(stem, ".png")), NA_character_)
  for (i in step[status == "ok"]) {
    title <- paste0("MAIC weights: ", label[i], if (!runs[[i]]$weighted) " (unweighted)")
    ggplot2::ggsave(weight_files[i], plot_weights(runs[[i]]$weight_fit, title = title),
                    width = plot_width, height = plot_height, dpi = dpi)
  }

  utils::write.csv(format_results(scenario_result$results), file.path(dir, "results.csv"), row.names = FALSE)
  utils::write.csv(scenario_result$results, file.path(dir, "results_numeric.csv"), row.names = FALSE)
  if (any(status == "ok")) {
    utils::write.csv(scenario_balance_path(scenario_result, ...), file.path(dir, "balance_path.csv"),
                     row.names = FALSE)
  }

  manifest <- tibble::tibble(
    step      = step,
    label     = label,
    variables = vapply(runs, function(r) paste(r$scenario$variables, collapse = ", "), ""),
    status    = status,
    error     = vapply(runs, function(r) if (is.null(r$error)) NA_character_ else r$error, ""),
    weighted  = vapply(runs, function(r) if (is.null(r$weighted)) NA else r$weighted, logical(1)),
    weights   = weight_files
  )
  utils::write.csv(manifest, file.path(dir, "manifest.csv"), row.names = FALSE)
  invisible(manifest)
}
