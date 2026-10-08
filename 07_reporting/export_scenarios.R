# Scenario export --------------------------------------------------------------
#
# Turns a run_maic_scenarios() result into files laid out the same way for
# every analysis, so a project with many analyses is navigable:
#
#   <dir>/
#     results.csv            formatted scenario table, one row per model, with
#                            that model's weight distribution
#     results_numeric.csv    the unformatted results tibble
#     balance_path.csv       one table: how balance changes step by step
#                            (scenario_balance_path())
#     weights/01_none.png    weight histogram per step, numbered in order
#     weights/02_fac_age.png
#     manifest.csv           step, label, variables, weighted, plot path
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
# starting position in the same table.
scenario_balance_path <- function(scenario_result, digits = 1, smd_digits = 3, na_label = "NR") {
  runs <- scenario_result$runs
  stopifnot(length(runs) > 0)
  base <- runs[[1]]$balance_before
  keys <- base[c("variable", "level", "row_type")]
  for (r in runs) {
    if (!identical(r$balance_after[c("variable", "level", "row_type")], keys)) {
      stop("Scenarios do not share balance rows; they must come from the same metadata and IPD.", call. = FALSE)
    }
  }

  pair <- function(b, name) {
    out <- tibble::tibble(
      format_cell(b$row_type, b$ipd_est, b$ipd_sd, digits, na_label),
      format_smd(b$smd, smd_digits, na_label),
      .name_repair = "minimal"
    )
    names(out) <- paste(name, c("IPD", "SMD"), sep = " | ")
    out
  }

  steps <- lapply(runs, function(r) pair(r$balance_after, r$scenario$label))
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

  step  <- seq_along(runs)
  label <- vapply(runs, function(r) r$scenario$label, "")
  stem  <- sprintf("%02d_%s", step, slugify(sub("^\\+\\s*", "", label)))

  weight_files <- file.path(dir, "weights", paste0(stem, ".png"))
  for (i in step) {
    title <- paste0("MAIC weights: ", label[i], if (!runs[[i]]$weighted) " (unweighted)")
    ggplot2::ggsave(weight_files[i], plot_weights(runs[[i]]$weight_fit, title = title),
                    width = plot_width, height = plot_height, dpi = dpi)
  }

  utils::write.csv(format_results(scenario_result$results), file.path(dir, "results.csv"), row.names = FALSE)
  utils::write.csv(scenario_result$results, file.path(dir, "results_numeric.csv"), row.names = FALSE)
  utils::write.csv(scenario_balance_path(scenario_result, ...), file.path(dir, "balance_path.csv"), row.names = FALSE)

  manifest <- tibble::tibble(
    step      = step,
    label     = label,
    variables = vapply(runs, function(r) paste(r$scenario$variables, collapse = ", "), ""),
    weighted  = vapply(runs, `[[`, logical(1), "weighted"),
    weights   = weight_files
  )
  utils::write.csv(manifest, file.path(dir, "manifest.csv"), row.names = FALSE)
  invisible(manifest)
}
