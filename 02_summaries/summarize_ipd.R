# summarize_ipd() --------------------------------------------------------------
#
# Metadata-driven loop: summarises every metadata variable in the IPD one at a
# time and stacks the rows into one summary-schema table.
#
# Design:
# - Dispatches on metadata$type to the vector-level summarisers. No statistics
#   live here; this function only orchestrates.
# - Summarises all metadata variables. Filtering by show_balance / match /
#   adjust is the concern of the module that consumes the summary.
# - Rows come out in metadata order, levels in level_order, Missing last.
#   Sorting by display_order is left to the consumer.
# - `weights` (optional) is one weight per IPD row, passed through unchanged
#   to the vector summarisers. NULL gives the unweighted summary.
# - Assumes inputs have passed validate_metadata() and validate_ipd().

summarize_ipd <- function(ipd, metadata, weights = NULL) {
  if (!is.null(weights)) check_weights(weights, nrow(ipd))

  rows <- lapply(seq_len(nrow(metadata)), function(i) {
    var  <- metadata$variable[i]
    type <- metadata$type[i]
    x    <- ipd[[var]]

    switch(
      type,
      con = summarize_continuous(x, variable = var, weights = weights),
      cat = summarize_categorical(x, variable = var, levels = metadata$level_order[[i]], weights = weights),
      stop("Unknown variable type `", type, "` for `", var, "`.", call. = FALSE)
    )
  })

  dplyr::bind_rows(rows)
}
