# create_balance_table() -------------------------------------------------------
#
# Pipeline over the balance module: filter metadata to show_balance, align the
# two summaries, append SMD. Nothing else lives here; if a new balance feature
# is needed it becomes a new small function in the pipeline, not a branch
# inside this one.
#
# Takes summary tables, not raw data, on purpose:
# - validation happens once at the input layer, not in every consumer;
# - the IPD summary can be unweighted or weighted, so the same function yields
#   both the pre- and post-matching balance table.
#
# Returns a table in BALANCE_COLUMNS order, sorted by display_order.

create_balance_table <- function(ipd_summary, sld_summary, metadata) {
  shown <- metadata[metadata$show_balance, , drop = FALSE]
  if (nrow(shown) == 0) {
    stop("No metadata variable has show_balance = TRUE.", call. = FALSE)
  }
  add_smd(align_summaries(ipd_summary, sld_summary, shown))
}
