# plot_weights() ---------------------------------------------------------------
#
# Histogram of the MAIC weights for one analysis, on the rescaled scale where
# a weight of 1 means "counts as one patient". Rows with weight 0 (excluded
# for missing matched covariates) are not drawn; their number is stated in
# the subtitle together with ESS, so the picture and the diagnostics agree.
#
# A dashed line marks weight 1. Weights far to the right are the patients
# who dominate the weighted analysis; the top-10% share in
# weight_diagnostics() quantifies what the tail shows.
#
# `fit` is an estimate_maic_weights() result (as `weight_fit` in a
# run_maic_anchored() / run_maic_unanchored() result), or any list with a `weights` vector.
# Returns a ggplot object; save it with ggplot2::ggsave().

plot_weights <- function(fit, bins = 30, title = "Distribution of MAIC weights") {
  w <- if (is.list(fit)) fit$weights else fit
  check_weights(w, length(w))
  wr <- rescale_weights(w)
  d  <- weight_diagnostics(list(weights = w, max_moment_error = NA_real_))

  subtitle <- sprintf(
    "%d weighted patients (%d excluded), ESS = %.1f (%.0f%% of weighted), max weight = %.2f",
    d$n_complete, d$n_excluded, d$ess, 100 * d$ess_pct, d$w_max
  )

  ggplot2::ggplot(data.frame(weight = wr[wr > 0]), ggplot2::aes(x = weight)) +
    ggplot2::geom_histogram(bins = bins, fill = "#256abf", colour = "#fcfcfb", linewidth = 0.4, boundary = 0) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "#52514e", linewidth = 0.5) +
    ggplot2::annotate("text", x = 1, y = Inf, label = "weight = 1", vjust = 1.5, hjust = -0.1,
                      colour = "#52514e", size = 3.3) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::labs(title = title, subtitle = subtitle, x = "Rescaled weight (1 = one patient)", y = "Patients") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = "#fcfcfb", colour = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "#e6e5e1", linewidth = 0.4),
      axis.text  = ggplot2::element_text(colour = "#52514e"),
      axis.title = ggplot2::element_text(colour = "#52514e"),
      plot.title = ggplot2::element_text(colour = "#0b0b0b", face = "bold"),
      plot.subtitle = ggplot2::element_text(colour = "#52514e", size = 10)
    )
}
