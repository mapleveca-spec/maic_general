# Weighted statistics ----------------------------------------------------------
#
# Pure helpers shared by the summarisers and, later, weighting diagnostics.
# All reduce to their unweighted counterparts when every weight is 1.
#
# NA handling is the caller's job: pass already-filtered x and w.

check_weights <- function(w, n) {
  if (!is.numeric(w)) stop("`weights` must be numeric.", call. = FALSE)
  if (length(w) != n) stop("`weights` must have length ", n, ", got ", length(w), ".", call. = FALSE)
  if (anyNA(w)) stop("`weights` contains NA.", call. = FALSE)
  if (any(w < 0)) stop("`weights` contains negative values.", call. = FALSE)
  if (sum(w) <= 0) stop("`weights` must have a positive sum.", call. = FALSE)
  invisible(w)
}

# Kish effective sample size. Equals length(w) when all weights are equal.
effective_n <- function(w) {
  if (length(w) == 0) return(0)
  sum(w)^2 / sum(w^2)
}

weighted_mean <- function(x, w) {
  if (length(x) == 0) return(NA_real_)
  sum(w * x) / sum(w)
}

# Reliability-weights form: denominator sum(w) - sum(w^2)/sum(w), which is
# n - 1 for unit weights. NA when fewer than two effective observations.
weighted_sd <- function(x, w) {
  if (length(x) < 2) return(NA_real_)
  sw <- sum(w)
  denom <- sw - sum(w^2) / sw
  if (denom <= 0) return(NA_real_)
  m <- weighted_mean(x, w)
  sqrt(sum(w * (x - m)^2) / denom)
}
