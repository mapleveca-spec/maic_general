# estimate_maic_weights() ------------------------------------------------------
#
# Method-of-moments MAIC weights (Signorovitch et al. 2010; NICE DSU TSD 18
# reference implementation) from a design matrix built by
# build_design_matrix().
#
# Solves  min_a  sum_i exp(X_i a)  by BFGS with analytic gradient
# colSums(X * exp(X a)). At the optimum the gradient is zero, i.e. the
# weighted column means of X are zero, i.e. every target moment is met.
# Weights are w_i = exp(X_i a), unscaled. Weighted summaries are invariant to
# the scale, so rescaling is left to reporting.
#
# Convergence follows the TSD 18 recommendation: optim's convergence code must
# be 0, with optim's default stopping rule. Failure is an error. The residual
# moment error is returned as a diagnostic (`max_moment_error`) and is also
# reported by weight_diagnostics(), but does not by itself stop the run.
#
# Implementation notes:
# - Columns are standardised internally before optimisation and `alpha` is
#   mapped back. This only improves conditioning; weights are unchanged.
# - Rows excluded by build_design_matrix() (complete = FALSE) get weight 0, so
#   `weights` has one entry per IPD row and feeds summarize_ipd() directly.
#
# Returns list(weights, alpha, ess, n_complete, max_moment_error, optim).

estimate_maic_weights <- function(design, maxit = 100000) {
  X        <- design$X
  complete <- design$complete
  stopifnot(is.matrix(X), is.logical(complete), nrow(X) == sum(complete))

  scale <- apply(X, 2, stats::sd)
  scale[!is.finite(scale) | scale == 0] <- 1
  x_std <- sweep(X, 2, scale, "/")

  objfn  <- function(a) sum(exp(x_std %*% a))
  gradfn <- function(a) colSums(sweep(x_std, 1, exp(x_std %*% a), "*"))

  fit <- stats::optim(
    par = rep(0, ncol(x_std)), fn = objfn, gr = gradfn,
    method = "BFGS", control = list(maxit = maxit)
  )
  if (fit$convergence != 0) {
    stop("MAIC weight estimation did not converge (optim code ", fit$convergence, ") with ", ncol(X),
         " constraints on ", nrow(X), " rows [", paste(colnames(X), collapse = ", "),
         "]. Try a larger `maxit` or fewer matched moments.", call. = FALSE)
  }

  w_cc  <- as.numeric(exp(x_std %*% fit$par))
  alpha <- fit$par / scale
  names(alpha) <- colnames(X)

  # Residual imbalance on the standardised scale, relative to total weight.
  moment_error <- abs(colSums(x_std * w_cc)) / sum(w_cc)

  weights <- numeric(length(complete))
  weights[complete] <- w_cc

  list(
    weights          = weights,
    alpha            = alpha,
    ess              = effective_n(w_cc),
    n_complete       = sum(complete),
    max_moment_error = max(moment_error),
    optim            = fit[c("value", "counts", "convergence")]
  )
}
