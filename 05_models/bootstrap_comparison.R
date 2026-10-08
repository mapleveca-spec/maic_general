# bootstrap_comparison() -------------------------------------------------------
#
# Nonparametric bootstrap of one MAIC comparison, as recommended in NICE DSU
# TSD 18: resample IPD rows, re-estimate the weights, refit, compare. The
# percentile interval therefore includes uncertainty in the weights, which
# the robust single-fit SE treats as fixed.
#
# The IPD passed in is the analysis set already chosen by resolve_arms():
# both arms for an anchored comparison (resampling is then stratified by
# arm), or the intervention arm alone for an unanchored one. The SLD summary
# is fixed: published numbers do not vary.
#
# Replicates are never trimmed. Separation in a small resample can give an
# extreme but finite estimate; such replicates stay in, where they sit in the
# tails of the percentile interval, and are counted in `n_extreme`
# (log-scale |estimate| > `extreme_bound`, default 5, i.e. a ratio beyond
# ~150). A high extreme share means the comparison is unstable at this sample
# size; that is a finding to report, not something the code hides. The
# bootstrap SE is a diagnostic, not a headline, because a few extreme
# replicates dominate it.
#
# A replicate fails, and is excluded, only when it yields no estimate:
#   infeasible    a resample lacks a level the SLD reports, or has no
#                 complete rows (build_match_targets() / build_design_matrix())
#   nonconverged  the weight solver did not converge
#   degenerate    the outcome model returned a non-finite estimate
# Failures are counted by reason and never imputed. More than `max_fail_rate`
# failures in total is an error.
#
# Returns list(summary, diagnostics, replicates, ess, n_failed,
#              failure_counts, failures):
#   summary         one row: outcome, anchored, scale, boot_ci_low,
#                   boot_ci_high, n_boot_ok, n_extreme, extreme_share
#   diagnostics     one row: outcome, anchored, boot_se, boot_median,
#                   ess_median, ess_min, ess_max
#   replicates      numeric vector of successful replicate estimates
#   ess             weighting ESS per successful replicate
#   n_failed        total failed replicates
#   failure_counts  named integer vector over FAILURE_REASONS
#   failures        character vector of the error messages from failed replicates

FAILURE_REASONS <- c("infeasible", "nonconverged", "degenerate")

bootstrap_comparison <- function(ipd, sld_summary, metadata, outcome, sld_outcome,
                                 arm = NULL, reference_arm = NULL, include_adjust = FALSE,
                                 n_boot = 1000, seed = NULL, conf_level = 0.95, max_fail_rate = 0.05,
                                 extreme_bound = 5, na_action = c("complete_case", "error"), ...) {
  stopifnot(is.numeric(n_boot), n_boot >= 2, is.numeric(extreme_bound), extreme_bound > 0)
  na_action <- match.arg(na_action)
  check_outcome_pair(outcome, sld_outcome)
  if (!is.null(seed)) set.seed(seed)

  strata <- if (is.null(arm)) rep(1L, nrow(ipd)) else as.integer(factor(ipd[[arm]]))
  draw <- function() unlist(lapply(split(seq_len(nrow(ipd)), strata), function(i) sample(i, replace = TRUE)))

  # Replicate SEs are never used, so variance warnings from extreme resamples
  # (near-singular sandwich, NaN SE) are suppressed; errors are still caught.
  reps <- lapply(seq_len(n_boot), function(b) {
    tryCatch(
      suppressWarnings(.one_replicate(ipd[draw(), , drop = FALSE], sld_summary, metadata, outcome, sld_outcome,
                                      arm, reference_arm, include_adjust, na_action, ...)),
      error = function(e) structure(conditionMessage(e), class = "boot_failure")
    )
  })

  failed   <- vapply(reps, inherits, logical(1), "boot_failure")
  failures <- unlist(reps[failed])
  counts   <- .count_failures(failures)
  n_failed <- sum(failed)
  if (n_failed / n_boot > max_fail_rate) {
    stop(
      n_failed, " of ", n_boot, " bootstrap replicates failed (max allowed ", max_fail_rate * 100, "%): ",
      paste(names(counts), counts, sep = " = ", collapse = ", "), ". First error: ", failures[1],
      call. = FALSE
    )
  }
  ok         <- reps[!failed]
  replicates <- vapply(ok, `[[`, numeric(1), "estimate")
  ess        <- vapply(ok, `[[`, numeric(1), "ess")
  scale      <- ok[[1]]$scale
  is_log     <- scale %in% c("log_or", "log_hr")
  extreme    <- is_log & abs(replicates) > extreme_bound

  alpha <- (1 - conf_level) / 2
  summary <- tibble::tibble(
    outcome       = outcome$name,
    anchored      = sld_outcome$anchored,
    scale         = scale,
    boot_ci_low   = unname(stats::quantile(replicates, alpha)),
    boot_ci_high  = unname(stats::quantile(replicates, 1 - alpha)),
    n_boot_ok     = length(replicates),
    n_extreme     = sum(extreme),
    extreme_share = mean(extreme)
  )
  diagnostics <- tibble::tibble(
    outcome     = outcome$name,
    anchored    = sld_outcome$anchored,
    boot_se     = stats::sd(replicates),
    boot_median = stats::median(replicates),
    ess_median  = stats::median(ess),
    ess_min     = min(ess),
    ess_max     = max(ess)
  )

  list(
    summary        = summary,
    diagnostics    = diagnostics,
    replicates     = replicates,
    ess            = ess,
    n_failed       = n_failed,
    failure_counts = counts,
    failures       = failures
  )
}

# One full pass on a resampled IPD. Returns the comparison estimate, its
# scale, and the weighting ESS. A non-finite estimate is raised as a
# "Degenerate replicate" error.
.one_replicate <- function(ipd_b, sld_summary, metadata, outcome, sld_outcome, arm, reference_arm,
                           include_adjust, na_action, ...) {
  fit     <- estimate_weights(ipd_b, summarize_ipd(ipd_b, metadata), sld_summary, metadata,
                              include_adjust = include_adjust, na_action = na_action, ...)$fit
  model   <- fit_outcome_model(ipd_b, outcome, weights = fit$weights, arm = arm, reference_arm = reference_arm)
  result  <- compare_to_sld(model, sld_outcome)
  if (!is.finite(result$estimate)) {
    stop("Degenerate replicate: non-finite estimate for ", outcome$name, ".", call. = FALSE)
  }
  list(estimate = result$estimate, scale = result$scale, ess = fit$ess)
}

.count_failures <- function(failures) {
  reason <- ifelse(
    grepl("^Degenerate replicate", failures), "degenerate",
    ifelse(grepl("did not converge", failures), "nonconverged", "infeasible")
  )
  counts <- table(factor(reason, levels = FAILURE_REASONS))
  stats::setNames(as.integer(counts), FAILURE_REASONS)
}
