# fit_outcome_model() ----------------------------------------------------------
#
# Fits one weighted outcome model on the IPD and returns the single estimate
# a MAIC reports for it. Following NICE TSD 18, the model carries no
# covariates: all adjustment is done by the weights.
#
# Anchored (arm given):   outcome ~ arm, weighted. `reference_arm` is required
#                         and names the comparator; the estimate is the other
#                         arm vs the reference.
#   binary      log odds ratio   (quasibinomial glm, HC0 sandwich SE)
#   continuous  mean difference  (gaussian glm, HC0 sandwich SE)
#   tte         log hazard ratio (coxph, robust SE)
# Unanchored (arm NULL):  intercept-only weighted model on the rows given.
#   binary      logit of weighted proportion
#   continuous  weighted mean
#   tte         not supported: needs pseudo-IPD from the SLD curve.
#
# Rows with weight 0 or NA in the outcome / arm are dropped; `n` and `ess`
# report what was used. Weights are rescaled to sum to n before fitting so
# robust SEs do not depend on the arbitrary scale of the raw weights.
#
# Returns list(outcome, anchored, contrast, n, ess, model, estimate) where
# `estimate` is a one-row tibble: outcome, term, scale, estimate, se,
# ci_low, ci_high.

fit_outcome_model <- function(ipd, outcome, weights = NULL,
                              arm = NULL, reference_arm = NULL, conf_level = 0.95) {
  stopifnot(inherits(outcome, "maic_outcome"))
  anchored <- !is.null(arm)
  if (!anchored && outcome$type == "tte") {
    stop("Unanchored tte comparison is not supported: it needs pseudo-IPD from the SLD.", call. = FALSE)
  }
  if (anchored && is.null(reference_arm)) {
    stop("`reference_arm` is required with `arm`: it names the comparator the estimate is relative to.",
         call. = FALSE)
  }

  needed <- c(outcome_columns(outcome), arm)
  absent <- setdiff(needed, names(ipd))
  if (length(absent) > 0) {
    stop("Column(s) absent from IPD: ", paste(absent, collapse = ", "), ".", call. = FALSE)
  }

  w <- if (is.null(weights)) rep(1, nrow(ipd)) else check_weights(weights, nrow(ipd))
  keep <- w > 0 & stats::complete.cases(ipd[needed])
  dat  <- ipd[keep, needed, drop = FALSE]
  w    <- rescale_weights(w[keep])
  if (anchored) {
    dat[[arm]] <- .relevel_arm(dat[[arm]], reference_arm)
    if (nlevels(dat[[arm]]) != 2) stop("`arm` must have exactly two levels.", call. = FALSE)
  }

  rhs <- if (anchored) arm else "1"
  fit <- switch(
    outcome$type,
    binary     = .fit_glm(dat, outcome$var, rhs, w, stats::quasibinomial()),
    continuous = .fit_glm(dat, outcome$var, rhs, w, stats::gaussian()),
    tte        = .fit_cox(dat, outcome$var, outcome$event, rhs, w)
  )

  other    <- if (anchored) levels(dat[[arm]])[2] else NULL
  term     <- if (anchored) paste0(arm, other) else "(Intercept)"
  contrast <- if (anchored) paste(other, "vs", reference_arm) else "IPD (unanchored)"
  scale    <- estimate_scale(outcome$type, anchored)
  est      <- unname(fit$coef[term])
  se       <- sqrt(fit$vcov[term, term])
  z        <- stats::qnorm(1 - (1 - conf_level) / 2)

  list(
    outcome  = outcome,
    anchored = anchored,
    contrast = contrast,
    n        = nrow(dat),
    ess      = effective_n(w),
    model    = fit$model,
    estimate = tibble::tibble(
      outcome = outcome$name, term = term, scale = scale,
      estimate = est, se = se, ci_low = est - z * se, ci_high = est + z * se
    )
  )
}

.fit_glm <- function(dat, y, rhs, w, family) {
  dat$.w <- w
  f <- stats::as.formula(paste(y, "~", rhs))
  m <- stats::glm(f, data = dat, weights = .w, family = family)
  list(model = m, coef = stats::coef(m), vcov = sandwich::vcovHC(m, type = "HC0"))
}

.fit_cox <- function(dat, time, event, rhs, w) {
  dat$.w <- w
  f <- stats::as.formula(paste0("survival::Surv(", time, ", ", event, ") ~ ", rhs))
  m <- survival::coxph(f, data = dat, weights = .w, robust = TRUE)
  list(model = m, coef = stats::coef(m), vcov = stats::vcov(m))
}

.relevel_arm <- function(x, reference_arm) {
  x <- droplevels(factor(x))
  if (!reference_arm %in% levels(x)) stop("`reference_arm` is not an arm level.", call. = FALSE)
  stats::relevel(x, ref = reference_arm)
}
