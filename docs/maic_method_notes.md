# MAIC: main steps and key decisions

A working summary of matching-adjusted indirect comparison (MAIC) as described
in NICE Decision Support Unit Technical Support Document 18 (Phillippo et al.,
2016, *Methods for population-adjusted indirect comparisons in submissions to
NICE*), with the decisions an analyst has to make at each step and where each
step lives in this framework. It is a guide, not a substitute for the TSD.

## 1. Decide whether population adjustment is needed at all

Standard indirect comparison (Bucher) assumes the trials are similar enough
that relative effects transport across them. Population adjustment is
justified only when there is evidence that effect modifiers differ between
the trial populations. TSD 18 is explicit that population-adjusted methods
should not be the default and that their use must be argued for.

Decisions:

- Is there a plausible, documented imbalance in effect modifiers between the
  IPD trial and the comparator trial?
- Would a standard indirect comparison be acceptable?

## 2. Anchored or unanchored

Anchored: both trials share a common comparator arm. The IPD trial's relative
effect (A vs C) is adjusted and then combined with the published relative
effect (B vs C) by the Bucher method. Randomisation within each trial
protects the within-trial contrasts, so only effect modifiers need to be
balanced.

Unanchored: no common comparator; absolute outcomes are compared directly.
TSD 18 treats this as a last resort. It requires that all effect modifiers
and all prognostic variables are balanced, an assumption that can never be
verified, and the residual bias is unquantifiable. It should be accompanied
by a statement of why an anchored comparison was impossible.

Decisions:

- Is a connected network with a common comparator available? If so, use it.
- For an unanchored comparison, the weighting set must include every
  prognostic variable and effect modifier that can be matched on.

Framework: `run_maic_anchored()` and `run_maic_unanchored()` are separate
entry points; the unanchored one has no arm concept and defaults to weighting
on both metadata tiers.

## 3. Choose the covariates

TSD 18 distinguishes:

- Effect modifiers: variables that change the relative treatment effect.
  Required in the weighting set for anchored comparisons.
- Prognostic variables: variables that predict the outcome regardless of
  treatment. Required additionally for unanchored comparisons; optional
  sensitivity for anchored ones.

The choice should be prespecified from clinical knowledge and prior evidence,
not data-driven. Every variable included costs effective sample size, so the
set should be as small as the argument allows. Variables that are not
reported in the comparator study cannot be matched on and this limitation
must be stated.

Decisions:

- Which variables are effect modifiers, which are prognostic, and on what
  evidence?
- Match on the mean only, or also the standard deviation (and so the spread)
  of continuous variables? TSD 18 notes that matching higher moments is
  possible; it is a per-variable judgement and costs further ESS.
- How to handle a categorical variable whose levels differ between sources:
  collapse levels so both share them, or exclude the variable.

Framework: `metadata$match` is the effect-modifier tier, `metadata$adjust`
the prognostic tier, `match_sd` the second-moment flag. A level reported by
the comparator but absent from the IPD, or a variable unreported by the
comparator, is refused rather than silently dropped.

## 4. Prepare the comparator summary data

The comparator study's baseline table gives means and SDs for continuous
variables and proportions for categorical ones. Points to settle before
entry:

- Reported medians and ranges must be converted to means and SDs with a
  stated method.
- Percentages may be of the randomised population or of those with data;
  record which and keep denominators consistent.
- Missingness in the comparator study is usually invisible; where it is
  reported it should be shown in the balance table, and the matched targets
  should be the proportions among those with data.
- Subgroup baselines, if the comparison is to a subgroup, must come from the
  subgroup, not the whole trial.

Framework: the long-format SLD table with explicit `Missing` rows; `Missing`
mass is shown in the balance table, never renormalised there, and rescaled
out of the weighting targets with the amount recorded.

## 5. Assess overlap and feasibility before weighting

Weighting can only reweight patients the IPD has. If the comparator
population lies partly outside the IPD's range (a mean outside the observed
range, a category the IPD lacks), no set of weights can reach it, and the
analysis should not proceed on that variable.

Decisions:

- Compare the unweighted IPD with the comparator on every candidate variable
  first; look at SMDs and at ranges, not just means.
- Where overlap is poor, restrict the IPD to the overlapping population if
  that is clinically defensible, or drop the variable and say so.

Framework: the pre-weighting balance table; `build_match_targets()` and
`build_design_matrix()` refuse infeasible targets and name them.

## 6. Estimate the weights

TSD 18 describes the method of moments: each IPD patient receives a weight
that is a logistic function of their centred covariates, with coefficients
chosen so that the weighted means (and any higher moments) of the IPD equal
the comparator's. This is a propensity score for membership of the comparator
trial estimated without comparator IPD. The reference implementation solves
the convex objective by BFGS and takes the optimiser's convergence code as
the convergence criterion.

Decisions:

- Complete-case handling for patients with missing values in matched
  variables (they cannot be weighted), or imputation beforehand. Record how
  many were excluded.
- Whether weights are estimated on the whole two-arm trial (anchored) or on
  the single analysis population (unanchored).

Framework: `estimate_maic_weights()`; `na_action` for missingness; an empty
weighting set yields the unweighted analysis as a baseline rather than an
error.

## 7. Check the weights

TSD 18 asks for the distribution of weights and the effective sample size
(ESS), and treats a large ESS loss as a warning that the populations are far
apart and the adjusted estimate rests on few patients. Balance after
weighting should be shown for matched and unmatched variables alike: matched
variables will balance by construction, so the informative part is what
happens to the unmatched ones.

Decisions:

- What ESS loss is acceptable for this decision problem? There is no fixed
  threshold; state the ESS and let the reader judge.
- Are a few patients carrying most of the weight? Look at the maximum weight
  and the share held by the heaviest patients, not just the ESS.

Framework: `weight_diagnostics()`, `plot_weights()`, the before/after balance
table, and the per-scenario weight distribution columns.

## 8. Fit the outcome model

The weighted outcome model is a regression of the outcome on treatment with
the MAIC weights and nothing else: weighted logistic regression for binary
outcomes, weighted Cox regression for time-to-event, weighted linear
regression for continuous. All adjustment is in the weights. For unanchored
comparisons the model is intercept-only, giving the weighted absolute
outcome.

Decisions:

- The outcome scale: log odds ratio, log hazard ratio, or mean difference for
  anchored; logit proportion or mean for unanchored. The comparator result
  must be on the same scale and, for anchored comparisons, in the same
  direction relative to the common comparator.
- Which arm is the reference. A wrong reference silently flips the sign.
- Proportional hazards must hold for a hazard ratio to be meaningful;
  weighting does not fix a non-proportional comparison.

Framework: `fit_outcome_model()` with `reference_arm` required for anchored
analyses and the contrast label carried into every result row.

## 9. Combine with the comparator result

Anchored: the Bucher difference of the two relative effects, with variances
added. Unanchored: the difference of absolute outcomes on the analysis scale.
Independence of the two studies is assumed.

For uncertainty, TSD 18 recommends a robust sandwich variance or, preferably,
a bootstrap that re-estimates the weights in every resample so that
uncertainty in the weights is propagated. Percentile intervals are the usual
choice.

Decisions:

- Sandwich SE for a quick result; bootstrap for the reported interval.
- How many replicates, and how to report replicates that fail (infeasible
  resample, solver not converged) or give extreme estimates in small samples.
  Nothing should be trimmed silently.

Framework: `compare_to_sld()`, `bootstrap_comparison()` with failure counts
by reason and extreme-replicate counts, replicates never trimmed.

## 10. Sensitivity analyses

TSD 18 expects the sensitivity of the result to the choice of matched
variables to be explored. Common designs:

- Sequential: start from the unweighted comparison and add variables one at
  a time in a prespecified order, showing the estimate, the ESS, and the
  balance at each step.
- Univariate: each candidate variable alone, to see which ones move the
  estimate and which ones cost the most ESS.
- With and without matching the variance of continuous variables.
- Anchored with effect modifiers only versus with prognostic variables added.

Decisions:

- The order of entry in a sequential analysis is a prespecified judgement
  (effect modifiers first, strongest evidence first), not an output of the
  data.

Framework: `define_scenarios_sequential()` / `define_scenarios_univariate()`
and `run_scenarios_*()`, with every scenario row carrying its model's weight
distribution and `scenario_balance_path()` showing balance step by step.

## 11. Report

TSD 18 asks for the following to be reported so that the adjustment can be
assessed:

- The justification for population adjustment and for anchored versus
  unanchored.
- The list of effect modifiers and prognostic variables with the evidence
  for each, and which were matched on and why.
- Baseline characteristics before and after weighting, for all variables
  including those not matched, with the comparator values.
- The distribution of weights and the ESS.
- The outcome model used, the scale, and how uncertainty was estimated.
- Results of the sensitivity analyses.
- For unanchored comparisons, an explicit statement that residual bias from
  unmeasured or unmatched variables cannot be quantified.

Framework: `format_balance_comparison()`, `format_results()`,
`export_scenarios()` with the fixed folder layout per analysis.

## Important cautions, in one place

- Population adjustment changes the target population: an anchored MAIC
  estimates the effect in the comparator trial's population, not in the IPD
  trial's. State which population the result applies to.
- Weighting can only reweight; it cannot extrapolate. Poor overlap means an
  unreliable or impossible adjustment.
- Every matched variable costs ESS. Matching on variables that are not effect
  modifiers (in anchored comparisons) adds variance without removing bias.
- An unanchored comparison assumes no unmeasured prognostic variables or
  effect modifiers. This cannot be checked.
- Small ESS makes the outcome model fragile: separation in logistic models,
  unstable hazard ratios, wide bootstrap intervals. Report it rather than
  hide it.
- Comparator summary statistics carry their own uncertainty and reporting
  conventions; document every conversion.
- The direction of contrasts and the reference arm must match between the
  IPD estimate and the published estimate before they are combined.

## Reference

Phillippo DM, Ades AE, Dias S, Palmer S, Abrams KR, Welton NJ. *NICE DSU
Technical Support Document 18: Methods for population-adjusted indirect
comparisons in submissions to NICE.* 2016. Signorovitch JE et al. *Comparative
effectiveness without head-to-head trials: a method for matching-adjusted
indirect comparisons applied to psoriasis treatment with adalimumab or
etanercept.* PharmacoEconomics 2010.
