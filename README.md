# maic_general

A metadata-driven R framework for matching-adjusted indirect comparison (MAIC).
One metadata table describes the covariates; two small tables describe the
outcomes. Everything else, from balance tables to weights to indirect
comparisons, is derived from those tables by small, single-purpose functions.

## Quick start

```r
source("08_main/source_framework.R")
source_framework(".")

toy <- readRDS("sandbox/toy_data.rds")   # or make_toy_data() from sandbox/make_toy_data.R

# One outcome: one IPD outcome spec against one published result
os <- run_maic_analysis(
  toy$ipd, toy$sld, toy$metadata,
  outcome     = define_outcome("OS", "tte", "y_time", event = "y_event"),
  sld_outcome = define_sld_outcome("OS", "log_hr", estimate = log(0.8), ci_low = log(0.6), ci_high = log(1.07)),
  arm = "ARM", reference_arm = "A",   # anchored: estimate is "other arm vs reference_arm"
  include_adjust = FALSE,             # TRUE adds the `adjust` tier to the weighting set
  n_boot = 0                          # set e.g. 1000 for bootstrap intervals
)
os$diagnostics                                          # ESS, weight distribution
format_balance_comparison(compare_balance_tables(os$balance_before, os$balance_after))
format_results(os$result)                               # one row: HR with 95% CI

# Many outcomes, each analysed independently with the same specifications
all <- run_maic_outcomes(
  toy$ipd, toy$sld, toy$metadata, toy$outcomes, toy$sld_outcomes,
  arm = "ARM", reference_arm = "A", intervention_arm = "A"
)
format_results(all$summary)                             # one row per published result
all$analyses[["Response [unanchored]"]]$balance_after   # each analysis is complete on its own
```

Comparison kind follows from the published result's scale. Anchored
comparisons need `arm` and `reference_arm` (the common comparator). Unanchored
comparisons on multi-arm IPD need `intervention_arm`, and the IPD is subset
to that arm before weighting; single-arm IPD needs no arm arguments.

`sandbox/run_toy.R` runs this end to end and prints every table.

## Starting a new study

Two annotated analysis scripts live in `templates/`, both filled with the toy
data so they run as is; replace the CSV contents for a real study.

- `new_study_template.R`: the multi-outcome route. Reads the four input
  tables, validates them, runs every published result as its own analysis on
  the primary and full weighting sets, runs sensitivity scenarios for one
  outcome, and writes the tables and a weight plot.
- `single_outcome_template.R`: the single-outcome route for a binary
  response, with the outcome and published results specified inline. Runs
  the anchored comparison on both weighting sets and the unanchored
  comparison on the intervention arm, each with a bootstrap, plus the
  sensitivity scenarios, balance tables, weight plots, and bootstrap
  stability diagnostics.
- `step_by_step_template.R`: the same analysis done by calling each module
  function in turn and printing every intermediate object: summaries,
  aligned balance, targets, design matrix, solver output, weights and their
  diagnostics, weighted balance, outcome model, comparison, bootstrap, and
  the scenario definitions. Run it line by line to learn or debug the
  process; the pipelines in `08_main` make exactly these calls.
- `step_by_step_unanchored_template.R`: the same walk-through for an
  unanchored comparison on the intervention arm, with no `match` tier and
  every usable covariate in the `adjust` tier. Starts from the naive
  unweighted baseline, shows the errors raised for variables that cannot be
  weighted on, and ends with sequential and univariate scenarios over the
  adjust tier. Metadata is maintained as a
flat file with `level_order` written as `Mild|Moderate|Severe` and read with
`read_metadata_csv()`. Published ratios with confidence intervals convert with
`se_from_ci()` on the log scale.

## Module layout

Dependencies only point downward (a module uses lower-numbered modules, never
higher). `08_main` sequences the others and contains no statistics.

| Module | Responsibility | Key functions |
|---|---|---|
| `01_inputs` | Input contracts and validators | `validate_metadata()`, `validate_sld()`, `validate_ipd()`, `validate_outcomes()`, `validate_sld_outcomes()` |
| `02_summaries` | Weighted summaries in one shared schema | `summarize_ipd()`, `summarize_sld()`, `summarize_continuous()`, `summarize_categorical()` |
| `03_balance` | Align both sides, SMD, balance table | `align_summaries()`, `add_smd()`, `create_balance_table()` |
| `04_weighting` | Target moments, design matrix, solver, diagnostics | `build_match_targets()`, `build_design_matrix()`, `estimate_maic_weights()`, `weight_diagnostics()` |
| `05_models` | Outcome specs, arm resolution, weighted fits, comparison, bootstrap | `define_outcome()`, `define_sld_outcome()`, `resolve_arms()`, `fit_outcome_model()`, `compare_to_sld()`, `bootstrap_comparison()` |
| `06_scenarios` | Scenario definitions as metadata variations | `define_scenarios_sequential()`, `define_scenarios_univariate()` |
| `07_reporting` | Display tables and plots | `compare_balance_tables()`, `format_balance_comparison()`, `format_results()`, `plot_weights()` |
| `08_main` | Loader and pipelines | `source_framework()`, `run_maic_weighting()`, `run_maic_analysis()`, `run_maic_outcomes()`, `run_maic_scenarios()` |

Every validator is pure: it never transforms its input, collects every problem
before failing, and returns the input invisibly.

## Input contracts

### Covariate metadata (`metadata`)

One row per covariate. All downstream behaviour follows from these flags.

| Column | Type | Meaning |
|---|---|---|
| `variable` | character | IPD column name and SLD `var_name` |
| `type` | `"con"` / `"cat"` | continuous or categorical |
| `show_balance` | logical | appears in balance tables |
| `match` | logical | primary weighting set, always matched (effect modifiers in TSD 18 terms) |
| `adjust` | logical | second weighting tier (prognostic variables), matched only with `include_adjust = TRUE` |
| `match_sd` | logical | also match the SD (`con` only, requires `match` or `adjust`) |
| `display_order` | numeric | row order in outputs, unique |
| `level_order` | list of character | category order for `cat`; must contain every level seen in IPD or SLD; `NULL` for `con` |

A variable belongs to at most one weighting tier. Both tiers feed the weights
and nothing else: following NICE TSD 18, "adjustment" in MAIC is the
weighting, and the outcome model carries no covariates. The primary analysis
weights on `match` alone (`include_adjust = FALSE`, the default); the fuller
specification adds `adjust` (`include_adjust = TRUE`), which TSD 18 requires
for unanchored comparisons and treats as a sensitivity analysis for anchored
ones.

The string `"Missing"` is reserved. It never appears in `level_order` or as an
IPD value; IPD missingness is `NA`.

### Summary-level data (`sld`)

Long format, one row per variable and level, study N repeated on every row.

| Column | Meaning |
|---|---|
| `var_name`, `var_type` | match `metadata$variable` and `metadata$type` |
| `var_level` | category for `cat` rows; `NA` for the `con` summary row; `"Missing"` for a missingness row |
| `sld_n` | study N, identical on every row |
| `sld_est` | mean (`con`) or proportion (`cat` and `Missing` rows); `NA` if the study did not report the variable |
| `sld_sd` | SD (`con`) or `sqrt(p(1-p))`; `NA` with `sld_est` |

Rules: a `con` variable has exactly one summary row and at most one `Missing`
row; `cat` proportions including `Missing` sum to 1 within `prop_tol`
(default 0.02, for publication rounding); every `metadata` variable must
appear, with `NA` if unreported; variables not in `metadata` are ignored.

### Individual patient data (`ipd`)

One row per patient. Every `metadata` variable must be a column: numeric for
`con`, factor or character for `cat` with values in `level_order` or `NA`.
Other columns (IDs, arm, outcomes) are untouched by the covariate layer.

### Outcome tables (`outcomes`, `sld_outcomes`)

`outcomes`: `name`, `type` (`binary` / `continuous` / `tte`), `var`, `event`
(tte only, else `NA`).

`sld_outcomes`: `name`, `anchored` (logical), `scale`, `estimate`, `se`. One
row is one independent analysis. The scale must be what the framework
estimates for that type and comparison kind, and `anchored` must agree with
it:

| | binary | continuous | tte |
|---|---|---|---|
| anchored | `log_or` | `mean_diff` | `log_hr` |
| unanchored | `logit_p` | `mean` | not supported |

**SLD proportion rows need no SD.** `sld_sd` is read only for continuous
summary rows; for categorical levels and `Missing` rows the framework derives
`sqrt(p(1-p))`, as it does on the IPD side, and ignores any entered value.

## Design decisions

**Missingness is visible, never dropped.** A `Missing` row appears for a
variable whenever either source has missingness. For categorical variables it
is an extra category; for continuous variables it is a second row under the
summary. Proportions are never renormalised in the balance table.

**Levels are the union across sources, ordered by `level_order`.** A level one
side lacks is filled with proportion 0 when that side reported the variable,
and left `NA` when it did not. `Missing` always sorts last.

**`row_type` is the stable key.** Every balance row is `summary`, `level`, or
`missing`. Downstream code filters and formats on `row_type`, never by
inspecting the level string.

**`n` is the denominator of the estimate.** Observed count for a continuous
summary, full N for every proportion row. Under weights it is the Kish
effective sample size, so unit weights reproduce the unweighted tables exactly.

**One SMD formula.** Because proportion rows carry `sqrt(p(1-p))` as their SD,
the pooled-SD formula gives the textbook proportion SMD without a second code
path. Identical constants give 0; 100% vs 0% gives signed `Inf`.

**An empty weighting set is the naive analysis.** With no `match` variable
and no `adjust` variable in play, every patient gets weight 1 and the
comparison is unweighted; `weighted = FALSE` in the result. This is the
baseline row of a sequential matching table, not an error.

**Matching is reweighting.** No patient is removed for failing to match.
Patients with `NA` in a matched variable get weight 0 (complete-case default;
`na_action = "error"` refuses instead). An SLD level absent from the IPD, or an
SLD proportion of 0 for a level present in the IPD, is a hard error rather
than a silent exclusion.

**Targets.** One mean constraint per matched continuous variable, plus a
second-moment constraint if `match_sd`. One proportion constraint per
category except the first in `level_order` (the reference). SLD `Missing`
mass is rescaled out of matched categorical targets and recorded in
`sld_missing`.

**Solver.** Method of moments (Signorovitch 2010; NICE DSU TSD 18): BFGS on
`sum(exp(X a))` with analytic gradient. Convergence is optim's convergence
code, per TSD 18. The residual moment error is returned as a diagnostic.

**One outcome per analysis.** `run_maic_analysis()` takes one IPD outcome
and one published result and is self-contained: its own weights, balance,
estimate, bootstrap. `run_maic_outcomes()` repeats it over the published
results table and returns the separate analyses plus a stacked summary.
Every result row carries a `contrast` label such as `B vs A` or
`A (unanchored)`.

**Arms.** Anchored comparisons require `arm` and `reference_arm`; the
estimate is the other arm versus the reference, which must be the common
comparator in the published study. Unanchored comparisons on multi-arm IPD
require `intervention_arm`; the IPD is subset to that arm before weighting,
so no randomisation assumption is needed.

**Outcome models.** Anchored: `outcome ~ arm`, weighted, no covariates;
quasibinomial / gaussian glm with HC0 sandwich SE, or `coxph(robust = TRUE)`.
Unanchored: intercept-only weighted model. Weights are rescaled
to sum to n before fitting so SEs do not depend on their arbitrary scale.
Indirect comparison is IPD minus SLD with summed variances (Bucher when
anchored). Results stay on the log scale; `07_reporting` exponentiates.

**Bootstrap.** Resamples IPD rows (stratified by arm when anchored),
re-estimates weights, refits, compares, as TSD 18 recommends. Intervals are
percentile intervals over all replicates; nothing finite is trimmed. A
replicate whose outcome model separates and gives an extreme estimate stays
in and is counted (`n_extreme`, `extreme_share`: log-scale estimates beyond
`extreme_bound`, default 5). A high extreme share means the comparison is
unstable at this sample size and is reported, not hidden. The bootstrap SE
and the spread of replicate ESS are returned as diagnostics rather than
headline columns. Replicates that yield no estimate at all (infeasible
resample, solver not converged, non-finite estimate) are excluded and
counted by reason; more than `max_fail_rate` is an error.

**Scenarios.** A scenario is a metadata table with one weighting tier set to
a pattern, plus a label. Sequential: first k variables of a given order on,
for each k. Univariate: one variable on at a time. `flag = "adjust"` adds
prognostic variables one by one on top of the fixed primary set (the runner
weights with `include_adjust = TRUE` for such lists). `flag = "match"` varies
the primary set itself. A scenario never moves a variable between tiers.
`run_maic_scenarios()` reuses weights across scenarios with an identical
weighting set. Every scenario row carries its run's weight distribution
(ESS and its share of n, exclusions, min / quartiles / max of the rescaled
weights, top-10% share), so each model can be judged from the table alone;
`format_results()` renders these and `format_weight_summary()` gives them
as one string per row.

## Tests and style

```r
testthat::test_dir("tests/testthat")   # 23 files, ~ 600 assertions
lintr::lint_dir(".")                   # config in .lintr: 120 chars, snake_case
```

Tests source the framework through `08_main/source_framework.R` and build the
toy fixture with `sandbox/make_toy_data.R`. The toy data covers: continuous
variables with missingness in IPD only, SLD only (unreported), both, and an
SLD `Missing` row; categorical variables with an SLD-only level, an IPD-only
level, missingness on each side; and binary, continuous, and time-to-event
outcomes with anchored and unanchored published results.

Packages used: `tibble`, `dplyr`, `survival`, `sandwich`, `ggplot2`; `testthat`,
`lintr`, `cyclocomp` for development.

## Extending the framework

- A new input rule goes in the relevant `01_inputs` validator, using
  `problem_collector()`.
- A new moment type is a row in `MOMENTS`, a branch in
  `build_match_targets()`, and one line in the design-matrix column builder.
- A new weight estimator is a new file in `04_weighting` that consumes the
  design matrix and returns the same list shape as `estimate_maic_weights()`.
- A new outcome type is an entry in `OUTCOME_TYPES`, a row in
  `estimate_scale()`, and a branch in `fit_outcome_model()`.
- A new scenario kind is a generator in `06_scenarios` returning the same
  scenario list shape; the runner needs no change.
- Pipelines in `08_main` only sequence calls. If a feature needs a branch,
  it becomes a module function, not an argument to a pipeline.

## Open decisions

- Output to Word or HTML is not implemented; `07_reporting` produces character
  tibbles for a document layer to place.
- Unanchored time-to-event comparisons need reconstructed pseudo-IPD and are
  refused.
