# maic_general

A metadata-driven R framework for matching-adjusted indirect comparison (MAIC).
One metadata table describes the covariates. Each analysis is one IPD outcome
against one published result, run through a dedicated anchored or unanchored
entry point. Everything else, from balance tables to weights to indirect
comparisons, is derived by small, single-purpose functions.

## Quick start

```r
source("08_main/source_framework.R")
source_framework(".")
toy <- readRDS("sandbox/toy_data.rds")   # or make_toy_data() from sandbox/make_toy_data.R

# Anchored: two-arm IPD, published contrast vs the common comparator
anc <- run_maic_anchored(
  toy$ipd, toy$sld, toy$metadata,
  outcome     = define_outcome("OS", "tte", "y_time", event = "y_event"),
  sld_outcome = define_sld_outcome("OS", "log_hr", estimate = log(0.8), ci_low = log(0.6), ci_high = log(1.07)),
  arm = "ARM", reference_arm = "A"      # estimate is "other arm vs A"
)

# Unanchored: the IPD is the analysis population as is (single arm), published absolute outcome
arm_a <- toy$ipd[toy$ipd$ARM == "A", setdiff(names(toy$ipd), "ARM")]
una <- run_maic_unanchored(
  arm_a, toy$sld, toy$metadata,
  outcome     = define_outcome("Response", "binary", "y_resp"),
  sld_outcome = sld_outcome_from_proportion("Response", p = 0.40, n = 100)
)

anc$diagnostics                                           # ESS, weight distribution
format_balance_comparison(compare_balance_tables(anc$balance_before, anc$balance_after))
format_results(dplyr::bind_rows(anc$result, una$result)) # HR / OR with 95% CI
plot_weights(anc$weight_fit)
```

`sandbox/run_toy.R` runs this and prints every table.

## Templates

Four annotated scripts in `templates/`, all runnable as is on inputs built
from the toy data by `sandbox/toy_analysis_inputs.R`. Each starts from an
`analysis_inputs` list element; replace it with your own.

| Script | What it does |
|---|---|
| `step_by_step_unanchored.R` | One unanchored MAIC, each module function called in turn with its output printed: summaries, balance, naive baseline, targets, design matrix, solver, weights and plot, weighted balance, outcome model, comparison, bootstrap, the one-call equivalent, scenarios with the balance path, export. |
| `step_by_step_anchored.R` | The same for an anchored MAIC, including the arm rules. |
| `multi_analysis_unanchored.R` | `run_analyses_unanchored()` over an input list: sequential scenario from the unweighted model plus univariate scenario per analysis, exported in a fixed layout and stacked. Includes two deliberately broken inputs to show how failures are reported. |
| `multi_analysis_anchored.R` | `run_analyses_anchored()` over an input list, sequential from the effect-modifier set. |

An `analysis_inputs` element holds: `analysis_name`, `population`,
`comparator`, `ipd`, `sld`, `metadata`, `outcome` (`define_outcome()`),
`sld_outcome` (`define_sld_outcome()` or `sld_outcome_from_proportion()`),
optional `adjust_order`, and for anchored analyses `arm` and `reference_arm`.

## Module layout

Dependencies only point downward. `08_main` sequences the others and contains
no statistics.

| Module | Responsibility | Key functions |
|---|---|---|
| `01_inputs` | Input contracts and validators | `validate_metadata()`, `validate_sld()`, `validate_ipd()`, `read_metadata_csv()` |
| `02_summaries` | Weighted summaries in one shared schema | `summarize_ipd()`, `summarize_sld()`, `summarize_continuous()`, `summarize_categorical()` |
| `03_balance` | Align both sides, SMD, balance table | `align_summaries()`, `add_smd()`, `create_balance_table()` |
| `04_weighting` | Targets, design matrix, solver, diagnostics | `build_match_targets()`, `build_design_matrix()`, `estimate_maic_weights()`, `estimate_weights()`, `weight_diagnostics()` |
| `05_models` | Outcome specs, arm rules, weighted fits, comparison, bootstrap | `define_outcome()`, `define_sld_outcome()`, `check_anchored_arms()`, `fit_outcome_model()`, `compare_to_sld()`, `bootstrap_comparison()` |
| `06_scenarios` | Scenario definitions as metadata variations | `define_scenarios_sequential()`, `define_scenarios_univariate()` |
| `07_reporting` | Display tables, plots, export | `compare_balance_tables()`, `format_balance_comparison()`, `format_results()`, `plot_weights()`, `scenario_balance_path()`, `export_scenarios()` |
| `08_main` | Loader and pipelines | `source_framework()`, `run_maic_weighting()`, `run_maic_unanchored()` / `run_maic_anchored()`, `run_scenarios_unanchored()` / `run_scenarios_anchored()`, `run_analyses_unanchored()` / `run_analyses_anchored()` |

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
weighting, and the outcome model carries no covariates. Anchored entry points
default to the primary set (`include_adjust = FALSE`); unanchored ones default
to both tiers (`include_adjust = TRUE`), as TSD 18 requires.

Metadata is maintained as a flat file with `level_order` written as
`Mild|Moderate|Severe` and read with `read_metadata_csv()`. The string
`"Missing"` is reserved; IPD missingness is `NA`.

### Summary-level data (`sld`)

Long format, one row per variable and level, study N repeated on every row.

| Column | Meaning |
|---|---|
| `var_name`, `var_type` | match `metadata$variable` and `metadata$type` |
| `var_level` | category for `cat` rows; `NA` for the `con` summary row; `"Missing"` for a missingness row |
| `sld_n` | study N, identical on every row |
| `sld_est` | mean (`con`) or proportion (`cat` and `Missing` rows); `NA` if the study did not report the variable |
| `sld_sd` | SD for the `con` summary row; ignored on proportion rows, where `sqrt(p(1-p))` is derived |

Rules: a `con` variable has exactly one summary row and at most one `Missing`
row; `cat` proportions including `Missing` sum to 1 within `prop_tol`
(default 0.02); every `metadata` variable must appear, with `NA` if
unreported; variables not in `metadata` are ignored.

### Individual patient data (`ipd`)

One row per patient. Every `metadata` variable must be a column: numeric for
`con`, factor or character for `cat` with values in `level_order` or `NA`.
For an unanchored analysis the IPD is the analysis population as supplied;
no arm column is used. For an anchored analysis it has a two-level arm column.

### Outcome and published result

`define_outcome(name, type, var, event)` with `type` in `binary`,
`continuous`, `tte`. `define_sld_outcome(name, scale, estimate, se | ci)` on
the scale the framework estimates for that type and comparison kind:

| | binary | continuous | tte |
|---|---|---|---|
| anchored | `log_or` | `mean_diff` | `log_hr` |
| unanchored | `logit_p` | `mean` | not supported |

`se_from_ci()` converts a published interval on the log scale;
`sld_outcome_from_proportion()` converts a response count.

## Design decisions

**Two entry points, nothing inferred.** `run_maic_unanchored()` has no arm
argument and refuses an anchored published result; `run_maic_anchored()`
requires `arm` and `reference_arm` and refuses an unanchored one. The same
split applies to the scenario and multi-analysis runners. A shared core does
the work.

**One outcome per analysis.** Each call is self-contained: its own weights,
balance, estimate, bootstrap. Several analyses are a list handed to
`run_analyses_*()`, which keeps them separate and stacks the results.

**Errors are recorded, not fatal, in batch runs.** A scenario step that fails
gives a results row with `status = "error"`, the message, and the variables in
play; the remaining steps run. An analysis that cannot start is listed in
`errors` with its stage. Solver failures name the constraints they were
solving.

**An empty weighting set is the naive analysis.** Unit weights, balance
unchanged, `weighted = FALSE`. It is the first row of a sequential table.

**Missingness is visible, never dropped.** A `Missing` row appears for a
variable whenever either source has missingness. Proportions are never
renormalised in the balance table.

**Levels are the union across sources, ordered by `level_order`.** A level one
side lacks is filled with 0 when that side reported the variable, NA when not.

**`row_type` is the stable key.** Every balance row is `summary`, `level`, or
`missing`; downstream code filters and formats on it.

**`n` is the denominator of the estimate**; under weights it is the Kish
effective sample size, so unit weights reproduce the unweighted tables exactly.

**Matching is reweighting.** No patient is removed for failing to match.
Patients with `NA` in a weighted variable get weight 0 (complete-case default).
A level absent from one side, or an unreported variable, is a hard error for
that step.

**Targets.** One mean constraint per weighted continuous variable, plus a
second-moment constraint if `match_sd`; K-1 proportion constraints per
categorical with the first `level_order` level as reference; SLD `Missing`
mass rescaled out and recorded.

**Solver.** Method of moments (Signorovitch 2010; NICE DSU TSD 18): BFGS on
`sum(exp(X a))`; convergence is optim's code, per TSD 18.

**Outcome models.** Anchored: `outcome ~ arm`, weighted, no covariates.
Unanchored: intercept-only. HC0 sandwich SE for regressions,
`coxph(robust = TRUE)` for survival. Bucher difference when anchored.

**Bootstrap.** Resamples IPD rows (stratified by arm when anchored),
re-estimates weights, refits, compares. Percentile intervals over all finite
replicates; extreme replicates counted, never trimmed; failures counted by
reason.

**Scenarios.** A scenario is a metadata table with one weighting tier set to
a pattern. Sequential: first k variables of an order on; univariate: one at a
time. Every scenario row carries its model's weight distribution (ESS and
share, exclusions, min / quartiles / max, top-10% share).
`export_scenarios()` writes results, one `balance_path.csv` with the weighted
IPD summary and SMD side by side for every step, a weight histogram per
successful step, and a manifest.

## Tests and style

```r
testthat::test_dir("tests/testthat")
lintr::lint_dir(".")                   # config in .lintr: 120 chars, snake_case
```

Packages used: `tibble`, `dplyr`, `survival`, `sandwich`, `ggplot2`; `testthat`,
`lintr`, `cyclocomp` for development.

## Open items

- Output to Word or HTML is not implemented; `07_reporting` produces character
  tibbles and ggplot objects.
- Unanchored time-to-event comparisons need reconstructed pseudo-IPD and are
  refused.
