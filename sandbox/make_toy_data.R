# Toy dataset ------------------------------------------------------------------
#
# Deterministic toy IPD, SLD, and metadata covering the missingness and
# category-alignment edge cases the framework must handle.
#
# SLD input contract:
#   Columns: var_name, var_type, var_level, sld_n, sld_est, sld_sd
#   - Long format: one row per (variable, level).
#   - var_type is "con" or "cat" (same codes as metadata$type).
#   - sld_n is the study N, repeated on every row.
#   - Continuous rows:  var_level = NA, sld_est = mean, sld_sd = SD.
#                       A variable not reported by the study has NA est and sd.
#   - Categorical rows: var_level = category, sld_est = proportion,
#                       sld_sd = sqrt(p * (1 - p)).
#   - Missingness is an explicit row with var_level = "Missing", for both
#     categorical and continuous variables. Categorical proportions within a
#     variable, including Missing, sum to 1.
#
# Outcomes (IPD columns): y_resp binary, y_time / y_event tte, y_score
# continuous. `outcomes` lists them; `sld_outcomes` holds the comparator
# study's published results for anchored and unanchored comparisons.
#
# Edge cases:
#   Continuous
#     fac_age                 no missing
#     fac_bmi                 missing in IPD only
#     fac_weight              not reported in SLD (NA est / sd)
#     fac_height              missing in IPD, not reported in SLD
#     fac_egfr                reported in SLD with an explicit Missing row
#   Categorical
#     fac_sev_hb              no missing
#     fac_tar_jnt_lead        no missing, binary
#     fac_prior_treatment     missing in IPD only
#     fac_color_sld_extra     SLD-only category (Yellow) + SLD Missing row
#     fac_color_ipd_extra     IPD-only category (Red)
#     fac_color_missing_both  missing in both

make_toy_data <- function(n_ipd = 60, sld_n = 100, seed = 2026) {
  set.seed(seed)

  set_na <- function(x, n_na) {
    x[sample.int(length(x), n_na)] <- NA
    x
  }
  draw_cat <- function(levels, prob, n_na = 0) {
    x <- sample(levels, n_ipd, replace = TRUE, prob = prob)
    factor(set_na(x, n_na), levels = levels)
  }

  # IPD ------------------------------------------------------------------------
  ipd <- tibble::tibble(
    USUBJID = sprintf("SUBJ%03d", seq_len(n_ipd)),
    ARM     = factor(rep(c("A", "B"), length.out = n_ipd)),

    fac_sev_hb             = draw_cat(c("Mild", "Moderate", "Severe"), c(0.2, 0.4, 0.4)),
    fac_tar_jnt_lead       = draw_cat(c("No", "Yes"), c(0.6, 0.4)),
    fac_prior_treatment    = draw_cat(c("Standard half-life", "Extended half-life", "Other"),
                                      c(0.5, 0.4, 0.1), n_na = 5),
    fac_color_sld_extra    = draw_cat(c("Blue", "Red"), c(0.7, 0.3)),
    fac_color_ipd_extra    = draw_cat(c("Blue", "Red"), c(0.6, 0.4)),
    fac_color_missing_both = draw_cat(c("Blue", "Red"), c(0.5, 0.5), n_na = 6),

    fac_age    = round(rnorm(n_ipd, mean = 48, sd = 11), 1),
    fac_bmi    = set_na(round(rnorm(n_ipd, mean = 28, sd = 4), 1), n_na = 7),
    fac_weight = round(rnorm(n_ipd, mean = 78, sd = 12), 1),
    fac_height = set_na(round(rnorm(n_ipd, mean = 170, sd = 9), 1), n_na = 4),
    fac_egfr   = round(rnorm(n_ipd, mean = 90, sd = 15), 1)
  )

  # Outcomes, one per supported type, all depending on ARM and age.
  lp <- -0.5 + 0.8 * (ipd$ARM == "A") + 0.02 * (ipd$fac_age - 48)
  ipd$y_resp  <- rbinom(n_ipd, 1, plogis(lp))                                       # binary
  ipd$y_time  <- round(rexp(n_ipd, rate = exp(-2.5 + 0.3 * (ipd$ARM == "B"))), 2)   # tte
  ipd$y_event <- rbinom(n_ipd, 1, 0.7)
  ipd$y_score <- round(-5 - 3 * (ipd$ARM == "A") + 0.1 * (ipd$fac_age - 48) +       # continuous
                         rnorm(n_ipd, 0, 4), 1)

  # Outcome specs: which IPD columns hold each outcome.
  outcomes <- tibble::tibble(
    name  = c("Response", "OS", "Score change"),
    type  = c("binary", "tte", "continuous"),
    var   = c("y_resp", "y_time", "y_score"),
    event = c(NA, "y_event", NA)
  )

  # Published outcomes of the comparator study, on the scale the framework
  # compares on. Anchored rows are "intervention vs common comparator";
  # unanchored rows are absolute outcomes in the comparator arm.
  sld_outcomes <- tibble::tibble(
    name     = c("Response", "OS", "Score change", "Response", "Score change"),
    anchored = c(TRUE, TRUE, TRUE, FALSE, FALSE),
    scale    = c("log_or", "log_hr", "mean_diff", "logit_p", "mean"),
    estimate = c(log(1.5), log(0.8), -2.0, qlogis(0.40), -6.5),
    se       = c(0.26, 0.15, 0.9, 1 / sqrt(sld_n * 0.4 * 0.6), 0.6)
  )

  # SLD ------------------------------------------------------------------------
  con_row <- function(var_name, est, sd, p_missing = NULL) {
    out <- tibble::tibble(
      var_name = var_name, var_type = "con", var_level = NA_character_,
      sld_n = sld_n, sld_est = est, sld_sd = sd
    )
    if (!is.null(p_missing)) {
      out <- dplyr::bind_rows(out, tibble::tibble(
        var_name = var_name, var_type = "con", var_level = "Missing",
        sld_n = sld_n, sld_est = p_missing, sld_sd = sqrt(p_missing * (1 - p_missing))
      ))
    }
    out
  }
  cat_rows <- function(var_name, props) {
    stopifnot(abs(sum(props) - 1) < 1e-8)
    p <- unname(props)
    tibble::tibble(
      var_name = var_name, var_type = "cat", var_level = names(props),
      sld_n = sld_n, sld_est = p, sld_sd = sqrt(p * (1 - p))
    )
  }

  sld <- dplyr::bind_rows(
    cat_rows("fac_sev_hb",             c(Mild = 0.05, Moderate = 0.30, Severe = 0.65)),
    cat_rows("fac_tar_jnt_lead",       c(No = 0.50, Yes = 0.50)),
    cat_rows("fac_prior_treatment",    c("Standard half-life" = 0.30, "Extended half-life" = 0.55, Other = 0.15)),
    cat_rows("fac_color_sld_extra",    c(Blue = 0.45, Red = 0.30, Yellow = 0.15, Missing = 0.10)),
    cat_rows("fac_color_ipd_extra",    c(Blue = 1.00)),
    cat_rows("fac_color_missing_both", c(Blue = 0.50, Red = 0.40, Missing = 0.10)),

    con_row("fac_age",    est = 50, sd = 10),
    con_row("fac_bmi",    est = 30, sd = 3),
    con_row("fac_weight", est = NA_real_, sd = NA_real_),   # not reported
    con_row("fac_height", est = NA_real_, sd = NA_real_),   # not reported
    con_row("fac_egfr",   est = 85, sd = 14, p_missing = 0.12)
  )

  # Metadata -------------------------------------------------------------------
  metadata <- tibble::tibble(
    variable = c(
      "fac_sev_hb", "fac_tar_jnt_lead", "fac_prior_treatment",
      "fac_color_sld_extra", "fac_color_ipd_extra", "fac_color_missing_both",
      "fac_age", "fac_bmi", "fac_weight", "fac_height", "fac_egfr"
    ),
    type          = rep(c("cat", "con"), times = c(6, 5)),
    show_balance  = rep(TRUE, 11),
    # match  = primary weighting set (effect modifiers): sev_hb, tar_jnt_lead, age
    # adjust = second tier (prognostic): prior_treatment, bmi, egfr
    match         = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
    adjust        = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE),
    match_sd      = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
    display_order = 1:11,
    level_order   = list(
      c("Mild", "Moderate", "Severe"),
      c("No", "Yes"),
      c("Standard half-life", "Extended half-life", "Other"),
      c("Blue", "Red", "Yellow"),
      c("Blue", "Red"),
      c("Blue", "Red"),
      NULL, NULL, NULL, NULL, NULL
    )
  )

  list(
    ipd = ipd, sld = sld, metadata = metadata,
    outcomes = outcomes, sld_outcomes = sld_outcomes
  )
}

if (sys.nframe() == 0) {
  toy <- make_toy_data()
  saveRDS(toy, file.path("sandbox", "toy_data.rds"))
  cat("Wrote sandbox/toy_data.rds\n")
}
