# =============================================================================
# 06b_poc_sensitivity.R
# Purpose : Sensitivity analyses (prereg Section 9)
# Project : A Safe Bet (ASB) — Proof-of-Concept evaluation.
#           Putting Design into Practice: A Safe Bet — A Proof-of-Concept
#           Evaluation of a Tailored Online Responsible Gambling Framework
# Inputs  : poc_synthetic.rds or poc_analytical_LOCKED.rds (via 00_config.R)
# Outputs : results/06b_sensitivity/sensitivity_results.rds
# Flags   : USE_SYNTHETIC — TRUE runs on poc_synthetic.rds (pre-registration
#           phase); FALSE runs on poc_analytical_LOCKED.rds
#           (post-registration). Currently FALSE.
# =============================================================================
# PRE-REG:  OSF [URL to be added] - implements prereg Section 9
#           (script table, prereg Section 10.1)
# =============================================================================
#
# COVERAGE - six of the seven prereg Section 9 sensitivity analyses (internal
# block labels 11.1-11.6 are retained for continuity with the saved object
# names). The seventh, the BetBuddy recalibration adjustment, is implemented
# where its models live: 07_poc_its.R (ITS) and 08_poc_markov.R (msm).
#
#   11.1  Activity threshold           Refit H1-H4 at AGD thresholds 10, 20, 30
#                                      (primary = 15)
#   11.2  Outlier influence            Refit H1-H4 excluding participants in the
#                                      top 1% of deposit or net-loss totals
#   11.3  Period matching              Refit H1-H4 using only Apr-Jun (baseline)
#                                      and Oct-Dec (intervention) calendar
#                                      quarters as a robustness check on the
#                                      6-month windows
#   11.4  Risk classification          Refit Markov transitions using last-
#                                      month dominant risk vs 6-month dominant
#                                      (primary)
#   11.5  Young Adult exclusion        Refit H1-H4 excluding the n = 168 YA
#                                      players (under 25 at offer date)
#   11.6  ITS autocorrelation          AR(1) vs default mixed-model standard
#                                      errors (07_poc_its.R fits both in all
#                                      cases per prereg 6.4)
#
# OUTPUT FORMAT: A single sensitivity_summary table compares primary vs each
# sensitivity for every relevant statistic. Detailed sub-results saved per
# sensitivity for inspection.
#
# NOTES:
# - Sensitivity 11.4 requires access to raw event data (model_results) and
#   runs only on USE_SYNTHETIC = FALSE (post-OSF) since synthetic data is
#   generated from already-aggregated state counts.
# - All other sensitivities can run on synthetic for development.
# =============================================================================

library(dplyr)
library(tidyr)
library(lubridate)

source("00_config.R")

# Resolve namespace conflicts (conflicted may be loaded from .Rprofile)
if (requireNamespace("conflicted", quietly = TRUE)) {
  library(conflicted)
  conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::pull)
  conflicts_prefer(dplyr::select)
  conflicts_prefer(dplyr::mutate)
  conflicts_prefer(dplyr::summarise)
  conflicts_prefer(dplyr::arrange)
  conflicts_prefer(dplyr::count)
  conflicts_prefer(dplyr::group_by)
  conflicts_prefer(dplyr::lag)
}

# Timestamped progress marker so long runs are never silent about what is
# currently executing (print-only; no effect on any computation)
stamp <- function(msg) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), msg))

USE_SYNTHETIC <- FALSE

if (USE_SYNTHETIC) {
  dat <- readRDS(FILE_POC_SYNTHETIC)
  pp  <- dat$player_period_synth
  pm  <- dat$player_month_synth
  cat("Running on SYNTHETIC data\n")
} else {
  dat <- readRDS(FILE_POC_LOCKED)
  pp  <- dat$player_period
  pm  <- dat$player_month
  cat("Running on LOCKED real data\n")
}
stamp(sprintf("06b_poc_sensitivity.R started; data loaded (%d player-period rows)", nrow(pp)))

dir.create("results/06b_sensitivity", recursive = TRUE, showWarnings = FALSE)

cat("=== 06b_poc_sensitivity.R ===\n")
cat("Started:", format(Sys.time()), "\n\n")

# =============================================================================
# HELPERS
# =============================================================================

# Wilcoxon paired-test wrapper returning V, p, and rank-biserial r
wilcox_paired_compact <- function(bl, int) {
  complete <- !is.na(bl) & !is.na(int)
  bl_c     <- bl[complete]
  int_c    <- int[complete]
  n        <- length(bl_c)
  if (n < 10) return(list(n = n, V = NA, p = NA, r_rb = NA))

  wt <- tryCatch(
    wilcox.test(bl_c, int_c, paired = TRUE, alternative = "greater",
                exact = FALSE, conf.int = FALSE),
    error = function(e) NULL
  )
  if (is.null(wt)) return(list(n = n, V = NA, p = NA, r_rb = NA))

  W_expected <- n * (n + 1) / 4
  W_sd       <- sqrt(n * (n + 1) * (2 * n + 1) / 24)
  Z          <- (wt$statistic - W_expected) / W_sd
  r_rb       <- as.numeric(Z / sqrt(n))

  list(n = n, V = unname(wt$statistic), p = wt$p.value, r_rb = r_rb)
}

# Reshape pp from long to wide for primary outcomes
pp_to_wide <- function(pp_long) {
  pp_long |>
    dplyr::select(participant_id, period, total_deposits, total_losses, total_wagers,
                  play_hours_total, agd_count,
                  dplyr::any_of(c("dominant_risk", "young_adult_flag",
                                   "engaged_cohort", "age_years"))) |>
    pivot_wider(names_from = period,
                values_from = c(total_deposits, total_losses, total_wagers,
                                play_hours_total, agd_count,
                                dplyr::any_of(c("dominant_risk"))))
}

# Run H1-H4 on a wide-format pp (full cohort)
run_h1_h4 <- function(pp_wide) {
  outcomes <- c("total_deposits", "total_losses", "total_wagers", "play_hours_total")
  hyps     <- c("H1", "H2", "H3", "H4")
  bind_rows(lapply(seq_along(outcomes), function(i) {
    o   <- outcomes[i]
    bl  <- pp_wide[[paste0(o, "_baseline")]]
    int <- pp_wide[[paste0(o, "_intervention")]]
    r   <- wilcox_paired_compact(bl, int)
    data.frame(hypothesis = hyps[i], outcome = o,
               n = r$n, V = r$V, p = r$p, r_rb = r$r_rb)
  }))
}

# Compute Markov transition matrix from pm
compute_trans <- function(pm_data, period_filter, state_col = "markov_state") {
  states <- MARKOV_STATES   # c("L","M","H","I") from config
  trans  <- matrix(0, 4, 4, dimnames = list(states, states))
  pm_f <- pm_data |>
    filter(is_intervention == period_filter) |>
    arrange(participant_id, year_month)
  for (pid in unique(pm_f$participant_id)) {
    seq <- pm_f[[state_col]][pm_f$participant_id == pid]
    seq <- seq[!is.na(seq)]
    if (length(seq) < 2) next
    for (t in seq_len(length(seq) - 1)) {
      s1 <- seq[t]; s2 <- seq[t + 1]
      if (s1 %in% states && s2 %in% states)
        trans[s1, s2] <- trans[s1, s2] + 1
    }
  }
  sweep(trans, 1, pmax(rowSums(trans), 1), "/")
}

# =============================================================================
# PRIMARY ANALYSIS BENCHMARK (for comparison)
# =============================================================================

cat("--- Primary analysis (benchmark) ---\n")
t0 <- Sys.time()
stamp("Primary benchmark (H1-H4) starting")
pp_wide_primary <- pp_to_wide(pp)
primary_results <- run_h1_h4(pp_wide_primary) |> mutate(spec = "primary")
print(primary_results)
stamp(sprintf("Primary benchmark done (%.1f s)",
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))

# =============================================================================
# 11.1  ACTIVITY THRESHOLD (refit H1-H4 at thresholds 10, 20, 30)
# Prereg Section 9, activity threshold row - sensitivity
# =============================================================================

cat("\n--- §11.1 Activity threshold sensitivity ---\n")
cat("Primary threshold:", AGD_THRESHOLD, "| Sensitivity thresholds:",
    paste(SENSITIVITY_AGD_THRESHOLDS[SENSITIVITY_AGD_THRESHOLDS != AGD_THRESHOLD],
          collapse = ", "), "\n")

agd_baseline_per_player <- pp |>
  filter(period == "baseline") |>
  dplyr::select(participant_id, agd_count_baseline = agd_count)

t0 <- Sys.time()
stamp("Sensitivity 11.1 (activity threshold) starting")
s11_1_results <- bind_rows(lapply(SENSITIVITY_AGD_THRESHOLDS, function(thr) {
  stamp(sprintf("Sensitivity 11.1: refitting H1-H4 at threshold %d", thr))
  retained_ids <- agd_baseline_per_player |>
    filter(agd_count_baseline >= thr) |>
    pull(participant_id)
  pp_subset <- pp |> filter(participant_id %in% retained_ids)
  pp_wide   <- pp_to_wide(pp_subset)
  run_h1_h4(pp_wide) |>
    mutate(spec = paste0("threshold_", thr),
           n_retained = length(retained_ids))
}))

cat("Results across thresholds:\n")
print(s11_1_results)
stamp(sprintf("Sensitivity 11.1 done (%.1f s)",
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))

# =============================================================================
# 11.2  OUTLIER INFLUENCE (top 1% of deposit or net-loss totals excluded)
# Prereg Section 9, outlier handling row - sensitivity
# =============================================================================

cat("\n--- §11.2 Outlier sensitivity (top",
    sprintf("%.0f%%", (1 - OUTLIER_PERCENTILE) * 100), "of deposit/loss outcomes excluded) ---\n")

t0 <- Sys.time()
stamp("Sensitivity 11.2 (outlier exclusion) starting")
# Prereg Section 7 / Section 9: EXCLUDE participants in the top 1% of deposit or loss values
# (extreme high-volume gamblers). Whole participants are dropped to preserve the
# paired structure; per-period thresholds mirror the period grouping.
caps <- pp |>
  group_by(period) |>
  summarise(dep_cap  = quantile(total_deposits, OUTLIER_PERCENTILE, na.rm = TRUE),
            loss_cap = quantile(total_losses,   OUTLIER_PERCENTILE, na.rm = TRUE),
            .groups = "drop")

outlier_ids <- pp |>
  left_join(caps, by = "period") |>
  filter(total_deposits > dep_cap | total_losses > loss_cap) |>
  pull(participant_id) |>
  unique()

pp_excl <- pp |>
  filter(!participant_id %in% outlier_ids)

cat("  Excluded participants (top 1% of deposit or loss outcomes):", length(outlier_ids), "\n")

s11_2_results <- run_h1_h4(pp_to_wide(pp_excl)) |>
  mutate(spec = "excluded_top1pct")

cat("Results with top 1% excluded:\n")
print(s11_2_results)
stamp(sprintf("Sensitivity 11.2 done (%.1f s)",
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))

# =============================================================================
# 11.3  PERIOD MATCHING (Apr-Jun baseline vs Oct-Dec intervention)
# Prereg Section 9, period matching row - sensitivity
# =============================================================================

cat("\n--- §11.3 Period matching: Apr-Jun vs Oct-Dec quarters only ---\n")

t0 <- Sys.time()
stamp("Sensitivity 11.3 (period matching) starting")
# Recompute pp from pm restricted to those calendar quarters
quarter_pp <- pm |>
  filter(
    (is_intervention == 0 & month(year_month) %in% 4:6) |
    (is_intervention == 1 & month(year_month) %in% 10:12)
  ) |>
  mutate(period = if_else(is_intervention == 0, "baseline", "intervention")) |>
  group_by(participant_id, period) |>
  summarise(
    total_deposits   = sum(total_deposits,   na.rm = TRUE),
    total_losses     = sum(total_losses,     na.rm = TRUE),
    total_wagers     = sum(total_wagers,     na.rm = TRUE),
    play_hours_total = sum(play_hours_total, na.rm = TRUE),
    agd_count        = sum(agd_count,        na.rm = TRUE),
    .groups = "drop"
  )

s11_3_results <- run_h1_h4(pp_to_wide(quarter_pp)) |>
  mutate(spec = "matched_quarters")

cat("Results restricted to matched quarters:\n")
print(s11_3_results)
stamp(sprintf("Sensitivity 11.3 done (%.1f s)",
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))

# =============================================================================
# 11.4  RISK CLASSIFICATION (last-month vs 6-month dominant)
# Prereg Section 9, risk classification row - sensitivity
# =============================================================================

cat("\n--- §11.4 Risk classification: last-month dominant ---\n")
t0 <- Sys.time()
stamp("Sensitivity 11.4 (risk classification) starting")

if (USE_SYNTHETIC) {
  cat("  Synthetic data: markov_state already simulated from per-month risk;\n")
  cat("  last-month vs 6-month sensitivity not meaningfully different on synthetic.\n")
  cat("  Will run on real data (USE_SYNTHETIC = FALSE) post-OSF.\n")
  s11_4_results <- data.frame(
    spec = "last_month_dominant",
    note = "deferred to post-OSF run on real data"
  )
} else {
  # Last-month dominant: take final month's monthly_risk as the period state
  pp_lastmonth <- pm |>
    arrange(participant_id, year_month) |>
    group_by(participant_id) |>
    mutate(period = if_else(is_intervention == 0, "baseline", "intervention")) |>
    group_by(participant_id, period) |>
    summarise(
      lastmonth_risk = dplyr::last(monthly_risk[!is.na(monthly_risk)]),
      .groups = "drop"
    )

  trans_lastmonth_bl  <- pp_lastmonth |>
    filter(period == "baseline")     |>
    dplyr::count(lastmonth_risk)
  trans_lastmonth_int <- pp_lastmonth |>
    filter(period == "intervention") |>
    dplyr::count(lastmonth_risk)

  cat("Risk distribution (last-month rule):\n")
  cat("  Baseline:    \n"); print(trans_lastmonth_bl)
  cat("  Intervention:\n"); print(trans_lastmonth_int)

  # Baseline x intervention transition table under the last-month rule, with
  # the McNemar-Bowker symmetry test (same implementation as the primary H5
  # test in 06_poc_prepost.R), so that "whether results change depending on
  # how the dominant risk level is defined" (prereg Section 9) is tested
  # directly rather than inferred from the marginal distributions above.
  # The whole last-month transition/symmetry block is wrapped in tryCatch:
  # it first executes on real data (it cannot be exercised on synthetic), so
  # any failure prints the error and the run continues with the
  # marginal-distribution comparison above rather than halting.
  trans_matrix_lm   <- NULL
  mcnemar_lastmonth <- NULL
  tryCatch({
    lastmonth_wide <- pp_lastmonth |>
      tidyr::pivot_wider(names_from = period, values_from = lastmonth_risk)

    risk_levels_lm <- c("low", "medium", "high")
    lastmonth_trans <- lastmonth_wide |>
      dplyr::filter(!is.na(baseline), !is.na(intervention)) |>
      dplyr::count(baseline, intervention)

    trans_matrix_lm <- matrix(0, nrow = 3, ncol = 3,
                              dimnames = list(risk_levels_lm, risk_levels_lm))
    for (i in seq_len(nrow(lastmonth_trans))) {
      r <- lastmonth_trans$baseline[i]
      c <- lastmonth_trans$intervention[i]
      if (r %in% risk_levels_lm && c %in% risk_levels_lm)
        trans_matrix_lm[r, c] <- lastmonth_trans$n[i]
    }
    cat("\nTransition table (last-month rule; baseline rows x intervention columns):\n")
    print(trans_matrix_lm)

    mcnemar_lastmonth <- tryCatch({
      stat <- 0; df <- 0
      for (i in 1:3) {
        for (j in (i+1):3) {
          if (i < j && j <= 3) {
            n_ij <- trans_matrix_lm[i, j]; n_ji <- trans_matrix_lm[j, i]
            if (n_ij + n_ji > 0) { stat <- stat + (n_ij - n_ji)^2 / (n_ij + n_ji); df <- df + 1 }
          }
        }
      }
      list(statistic = stat, df = df, p.value = pchisq(stat, df, lower.tail = FALSE))
    }, error = function(e) NULL)
    if (!is.null(mcnemar_lastmonth)) {
      cat(sprintf("McNemar-Bowker symmetry test (last-month rule): chi-sq(%d) = %.3f, p = %.4f\n",
                  mcnemar_lastmonth$df, mcnemar_lastmonth$statistic, mcnemar_lastmonth$p.value))
      cat("  Compare against the primary (6-month dominant) H5 test reported by 06_poc_prepost.R\n")
    }
  }, error = function(e) {
    cat("  Last-month transition/symmetry block FAILED:", conditionMessage(e), "\n")
    cat("  Continuing: the marginal-distribution comparison above is unaffected;\n")
    cat("  lastmonth_transition and lastmonth_mcnemar are saved as NULL.\n")
  })

  # Compare to primary: 6-month dominant
  s11_4_results <- list(
    lastmonth_baseline     = trans_lastmonth_bl,
    lastmonth_intervention = trans_lastmonth_int,
    lastmonth_transition   = trans_matrix_lm,
    lastmonth_mcnemar      = mcnemar_lastmonth,
    primary_baseline       = pp |> filter(period == "baseline") |>
                                dplyr::count(dominant_risk),
    primary_intervention   = pp |> filter(period == "intervention") |>
                                dplyr::count(dominant_risk)
  )
}

# =============================================================================
# 11.5  YOUNG ADULT EXCLUSION (under 25 at offer date, ages 18-24; n = 168)
# Prereg Section 9, Young Adult exclusion row (and prereg 4.4) - sensitivity
# =============================================================================

cat("\n--- §11.5 Young adult exclusion sensitivity ---\n")
stamp(sprintf("Sensitivity 11.4 done (%.1f s); 11.5 (young adult exclusion) starting",
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))
t0 <- Sys.time()

if ("young_adult_flag" %in% names(pp)) {
  ya_excluded_ids <- pp |>
    filter(young_adult_flag != 1L) |>
    pull(participant_id) |>
    unique()
  pp_no_ya  <- pp |> filter(participant_id %in% ya_excluded_ids)

  s11_5_results <- run_h1_h4(pp_to_wide(pp_no_ya)) |>
    mutate(spec        = "no_young_adults",
           n_retained  = length(ya_excluded_ids))

  cat("Results with YA excluded (n =", length(ya_excluded_ids), "):\n")
  print(s11_5_results)
} else {
  cat("  young_adult_flag not in pp — skipping\n")
  s11_5_results <- NULL
}

# =============================================================================
# 11.6  ITS AUTOCORRELATION (with vs without corAR1)
# Prereg Section 9, ITS autocorrelation row - sensitivity
# =============================================================================

cat("\n--- §11.6 ITS autocorrelation: refit without corAR1 ---\n")
stamp(sprintf("Sensitivity 11.5 done (%.1f s); 11.6 (ITS autocorrelation) starting",
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))

if (requireNamespace("nlme", quietly = TRUE)) {
  pm_its <- pm |>
    mutate(
      time_t           = month_number,
      intervention_t   = as.integer(is_intervention),
      time_post_t      = dplyr::coalesce(months_post, 0L),
      # Transforms per prereg 6.4 (v28): asinh for net loss, defined for
      # negative values, approximates log for large positive values;
      # log1p unchanged for the other three outcomes
      log1p_deposits   = log1p(total_deposits),
      asinh_losses     = asinh(total_losses),
      log1p_wagers     = log1p(total_wagers),
      log1p_play_hours = log1p(play_hours_total),
      player_fac       = factor(participant_id)
    )

  # Guard: no transform may silently produce NaN
  stopifnot(
    "NaN in transformed ITS outcomes - check transform domains" =
      !any(is.nan(pm_its$log1p_deposits)) && !any(is.nan(pm_its$asinh_losses)) &&
      !any(is.nan(pm_its$log1p_wagers))   && !any(is.nan(pm_its$log1p_play_hours))
  )

  fit_its <- function(outcome_var, use_ar1) {
    fml <- as.formula(sprintf("%s ~ time_t + intervention_t + time_post_t",
                              outcome_var))
    args <- list(
      fixed     = fml,
      random    = ~ 1 | player_fac,
      data      = pm_its,
      method    = "ML",
      na.action = na.omit,
      control   = nlme::lmeControl(opt = "optim", maxIter = 200,
                                   tolerance = 1e-4)
    )
    if (use_ar1) {
      args$correlation <- nlme::corAR1(form = ~ time_t | player_fac)
    }
    tryCatch(do.call(nlme::lme, args), error = function(e) NULL)
  }

  outcomes_its <- c("log1p_deposits", "asinh_losses",
                    "log1p_wagers", "log1p_play_hours")

  s11_6_results <- bind_rows(lapply(outcomes_its, function(o) {
    t_fit <- Sys.time()
    stamp(sprintf("Sensitivity 11.6: fitting %s with corAR1", o))
    m_primary <- fit_its(o, use_ar1 = TRUE)
    stamp(sprintf("Sensitivity 11.6: fitting %s without corAR1 (with-AR1 fit took %.1f s)",
                  o, as.numeric(difftime(Sys.time(), t_fit, units = "secs"))))
    m_no_ar1  <- fit_its(o, use_ar1 = FALSE)
    stamp(sprintf("Sensitivity 11.6: %s done (%.1f s total)",
                  o, as.numeric(difftime(Sys.time(), t_fit, units = "secs"))))

    extract <- function(m, label) {
      if (is.null(m)) return(data.frame(spec = label, outcome = o,
                                          beta_2 = NA, se_b2 = NA,
                                          beta_3 = NA, se_b3 = NA))
      cf <- nlme::fixed.effects(m); se <- sqrt(diag(vcov(m)))
      data.frame(spec    = label,
                 outcome = o,
                 beta_2  = unname(cf["intervention_t"]),
                 se_b2   = unname(se["intervention_t"]),
                 beta_3  = unname(cf["time_post_t"]),
                 se_b3   = unname(se["time_post_t"]))
    }

    bind_rows(extract(m_primary, "with_corAR1"),
              extract(m_no_ar1,  "no_corAR1"))
  }))

  cat("ITS coefficients with vs without corAR1:\n")
  print(s11_6_results)
} else {
  cat("  nlme not installed — §11.6 skipped\n")
  s11_6_results <- NULL
}

# =============================================================================
# SAVE ALL SENSITIVITY RESULTS
# =============================================================================

sensitivity_summary <- bind_rows(
  primary_results |> mutate(rule = "primary"),
  s11_1_results   |> mutate(rule = "11.1_threshold"),
  s11_2_results   |> mutate(rule = "11.2_excluded"),
  s11_3_results   |> mutate(rule = "11.3_quarters")
)
if (!is.null(s11_5_results)) {
  sensitivity_summary <- bind_rows(
    sensitivity_summary,
    s11_5_results |> mutate(rule = "11.5_no_ya")
  )
}

cat("\n=== SENSITIVITY SUMMARY (H1-H4 across rules) ===\n")
print(sensitivity_summary)

saveRDS(list(
  primary_results = primary_results,
  s11_1           = s11_1_results,    # threshold
  s11_2           = s11_2_results,    # top 1% excluded
  s11_3           = s11_3_results,    # quarters
  s11_4           = s11_4_results,    # last-month risk
  s11_5           = s11_5_results,    # no YA
  s11_6           = s11_6_results,    # ITS no corAR1
  summary         = sensitivity_summary
), "results/06b_sensitivity/sensitivity_results.rds")

stamp("Results saved to results/06b_sensitivity/sensitivity_results.rds")
cat("\nSaved: results/06b_sensitivity/sensitivity_results.rds\n")
cat("\n=== 06b_poc_sensitivity.R complete ===\n")
