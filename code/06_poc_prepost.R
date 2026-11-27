# =============================================================================
# 06_poc_prepost.R
# Purpose : Pre-post comparisons — H1-H5, H8, and intensity-conditional
#           secondaries
# Project : A Safe Bet (ASB) — Proof-of-Concept evaluation.
#           Putting Design into Practice: A Safe Bet — A Proof-of-Concept
#           Evaluation of a Tailored Online Responsible Gambling Framework
# Inputs  : poc_synthetic.rds or poc_analytical_LOCKED.rds (via 00_config.R)
# Outputs : results/06_prepost/prepost_results.rds
# Flags   : USE_SYNTHETIC — TRUE runs on poc_synthetic.rds (pre-registration
#           phase); FALSE runs on poc_analytical_LOCKED.rds
#           (post-registration). Currently FALSE.
# =============================================================================
# PRE-REG:  OSF [URL to be added] - implements prereg Sections 6.2, 6.3, 6.5
#           (script table, prereg Section 10.1)
# =============================================================================
#
# HYPOTHESES:
#   H1: Total deposit_amount lower in intervention vs baseline
#   H2: Total net_loss lower in intervention vs baseline
#   H3: Total wagers lower in intervention vs baseline
#   H4: Total play hours lower in intervention vs baseline
#      (with night play hours analysed separately)
#   H5: Risk tier transitions show significant asymmetry (more improving
#       than worsening), particularly among medium-risk players
#   Plus: intensity-conditional per-AGD secondary outcomes (prereg 6.2)
#         and the H8 risk-stratified analyses (prereg 6.5)
#
# ANALYSIS PLAN:
#   Frequentist: Wilcoxon signed-rank, one-sided (alternative = "greater";
#                prereg 6.2 and Section 8), alpha = .05; stats::wilcox.test
#                default zero and tie handling (zero-exclusion, average
#                ranks, normal approximation with continuity correction),
#                named explicitly in prereg 6.2
#   Bayesian:    JZS Bayes factor via direct numerical integration of the
#                Cauchy(0, r=1/sqrt(2)) prior (replaces wilcoxonBF() per
#                prereg 10.4 item 6)
#   Effect size: rank-biserial correlation r = Z/sqrt(N)
#   H5: Bayesian A/B test (proportionBF) comparing improvement vs
#       worsening proportions; posterior distribution under H1 reported
# =============================================================================

library(dplyr)
library(BayesFactor)

source("00_config.R")
library(conflicted)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::summarise)
conflicts_prefer(dplyr::arrange)
conflicts_prefer(dplyr::count)
conflicts_prefer(dplyr::group_by)

# Timestamped progress marker so long runs are never silent about what is
# currently executing (print-only; no effect on any computation)
stamp <- function(msg) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), msg))

USE_SYNTHETIC <- FALSE

if (USE_SYNTHETIC) {
  dat <- readRDS(FILE_POC_SYNTHETIC)
  pp  <- dat$player_period_synth
  cat("Running on SYNTHETIC data\n")
} else {
  dat <- readRDS(FILE_POC_LOCKED)
  pp  <- dat$player_period
  cat("Running on LOCKED real data\n")
}
stamp(sprintf("06_poc_prepost.R started; data loaded (%d rows)", nrow(pp)))

dir.create("results/06_prepost", recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

# Wilcoxon signed-rank with rank-biserial r effect size
# Directional test: alt = "greater" tests baseline > intervention
wilcox_test_paired <- function(baseline_vec, intervention_vec, label) {
  # Remove pairs where either value is NA
  complete <- !is.na(baseline_vec) & !is.na(intervention_vec)
  bl  <- baseline_vec[complete]
  int <- intervention_vec[complete]
  n   <- sum(complete)

  # Zero paired differences (baseline total exactly equals intervention total)
  # are excluded from ranking by the stats::wilcox.test default (Wilcoxon
  # zero-exclusion); their count is reported per outcome (prereg 6.2).
  n_zero_diff <- sum(bl == int)

  wtest <- wilcox.test(bl, int, paired = TRUE,
                       alternative = "greater",  # directional: expect baseline > intervention
                       exact = FALSE, conf.int = TRUE)
  
  # Rank-biserial r = Z / sqrt(N)
  # Rank-biserial r for paired Wilcoxon
  # W is the sum of positive ranks; expected value under H0 = n*(n+1)/4
  # Standardise directly from W rather than back-computing from p-value
  W_expected <- n * (n + 1) / 4
  W_sd       <- sqrt(n * (n + 1) * (2 * n + 1) / 24)
  z_stat     <- (wtest$statistic - W_expected) / W_sd
  r_rb       <- z_stat / sqrt(n)
  
  # Hodges-Lehmann estimator (median pairwise difference)
  hl_est <- wtest$estimate
  
  list(
    label       = label,
    n           = n,
    n_zero_diff = n_zero_diff,
    stat_V      = wtest$statistic,
    z           = z_stat,
    p_value     = wtest$p.value,
    r_rb        = r_rb,
    hl_est      = hl_est,
    ci_lo       = wtest$conf.int[1],
    ci_hi       = wtest$conf.int[2]
  )
}

# Bayesian analysis for H1-H4
# -----------------------------------------------------------------------
# DESIGN RATIONALE:
#   bf10_t() returns BF10 directly (not log BF10). For N=5,808 with
#   r >= 0.25, BF10 overflows R double precision (max ~1.8e308).
#   H3 wagers is an exception: wager differences are extremely heavy-tailed
#   so the parametric t-stat on standardised differences is small even
#   when the Wilcoxon r is large.
#
#   Solution: compute log(BF10) via direct numerical integration of the
#   JZS (Jeffreys-Zellner-Siow) integral (Rouder et al., 2009, PBR).
#   This is stable at any N and effect size.
#
#   Formula: log BF10 = log[integral_0_inf f(g) dg]
#   where f(g) is the JZS integrand in terms of the mixing variable g.
#   Cauchy(0, r) prior on delta corresponds to half-t(1) on sqrt(g).
#
# Prior: Cauchy(0, r) with r = sqrt(2)/2 ("medium" rscale).
# -----------------------------------------------------------------------

log_bf10_jzs <- function(t_stat, n, r_cauchy = sqrt(2) / 2) {
  # Log BF10 for one-sample/paired t via JZS numerical integration.
  # Reference: Rouder et al. (2009) Psychonomic Bulletin & Review.
  # Returns log(BF10) on natural log scale.
  #
  # KEY FIX: integrate() passes a VECTOR of g values to the integrand.
  # All operations must be vectorised — no scalar if(), use ifelse().
  
  df <- n - 1
  
  # Vectorised log-integrand (safe for g <= 0)
  log_integrand <- function(g) {
    ifelse(
      g <= 0,
      -Inf,
      -0.5 * log(1 + n * g) +
        (-(df + 1) / 2) * log(1 + t_stat^2 / ((1 + n * g) * df)) +
        -1.5 * log(g) +
        -(r_cauchy^2) / (2 * g)
    )
  }
  
  # Find mode for numerical stability
  g_mode <- tryCatch({
    opt <- optimise(function(g) log_integrand(g), interval = c(1e-8, 1e4), maximum = TRUE)
    opt$maximum
  }, error = function(e) 1 / n)
  
  log_peak <- log_integrand(g_mode)
  
  if (!is.finite(log_peak)) return(NA_real_)
  
  # Integrate on shifted scale to avoid underflow
  int_result <- tryCatch(
    integrate(
      function(g) exp(log_integrand(g) - log_peak),
      lower = 1e-8, upper = Inf,
      rel.tol = 1e-6, subdivisions = 500
    )$value,
    error = function(e) NA_real_
  )
  
  if (is.na(int_result) || int_result <= 0) return(NA_real_)
  
  # log BF10 = log_peak + log(integral) + log(r/sqrt(2pi)) - log p(t | H0)
  log_marginal_h0 <- dt(t_stat, df = df, log = TRUE)
  log_peak + log(int_result) + log(r_cauchy) - 0.5 * log(2 * pi) - log_marginal_h0
}

bayesian_wilcox <- function(baseline_vec, intervention_vec, label) {
  
  # Data preparation
  complete <- !is.na(baseline_vec) & !is.na(intervention_vec)
  d        <- baseline_vec[complete] - intervention_vec[complete]
  n_obs    <- length(d)
  d_mean   <- mean(d)
  d_sd     <- sd(d)
  
  if (n_obs < 10 || d_sd == 0) {
    cat(sprintf("  %s: skipped (n=%d)\n", label, n_obs))
    return(list(label = label, bf10 = NA, log10_bf = NA,
                interpretation = "Insufficient data"))
  }
  
  # Paired t-statistic on raw differences
  t_stat <- d_mean / (d_sd / sqrt(n_obs))
  
  # Log BF10 via JZS numerical integration
  log_bf10 <- log_bf10_jzs(t_stat, n_obs)
  
  if (is.na(log_bf10) || !is.finite(log_bf10)) {
    cat(sprintf("  %s: BF integration failed\n", label))
    return(list(label = label, bf10 = NA, log10_bf = NA,
                interpretation = "Integration failed"))
  }
  
  log10_bf <- log_bf10 / log(10)
  
  # Report
  if (log10_bf > 300) {
    cat(sprintf("  %s: log10(BF10) > 300 (extreme evidence, overflow)\n", label))
  } else if (log10_bf > 6) {
    cat(sprintf("  %s: log10(BF10) = %.1f\n", label, log10_bf))
  } else {
    cat(sprintf("  %s: BF10 = %.2f\n", label, exp(log_bf10)))
  }
  
  interp <- dplyr::case_when(
    log10_bf >  2    ~ "Extreme evidence for H1",
    log10_bf >  1.48 ~ "Very strong evidence for H1",
    log10_bf >  1    ~ "Strong evidence for H1",
    log10_bf >  0.48 ~ "Moderate evidence for H1",
    log10_bf >  0    ~ "Anecdotal evidence for H1",
    log10_bf == 0    ~ "No evidence either way",
    log10_bf > -0.48 ~ "Anecdotal evidence for H0",
    log10_bf > -1    ~ "Moderate evidence for H0",
    TRUE             ~ "Strong evidence for H0"
  )
  
  list(
    label          = label,
    bf10           = if (log10_bf > 300) Inf else exp(log_bf10),
    log10_bf       = log10_bf,
    t_stat         = t_stat,
    n              = n_obs,
    prior          = "Cauchy r = 1/sqrt(2), JZS",
    interpretation = interp
  )
}


# =============================================================================
# RESHAPE TO WIDE FORMAT (one row per player)
# =============================================================================

pp_wide <- pp |>
  dplyr::select(participant_id, period, total_deposits, total_losses, total_wagers,
                play_hours_total, agd_count, dominant_risk,
                dplyr::any_of(c("night_hours_total", "engaged_cohort"))) |>
  tidyr::pivot_wider(names_from = period,
                     values_from = c(total_deposits, total_losses, total_wagers,
                                     play_hours_total, agd_count, dominant_risk,
                                     dplyr::any_of(c("night_hours_total"))))

cat("N players in wide format:", nrow(pp_wide), "\n")
if ("engaged_cohort" %in% names(pp_wide)) {
  cat("N engaged cohort:", sum(pp_wide$engaged_cohort, na.rm = TRUE), "\n")
} else {
  cat("N engaged cohort: not available in synthetic data\n")
}

# =============================================================================
# H1-H4 Wilcoxon signed-rank, prereg 6.2 - confirmatory
# Full cohort (N = 5,808) and engaged cohort (n = 5,103)
# =============================================================================

outcomes <- list(
  list(var = "total_deposits",   label = "Total deposit_amount (GBP)",    hyp = "H1"),
  list(var = "total_losses",     label = "Total net_loss (GBP)",      hyp = "H2"),
  list(var = "total_wagers",     label = "Total wagers (GBP)",      hyp = "H3"),
  list(var = "play_hours_total", label = "Total play hours",        hyp = "H4")
)
# H4b (night play hours) only available on real data — not in synthetic
if ("night_hours_total_baseline" %in% names(pp_wide)) {
  outcomes <- c(outcomes, list(
    list(var = "night_hours_total", label = "Night play hours", hyp = "H4b")
  ))
} else {
  cat("Note: night_hours_total not in synthetic data — H4b skipped\n")
}

freq_results_full    <- list()
freq_results_engaged <- list()
bayes_results_full   <- list()

cat("\n=== FREQUENTIST TESTS (full cohort N =", nrow(pp_wide), ") ===\n")
stamp("H1-H4 frequentist block starting")

for (o in outcomes) {
  stamp(sprintf("H1-H4 frequentist: %s", o$label))
  bl_col  <- paste0(o$var, "_baseline")
  int_col <- paste0(o$var, "_intervention")

  # Full cohort
  res_f <- wilcox_test_paired(pp_wide[[bl_col]], pp_wide[[int_col]], o$label)
  freq_results_full[[o$hyp]] <- res_f
  
  cat(sprintf("%s (%s): V = %.0f, p = %.4f, r = %.3f, HL = %.2f, zero-diff pairs = %d\n",
              o$hyp, o$label, res_f$stat_V, res_f$p_value, res_f$r_rb, res_f$hl_est,
              res_f$n_zero_diff))
  
  # Engaged cohort
  if ("engaged_cohort" %in% names(pp_wide)) {
    engaged_wide <- pp_wide |> dplyr::filter(engaged_cohort == TRUE)
  } else {
    engaged_wide <- pp_wide  # fall back to full cohort on synthetic
    cat("  Note: engaged_cohort not in synthetic — using full cohort as proxy\n")
  }
  res_e <- wilcox_test_paired(engaged_wide[[bl_col]], engaged_wide[[int_col]],
                              paste0(o$label, " (engaged)"))
  freq_results_engaged[[o$hyp]] <- res_e
}

# H1-H4 JZS Bayes factors, prereg 6.3 - confirmatory (Bayesian counterpart)
cat("\n=== BAYESIAN TESTS (full cohort) ===\n")
cat("Prior: Cauchy r = 1/sqrt(2) | Two-sided BF10 (JZS; prereg 6.3).\n")
cat("Frequentist tests are one-sided (baseline > intervention); the two-sided\n")
cat("BF10 is conservative with respect to the predicted direction.\n\n")
stamp("H1-H4 Bayesian (JZS) block starting")

for (o in outcomes) {
  stamp(sprintf("H1-H4 Bayesian JZS: %s", o$label))
  bl_col  <- paste0(o$var, "_baseline")
  int_col <- paste0(o$var, "_intervention")

  res_b <- bayesian_wilcox(pp_wide[[bl_col]], pp_wide[[int_col]], o$label)
  bayes_results_full[[o$hyp]] <- res_b
}
stamp("H1-H4 Bayesian block done")

# =============================================================================
# SECONDARY: INTENSITY-CONDITIONAL OUTCOMES PER AGD (prereg 6.2) - secondary
# Engaged cohort only (n = 5,103 expected in real data; analytical-sample
# participants with at least one intervention AGD). Four per-AGD ratios of the
# H1-H4 outcomes (participant-period total / participant-period AGD count),
# tested with the identical one-sided Wilcoxon signed-rank procedure and
# reported with the same effect sizes and zero-difference counts. Frequentist
# plus effect sizes only; no Bayes factors (prereg 6.2). Purpose is
# discriminative under the Section 8 harm-reduction versus disengagement
# adjudication rule. analysis_class = "secondary_intensity".
# =============================================================================

cat("\n=== SECONDARY: INTENSITY-CONDITIONAL OUTCOMES PER AGD (engaged cohort) ===\n")
stamp("Secondary intensity-conditional block starting")

# Reuse the engaged-cohort flag built in 01_poc_build.R. On synthetic data the
# flag is carried through 03_poc_simulate.R but is synthesised parametrically,
# so it is not guaranteed consistent with the synthesised AGD counts; where the
# flag is absent, the cohort is reconstructed from its definition (>= 1
# intervention AGD) with a printed note.
if ("engaged_cohort" %in% names(pp_wide)) {
  intensity_wide <- pp_wide |> dplyr::filter(engaged_cohort == TRUE)
} else {
  cat("  Note: engaged_cohort flag not present - reconstructed from its\n")
  cat("  definition (agd_count_intervention >= 1) for this block only\n")
  intensity_wide <- pp_wide |> dplyr::filter(agd_count_intervention >= 1)
}
cat("  Engaged cohort n =", nrow(intensity_wide), "(expected 5,103 in real data)\n")

# Denominator guard: baseline AGD >= 15 by inclusion; intervention AGD >= 1 by
# the engaged-cohort definition, so zero denominators are impossible on the
# real data. Enforced hard on the locked real data; on synthetic data the
# parametric synthesis does not enforce the cohort logic, so violating rows
# are dropped with a printed count instead of aborting the pipeline check.
if (USE_SYNTHETIC) {
  n_bad_denom <- sum(intensity_wide$agd_count_baseline < 1 |
                       intensity_wide$agd_count_intervention < 1)
  if (n_bad_denom > 0) {
    cat("  Synthetic-data note:", n_bad_denom,
        "rows with zero-AGD denominators dropped (synthesis artefact)\n")
    intensity_wide <- intensity_wide |>
      dplyr::filter(agd_count_baseline >= 1, agd_count_intervention >= 1)
  }
} else {
  stopifnot(
    "Zero baseline AGD in engaged cohort (inclusion requires >= 15)" =
      all(intensity_wide$agd_count_baseline >= 1),
    "Zero intervention AGD in engaged cohort (violates cohort definition)" =
      all(intensity_wide$agd_count_intervention >= 1)
  )
}

intensity_wide <- intensity_wide |>
  dplyr::mutate(
    deposits_per_agd_baseline     = total_deposits_baseline       / agd_count_baseline,
    deposits_per_agd_intervention = total_deposits_intervention   / agd_count_intervention,
    loss_per_agd_baseline         = total_losses_baseline         / agd_count_baseline,
    loss_per_agd_intervention     = total_losses_intervention     / agd_count_intervention,
    wagers_per_agd_baseline       = total_wagers_baseline         / agd_count_baseline,
    wagers_per_agd_intervention   = total_wagers_intervention     / agd_count_intervention,
    hours_per_agd_baseline        = play_hours_total_baseline     / agd_count_baseline,
    hours_per_agd_intervention    = play_hours_total_intervention / agd_count_intervention
  )

intensity_outcomes <- list(
  list(var = "deposits_per_agd", label = "Deposits per AGD (GBP)"),
  list(var = "loss_per_agd",     label = "Net loss per AGD (GBP)"),
  list(var = "wagers_per_agd",   label = "Wagers per AGD (GBP)"),
  list(var = "hours_per_agd",    label = "Play hours per AGD")
)

intensity_results <- list()
for (o in intensity_outcomes) {
  stamp(sprintf("Intensity secondary: %s", o$label))
  bl_col  <- paste0(o$var, "_baseline")
  int_col <- paste0(o$var, "_intervention")
  res_i <- wilcox_test_paired(intensity_wide[[bl_col]], intensity_wide[[int_col]],
                              paste0(o$label, " (engaged, per AGD)"))
  res_i$analysis_class <- "secondary_intensity"
  intensity_results[[o$var]] <- res_i
  cat(sprintf("SECONDARY %s: V = %.0f, p = %.4f, r = %.3f, HL = %.2f [%.2f, %.2f], zero-diff pairs = %d\n",
              o$label, res_i$stat_V, res_i$p_value, res_i$r_rb, res_i$hl_est,
              res_i$ci_lo, res_i$ci_hi, res_i$n_zero_diff))
}
stamp("Secondary intensity-conditional block done")

# =============================================================================
# H5 risk tier transitions (McNemar-Bowker), prereg 6.2 - confirmatory
# Primary: McNemar-Bowker symmetry test (frequentist); Secondary: Bayesian A/B test
# =============================================================================

cat("\n=== H5: RISK TIER TRANSITIONS ===\n")
stamp("H5 block starting")
cat("Primary: McNemar-Bowker symmetry test; Secondary: Bayesian A/B test (improvement vs worsening)\n\n")

# Build 3x3 transition matrix (baseline -> intervention dominant risk)
risk_trans <- pp_wide |>
  dplyr::filter(!is.na(dominant_risk_baseline), !is.na(dominant_risk_intervention)) |>
  dplyr::count(dominant_risk_baseline, dominant_risk_intervention)

cat("Risk transition matrix:\n")
risk_matrix <- risk_trans |>
  tidyr::pivot_wider(names_from = dominant_risk_intervention, values_from = n, values_fill = 0)
print(risk_matrix)

# Define improvement and worsening
# Improvement: medium->low, high->medium, high->low
# Worsening:   low->medium, low->high, medium->high
risk_order <- c("low" = 1, "medium" = 2, "high" = 3)

risk_trans <- risk_trans |>
  dplyr::mutate(
    from_num = risk_order[dominant_risk_baseline],
    to_num   = risk_order[dominant_risk_intervention],
    direction = dplyr::case_when(
      to_num < from_num ~ "improved",
      to_num > from_num ~ "worsened",
      TRUE              ~ "stable"
    )
  )

n_improved <- sum(risk_trans$n[risk_trans$direction == "improved"])
n_worsened <- sum(risk_trans$n[risk_trans$direction == "worsened"])
n_stable   <- sum(risk_trans$n[risk_trans$direction == "stable"])
n_changed  <- n_improved + n_worsened

cat(sprintf("\nOf players who changed risk tier:\n"))
cat(sprintf("  Improved: %d (%.1f%%)\n", n_improved, n_improved/n_changed*100))
cat(sprintf("  Worsened: %d (%.1f%%)\n", n_worsened, n_worsened/n_changed*100))
cat(sprintf("  Stable:   %d\n", n_stable))

# PRIMARY (prereg 6.2): McNemar-Bowker test of symmetry (frequentist)
risk_levels <- c("low", "medium", "high")
trans_matrix_sq <- matrix(0, nrow = 3, ncol = 3,
                          dimnames = list(risk_levels, risk_levels))
for (i in seq_len(nrow(risk_trans))) {
  r <- risk_trans$dominant_risk_baseline[i]
  c <- risk_trans$dominant_risk_intervention[i]
  if (r %in% risk_levels && c %in% risk_levels)
    trans_matrix_sq[r, c] <- risk_trans$n[i]
}
mcnemar_result <- tryCatch({
  stat <- 0; df <- 0
  for (i in 1:3) {
    for (j in (i+1):3) {
      if (i < j && j <= 3) {
        n_ij <- trans_matrix_sq[i, j]; n_ji <- trans_matrix_sq[j, i]
        if (n_ij + n_ji > 0) { stat <- stat + (n_ij - n_ji)^2 / (n_ij + n_ji); df <- df + 1 }
      }
    }
  }
  list(statistic = stat, df = df, p.value = pchisq(stat, df, lower.tail = FALSE))
}, error = function(e) NULL)
if (!is.null(mcnemar_result)) {
  cat(sprintf("\nPrimary — McNemar-Bowker symmetry test:\n"))
  cat(sprintf("  chi-sq(%d) = %.3f, p = %.4f\n",
              mcnemar_result$df, mcnemar_result$statistic, mcnemar_result$p.value))
}

# SECONDARY: Bayesian A/B test — does improvement proportion > 0.5 among changers?
# proportionBF tests H1: p_improve > 0.5 vs H0: p_improve = 0.5
bf_h5 <- tryCatch(
  proportionBF(n_improved, n_improved + n_worsened,
               p = 0.5,
               nullInterval = c(0.5, 1)),  # directional: expect improvement > 0.5
  error = function(e) { cat("H5 BF error:", conditionMessage(e), "\n"); NULL }
)

if (!is.null(bf_h5)) {
  cat(sprintf("\nSecondary — Bayesian A/B test H5:\n"))
  # BayesFactor S4 object — extract via extractBF()
  bf_h5_val <- tryCatch(
    extractBF(bf_h5)$bf[1],
    error = function(e) as.vector(bf_h5@bayesFactor$bf)[1]
  )
  cat(sprintf("  BF10 (improvement > 0.5): %.2f\n", bf_h5_val))
  
  # Posterior distribution under H1
  # Beta posterior: Beta(n_improved + 1, n_worsened + 1)
  alpha_post <- n_improved + 1
  beta_post  <- n_worsened + 1
  post_mean  <- alpha_post / (alpha_post + beta_post)
  post_ci    <- qbeta(c(0.025, 0.975), alpha_post, beta_post)
  p_gt_half  <- pbeta(0.5, alpha_post, beta_post, lower.tail = FALSE)
  
  cat(sprintf("  Posterior mean (improvement rate): %.3f\n", post_mean))
  cat(sprintf("  95%% CrI: [%.3f, %.3f]\n", post_ci[1], post_ci[2]))
  cat(sprintf("  P(improvement > 0.5 | data): %.4f\n", p_gt_half))
}

# H8 risk-stratified analyses, prereg 6.5 - pre-specified; interpreted as
# exploratory per prereg 6.5 (low-risk group substantially larger than medium)
cat("\n=== H8: RISK-STRATIFIED SUBGROUP ===\n")
stamp("H5 block done; H8 block starting")

# Descriptive: primary outcomes replicated separately by baseline risk level
cat("Descriptive — primary outcomes by baseline risk level:\n\n")
for (risk_grp in c("low", "medium")) {
  stamp(sprintf("H8 descriptive stratified Wilcoxons: %s-risk group", risk_grp))
  grp_wide <- pp_wide |>
    dplyr::filter(dominant_risk_baseline == risk_grp)
  cat(sprintf("--- %s-risk (n = %d) ---\n", risk_grp, nrow(grp_wide)))
  for (o in outcomes[1:4]) {
    bl_col  <- paste0(o$var, "_baseline")
    int_col <- paste0(o$var, "_intervention")
    res <- wilcox_test_paired(grp_wide[[bl_col]], grp_wide[[int_col]], o$label)
    cat(sprintf("  %s: V = %.0f, p = %.4f, r = %.3f\n",
                o$label, res$stat_V, res$p_value, res$r_rb))
  }
}

# Shared H8 change-score frame: one row per participant (low/medium baseline
# risk only; high-risk n=2 excluded per prereg 6.5), with per-outcome change
# scores computed inside each loop below.
h8_wide <- pp_wide |>
  dplyr::filter(dominant_risk_baseline %in% c("low", "medium")) |>
  dplyr::mutate(risk_tier = factor(dominant_risk_baseline,
                                   levels = c("low", "medium")))

# H8 primary: Mann-Whitney on change scores, prereg 6.5 - one-tailed
# Raw-scale change chg = baseline - intervention per participant (positive =
# reduction), matching the raw scale of the H1-H4 primary tests. Direction:
# H8 predicts a GREATER reduction in the medium-risk group, i.e. chg(low) <
# chg(medium); with risk_tier levels ordered c("low", "medium"), R compares
# low against medium, so the one-tailed alternative is "less".
# Rank-biserial effect size r_rb = 1 - 2W/(n_low x n_medium), where W is R's
# Mann-Whitney U for the low group; positive r_rb = larger reduction in the
# medium-risk group, i.e. in the H8 direction.
stamp("H8 primary: Mann-Whitney on change scores (one-tailed) starting")
h8_primary_mw <- list()
for (o in outcomes[1:4]) {
  stamp(sprintf("H8 Mann-Whitney: %s", o$label))
  bl_col  <- paste0(o$var, "_baseline")
  int_col <- paste0(o$var, "_intervention")
  mw_df <- h8_wide |>
    dplyr::transmute(
      risk_tier,
      chg = .data[[bl_col]] - .data[[int_col]]
    ) |>
    dplyr::filter(!is.na(chg), !is.na(risk_tier))
  n_low <- sum(mw_df$risk_tier == "low")
  n_med <- sum(mw_df$risk_tier == "medium")
  mw <- tryCatch(
    wilcox.test(chg ~ risk_tier, data = mw_df,
                alternative = "less",   # low < medium: greater reduction at medium
                exact = FALSE),
    error = function(e) { cat("  ", o$hyp, "Mann-Whitney error:", conditionMessage(e), "\n"); NULL }
  )
  if (!is.null(mw)) {
    r_rb_mw <- 1 - 2 * as.numeric(mw$statistic) / (n_low * n_med)
    h8_primary_mw[[o$hyp]] <- list(
      label = o$label, W = as.numeric(mw$statistic), p_value = mw$p.value,
      r_rb = r_rb_mw, n_low = n_low, n_medium = n_med,
      alternative = "chg(low) < chg(medium), one-tailed"
    )
    cat(sprintf("  %s (%s): W = %.0f, p = %.4f, r_rb = %.3f (n_low = %d, n_medium = %d)\n",
                o$hyp, o$label, as.numeric(mw$statistic), mw$p.value, r_rb_mw,
                n_low, n_med))
  }
}
stamp("H8 primary Mann-Whitney block done")

# H8 interaction, change-score formulation, prereg 6.5 (v28): the Risk x Time
# interaction is computed on within-participant change on the transformed
# scale, matching Section 6.4's transforms. Per prereg 6.4 (v28): asinh for
# net loss, defined for negative values (net winners, for which log1p is NaN
# and silently dropped ~1,970 participants), approximates log for large
# positive values; log1p unchanged for deposits, wagers, play hours.
# Frequentist: classic two-sample t-test with var.equal = TRUE, which is
# exactly equivalent to the mixed-ANOVA interaction F on a two-level
# within-participant factor (t squared = F); two-sided, as the ANOVA F is.
# Bayesian: BayesFactor::lmBF(chg_trans ~ risk_tier) reporting BF10 for the
# risk term against the intercept-only model; a single fixed factor, so
# deterministic quadrature with no participant-level random factor (anovaBF
# with a 5,799-level random factor was computationally infeasible and was
# removed per prereg 6.5 v27). Default priors throughout.
stamp("H8 interaction (change-score formulation) starting")
h8_interaction_bf <- list()
h8_interaction_t  <- list()
for (o in outcomes[1:4]) {
  bl_col  <- paste0(o$var, "_baseline")
  int_col <- paste0(o$var, "_intervention")
  # Transform per prereg 6.4 (v28): asinh for net loss, defined for negative
  # values, approximates log for large positive values; log1p otherwise
  trans_fun <- if (o$var == "total_losses") asinh else log1p
  chg_df <- h8_wide |>
    dplyr::transmute(
      risk_tier,
      chg_trans = trans_fun(.data[[bl_col]]) - trans_fun(.data[[int_col]])
    ) |>
    dplyr::filter(!is.na(risk_tier)) |>
    as.data.frame()

  # Guard: the transform must never silently produce NaN (is.na() also
  # matches NaN, which is how log1p of negative net loss dropped net winners
  # unnoticed); genuine NAs are filtered only after this check
  stopifnot("NaN in H8 change scores - check transform domains" =
              !any(is.nan(chg_df$chg_trans)))
  chg_df <- chg_df |> dplyr::filter(!is.na(chg_trans))

  # Frequentist interaction counterpart (two-sided, ANOVA-equivalent)
  tt <- tryCatch(
    t.test(chg_trans ~ risk_tier, data = chg_df, var.equal = TRUE),
    error = function(e) { cat("  ", o$hyp, "t-test error:", conditionMessage(e), "\n"); NULL }
  )
  if (!is.null(tt)) {
    h8_interaction_t[[o$hyp]] <- list(
      label = o$label, t = unname(tt$statistic), df = unname(tt$parameter),
      p_value = tt$p.value, mean_low = unname(tt$estimate[1]),
      mean_medium = unname(tt$estimate[2])
    )
    cat(sprintf("  %s (%s): interaction t(%d) = %.3f, p = %.4f (change-score formulation)\n",
                o$hyp, o$label, as.integer(unname(tt$parameter)),
                unname(tt$statistic), tt$p.value))
  }

  # Bayesian interaction: BF10 for risk_tier vs intercept-only on change scores
  t0 <- Sys.time()
  stamp(sprintf("H8 lmBF: %s (chg_trans ~ risk_tier)", o$label))
  bf <- tryCatch(
    lmBF(chg_trans ~ risk_tier, data = chg_df),
    error = function(e) { cat("  ", o$hyp, "lmBF error:", conditionMessage(e), "\n"); NULL }
  )
  if (!is.null(bf)) {
    bf_int <- tryCatch(extractBF(bf)$bf[1], error = function(e) NA_real_)
    h8_interaction_bf[[o$hyp]] <- bf_int
    cat(sprintf("  %s (%s): interaction BF10 = %.3f\n", o$hyp, o$label, bf_int))
  }
  stamp(sprintf("H8 lmBF: %s done (%.1f s)", o$label,
                as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}
stamp("H8 interaction block done")

# =============================================================================
# SAVE RESULTS
# =============================================================================

saveRDS(list(
  freq_full    = freq_results_full,
  freq_engaged = freq_results_engaged,
  bayes_full   = bayes_results_full,
  secondary_intensity = intensity_results,   # per-AGD ratios, engaged cohort (prereg 6.2)
  risk_trans   = risk_trans,
  risk_matrix  = risk_matrix,
  bf_h5        = if (exists("bf_h5")) bf_h5 else NULL,
  h8_primary_mw     = if (exists("h8_primary_mw"))     h8_primary_mw     else NULL,
  h8_interaction_t  = if (exists("h8_interaction_t"))  h8_interaction_t  else NULL,
  h8_interaction_bf = if (exists("h8_interaction_bf")) h8_interaction_bf else NULL
), "results/06_prepost/prepost_results.rds")
stamp("Results saved to results/06_prepost/prepost_results.rds")

cat("\n=== 06_poc_prepost.R complete ===\n")
cat("Interpretation framework:\n")
cat("  BF10 > 10  = strong evidence for H1 (effect present)\n")
cat("  BF10 < 1/10 = strong evidence for H0 (effect absent)\n")
cat("  Significant frequentist result with BF10 < 3: requires replication\n")