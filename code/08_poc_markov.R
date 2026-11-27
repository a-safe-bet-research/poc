# =============================================================================
# 08_poc_markov.R
# A Safe Bet – POC Analysis
# Markov risk transition model — monthly and weekly
# =============================================================================
# PRE-REG:  OSF [URL to be added] - implements prereg Section 6.7
#           (script table, prereg Section 10.1)
# RUNS ON:  poc_synthetic.rds          (USE_SYNTHETIC = TRUE)
#           poc_analytical_LOCKED.rds  (USE_SYNTHETIC = FALSE)
# INPUTS:   poc_synthetic.rds or poc_analytical_LOCKED.rds (via 00_config.R);
#           poc_raw.rds for the weekly Markov (real-data runs only)
# OUTPUTS:  results/08_markov/markov_results.rds
# =============================================================================
#
# STATES: L (low), M (medium), H (high), I (inactive)
#
# MODELS (all pre-registered in prereg 6.7):
#   1. Monthly Markov chain (primary)
#   2. Homogeneity test across intervention months (Anderson-Goodman)
#   3. Log-linear baseline vs intervention period comparison
#   4. Bayesian Dirichlet-Multinomial: Dirichlet(1,1,1,1) priors per row
#   4b. Active-state (L/M/H) transition submatrix, Dirichlet(1,1,1) rows
#   5. msm continuous-time model with engagement covariates, plus a
#      recalibration-adjusted specification (pre-specified) alongside
#   6. Weekly Markov chain (secondary)
#
# NA STATES (prereg 6.7): participant-months with an unresolvable NA state
# are treated as missing observations and omitted from the state sequence,
# with no imputation; the msm continuous-time formulation absorbs the
# resulting unequal observation intervals.
#
# ENGAGEMENT COVARIATES (prereg 5.4 / 6.6):
#   Four continuous engagement covariates entered as time-varying covariates
#   in the msm specification:
#     - msg_response_rate   (proportion in [0, 1] — reactive engagement)
#     - n_limit_intents     (count; entered as log1p — active engagement)
#     - limit_page_dwell    (seconds; entered as log1p — info-seeking)
#     - rg_support_dwell    (seconds; entered as log1p — info-seeking)
#   Direction-conditional analysis (protective vs escalating limit modifications)
#   is out of scope for this paper.
# =============================================================================

library(dplyr)
library(tidyr)
library(lubridate)
library(minqa)

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

# OVERWRITE = FALSE makes the msm fits crash-resumable: each fit is saved to
# its own cache .rds immediately after fitting and loaded instead of refitted
# on rerun. Caches are suffixed by data path (synthetic/real) because msm has
# no built-in staleness check: a synthetic-run cache must never be loaded in
# a real-data run. Set TRUE to refit from scratch.
OVERWRITE <- FALSE

if (USE_SYNTHETIC) {
  dat <- readRDS(FILE_POC_SYNTHETIC)
  pm  <- dat$player_month_synth
  cat("Running on SYNTHETIC data\n")
} else {
  dat <- readRDS(FILE_POC_LOCKED)
  pm  <- dat$player_month
  cat("Running on LOCKED real data\n")
}
stamp(sprintf("08_poc_markov.R started; data loaded (%d rows)", nrow(pm)))

dir.create("results/08_markov", recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# Schema validation — fail loudly if required columns missing
# -----------------------------------------------------------------------------
required_cols <- c(
  "participant_id", "year_month", "is_intervention", "markov_state",
  ENGAGEMENT_COVARIATES   # msg_response_rate, n_limit_intents, limit_page_dwell, rg_support_dwell
)
missing_cols <- setdiff(required_cols, names(pm))
stopifnot(
  "Missing required columns in player_month — re-run 01_poc_build.R" =
    length(missing_cols) == 0
)
cat("  Required columns present:", paste(required_cols, collapse = ", "), "\n")

unexpected_states <- setdiff(unique(pm$markov_state), c(MARKOV_STATES, NA_character_))
stopifnot(
  "Unexpected markov_state values" =
    length(unexpected_states) == 0
)

stopifnot(
  "msg_response_rate outside [0, 1]" =
    all(pm$msg_response_rate >= 0 & pm$msg_response_rate <= 1, na.rm = TRUE),
  "Negative n_limit_intents"   = all(pm$n_limit_intents  >= 0, na.rm = TRUE),
  "Negative limit_page_dwell"  = all(pm$limit_page_dwell >= 0, na.rm = TRUE),
  "Negative rg_support_dwell"  = all(pm$rg_support_dwell >= 0, na.rm = TRUE)
)
cat("  Schema validation passed.\n")

# NA-state participant-months are treated as missing observations and omitted
# from the state sequence, with no imputation (prereg 6.7); count reported here
n_na_state <- sum(is.na(pm$markov_state))
cat("  Participant-months with unresolvable NA state (omitted, no imputation):",
    n_na_state, "\n")


# =============================================================================
# HELPERS
# =============================================================================

compute_trans_matrix <- function(pm_data, period_filter = NULL) {
  states <- c("L", "M", "H", "I")
  trans  <- matrix(0, 4, 4, dimnames = list(from = states, to = states))
  
  if (!is.null(period_filter)) {
    pm_data <- pm_data |> dplyr::filter(is_intervention == period_filter)
  }
  
  pm_ordered <- pm_data |> dplyr::arrange(participant_id, year_month)
  
  for (pid in unique(pm_ordered$participant_id)) {
    seq <- pm_ordered$markov_state[pm_ordered$participant_id == pid]
    seq <- seq[!is.na(seq)]
    if (length(seq) < 2) next
    for (t in seq_len(length(seq) - 1)) {
      s1 <- seq[t]; s2 <- seq[t + 1]
      if (s1 %in% states && s2 %in% states)
        trans[s1, s2] <- trans[s1, s2] + 1
    }
  }
  trans
}

normalise_rows <- function(mat) {
  sweep(mat, 1, pmax(rowSums(mat), 1), "/")
}

# =============================================================================
# 1. MONTHLY MARKOV - TRANSITION MATRICES (prereg 6.7 - primary)
# =============================================================================

cat("\n=== MONTHLY MARKOV ===\n")
stamp("Monthly transition matrices starting (baseline then intervention)")

trans_baseline     <- compute_trans_matrix(pm, period_filter = 0)
trans_intervention <- compute_trans_matrix(pm, period_filter = 1)
stamp("Monthly transition matrices done")

prob_baseline     <- normalise_rows(trans_baseline)
prob_intervention <- normalise_rows(trans_intervention)

cat("Baseline transition matrix (counts):\n");        print(trans_baseline)
cat("\nBaseline transition matrix (probabilities):\n");  print(round(prob_baseline, 3))
cat("\nIntervention transition matrix (counts):\n");     print(trans_intervention)
cat("\nIntervention transition matrix (probabilities):\n"); print(round(prob_intervention, 3))

escalation_pairs <- list(
  c("L", "M"), c("L", "H"), c("M", "H")
)

cat("\nEscalation transition comparison (baseline vs intervention):\n")
for (ep in escalation_pairs) {
  s1 <- ep[1]; s2 <- ep[2]
  cat(sprintf("  %s->%s: baseline = %.3f, intervention = %.3f\n",
              s1, s2, prob_baseline[s1, s2], prob_intervention[s1, s2]))
}

# =============================================================================
# 2. HOMOGENEITY TEST ACROSS INTERVENTION MONTHS (prereg 6.7)
# Anderson & Goodman (1957): test homogeneity of departing probabilities
# across months separately for each from-state, then sum chi-sq and df under
# the Markov assumption that rows are independent.
# =============================================================================

cat("\n--- Homogeneity test across intervention months ---\n")
stamp("Homogeneity test starting (per-month transition matrices)")

int_months <- sort(unique(pm$year_month[pm$is_intervention == 1]))
monthly_matrices <- lapply(int_months, function(m_date) {
  pm_month <- pm |>
    dplyr::filter(year_month == m_date | year_month == m_date + months(1)) |>
    dplyr::arrange(participant_id, year_month)
  compute_trans_matrix(pm_month)
})
names(monthly_matrices) <- format(int_months)

cat("Intervention months:", paste(format(int_months), collapse = ", "), "\n\n")

K   <- length(monthly_matrices)
arr <- array(0, dim = c(K, 4, 4),
             dimnames = list(month = format(int_months),
                             from  = c("L","M","H","I"),
                             to    = c("L","M","H","I")))
for (k in seq_len(K)) arr[k, , ] <- monthly_matrices[[k]]

chi_total <- 0
df_total  <- 0
homog_per_state <- list()
for (s in c("L","M","H","I")) {
  ct <- arr[, s, ]
  if (sum(ct) == 0) next
  # Drop empty rows AND empty columns. Empty rows arise when a month has no
  # transitions of a given from-type: the last intervention month necessarily
  # has zero transitions because there is no subsequent month within the
  # period to transition to. Empty columns arise when a to-state is never
  # visited from this from-state.
  ct <- ct[rowSums(ct) > 0, , drop = FALSE]
  ct <- ct[, colSums(ct) > 0, drop = FALSE]
  if (ncol(ct) < 2 || nrow(ct) < 2) next
  cs <- suppressWarnings(stats::chisq.test(ct))
  if (is.na(cs$statistic) || is.nan(cs$statistic)) {
    cat(sprintf("  from=%s: skipped (test statistic NaN — likely sparse cells)\n", s))
    next
  }
  cat(sprintf("  from=%s: chi-sq(%d) = %.2f, p = %.4f\n",
              s, cs$parameter, cs$statistic, cs$p.value))
  homog_per_state[[s]] <- cs
  chi_total <- chi_total + as.numeric(cs$statistic)
  df_total  <- df_total  + as.numeric(cs$parameter)
}
if (df_total > 0) {
  p_homog <- pchisq(chi_total, df = df_total, lower.tail = FALSE)
  cat(sprintf("\n  POOLED: chi-sq(%d) = %.2f, p = %.4f\n",
              df_total, chi_total, p_homog))
} else {
  p_homog <- NA_real_
  cat("\n  POOLED: no from-state had a valid chi-square test (insufficient transitions)\n")
}
cat("  H0 = transition probabilities homogeneous across intervention months\n")

homog_test <- list(
  per_state = homog_per_state,
  chi_total = chi_total,
  df_total  = df_total,
  p_value   = p_homog
)

# =============================================================================
# 2b. LOG-LINEAR PERIOD COMPARISON (prereg 6.7)
# H0 (transitions homogeneous):     ~ from*to + from*period
# H1 (transitions period-specific): ~ from*to*period
# Deviance difference is the test of interest.
# =============================================================================

cat("\n--- Log-linear period comparison (baseline vs intervention) ---\n")
stamp("Homogeneity test done; log-linear period comparison starting")
t_ll <- Sys.time()

if (requireNamespace("MASS", quietly = TRUE)) {
  arr_3d <- array(0, dim = c(4, 4, 2),
                  dimnames = list(from   = c("L","M","H","I"),
                                  to     = c("L","M","H","I"),
                                  period = c("baseline", "intervention")))
  arr_3d[,,1] <- trans_baseline
  arr_3d[,,2] <- trans_intervention
  
  m_homog_ll <- MASS::loglm(~ from*to + from*period, data = arr_3d)
  m_full_ll  <- MASS::loglm(~ from*to*period,         data = arr_3d)
  loglin_anova <- stats::anova(m_homog_ll, m_full_ll, test = "Chisq")
  print(loglin_anova)
  cat("  H0 = transition pattern (from -> to) is homogeneous across periods\n")
} else {
  cat("MASS not installed — log-linear test skipped (install.packages('MASS'))\n")
  loglin_anova <- NULL
}
stamp(sprintf("Log-linear period comparison done (%.1f s)",
              as.numeric(difftime(Sys.time(), t_ll, units = "secs"))))

# =============================================================================
# 3. BAYESIAN DIRICHLET-MULTINOMIAL (prereg 6.7)
# =============================================================================

cat("\n--- Bayesian Dirichlet-Multinomial ---\n")
stamp("Bayesian Dirichlet-Multinomial block starting (four-state)")
cat("Prior: Dirichlet(1,1,1,1) on each row (uniform)\n\n")

states <- c("L", "M", "H", "I")

# states_use defaults to the four-state space; the active-state (L/M/H)
# submatrix block reuses this helper with three states (Dirichlet(1,1,1) rows)
bayes_markov <- function(trans_counts, label, states_use = states) {
  cat(sprintf("=== %s ===\n", label))
  results <- list()

  for (s in states_use) {
    alpha_prior <- rep(1, length(states_use))
    n_obs       <- trans_counts[s, ]
    alpha_post  <- alpha_prior + n_obs
    total       <- sum(alpha_post)

    post_means <- alpha_post / total
    post_ci    <- sapply(seq_along(states_use), function(i) {
      qbeta(c(0.025, 0.975), alpha_post[i], total - alpha_post[i])
    })

    cat(sprintf("  From %s: ", s))
    for (j in seq_along(states_use)) {
      cat(sprintf("%s = %.3f [%.3f, %.3f]  ",
                  states_use[j], post_means[j], post_ci[1, j], post_ci[2, j]))
    }
    cat("\n")

    results[[s]] <- list(
      alpha_post = alpha_post,
      post_means = post_means,
      post_ci    = post_ci
    )
  }
  results
}

bayes_bl  <- bayes_markov(trans_baseline,     "Baseline")
bayes_int <- bayes_markov(trans_intervention, "Intervention")

cat("\n--- Posterior P(escalation rate lower in intervention than baseline) ---\n")

set.seed(GLOBAL_SEED)
n_samp <- 50000

p_escalation <- list()
for (ep in escalation_pairs) {
  s1 <- ep[1]; s2 <- ep[2]
  j  <- which(states == s2)
  
  alpha_bl  <- bayes_bl[[s1]]$alpha_post
  alpha_int <- bayes_int[[s1]]$alpha_post
  
  total_bl  <- sum(alpha_bl)
  total_int <- sum(alpha_int)
  samp_bl   <- rbeta(n_samp, alpha_bl[j],  total_bl  - alpha_bl[j])
  samp_int  <- rbeta(n_samp, alpha_int[j], total_int - alpha_int[j])
  
  p_lower <- mean(samp_int < samp_bl)
  cat(sprintf("  P(pi_int[%s->%s] < pi_bl[%s->%s]) = %.4f\n",
              s1, s2, s1, s2, p_lower))
  p_escalation[[sprintf("%s_to_%s", s1, s2)]] <- p_lower
}

# =============================================================================
# 3b. ACTIVE-STATE (L/M/H) TRANSITION SUBMATRIX (prereg 6.7) - reported
# alongside the four-state model, which remains the primary specification.
# Restricted to transitions where BOTH the origin and the destination month
# are in {L, M, H}; any transition into or out of the Inactive state is
# excluded, so that conclusions about risk dynamics are not driven by flows
# into the Inactive state (prereg 6.7 and the Section 8 adjudication rule).
# =============================================================================

cat("\n--- Active-state (L/M/H) transition submatrix (prereg 6.7) ---\n")
stamp("Four-state Dirichlet block done; active-state submatrix block starting")

active_states <- c("L", "M", "H")

# The L/M/H block of the four-state count matrices is exactly the set of
# transitions with both origin and destination active
trans_baseline_active     <- trans_baseline[active_states, active_states]
trans_intervention_active <- trans_intervention[active_states, active_states]

prob_baseline_active     <- normalise_rows(trans_baseline_active)
prob_intervention_active <- normalise_rows(trans_intervention_active)

cat("Baseline active-state matrix (counts):\n")
print(trans_baseline_active)
cat("\nBaseline active-state matrix (probabilities):\n")
print(round(prob_baseline_active, 3))
cat("\nIntervention active-state matrix (counts):\n")
print(trans_intervention_active)
cat("\nIntervention active-state matrix (probabilities):\n")
print(round(prob_intervention_active, 3))

cat("\nPrior: Dirichlet(1,1,1) on each row (uniform, three states)\n\n")
bayes_bl_active  <- bayes_markov(trans_baseline_active,
                                 "Baseline (active-state submatrix)",
                                 states_use = active_states)
bayes_int_active <- bayes_markov(trans_intervention_active,
                                 "Intervention (active-state submatrix)",
                                 states_use = active_states)

cat("\n--- Active-state posterior P(escalation rate lower in intervention) ---\n")

set.seed(GLOBAL_SEED)
p_escalation_active <- list()
for (ep in escalation_pairs) {
  s1 <- ep[1]; s2 <- ep[2]
  j  <- which(active_states == s2)

  alpha_bl  <- bayes_bl_active[[s1]]$alpha_post
  alpha_int <- bayes_int_active[[s1]]$alpha_post

  total_bl  <- sum(alpha_bl)
  total_int <- sum(alpha_int)
  samp_bl   <- rbeta(n_samp, alpha_bl[j],  total_bl  - alpha_bl[j])
  samp_int  <- rbeta(n_samp, alpha_int[j], total_int - alpha_int[j])

  p_lower <- mean(samp_int < samp_bl)
  cat(sprintf("  P(pi_int[%s->%s] < pi_bl[%s->%s]) = %.4f (active-state submatrix)\n",
              s1, s2, s1, s2, p_lower))
  p_escalation_active[[sprintf("%s_to_%s", s1, s2)]] <- p_lower
}

# =============================================================================
# 4. MSM - CONTINUOUS-TIME MARKOV WITH ENGAGEMENT COVARIATES (prereg 6.6 / 6.7)
# =============================================================================

cat("\n--- msm: Markov model with engagement covariates ---\n")
stamp("Active-state submatrix block done; msm block starting")
cat("Covariates entered as time-varying at player-month grain:\n")
cat("  msg_response   : msg_response_rate (proportion, [0, 1])\n")
cat("  log_n_intents  : log1p(n_limit_intents)\n")
cat("  log_limit_dwell: log1p(limit_page_dwell)\n")
cat("  log_rg_dwell   : log1p(rg_support_dwell)\n")
cat("Results interpreted as associative; direction-conditional analysis\n")
cat("is out of scope for this paper.\n\n")

msm_model <- NULL
if (requireNamespace("msm", quietly = TRUE)) {
  library(msm)
  
  state_map <- c("L" = 1, "M" = 2, "H" = 3, "I" = 4)
  
  pm_msm <- pm |>
    dplyr::filter(is_intervention == 1) |>
    dplyr::arrange(participant_id, year_month) |>
    dplyr::mutate(
      state_num       = state_map[markov_state],
      time_num        = lubridate::interval(min(year_month), year_month) / months(1),
      msg_response    = dplyr::coalesce(msg_response_rate, 0),
      log_n_intents   = log1p(dplyr::coalesce(n_limit_intents,  0)),
      log_limit_dwell = log1p(dplyr::coalesce(limit_page_dwell, 0)),
      log_rg_dwell    = log1p(dplyr::coalesce(rg_support_dwell, 0)),
      # Recalibration step indicator: 1 from December 2024 onwards
      # (BetBuddy recalibration, BETBUDDY_RECALIB_DATE in 00_config.R);
      # used only by the recalibration-adjusted specification below
      recalib_t       = as.integer(year_month >= BETBUDDY_RECALIB_DATE)
    ) |>
    dplyr::filter(!is.na(state_num))
  
  # Synthetic dev: subset to a smaller cohort for tractable MLE. msm with
  # 60 free parameters (12 transitions × (1 baseline + 4 covariates)) on
  # 5,808 players via BFGS is slow; the synthetic estimates are not
  # interpretable anyway because synthetic engagement covariates are drawn
  # from period-level fits and do not recover association structure.
  # Real-data runs (USE_SYNTHETIC = FALSE) use the full cohort.
  if (USE_SYNTHETIC) {
    SYNTH_MSM_SUBSET_N <- 500L
    set.seed(GLOBAL_SEED)
    sub_ids <- sample(unique(pm_msm$participant_id), SYNTH_MSM_SUBSET_N)
    pm_msm  <- pm_msm |> dplyr::filter(participant_id %in% sub_ids)
    cat(sprintf("Synthetic dev mode: msm fit on subset of %d players (%d rows)\n",
                SYNTH_MSM_SUBSET_N, nrow(pm_msm)))
  }
  
  # Data-derived starting values via crudeinits.msm. q_skeleton encodes
  # the allowed transitions (off-diagonal = 1 means transition permitted).
  q_skeleton <- rbind(
    c(0, 1, 1, 1),
    c(1, 0, 1, 1),
    c(1, 1, 0, 1),
    c(1, 1, 1, 0)
  )
  Q_init <- crudeinits.msm(state_num ~ time_num,
                           subject = participant_id,
                           qmatrix = q_skeleton,
                           data    = pm_msm)
  
  # Resume logic: cache per data path (synthetic vs real); load, do not
  # refit, when the cache exists and OVERWRITE = FALSE
  msm_data_tag  <- if (USE_SYNTHETIC) "synthetic" else "real"
  msm_cache     <- sprintf("results/08_markov/msm_model_%s.rds", msm_data_tag)

  if (file.exists(msm_cache) && !OVERWRITE) {
    message(sprintf(
      "SKIPPING msm primary refit: cache %s exists and OVERWRITE = FALSE (loading saved fit)",
      msm_cache))
    msm_model <- readRDS(msm_cache)
  } else {
    if (file.exists(msm_cache) && OVERWRITE) {
      message(sprintf("OVERWRITE = TRUE: removing cache %s and refitting", msm_cache))
      file.remove(msm_cache)
    }
    t_msm <- Sys.time()
    stamp(sprintf("msm primary fit starting (%d rows; BFGS; can run for many minutes without console output)",
                  nrow(pm_msm)))
    msm_model <- tryCatch(
      msm(state_num ~ time_num, subject = participant_id, data = pm_msm,
          qmatrix    = Q_init,
          covariates = ~ msg_response + log_n_intents + log_limit_dwell + log_rg_dwell,
          opt.method = "bobyqa"),
      error = function(e) {
        cat("  msm error:", conditionMessage(e), "\n")
        NULL
      }
    )
    stamp(sprintf("msm primary fit done (%.1f s)",
                  as.numeric(difftime(Sys.time(), t_msm, units = "secs"))))
    # Checkpoint immediately so a later crash never forces this refit
    if (!is.null(msm_model)) {
      saveRDS(msm_model, msm_cache)
      stamp(sprintf("msm primary fit checkpointed to %s", msm_cache))
    }
  }
  invisible(gc(verbose = FALSE))

  if (!is.null(msm_model)) {
    cat("msm model converged. Hazard ratios per covariate:\n")
    print(hazard.msm(msm_model))
  }

  # Recalibration-adjusted msm (pre-specified; prereg 6.7, Section 9) - sensitivity; adds the
  # recalib_t step indicator (1 December 2024 BetBuddy recalibration) as a
  # time-varying covariate. Reported alongside the primary msm specification
  # above, which is unchanged.
  cat("\n--- msm: recalibration-adjusted specification (sensitivity) ---\n")
  msm_recal_cache <- sprintf("results/08_markov/msm_model_recal_%s.rds", msm_data_tag)

  if (file.exists(msm_recal_cache) && !OVERWRITE) {
    message(sprintf(
      "SKIPPING msm recalibration-adjusted refit: cache %s exists and OVERWRITE = FALSE (loading saved fit)",
      msm_recal_cache))
    msm_model_recal <- readRDS(msm_recal_cache)
  } else {
    if (file.exists(msm_recal_cache) && OVERWRITE) {
      message(sprintf("OVERWRITE = TRUE: removing cache %s and refitting", msm_recal_cache))
      file.remove(msm_recal_cache)
    }
    t_msm2 <- Sys.time()
    stamp("msm recalibration-adjusted fit starting (BFGS; can run for many minutes without console output)")
    msm_model_recal <- tryCatch(
      msm(state_num ~ time_num, subject = participant_id, data = pm_msm,
          qmatrix    = Q_init,
          covariates = ~ msg_response + log_n_intents + log_limit_dwell +
                         log_rg_dwell + recalib_t,
          opt.method = "bobyqa"),
      error = function(e) {
        cat("  msm (recalibration-adjusted) error:", conditionMessage(e), "\n")
        NULL
      }
    )
    stamp(sprintf("msm recalibration-adjusted fit done (%.1f s)",
                  as.numeric(difftime(Sys.time(), t_msm2, units = "secs"))))
    # Checkpoint immediately so a later crash never forces this refit
    if (!is.null(msm_model_recal)) {
      saveRDS(msm_model_recal, msm_recal_cache)
      stamp(sprintf("msm recalibration-adjusted fit checkpointed to %s", msm_recal_cache))
    }
  }
  invisible(gc(verbose = FALSE))

  if (!is.null(msm_model_recal)) {
    cat("Recalibration-adjusted msm converged. Hazard ratios per covariate:\n")
    print(hazard.msm(msm_model_recal))
  }
} else {
  msm_model_recal <- NULL
  cat("msm not installed — install.packages('msm')\n")
}

# =============================================================================
# 5. WEEKLY MARKOV (prereg 6.7 - secondary analysis)
# Real-data only: requires daily tbl_daily_risk and daily activity
# from poc_raw. Not exercised in the synthetic dev pass.
# =============================================================================

cat("\n\n=== WEEKLY MARKOV ===\n")
stamp("msm block done; weekly Markov block starting")

trans_weekly_bl     <- NULL
trans_weekly_int    <- NULL
bayes_weekly_bl     <- NULL
bayes_weekly_int    <- NULL

if (!USE_SYNTHETIC) {
  poc_raw    <- readRDS(FILE_POC_RAW)
  risk_daily <- poc_raw$model_results
  
  state_map_weekly <- c("low" = "L", "medium" = "M", "high" = "H")
  
  # Pre-reg tie-breaker hierarchy (prereg 6.7):
  #   Tier 1: dominant percentage frequency
  #   Tier 2: most recent 3 days within the week (restricted to tied winners)
  #   Tier 3: final day's valid assessment
  #   Tier 4: NA only if no valid assessment exists at all
  pct_freq_risk_temporal <- function(x, dates = NULL) {
    valid   <- c("low", "medium", "high")
    x_clean <- x[!is.na(x) & x %in% valid]
    if (length(x_clean) == 0) return(NA_character_)
    
    tbl     <- table(x_clean) / length(x_clean)
    max_pct <- max(tbl)
    winners <- names(tbl)[tbl == max_pct]
    if (length(winners) == 1) return(winners)
    
    if (!is.null(dates) && length(dates) == length(x)) {
      
      # Tier 2
      recent_idx <- order(dates, decreasing = TRUE)[seq_len(min(3, length(dates)))]
      x_recent   <- x[recent_idx]
      x_recent   <- x_recent[!is.na(x_recent) & x_recent %in% valid]
      if (length(x_recent) > 0) {
        tbl2 <- table(x_recent) / length(x_recent)
        tbl2 <- tbl2[names(tbl2) %in% winners]
        if (length(tbl2) > 0) {
          winners2 <- names(tbl2)[tbl2 == max(tbl2)]
          if (length(winners2) == 1) return(winners2)
        }
      }
      
      # Tier 3
      last_valid <- x[order(dates, decreasing = TRUE)]
      last_valid <- last_valid[!is.na(last_valid) & last_valid %in% valid]
      if (length(last_valid) > 0) return(last_valid[1])
    }
    
    return(NA_character_)
  }
  
  risk_weekly <- risk_daily |>
    dplyr::filter(participant_id %in% unique(pm$participant_id)) |>
    dplyr::mutate(
      week_start = floor_date(evaluation_date, "week", week_start = 1),
      period     = dplyr::case_when(
        evaluation_date >= BASELINE_START     & evaluation_date <= BASELINE_END     ~ "baseline",
        evaluation_date >= INTERVENTION_START & evaluation_date <= INTERVENTION_END ~ "intervention",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(period)) |>
    dplyr::group_by(participant_id, week_start, period) |>
    dplyr::summarise(
      weekly_risk     = pct_freq_risk_temporal(overall_risk_level, evaluation_date),
      n_days_assessed = sum(!is.na(overall_risk_level)),
      .groups = "drop"
    )
  
  # Inactive state defined by activity (zero AGD), mirroring the monthly
  # definition. is_agd is the daily 0/1 indicator from 00_poc_pull.R; the
  # sum across days in a week gives the weekly active-day count.
  activity_daily <- poc_raw$daily_stats
  stopifnot(all(c("participant_id", "evaluation_date", "is_agd") %in% names(activity_daily)))
  
  activity_weekly <- activity_daily |>
    dplyr::filter(participant_id %in% unique(pm$participant_id)) |>
    dplyr::mutate(week_start = floor_date(evaluation_date, "week", week_start = 1)) |>
    dplyr::group_by(participant_id, week_start) |>
    dplyr::summarise(weekly_agd = sum(is_agd, na.rm = TRUE), .groups = "drop")
  
  risk_weekly <- risk_weekly |>
    dplyr::left_join(activity_weekly, by = c("participant_id", "week_start")) |>
    dplyr::mutate(
      weekly_agd          = dplyr::coalesce(weekly_agd, 0L),
      weekly_markov_state = dplyr::case_when(
        weekly_agd == 0    ~ "I",                          # Inactive: zero AGD
        is.na(weekly_risk) ~ NA_character_,                # Active but no risk: drop
        TRUE               ~ state_map_weekly[weekly_risk] # L / M / H
      ),
      is_intervention = as.integer(period == "intervention")
    )

  # Memory: the daily-grain raw inputs are no longer needed once the weekly
  # frame is built (poc_raw alone is several GB on the real data)
  rm(poc_raw, risk_daily, activity_daily, activity_weekly)
  invisible(gc(verbose = FALSE))
  
  compute_weekly_trans <- function(rw_data, period_filter) {
    states <- c("L", "M", "H", "I")
    trans  <- matrix(0, 4, 4, dimnames = list(states, states))
    
    rw_f <- rw_data |>
      dplyr::filter(is_intervention == period_filter) |>
      dplyr::arrange(participant_id, week_start)
    
    for (pid in unique(rw_f$participant_id)) {
      seq <- rw_f$weekly_markov_state[rw_f$participant_id == pid]
      seq <- seq[!is.na(seq)]
      if (length(seq) < 2) next
      for (t in seq_len(length(seq) - 1)) {
        s1 <- seq[t]; s2 <- seq[t + 1]
        if (s1 %in% states && s2 %in% states)
          trans[s1, s2] <- trans[s1, s2] + 1
      }
    }
    trans
  }
  
  trans_weekly_bl  <- compute_weekly_trans(risk_weekly, 0)
  trans_weekly_int <- compute_weekly_trans(risk_weekly, 1)
  
  cat("Weekly baseline transition matrix (probabilities):\n")
  print(round(normalise_rows(trans_weekly_bl), 3))
  
  cat("\nWeekly intervention transition matrix (probabilities):\n")
  print(round(normalise_rows(trans_weekly_int), 3))
  
  cat("\n--- Weekly Bayesian Dirichlet-Multinomial ---\n")
  bayes_weekly_bl  <- bayes_markov(trans_weekly_bl,  "Weekly baseline")
  bayes_weekly_int <- bayes_markov(trans_weekly_int, "Weekly intervention")
  
} else {
  cat("Weekly Markov runs on real data only (requires daily risk scores).\n")
  cat("Will run automatically when USE_SYNTHETIC = FALSE.\n")
}

# =============================================================================
# SAVE
# =============================================================================

saveRDS(list(
  trans_baseline      = trans_baseline,
  trans_intervention  = trans_intervention,
  prob_baseline       = prob_baseline,
  prob_intervention   = prob_intervention,
  n_na_state          = n_na_state,
  monthly_matrices    = monthly_matrices,
  homog_test          = homog_test,
  loglin_anova        = loglin_anova,
  bayes_bl            = bayes_bl,
  bayes_int           = bayes_int,
  p_escalation        = p_escalation,
  # Active-state (L/M/H) submatrix (prereg 6.7); four-state model is primary
  trans_baseline_active     = trans_baseline_active,
  trans_intervention_active = trans_intervention_active,
  prob_baseline_active      = prob_baseline_active,
  prob_intervention_active  = prob_intervention_active,
  bayes_bl_active           = bayes_bl_active,
  bayes_int_active          = bayes_int_active,
  p_escalation_active       = p_escalation_active,
  msm_model           = msm_model,
  msm_model_recal     = if (exists("msm_model_recal")) msm_model_recal else NULL,
  trans_weekly_bl     = trans_weekly_bl,
  trans_weekly_int    = trans_weekly_int,
  bayes_weekly_bl     = bayes_weekly_bl,
  bayes_weekly_int    = bayes_weekly_int
), "results/08_markov/markov_results.rds")
stamp("Results saved to results/08_markov/markov_results.rds")

cat("\n=== 08_poc_markov.R complete ===\n")
message("=== 08 COMPLETE ===")