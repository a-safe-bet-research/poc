# =============================================================================
# 07_poc_its.R
# Purpose : Interrupted time series — H6-H7
# Project : A Safe Bet (ASB) — Proof-of-Concept evaluation.
#           Putting Design into Practice: A Safe Bet — A Proof-of-Concept
#           Evaluation of a Tailored Online Responsible Gambling Framework
# Inputs  : poc_synthetic.rds or poc_analytical_LOCKED.rds (via 00_config.R)
# Outputs : results/07_its/its_results.rds (+ cached brms fits)
# Flags   : USE_SYNTHETIC — TRUE runs on poc_synthetic.rds; FALSE runs on
#           poc_analytical_LOCKED.rds. Currently FALSE.
#           USE_BRMS — TRUE fits the pre-registered brms HMC analysis; FALSE
#           uses the asymptotic Gaussian development fallback. Currently TRUE.
#           OVERWRITE — FALSE loads cached fits (crash-resumable); TRUE
#           deletes caches and refits. Currently FALSE.
#           (Full flag documentation at the flag definitions below.)
# =============================================================================
# PRE-REG:  OSF [URL to be added] - implements prereg Section 6.4
#           (script table, prereg Section 10.1)
# =============================================================================
#
# HYPOTHESES (prereg 6.4):
#   H6: Significant immediate step-down in monthly total deposits (the single
#       confirmatory ITS outcome) at October 2024 (level change, beta_2)
#   H7: Significant change in the monthly trend of total deposits following
#       October 2024 (slope change, beta_3)
#   Monthly total losses, total wagers, and total play hours are modelled
#   identically as SECONDARY ITS outcomes and carry no confirmatory weight.
#   Outcome transforms per prereg 6.4 (v28): deposits, wagers, and play hours
#   are log1p-transformed; net loss uses asinh, which is defined for negative
#   values (net-winner participant-months) and approximates log for large
#   positive values. Applied identically in the nlme and brms fits.
#
# MODEL:
#   outcome_it = beta_0 + beta_1*time_t + beta_2*intervention_t
#                + beta_3*time_post_t + u_i + epsilon_it
#
#   time_t         = month 1-12
#   intervention_t = 0 (baseline) / 1 (intervention)
#   time_post_t    = 0 pre, 1-6 post
#   u_i            = random intercept by participant_id
#
# IMPLEMENTATION: nlme::lme()
#   nlme is used in place of lme4 (lme4 requires nloptr/cmake which is
#   unavailable on this server; prereg 10.4 item 5). nlme::lme() fits the
#   same mixed model and additionally supports corAR1().
#
# AR(1) SENSITIVITY (prereg 6.4 and Section 9):
#   The AR(1) within-player correlation model (nlme::corAR1) is fitted and
#   reported alongside the primary model in ALL cases, not contingent on the
#   Durbin-Watson value. The DW statistic is computed and reported
#   descriptively only.
#
# RECALIBRATION INDICATOR (pre-specified; prereg 6.4 and Section 9):
#   The 1 December 2024 BetBuddy recalibration (BETBUDDY_RECALIB_DATE) is
#   modelled via a recalibration-adjusted specification that adds a step
#   indicator (recalib_t) to the segmented regression; reported alongside
#   the primary model, which is unchanged.
#
# INFERENTIAL GOAL (per prereg 6.4):
#   Estimate posterior distributions of beta_2 (level change) and
#   beta_3 (slope change) via brms. Report:
#     - Posterior mean and 95% credible interval
#     - P(beta_2 < 0) and P(beta_3 < 0)
#   A posterior probability > 0.95 = strong evidence for directional H.
#
# PRIORS (prereg 6.4):
#   All beta:  Normal(0, 10)
#   sigma:     Student-t(3, 0, 10)
#
# SAMPLER (prereg 6.4):
#   4 chains; 2,000 warmup; 4,000 sampling; iter = warmup + sampling = 6,000.
#   Convergence assessed via R-hat < 1.01.
# =============================================================================

library(dplyr)
library(tidyr)
library(lubridate)
library(nlme)

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

# USE_BRMS = TRUE invokes the pre-registered brms HMC analysis (prereg 6.4:
# Normal(0, 10) on β, Student-t(3, 0, 10) on σ, 4 chains × 6,000 iter,
# R-hat < 1.01). This is the registered primary Bayesian ITS analysis.
#
# USE_BRMS = FALSE uses an asymptotic Gaussian fallback — sampling from
# MVN(β̂, vcov(β̂)) of the nlme::lme MLE. Kept for fast development iteration
# on synthetic data and as a robustness check; with Normal(0, 10) priors and
# N ≈ 70k player-months the two methods agree to high precision under
# Bernstein–von Mises. brms HMC is the pre-registered primary; flip to FALSE
# only for development.
#
# Note: rstan's pre-flight compile-test on Linux emits a harmless
# "fatal error: cmath: No such file or directory" message before the actual
# Stan compile succeeds. Cosmetic; ignore.
USE_BRMS <- TRUE

# OVERWRITE = FALSE makes every Bayesian fit crash-resumable: if a model's
# cache .rds already exists it is loaded rather than refitted, so a crash
# partway through the run never forces refitting completed models. Set TRUE
# to delete the caches and refit everything from scratch. Memory handling:
# each fit is released (rm + gc) after its summaries are extracted, so the
# script never holds two brms fits in memory simultaneously; the full fitted
# objects remain available in their per-model cache files.
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

dir.create("results/07_its", recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# PREPARE MONTHLY DATA
# =============================================================================

pm_its <- pm |>
  dplyr::mutate(
    time_t         = month_number,
    intervention_t = as.integer(is_intervention),
    time_post_t    = dplyr::coalesce(months_post, 0L),
    # Recalibration step indicator: 1 from December 2024 onwards
    # (BetBuddy recalibration, BETBUDDY_RECALIB_DATE in 00_config.R)
    recalib_t      = as.integer(year_month >= BETBUDDY_RECALIB_DATE),
    # Outcome transforms per prereg 6.4 (v28): log1p for deposits, wagers,
    # play hours (right-skewed; +1 to handle zeros); asinh for net loss,
    # defined for negative values (net winners, for which log1p is NaN) and
    # approximating log for large positive values
    log1p_deposits   = log1p(total_deposits),
    asinh_losses     = asinh(total_losses),
    log1p_wagers     = log1p(total_wagers),
    log1p_play_hours = log1p(play_hours_total),
    # participant_id as factor for nlme grouping
    player_fac = factor(participant_id)
  )

# Guard: no transform may silently produce NaN (net winners under log1p were
# dropped silently by na.action; asinh removes the cause, this makes any
# recurrence fail loudly)
stopifnot(
  "NaN in transformed ITS outcomes - check transform domains" =
    !any(is.nan(pm_its$log1p_deposits)) && !any(is.nan(pm_its$asinh_losses)) &&
    !any(is.nan(pm_its$log1p_wagers))   && !any(is.nan(pm_its$log1p_play_hours))
)

cat("Month-level observations:", nrow(pm_its), "\n")
cat("Players:", dplyr::n_distinct(pm_its$participant_id), "\n")
cat("Months:", dplyr::n_distinct(pm_its$year_month), "\n")
stamp(sprintf("07_poc_its.R started; monthly data prepared (%d rows)", nrow(pm_its)))

# Prereg 6.4: monthly total deposits is the single confirmatory ITS outcome;
# losses, wagers, and play hours are modelled identically as secondary ITS
# outcomes with no confirmatory weight.
its_outcomes <- list(
  list(var = "log1p_deposits",   label = "log(1+deposit_amount)", raw = "total_deposits",
       analysis_class = "confirmatory"),
  list(var = "asinh_losses",     label = "asinh(net_loss)",       raw = "total_losses",
       analysis_class = "secondary"),
  list(var = "log1p_wagers",     label = "log(1+wagers)",         raw = "total_wagers",
       analysis_class = "secondary"),
  list(var = "log1p_play_hours", label = "log(1+play_hours)",     raw = "play_hours_total",
       analysis_class = "secondary")
)
# Note: the losses brms cache tag follows var (brms_asinh_losses[_recal].rds),
# so the asinh fits cannot collide with any stale brms_log1p_losses* cache.

# =============================================================================
# H6-H7 frequentist ITS (nlme), prereg 6.4 - confirmatory outcome: deposits;
# losses, wagers, play hours secondary. AR(1) sensitivity fitted in all cases.
# =============================================================================

# rhs defaults to the pre-registered segmented regression; the
# recalibration-adjusted specification passes an extended rhs.
run_its_nlme <- function(outcome_var, label, pm_data,
                         analysis_class = NA_character_,
                         rhs = "time_t + intervention_t + time_post_t") {
  cat(sprintf("\n--- ITS (nlme): %s [%s] ---\n", label, analysis_class))

  fml <- as.formula(sprintf("%s ~ %s", outcome_var, rhs))

  t0 <- Sys.time()
  stamp(sprintf("nlme primary fit starting: %s [%s]", outcome_var, analysis_class))
  m <- tryCatch(
    nlme::lme(fml,
              random   = ~ 1 | player_fac,
              data     = pm_data,
              method   = "ML",
              na.action = na.omit,
              control  = lmeControl(opt = "optim", maxIter = 200,
                                    msMaxIter = 200, tolerance = 1e-4)),
    error = function(e) {
      cat("  Model error:", conditionMessage(e), "\n")
      NULL
    }
  )
  stamp(sprintf("nlme primary fit done: %s (%.1f s)", outcome_var,
                as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  if (is.null(m)) return(NULL)

  cf  <- nlme::fixed.effects(m)
  se  <- sqrt(diag(vcov(m)))
  df_resid <- m$fixDF$terms["time_t"]
  
  # Two-sided Wald t-tests for beta_2 and beta_3 (prereg Section 8: H6 and H7
  # are two-tailed in the frequentist track; the Bayesian decision rule
  # P(beta < 0) > 0.95 in prereg 6.4 is reported alongside as directional
  # evidence).
  t_b2 <- cf["intervention_t"] / se["intervention_t"]
  t_b3 <- cf["time_post_t"]    / se["time_post_t"]
  p_b2 <- 2 * pt(abs(t_b2), df = df_resid, lower.tail = FALSE)
  p_b3 <- 2 * pt(abs(t_b3), df = df_resid, lower.tail = FALSE)
  
  cat(sprintf("  beta_0 (intercept):        %.4f (SE=%.4f)\n",
              cf["(Intercept)"], se["(Intercept)"]))
  cat(sprintf("  beta_1 (pre trend):        %.4f (SE=%.4f)\n",
              cf["time_t"], se["time_t"]))
  cat(sprintf("  beta_2 (level change, H6): %.4f (SE=%.4f, p=%.4f)\n",
              cf["intervention_t"], se["intervention_t"], p_b2))
  cat(sprintf("  beta_3 (slope change, H7): %.4f (SE=%.4f, p=%.4f)\n",
              cf["time_post_t"], se["time_post_t"], p_b3))
  
  # Durbin-Watson on normalised residuals, reported descriptively only
  # (prereg 6.4: a value within [1.5, 2.5] is not interpreted as evidence of
  # absent autocorrelation; the statistic has low power on 12 monthly points)
  resids  <- residuals(m, type = "normalized")
  dw_stat <- sum(diff(resids)^2) / sum(resids^2)
  cat(sprintf("  Durbin-Watson (descriptive): %.3f\n", dw_stat))

  # AR(1) sensitivity model fitted and reported alongside the primary model
  # in ALL cases (prereg 6.4 and Section 9); not contingent on the DW value
  cat("  Fitting AR(1) sensitivity model (fitted in all cases)\n")
  t1 <- Sys.time()
  stamp(sprintf("nlme AR(1) fit starting: %s [%s]", outcome_var, analysis_class))
  cf_ar1 <- NULL
  se_ar1 <- NULL
  m_ar1 <- tryCatch(
    nlme::lme(fml,
              random      = ~ 1 | player_fac,
              correlation = nlme::corAR1(form = ~ time_t | player_fac),
              data        = pm_data,
              method      = "ML",
              na.action   = na.omit,
              control     = lmeControl(opt = "optim", maxIter = 200,
                                       tolerance = 1e-4)),
    error = function(e) {
      cat("  AR(1) model error:", conditionMessage(e), "\n")
      NULL
    }
  )
  stamp(sprintf("nlme AR(1) fit done: %s (%.1f s)", outcome_var,
                as.numeric(difftime(Sys.time(), t1, units = "secs"))))
  if (!is.null(m_ar1)) {
    cf_ar1 <- nlme::fixed.effects(m_ar1)
    se_ar1 <- sqrt(diag(vcov(m_ar1)))
    cat(sprintf("  AR(1) beta_2: %.4f (SE=%.4f)\n",
                cf_ar1["intervention_t"], se_ar1["intervention_t"]))
    cat(sprintf("  AR(1) beta_3: %.4f (SE=%.4f)\n",
                cf_ar1["time_post_t"], se_ar1["time_post_t"]))
  }

  list(
    label          = label,
    analysis_class = analysis_class,
    model          = m,
    model_ar1      = m_ar1,
    coefs          = cf,
    se             = se,
    p_b2           = p_b2,
    p_b3           = p_b3,
    coefs_ar1      = cf_ar1,
    se_ar1         = se_ar1,
    dw             = dw_stat
  )
}

cat("\n=== FREQUENTIST ITS (nlme) ===\n")
stamp("Frequentist ITS block starting (4 outcomes, primary + AR(1) each)")
freq_its_results <- lapply(its_outcomes, function(o) {
  run_its_nlme(o$var, o$label, pm_its, analysis_class = o$analysis_class)
})
names(freq_its_results) <- sapply(its_outcomes, `[[`, "var")
stamp("Frequentist ITS block done")

# -----------------------------------------------------------------------------
# Recalibration-adjusted ITS (pre-specified; prereg 6.4, Section 9) - sensitivity
# Adds the recalib_t step indicator (1 December 2024 BetBuddy recalibration)
# to the segmented regression; reported alongside the primary model, which
# remains unchanged above.
# -----------------------------------------------------------------------------

cat("\n=== FREQUENTIST ITS (nlme) - RECALIBRATION-ADJUSTED (sensitivity) ===\n")
stamp("Recalibration-adjusted frequentist ITS block starting")
freq_its_recal <- lapply(its_outcomes, function(o) {
  run_its_nlme(o$var, paste0(o$label, " + recalib_t"), pm_its,
               analysis_class = paste0(o$analysis_class, "_recal_adjusted"),
               rhs = "time_t + intervention_t + time_post_t + recalib_t")
})
names(freq_its_recal) <- sapply(its_outcomes, `[[`, "var")
stamp("Recalibration-adjusted frequentist ITS block done")

# =============================================================================
# H6-H7 Bayesian ITS (brms), prereg 6.4 - confirmatory outcome: deposits;
# losses, wagers, play hours secondary
# =============================================================================

cat("\n\n=== BAYESIAN ITS (brms) ===\n")
cat("Priors: Normal(0, 10) on all beta; Student-t(3, 0, 10) on sigma\n")
cat("Sampler: 4 chains, 2000 warmup, 4000 sampling (iter = 6000)\n")
cat("INFERENTIAL GOAL: P(beta_2 < 0) and P(beta_3 < 0)\n\n")

bayes_its_results <- NULL
bayes_its_recal   <- NULL

if (USE_BRMS && requireNamespace("brms", quietly = TRUE)) {
  library(brms)
  brms_safe_start()  # detaches conflicted — see 00_config.R
  
  # Priors per prereg 6.4. Random-intercept SD prior left at brms
  # default (Student-t with df=3, scale derived from outcome).
  its_priors <- c(
    prior(normal(0, 10), class = "b"),
    prior(normal(0, 10), class = "Intercept"),
    prior(student_t(3, 0, 10), class = "sigma")
  )
  
  # rhs defaults to the pre-registered segmented regression; the
  # recalibration-adjusted specification passes an extended rhs. file_tag
  # keeps the brms cache files distinct per specification.
  run_its_bayes <- function(outcome_var, label, pm_data,
                            analysis_class = NA_character_,
                            rhs = "time_t + intervention_t + time_post_t",
                            file_tag = outcome_var) {
    cat(sprintf("--- Bayesian ITS: %s [%s] ---\n", label, analysis_class))

    fml <- as.formula(sprintf(
      "%s ~ %s + (1 | player_fac)",
      outcome_var, rhs
    ))

    # Resume logic: if the cache exists and OVERWRITE is FALSE, brm() below
    # loads the saved fit instead of refitting (file_refit = "on_change"
    # still refits if formula, data, or priors changed, e.g. the synthetic
    # to real switch, which is the correct behaviour). OVERWRITE = TRUE
    # deletes the cache to force a fresh fit.
    cache_path <- sprintf("results/07_its/brms_%s.rds", file_tag)
    if (file.exists(cache_path) && !OVERWRITE) {
      message(sprintf(
        "SKIPPING refit: %s [%s] - cache %s exists and OVERWRITE = FALSE (saved fit loaded; refit occurs only if formula, data, or priors changed)",
        outcome_var, analysis_class, cache_path))
    } else if (file.exists(cache_path) && OVERWRITE) {
      message(sprintf("OVERWRITE = TRUE: removing cache %s and refitting",
                      cache_path))
      file.remove(cache_path)
    }

    t0 <- Sys.time()
    stamp(sprintf(
      "brms fit starting: %s [%s]; cache file results/07_its/brms_%s.rds (cached fits return in seconds; fresh HMC sampling can run for a long time without console output)",
      outcome_var, analysis_class, file_tag))

    # file_refit = "on_change" forces a refit when formula, data, or priors
    # change — including the synthetic -> real data switch. Sampler settings
    # are not tracked by file_refit, but they are hard-coded here so this
    # is moot for deposit; the cache only short-circuits genuine reruns
    # of the same fit.
    m_bayes <- tryCatch(
      brm(formula     = fml,
          data        = pm_data,
          prior       = its_priors,
          chains      = 4,
          warmup      = 2000,
          iter        = 6000,
          cores       = 4,
          seed        = GLOBAL_SEED,
          control     = list(adapt_delta = 0.99),
          silent      = 2,
          file        = sprintf("results/07_its/brms_%s", file_tag),
          file_refit  = "on_change"),
      error = function(e) {
        cat("  brms error:", conditionMessage(e), "\n"); NULL
      }
    )
    stamp(sprintf("brms fit done: %s [%s] (%.1f s)", outcome_var, analysis_class,
                  as.numeric(difftime(Sys.time(), t0, units = "secs"))))
    if (is.null(m_bayes)) return(NULL)

    max_rhat <- max(rhat(m_bayes), na.rm = TRUE)
    cat(sprintf("  Max R-hat: %.4f %s\n", max_rhat,
                if (max_rhat < 1.01) "[CONVERGED]" else "[WARNING]"))

    # Memory: extract ONLY the two fixed-effect draw vectors. An unrestricted
    # as_draws_df() materialises every random-intercept column (~5,800 of
    # them x 16,000 draws, several hundred MB per model) and was a principal
    # cause of the RAM accumulation.
    post <- as_draws_df(m_bayes,
                        variable = c("b_intervention_t", "b_time_post_t"))
    b2   <- post$b_intervention_t
    b3   <- post$b_time_post_t

    cat(sprintf("  beta_2: mean=%.4f, 95%%CrI=[%.4f,%.4f], P(b2<0)=%.4f\n",
                mean(b2), quantile(b2,.025), quantile(b2,.975), mean(b2<0)))
    cat(sprintf("  beta_3: mean=%.4f, 95%%CrI=[%.4f,%.4f], P(b3<0)=%.4f\n",
                mean(b3), quantile(b3,.025), quantile(b3,.975), mean(b3<0)))

    # Memory: release the fitted model before the next fit begins. The full
    # fit is already on disk at cache_path (saved by brm's file argument);
    # only the extracted draws and summaries are kept, so the script never
    # holds two brms fits simultaneously. model_file replaces the previous
    # in-memory model element in the returned list; reload any fit on demand
    # with readRDS(model_file).
    rm(post, m_bayes)
    invisible(gc(verbose = FALSE))
    stamp(sprintf("released %s [%s] from memory (fit retained at %s)",
                  outcome_var, analysis_class, cache_path))

    list(model_file = cache_path, b2 = b2, b3 = b3, max_rhat = max_rhat,
         analysis_class = analysis_class, method = "brms")
  }

  stamp("Bayesian ITS block starting (4 primary brms fits)")
  bayes_its_results <- lapply(its_outcomes, function(o) {
    run_its_bayes(o$var, o$label, pm_its, analysis_class = o$analysis_class)
  })
  names(bayes_its_results) <- sapply(its_outcomes, `[[`, "var")
  stamp("Bayesian ITS primary block done")

  # Recalibration-adjusted Bayesian ITS (pre-specified; prereg 6.4, Section 9) - sensitivity;
  # reported alongside the primary fits above, which are unchanged.
  cat("\n--- Bayesian ITS - RECALIBRATION-ADJUSTED (sensitivity) ---\n")
  stamp("Bayesian ITS recalibration-adjusted block starting (4 brms fits)")
  bayes_its_recal <- lapply(its_outcomes, function(o) {
    run_its_bayes(o$var, paste0(o$label, " + recalib_t"), pm_its,
                  analysis_class = paste0(o$analysis_class, "_recal_adjusted"),
                  rhs      = "time_t + intervention_t + time_post_t + recalib_t",
                  file_tag = paste0(o$var, "_recal"))
  })
  names(bayes_its_recal) <- sapply(its_outcomes, `[[`, "var")
  stamp("Bayesian ITS recalibration-adjusted block done")

  brms_safe_end()  # restores conflicted — see 00_config.R
  
} else {
  if (!USE_BRMS) {
    cat("\n--- Asymptotic Bayesian ITS (Gaussian approximation of MLE) ---\n")
    cat("USE_BRMS = FALSE: development fallback. Sampling from\n")
    cat("MVN(beta_hat, vcov(beta_hat)) of the nlme::lme fit. With Normal(0,10)\n")
    cat("priors and N =", nrow(pm_its), "player-months, this agrees with brms\n")
    cat("HMC to high precision. The brms HMC fit (USE_BRMS = TRUE) is the\n")
    cat("registered primary analysis per prereg 6.4.\n\n")
  } else {
    cat("brms not installed — asymptotic Bayesian fallback used\n")
    cat("Install with: install.packages('brms')\n\n")
  }
  
  asymptotic_bayes_its <- function(model, label) {
    cat(sprintf("--- %s ---\n", label))
    if (is.null(model)) return(NULL)
    
    cf <- nlme::fixed.effects(model)
    V  <- vcov(model)
    
    # Sample 4000 from MVN(beta_hat, V) using base-R Cholesky
    set.seed(GLOBAL_SEED)
    n_draws <- 4000L
    L       <- chol(V)
    z       <- matrix(rnorm(n_draws * length(cf)), nrow = n_draws)
    draws   <- z %*% L + matrix(cf, nrow = n_draws, ncol = length(cf), byrow = TRUE)
    colnames(draws) <- names(cf)
    
    b2 <- draws[, "intervention_t"]
    b3 <- draws[, "time_post_t"]
    
    cat(sprintf("  beta_2: mean=%.4f, 95%%CrI=[%.4f,%.4f], P(b2<0)=%.4f\n",
                mean(b2), quantile(b2, 0.025), quantile(b2, 0.975), mean(b2 < 0)))
    cat(sprintf("  beta_3: mean=%.4f, 95%%CrI=[%.4f,%.4f], P(b3<0)=%.4f\n",
                mean(b3), quantile(b3, 0.025), quantile(b3, 0.975), mean(b3 < 0)))
    
    list(model = NULL, b2 = b2, b3 = b3, method = "asymptotic_normal")
  }
  
  stamp("Asymptotic Bayesian fallback block starting")
  bayes_its_results <- lapply(its_outcomes, function(o) {
    asymptotic_bayes_its(freq_its_results[[o$var]]$model, o$label)
  })
  names(bayes_its_results) <- sapply(its_outcomes, `[[`, "var")

  # Recalibration-adjusted fallback, from the recalibration-adjusted nlme fits
  bayes_its_recal <- lapply(its_outcomes, function(o) {
    asymptotic_bayes_its(freq_its_recal[[o$var]]$model,
                         paste0(o$label, " + recalib_t"))
  })
  names(bayes_its_recal) <- sapply(its_outcomes, `[[`, "var")
}

# =============================================================================
# SAVE
# =============================================================================

# Memory note: the Bayesian entries carry model_file (the per-fit cache
# path) instead of the fitted brms object; reload any fit on demand with
# readRDS. Keeping eight ~1 GB fits inside this list crashed the analysis
# server; draws and summaries needed downstream are stored directly.
saveRDS(list(
  freq        = freq_its_results,   # primary + AR(1) sensitivity per outcome
  bayes       = bayes_its_results,  # primary Bayesian fits (draws + model_file)
  freq_recal  = freq_its_recal,     # recalibration-adjusted (sensitivity)
  bayes_recal = bayes_its_recal     # recalibration-adjusted (sensitivity)
), "results/07_its/its_results.rds")
stamp("Results saved to results/07_its/its_results.rds")

cat("\n=== 07_poc_its.R complete ===\n")
message("=== 07 COMPLETE ===")