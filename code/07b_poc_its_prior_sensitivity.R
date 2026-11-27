# =============================================================================
# 07b_poc_its_prior_sensitivity.R
# Purpose : Pre-registered brms prior-sensitivity check for the ITS analyses
#           (prereg 10.4 item 1): refit the four PRIMARY Bayesian ITS models
#           with tighter Normal(0, 5) priors on the regression coefficients
#           and compare the beta-2 / beta-3 posteriors against the registered
#           Normal(0, 10) primary fits from 07_poc_its.R.
#           ALSO (block 3): writes the slim verification export of the 07
#           results (its_results_slim.rds — model objects dropped) so the
#           full 199 MB file never needs to move off the server.
# Project : A Safe Bet (ASB) — Proof-of-Concept evaluation.
# Inputs  : poc_analytical_LOCKED.rds (via 00_config.R);
#           results/07_its/its_results.rds (the 07 primary posteriors)
# Outputs : results/07_its/its_prior_sensitivity.rds
#           results/07_its/its_results_slim.rds
#           brms fit caches results/07_its/brms_<outcome>_n05.rds (STAY on server)
# Log     : nohup Rscript 07b_poc_its_prior_sensitivity.R > logs/07b_real_YYYYMMDD.log 2>&1 &
#           (four fresh HMC fits; expect roughly 40–45 min each, ~3 h total)
# =============================================================================
# STATUS  : POST-REGISTRATION implementation of pre-registered work
#           (prereg 10.4 item 1: "brms prior sensitivity check using
#           Normal(0, 5) priors on the regression coefficients, implemented
#           post-registration and run alongside the primary Normal(0, 10)
#           ITS analyses to assess prior robustness; the random-intercept SD
#           prior remains at the brms default"). The plan executing, not a
#           deviation. Registered scope: the four PRIMARY ITS fits; the
#           recalibration-adjusted specifications are not part of the
#           registered check and are not refitted here.
#
# SPECIFICATION (everything except the coefficient priors is IDENTICAL to
# the registered primary fits in 07_poc_its.R):
#   - model: outcome ~ time_t + intervention_t + time_post_t + (1 | player_fac)
#   - transforms: log1p (deposits, wagers, play hours); asinh (net loss)
#   - priors: Normal(0, 5) on class b AND the Intercept (both are regression
#     coefficients; 07 set both to Normal(0, 10)); sigma prior UNCHANGED at
#     Student-t(3, 0, 10); random-intercept SD prior UNCHANGED at brms default
#   - sampler: 4 chains, 2,000 warmup + 4,000 sampling, adapt_delta = 0.99,
#     seed = GLOBAL_SEED; convergence criterion R-hat < 1.01
#   - separate cache files (brms_*_n05.rds) so the registered primary caches
#     are never touched
# LOCKED-FILE PROTECTION: reads only; writes nothing except its own outputs
# and its own caches; mtime integrity assertion at the end.
# =============================================================================

library(dplyr)
library(tidyr)
library(lubridate)

source("00_config.R")
library(conflicted)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::summarise)
conflicts_prefer(dplyr::arrange)
conflicts_prefer(dplyr::count)
conflicts_prefer(dplyr::group_by)

stamp <- function(msg) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), msg))

# OVERWRITE = FALSE: crash-resumable via the per-fit caches, exactly as in 07.
OVERWRITE <- FALSE

locked_mtime_at_load <- file.info(FILE_POC_LOCKED)$mtime
dat <- readRDS(FILE_POC_LOCKED)
pm  <- dat$player_month
cat("Running on LOCKED real data\n")

dir.create("results/07_its", recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 1. MONTHLY DATA — identical preparation to 07_poc_its.R
# -----------------------------------------------------------------------------
pm_its <- pm |>
  dplyr::mutate(
    time_t           = month_number,
    intervention_t   = as.integer(is_intervention),
    time_post_t      = dplyr::coalesce(months_post, 0L),
    log1p_deposits   = log1p(total_deposits),
    asinh_losses     = asinh(total_losses),
    log1p_wagers     = log1p(total_wagers),
    log1p_play_hours = log1p(play_hours_total),
    player_fac       = factor(participant_id)
  )
stopifnot(
  "NaN in transformed ITS outcomes - check transform domains" =
    !any(is.nan(pm_its$log1p_deposits)) && !any(is.nan(pm_its$asinh_losses)) &&
    !any(is.nan(pm_its$log1p_wagers))   && !any(is.nan(pm_its$log1p_play_hours))
)
stamp(sprintf("07b started; monthly data prepared (%d rows, %d players)",
              nrow(pm_its), dplyr::n_distinct(pm_its$participant_id)))

its_outcomes <- list(
  list(var = "log1p_deposits",   label = "log(1+deposit_amount)", analysis_class = "confirmatory"),
  list(var = "asinh_losses",     label = "asinh(net_loss)",       analysis_class = "secondary"),
  list(var = "log1p_wagers",     label = "log(1+wagers)",         analysis_class = "secondary"),
  list(var = "log1p_play_hours", label = "log(1+play_hours)",     analysis_class = "secondary")
)

# -----------------------------------------------------------------------------
# 2. REFERENCE POSTERIORS — the registered Normal(0, 10) primary fits,
#    read from the draws stored in its_results.rds by 07_poc_its.R
# -----------------------------------------------------------------------------
ITS_RESULTS_PATH <- "results/07_its/its_results.rds"
stopifnot("results/07_its/its_results.rds not found — run 07_poc_its.R first" =
            file.exists(ITS_RESULTS_PATH))
stamp("loading its_results.rds (large; needed once for reference draws + slim export)")
its_res <- readRDS(ITS_RESULTS_PATH)

ref_summ <- lapply(its_res$bayes, function(e) {
  if (is.null(e)) return(NULL)
  list(b2_mean = mean(e$b2), b2_lo = unname(quantile(e$b2, .025)),
       b2_hi = unname(quantile(e$b2, .975)), p_b2 = mean(e$b2 < 0),
       b3_mean = mean(e$b3), b3_lo = unname(quantile(e$b3, .025)),
       b3_hi = unname(quantile(e$b3, .975)), p_b3 = mean(e$b3 < 0))
})

# -----------------------------------------------------------------------------
# 3. SLIM VERIFICATION EXPORT (bonus block; not part of the registered check)
#    Drops the embedded nlme model objects so the file travels; every number
#    (coefs, SEs, p, DW, AR(1), Bayesian draws) is retained.
# -----------------------------------------------------------------------------
slim <- its_res
for (b in c("freq", "freq_recal")) {
  for (nm in names(slim[[b]])) {
    slim[[b]][[nm]]$model     <- NULL
    slim[[b]][[nm]]$model_ar1 <- NULL
  }
}
saveRDS(slim, "results/07_its/its_results_slim.rds")
stamp(sprintf("slim export written: results/07_its/its_results_slim.rds (%.1f MB)",
              file.size("results/07_its/its_results_slim.rds") / 1e6))
rm(its_res, slim); invisible(gc(verbose = FALSE))

# -----------------------------------------------------------------------------
# 4. NORMAL(0, 5) REFITS — identical to 07's primary fits except the priors
# -----------------------------------------------------------------------------
stopifnot("brms not installed" = requireNamespace("brms", quietly = TRUE))
library(brms)
brms_safe_start()   # detaches conflicted — see 00_config.R

# Prereg 10.4 item 1: Normal(0, 5) on the regression coefficients (class b
# and Intercept — 07 set both explicitly, so both are tightened here);
# sigma prior unchanged from 07; random-intercept SD prior at brms default.
priors_n05 <- c(
  prior(normal(0, 5), class = "b"),
  prior(normal(0, 5), class = "Intercept"),
  prior(student_t(3, 0, 10), class = "sigma")
)

fit_n05 <- function(outcome_var, label, analysis_class) {
  cat(sprintf("\n--- Prior-sensitivity fit (Normal(0,5)): %s [%s] ---\n",
              label, analysis_class))
  fml <- as.formula(sprintf(
    "%s ~ time_t + intervention_t + time_post_t + (1 | player_fac)", outcome_var))

  cache_path <- sprintf("results/07_its/brms_%s_n05.rds", outcome_var)
  if (file.exists(cache_path) && !OVERWRITE) {
    message(sprintf("SKIPPING refit: cache %s exists and OVERWRITE = FALSE", cache_path))
  } else if (file.exists(cache_path) && OVERWRITE) {
    message(sprintf("OVERWRITE = TRUE: removing cache %s and refitting", cache_path))
    file.remove(cache_path)
  }

  t0 <- Sys.time()
  stamp(sprintf("brms N(0,5) fit starting: %s (fresh HMC can run ~40+ min without output)",
                outcome_var))
  m <- tryCatch(
    brm(formula    = fml,
        data       = pm_its,
        prior      = priors_n05,
        chains     = 4,
        warmup     = 2000,
        iter       = 6000,
        cores      = 4,
        seed       = GLOBAL_SEED,
        control    = list(adapt_delta = 0.99),
        silent     = 2,
        file       = sprintf("results/07_its/brms_%s_n05", outcome_var),
        file_refit = "on_change"),
    error = function(e) { cat("  brms error:", conditionMessage(e), "\n"); NULL }
  )
  stamp(sprintf("brms N(0,5) fit done: %s (%.1f s)", outcome_var,
                as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  if (is.null(m)) return(NULL)

  max_rhat <- max(rhat(m), na.rm = TRUE)
  cat(sprintf("  Max R-hat: %.4f %s\n", max_rhat,
              if (max_rhat < 1.01) "[CONVERGED]" else "[WARNING]"))

  post <- as_draws_df(m, variable = c("b_intervention_t", "b_time_post_t"))
  b2 <- post$b_intervention_t; b3 <- post$b_time_post_t

  cat(sprintf("  beta_2: mean=%.4f, 95%%CrI=[%.4f,%.4f], P(b2<0)=%.4f\n",
              mean(b2), quantile(b2, .025), quantile(b2, .975), mean(b2 < 0)))
  cat(sprintf("  beta_3: mean=%.4f, 95%%CrI=[%.4f,%.4f], P(b3<0)=%.4f\n",
              mean(b3), quantile(b3, .025), quantile(b3, .975), mean(b3 < 0)))

  rm(post, m); invisible(gc(verbose = FALSE))
  stamp(sprintf("released %s from memory (fit retained at %s)", outcome_var, cache_path))
  list(model_file = cache_path, b2 = b2, b3 = b3, max_rhat = max_rhat,
       analysis_class = analysis_class, prior = "Normal(0,5) b + Intercept; sigma Student-t(3,0,10); RE SD default")
}

stamp("Prior-sensitivity block starting (4 fresh brms fits)")
n05_results <- lapply(its_outcomes, function(o) fit_n05(o$var, o$label, o$analysis_class))
names(n05_results) <- sapply(its_outcomes, `[[`, "var")
stamp("Prior-sensitivity block done")

brms_safe_end()   # restores conflicted — see 00_config.R

# -----------------------------------------------------------------------------
# 5. COMPARISON — Normal(0, 10) registered primary vs Normal(0, 5) check
# -----------------------------------------------------------------------------
cat("\n=== PRIOR SENSITIVITY COMPARISON (per outcome) ===\n")
comparison <- lapply(names(n05_results), function(v) {
  a <- ref_summ[[v]]; b <- n05_results[[v]]
  if (is.null(a) || is.null(b)) { cat(v, ": comparison unavailable\n"); return(NULL) }
  row <- data.frame(
    outcome        = v,
    b2_n10         = a$b2_mean, b2_n05 = mean(b$b2), b2_delta = mean(b$b2) - a$b2_mean,
    p_b2_n10       = a$p_b2,    p_b2_n05 = mean(b$b2 < 0),
    b3_n10         = a$b3_mean, b3_n05 = mean(b$b3), b3_delta = mean(b$b3) - a$b3_mean,
    p_b3_n10       = a$p_b3,    p_b3_n05 = mean(b$b3 < 0),
    max_rhat_n05   = b$max_rhat
  )
  row
})
comparison <- do.call(rbind, comparison)
print(comparison, row.names = FALSE, digits = 4)
cat("\nReading: with ~70k observations, coefficient posteriors should be\n")
cat("dominated by the likelihood; deltas materially different from zero would\n")
cat("indicate unexpected prior sensitivity and go to the analysis team as a query.\n")

# -----------------------------------------------------------------------------
# SAVE + locked-file integrity check
# -----------------------------------------------------------------------------
saveRDS(list(
  n05        = n05_results,     # draws + cache paths + R-hats, per outcome
  reference  = ref_summ,        # registered Normal(0,10) posterior summaries
  comparison = comparison
), "results/07_its/its_prior_sensitivity.rds")
stamp("Results saved to results/07_its/its_prior_sensitivity.rds")

stopifnot("LOCKED FILE MTIME CHANGED DURING RUN — investigate immediately" =
            identical(locked_mtime_at_load, file.info(FILE_POC_LOCKED)$mtime))
cat("Locked-file integrity check passed.\n")

cat("\n=== 07b_poc_its_prior_sensitivity.R complete ===\n")
