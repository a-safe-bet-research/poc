# =============================================================================
# 07c_poc_its_reparameterisation.R
# Purpose : Post-registration exploratory reparameterisation of the ITS models
#           (H6, H7). Under the pre-registered coding (time_post_t = 1 to 6 from
#           October 2024) beta_2 is the level shift at the September to October
#           boundary and the fitted October 2024 departure from the extrapolated
#           baseline trajectory is beta_2 + beta_3. This script (a) computes that
#           contrast with its standard error from the saved pre-registered fits
#           and (b) refits the same models with post-intervention time recoded
#           (October 2024 = 0, November 2024 to March 2025 = 1 to 5), under which
#           the level-change coefficient is the October departure directly.
#           Everything else is unchanged from 07_poc_its.R: participant-month
#           data, intervention date, outcomes, transformations (log1p; asinh for
#           net loss), participant random intercept, nlme::lme (ML) and brms
#           model families, priors, AR(1) and recalibration-adjusted
#           sensitivities, sampler settings, convergence criterion.
# Project : A Safe Bet (ASB) - Proof-of-Concept evaluation.
# Inputs  : poc_analytical_LOCKED.rds (via 00_config.R; READ-ONLY);
#           results/07_its/its_results.rds (the saved pre-registered fits)
# Outputs : results/07_its/expl/EXPL_*.csv and EXPL_freq_results.rds
#           (brms caches EXPL_brms_*.rds in the same folder when RUN_BAYES = TRUE)
# Flags   : RUN_BAYES - FALSE (default) runs the frequentist block only; TRUE
#           also refits the four brms models under the exploratory coding
#           (about three hours).
# Log     : Rscript -e 'setwd("<code folder>"); source("07c_poc_its_reparameterisation.R")' 2>&1 | tee logs/07c_real_YYYYMMDD.log
# =============================================================================
# STATUS  : POST-REGISTRATION EXPLORATORY ANALYSIS. Not pre-registered. Does not
#           replace, modify or re-run the pre-registered ITS analysis
#           (07_poc_its.R, prereg Section 6.4), which remains the confirmatory
#           analysis. The beta_2 + beta_3 October departure is reported
#           alongside the registered estimates; the recoded fits are exploratory.
# LOCKED-FILE PROTECTION: reads only; own outputs only; mtime assertion at end.
# =============================================================================

suppressPackageStartupMessages({ library(dplyr); library(nlme) })
source("00_config.R")
stamp <- function(msg) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), msg))

# ---------------------------------------------------------------------------
# 0. Configuration
# ---------------------------------------------------------------------------
OUT_DIR        <- "results/07_its/expl"
ITS_RESULTS    <- "results/07_its/its_results.rds"
RUN_BAYES      <- FALSE
BAYES_OUTCOMES <- c("log1p_deposits", "asinh_losses", "log1p_wagers", "log1p_play_hours")
BRMS_CORES     <- 4L
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
stopifnot("results/07_its/its_results.rds not found: run 07_poc_its.R first" = file.exists(ITS_RESULTS))
locked_mtime_at_load <- file.info(FILE_POC_LOCKED)$mtime

# ---------------------------------------------------------------------------
# 1. Data: the pre-registered participant-month ITS data frame (locked data)
# ---------------------------------------------------------------------------
stamp("loading saved pre-registered ITS results (read-only)")
orig <- readRDS(ITS_RESULTS)

dat <- readRDS(FILE_POC_LOCKED)
pm  <- dat$player_month
rm(dat)
pm_its <- pm |>
  mutate(time_t = month_number,
         intervention_t = as.integer(is_intervention),
         time_post_t = dplyr::coalesce(months_post, 0L),
         recalib_t = as.integer(year_month >= BETBUDDY_RECALIB_DATE),
         log1p_deposits = log1p(total_deposits), asinh_losses = asinh(total_losses),
         log1p_wagers = log1p(total_wagers), log1p_play_hours = log1p(play_hours_total),
         player_fac = factor(participant_id))
cat("Running on LOCKED real data (poc_analytical_LOCKED.rds)\n")
stopifnot(nrow(pm_its) == 69696L, dplyr::n_distinct(pm_its$participant_id) == 5808L)

# Exploratory recoding of post-intervention time. intervention_t is unchanged.
pm_expl <- pm_its |>
  mutate(time_post_expl = as.integer(pmax(0L, time_post_t - 1L)))

coding <- pm_expl |>
  distinct(year_month, time_t, intervention_t, time_post_t, time_post_expl, recalib_t) |>
  arrange(year_month)
print(as.data.frame(coding), row.names = FALSE)
stopifnot(identical(coding$time_post_expl, c(rep(0L, 7), 1:5)),
          identical(coding$intervention_t, c(rep(0L, 6), rep(1L, 6))))
write.csv(coding, file.path(OUT_DIR, "EXPL_coding_table.csv"), row.names = FALSE)

its_outcomes <- list(
  list(var = "log1p_deposits",   label = "log(1+deposit_amount)", analysis_class = "confirmatory"),
  list(var = "asinh_losses",     label = "asinh(net_loss)",       analysis_class = "secondary"),
  list(var = "log1p_wagers",     label = "log(1+wagers)",         analysis_class = "secondary"),
  list(var = "log1p_play_hours", label = "log(1+play_hours)",     analysis_class = "secondary"))

# ---------------------------------------------------------------------------
# 2. Frequentist refits (nlme::lme, ML; identical control settings to 07)
# ---------------------------------------------------------------------------
fit_lme <- function(fml, data, ar1 = FALSE) {
  if (ar1) {
    nlme::lme(fml, random = ~ 1 | player_fac, data = data, method = "ML", na.action = na.omit,
              correlation = nlme::corAR1(form = ~ time_t | player_fac),
              control = lmeControl(opt = "optim", maxIter = 200, tolerance = 1e-4))
  } else {
    nlme::lme(fml, random = ~ 1 | player_fac, data = data, method = "ML", na.action = na.omit,
              control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200, tolerance = 1e-4))
  }
}
coef_row <- function(m, spec, outcome, model_family) {
  cf <- nlme::fixed.effects(m); V <- vcov(m); se <- sqrt(diag(V))
  df <- m$fixDF$terms["time_t"]
  t2 <- cf["intervention_t"] / se["intervention_t"]; t3 <- cf["time_post_expl"] / se["time_post_expl"]
  data.frame(outcome = outcome, spec = spec, model_family = model_family, parameterisation = "exploratory",
             b1 = cf["time_t"], se_b1 = se["time_t"],
             b2_october_departure = cf["intervention_t"], se_b2 = se["intervention_t"],
             ci_lo_b2 = cf["intervention_t"] - qt(.975, df) * se["intervention_t"],
             ci_hi_b2 = cf["intervention_t"] + qt(.975, df) * se["intervention_t"],
             p_b2 = 2 * pt(abs(t2), df, lower.tail = FALSE),
             b3 = cf["time_post_expl"], se_b3 = se["time_post_expl"],
             ci_lo_b3 = cf["time_post_expl"] - qt(.975, df) * se["time_post_expl"],
             ci_hi_b3 = cf["time_post_expl"] + qt(.975, df) * se["time_post_expl"],
             p_b3 = 2 * pt(abs(t3), df, lower.tail = FALSE),
             recalib = if ("recalib_t" %in% names(cf)) cf["recalib_t"] else NA_real_,
             df = df, logLik = as.numeric(logLik(m)), AIC = AIC(m), BIC = BIC(m), row.names = NULL)
}
orig_contrast <- function(m, spec, outcome) {
  # pre-registered model: October departure = beta_2 + beta_3, SE from the covariance matrix
  cf <- nlme::fixed.effects(m); V <- vcov(m); df <- m$fixDF$terms["time_t"]
  L <- setNames(rep(0, length(cf)), names(cf)); L[c("intervention_t", "time_post_t")] <- 1
  est <- sum(L * cf); se <- sqrt(as.numeric(t(L) %*% V %*% L))
  data.frame(outcome = outcome, spec = spec, model_family = "nlme", parameterisation = "preregistered",
             b1 = cf["time_t"], se_b1 = sqrt(V["time_t", "time_t"]),
             b2_boundary = cf["intervention_t"], se_b2_boundary = sqrt(V["intervention_t", "intervention_t"]),
             ci_lo_b2_boundary = cf["intervention_t"] - qt(.975, df) * sqrt(V["intervention_t", "intervention_t"]),
             ci_hi_b2_boundary = cf["intervention_t"] + qt(.975, df) * sqrt(V["intervention_t", "intervention_t"]),
             p_b2_boundary = 2 * pt(abs(cf["intervention_t"] / sqrt(V["intervention_t", "intervention_t"])), df, lower.tail = FALSE),
             b3 = cf["time_post_t"], se_b3 = sqrt(V["time_post_t", "time_post_t"]),
             b2_plus_b3_october = est, se_b2_plus_b3 = se,
             ci_lo_b2_plus_b3 = est - qt(.975, df) * se, ci_hi_b2_plus_b3 = est + qt(.975, df) * se,
             p_b2_plus_b3 = 2 * pt(abs(est / se), df, lower.tail = FALSE),
             cov_b2_b3 = V["intervention_t", "time_post_t"],
             recalib = if ("recalib_t" %in% names(cf)) cf["recalib_t"] else NA_real_,
             df = df, logLik = as.numeric(logLik(m)), AIC = AIC(m), BIC = BIC(m), row.names = NULL)
}

freq_rows <- list(); orig_rows <- list(); check_rows <- list(); expl_models <- list()
for (o in its_outcomes) {
  for (spec in c("primary", "ar1", "recal", "recal_ar1")) {
    rhs <- if (grepl("recal", spec)) "time_t + intervention_t + time_post_expl + recalib_t" else "time_t + intervention_t + time_post_expl"
    fml <- as.formula(sprintf("%s ~ %s", o$var, rhs))
    stamp(sprintf("nlme exploratory fit: %s [%s]", o$var, spec))
    m <- fit_lme(fml, pm_expl, ar1 = grepl("ar1", spec))
    expl_models[[paste(o$var, spec, sep = "|")]] <- m
    freq_rows[[length(freq_rows) + 1]] <- coef_row(m, spec, o$var, "nlme")
    # matching pre-registered fit (saved object)
    m0 <- switch(spec,
                 primary   = orig$freq[[o$var]]$model,
                 ar1       = orig$freq[[o$var]]$model_ar1,
                 recal     = orig$freq_recal[[o$var]]$model,
                 recal_ar1 = orig$freq_recal[[o$var]]$model_ar1)
    orig_rows[[length(orig_rows) + 1]] <- orig_contrast(m0, spec, o$var)
    # reparameterisation checks: same segmented trajectory
    f0 <- fitted(m0, level = 0); f1 <- fitted(m, level = 0)
    f0i <- fitted(m0, level = 1); f1i <- fitted(m, level = 1)
    cf0 <- nlme::fixed.effects(m0); V0 <- vcov(m0); cf1 <- nlme::fixed.effects(m); V1 <- vcov(m)
    check_rows[[length(check_rows) + 1]] <- data.frame(
      outcome = o$var, spec = spec,
      max_abs_diff_fitted_population = max(abs(f0 - f1)),
      max_abs_diff_fitted_participant = max(abs(f0i - f1i)),
      logLik_prereg = as.numeric(logLik(m0)), logLik_expl = as.numeric(logLik(m)),
      diff_logLik = as.numeric(logLik(m)) - as.numeric(logLik(m0)),
      AIC_prereg = AIC(m0), AIC_expl = AIC(m),
      b3_prereg = cf0["time_post_t"], b3_expl = cf1["time_post_expl"], diff_b3 = cf1["time_post_expl"] - cf0["time_post_t"],
      b2_expl = cf1["intervention_t"], b2_plus_b3_prereg = cf0["intervention_t"] + cf0["time_post_t"],
      diff_b2 = cf1["intervention_t"] - (cf0["intervention_t"] + cf0["time_post_t"]),
      se_b2_expl = sqrt(V1["intervention_t", "intervention_t"]),
      se_b2_plus_b3_prereg = sqrt(V0["intervention_t", "intervention_t"] + V0["time_post_t", "time_post_t"] + 2 * V0["intervention_t", "time_post_t"]),
      sigma_prereg = m0$sigma, sigma_expl = m$sigma, row.names = NULL)
  }
}
freq_tab  <- bind_rows(freq_rows); orig_tab <- bind_rows(orig_rows); check_tab <- bind_rows(check_rows)
write.csv(freq_tab,  file.path(OUT_DIR, "EXPL_freq_estimates_exploratory.csv"), row.names = FALSE)
write.csv(orig_tab,  file.path(OUT_DIR, "EXPL_freq_estimates_preregistered_with_contrast.csv"), row.names = FALSE)
write.csv(check_tab, file.path(OUT_DIR, "EXPL_reparameterisation_checks.csv"), row.names = FALSE)
options(width = 250)
cat("\n=== Reparameterisation checks (exploratory vs pre-registered nlme fits) ===\n")
print(check_tab, digits = 10)
cat("\n=== Exploratory frequentist estimates ===\n"); print(freq_tab, digits = 6)
cat("\n=== Pre-registered estimates with the beta_2 + beta_3 contrast ===\n"); print(orig_tab, digits = 6)
saveRDS(list(freq_expl = freq_tab, freq_prereg = orig_tab, checks = check_tab, coding = coding,
             produced_at = Sys.time(), r_version = R.version.string),
        file.path(OUT_DIR, "EXPL_freq_results.rds"))
stamp("frequentist block done")

# ---------------------------------------------------------------------------
# 3. Bayesian refits (brms; identical priors and sampler settings to 07)
# ---------------------------------------------------------------------------
# Prior note: the independent Normal(0, 10) priors are retained on the
# coefficients of the NEW parameterisation. This is not strictly
# prior-equivalent to the pre-registered specification: under the
# pre-registered priors the implied prior on the October departure
# beta_2 + beta_3 is Normal(0, sqrt(10^2 + 10^2)) = Normal(0, 14.14), and
# under the exploratory priors the implied prior on the pre-registered
# boundary shift beta_2 - beta_3 is Normal(0, 14.14). The likelihood is the
# same segmented trajectory in both cases.
bayes_rows <- list()
if (RUN_BAYES && requireNamespace("brms", quietly = TRUE)) {
  # Environment fix only (no effect on the model): on a system whose R BH
  # package carries no headers, point rstan at the system Boost headers.
  if (requireNamespace("rstan", quietly = TRUE) && !file.exists(rstan::rstan_options("boost_lib")) &&
      dir.exists("/usr/include/boost")) rstan::rstan_options(boost_lib = "/usr/include")
  library(brms); brms_safe_start()
  its_priors <- c(prior(normal(0, 10), class = "b"),
                  prior(normal(0, 10), class = "Intercept"),
                  prior(student_t(3, 0, 10), class = "sigma"))
  for (o in its_outcomes) {
    if (!o$var %in% BAYES_OUTCOMES) { stamp(sprintf("brms skipped by BAYES_OUTCOMES: %s", o$var)); next }
    for (spec in c("primary", "recal")) {
      rhs <- if (spec == "recal") "time_t + intervention_t + time_post_expl + recalib_t" else "time_t + intervention_t + time_post_expl"
      fml <- as.formula(sprintf("%s ~ %s + (1 | player_fac)", o$var, rhs))
      tag <- paste0(o$var, if (spec == "recal") "_recal" else "")
      cache <- file.path(OUT_DIR, sprintf("EXPL_brms_%s", tag))
      stamp(sprintf("brms exploratory fit starting: %s [%s] (cache %s.rds)", o$var, spec, cache))
      t0 <- Sys.time()
      m_b <- tryCatch(
        brm(formula = fml, data = pm_expl, prior = its_priors,
            chains = 4, warmup = 2000, iter = 6000, cores = BRMS_CORES, seed = GLOBAL_SEED,
            control = list(adapt_delta = 0.99), silent = 2,
            file = cache, file_refit = "on_change"),
        error = function(e) { cat("  brms error:", conditionMessage(e), "\n"); NULL })
      stamp(sprintf("brms exploratory fit done: %s [%s] (%.1f s)", o$var, spec,
                    as.numeric(difftime(Sys.time(), t0, units = "secs"))))
      if (is.null(m_b)) next
      max_rhat <- max(rhat(m_b), na.rm = TRUE)
      post <- brms::as_draws_df(m_b, variable = c("b_intervention_t", "b_time_post_expl"))
      b2 <- post$b_intervention_t; b3 <- post$b_time_post_expl
      nd <- posterior::summarise_draws(post, "ess_bulk", "ess_tail")
      bayes_rows[[length(bayes_rows) + 1]] <- data.frame(
        outcome = o$var, spec = spec, model_family = "brms", parameterisation = "exploratory",
        b2_october_departure_mean = mean(b2), b2_cri_lo = quantile(b2, .025), b2_cri_hi = quantile(b2, .975), p_b2_lt0 = mean(b2 < 0),
        b3_mean = mean(b3), b3_cri_lo = quantile(b3, .025), b3_cri_hi = quantile(b3, .975), p_b3_lt0 = mean(b3 < 0),
        max_rhat = max_rhat, ess_bulk_b2 = nd$ess_bulk[1], ess_tail_b2 = nd$ess_tail[1],
        n_divergent = sum(brms::nuts_params(m_b, pars = "divergent__")$Value), n_draws = length(b2), row.names = NULL)
      saveRDS(list(b2 = b2, b3 = b3, max_rhat = max_rhat), file.path(OUT_DIR, sprintf("EXPL_brms_draws_%s.rds", tag)))
      cat(sprintf("  b2 (October departure): mean %.4f, 95%% CrI [%.4f, %.4f], P(<0) %.4f; b3: mean %.4f [%.4f, %.4f], P(<0) %.4f; max R-hat %.4f\n",
                  mean(b2), quantile(b2, .025), quantile(b2, .975), mean(b2 < 0), mean(b3), quantile(b3, .025), quantile(b3, .975), mean(b3 < 0), max_rhat))
      rm(m_b, post); invisible(gc(verbose = FALSE))
    }
  }
  brms_safe_end()
  if (length(bayes_rows)) {
    bayes_tab <- bind_rows(bayes_rows)
    write.csv(bayes_tab, file.path(OUT_DIR, "EXPL_bayes_estimates_exploratory.csv"), row.names = FALSE)
    print(bayes_tab, digits = 5)
  }
} else cat("Bayesian block not run (RUN_BAYES = FALSE or brms unavailable)\n")

# Posterior of beta_2 + beta_3 from the SAVED pre-registered draws, for the
# comparison table only (not a refit; the refit above is the exploratory result)
pre_rows <- list()
for (o in its_outcomes) for (spec in c("primary", "recal")) {
  b <- if (spec == "recal") orig$bayes_recal[[o$var]] else orig$bayes[[o$var]]
  s <- b$b2 + b$b3
  pre_rows[[length(pre_rows) + 1]] <- data.frame(
    outcome = o$var, spec = spec, model_family = "brms", parameterisation = "preregistered",
    b2_boundary_mean = mean(b$b2), b2_cri_lo = quantile(b$b2, .025), b2_cri_hi = quantile(b$b2, .975), p_b2_lt0 = mean(b$b2 < 0),
    b3_mean = mean(b$b3), b3_cri_lo = quantile(b$b3, .025), b3_cri_hi = quantile(b$b3, .975), p_b3_lt0 = mean(b$b3 < 0),
    b2_plus_b3_mean = mean(s), b2_plus_b3_cri_lo = quantile(s, .025), b2_plus_b3_cri_hi = quantile(s, .975), p_b2_plus_b3_lt0 = mean(s < 0),
    max_rhat = b$max_rhat, n_draws = length(s), row.names = NULL)
}
pre_tab <- bind_rows(pre_rows)
write.csv(pre_tab, file.path(OUT_DIR, "EXPL_bayes_estimates_preregistered_with_contrast.csv"), row.names = FALSE)
cat("\n=== Pre-registered Bayesian draws with the beta_2 + beta_3 posterior (saved draws, no refit) ===\n"); print(pre_tab, digits = 5)

# ---------------------------------------------------------------------------
# 4. Reparameterisation identity and integrity
#    The two codings describe the same segmented trajectory, so every
#    exploratory fit must reproduce its pre-registered counterpart: identical
#    fitted values, and exploratory beta_2 equal to registered beta_2 + beta_3.
# ---------------------------------------------------------------------------
stopifnot("Exploratory coding does not reproduce the registered trajectory in every fit" =
            all(abs(check_tab$diff_b2) < 1e-6) && all(check_tab$max_abs_diff_fitted_population < 1e-6))
cat("\nReparameterisation identity holds for all", nrow(check_tab), "fits.\n")
stopifnot("LOCKED FILE MTIME CHANGED DURING RUN" =
            identical(locked_mtime_at_load, file.info(FILE_POC_LOCKED)$mtime))
cat("Locked-file integrity check passed.\n")
stamp("07c_poc_its_reparameterisation.R complete")
