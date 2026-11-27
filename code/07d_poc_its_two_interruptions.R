# =============================================================================
# 07d_poc_its_two_interruptions.R
# STATUS  : POST-REGISTRATION EXPLORATORY ANALYSIS. Not pre-registered. Does
#           not replace, modify or re-run the pre-registered ITS analysis
#           (07_poc_its.R, prereg Section 6.4, H6 and H7), which remains the
#           confirmatory analysis. Written 19 November 2026, after the
#           confirmatory analyses were completed.
# Derives from: 07_poc_its.R and 07c_poc_its_reparameterisation.R.
#
# Question: how much of the October 2024 departure remains once the
#   operator's implementation of Gambling Commission financial
#   vulnerability checks on 1 September 2024 is modelled as its own
#   interruption?
#
# Why     : Monthly deposits rose from April to July 2024 and fell in August
#   and September, before the ASB launch on 1 October. The registered model
#   fits one straight baseline through April-September and projects it into
#   the intervention period, so the September fall is absorbed into the
#   October level change and the projected baseline keeps rising.
#
# Model   : the registered segmented model plus one regulatory step:
#     y ~ time_t + reg_t + intervention_t + time_post_t + (1 | participant)
#   time_t         = month 1-12 (April 2024 = 1)              [as registered]
#   reg_t          = 0 April-August 2024, 1 from September 2024  [NEW]
#   intervention_t = 0 April-September 2024, 1 from October 2024 [as registered]
#   time_post_t    = 0 April-September, 1-6 October-March         [as registered]
#   Under this model the projected course without ASB is
#   beta_0 + beta_1 * time + beta_reg * reg_t, i.e. the baseline trend with
#   the September step carried forward. The October departure from that
#   projection is beta_2 + beta_3 (registered time coding, see 07c).
#   A separate regulatory SLOPE change cannot be estimated: with one month
#   between the two interruptions, months-since-September minus
#   months-since-October equals reg_t, so the terms are collinear. Only the
#   regulatory step is identified.
#   Everything else as registered: participant-month dataset, outcomes,
#   transformations (log1p; asinh for net loss), participant random
#   intercept, nlme::lme (ML), AR(1) sensitivity (nlme::corAR1).
#
# Inputs  : as 07c. DATA_SOURCE "locked" (poc_analytical_LOCKED.rds via
#           00_config.R, ECDA) or "saved_model" (pm_its held inside the saved
#           pre-registered nlme fit, its_results.rds). Saved pre-registered
#           fits are read for the nested model comparison.
# Outputs : EXPL_outputs/ (all files prefixed EXPL_07d_). No existing file is
#           read for writing, edited or overwritten.
#           EXPL_07d_coding_table.csv
#           EXPL_07d_estimates.csv          coefficients, 95% CIs, contrasts
#           EXPL_07d_model_comparison.csv   registered vs two-interruption
#           EXPL_07d_results.rds            fixed effects + vcov per fit
#                                           (read by 12_poc_manuscript_figures.R)
# Run     : Rscript 07d_poc_its_two_interruptions.R 2>&1 | tee EXPL_outputs/EXPL_07d_run_<date>.log
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(nlme)
})
stamp <- function(msg) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), msg))

# ---------------------------------------------------------------------------
# 0. Configuration
# ---------------------------------------------------------------------------
EXPL_DIR    <- Sys.getenv("EXPL_DIR", unset = ".")
OUT_DIR     <- file.path(EXPL_DIR, "EXPL_outputs")
ITS_RESULTS <- Sys.getenv("EXPL_ITS_RESULTS", unset = "results/07_its/its_results.rds")
DATA_SOURCE <- Sys.getenv("EXPL_DATA_SOURCE", unset = "locked")   # "locked" | "saved_model"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

if (file.exists("00_config.R")) {
  source("00_config.R")
} else {
  INTERVENTION_START <- as.Date("2024-10-01")
}
REG_START <- as.Date("2024-09-01")   # operator implementation, operator-verified

# ---------------------------------------------------------------------------
# 1. Data: the pre-registered participant-month ITS data frame
# ---------------------------------------------------------------------------
stamp("loading saved pre-registered ITS results (read-only)")
orig <- readRDS(ITS_RESULTS)

if (DATA_SOURCE == "locked") {
  dat <- readRDS(FILE_POC_LOCKED)
  pm  <- dat$player_month
  pm_its <- pm |>
    mutate(time_t = month_number,
           intervention_t = as.integer(is_intervention),
           time_post_t = dplyr::coalesce(months_post, 0L),
           log1p_deposits = log1p(total_deposits), asinh_losses = asinh(total_losses),
           log1p_wagers = log1p(total_wagers), log1p_play_hours = log1p(play_hours_total),
           player_fac = factor(participant_id))
  cat("DATA_SOURCE = locked (poc_analytical_LOCKED.rds)\n")
} else {
  pm_its <- orig$freq$log1p_deposits$model$data
  cat("DATA_SOURCE = saved_model (pm_its as held inside the saved pre-registered nlme fit)\n")
}

# Regulatory step from September 2024 (month 6). Registered terms unchanged.
pm_two <- pm_its |> mutate(reg_t = as.integer(time_t >= 6L))

coding <- pm_two |>
  distinct(time_t, reg_t, intervention_t, time_post_t) |>
  arrange(time_t)
print(as.data.frame(coding), row.names = FALSE)
stopifnot(identical(as.integer(coding$time_t), 1:12),
          identical(as.integer(coding$reg_t), c(rep(0L, 5), rep(1L, 7))),
          identical(as.integer(coding$intervention_t), c(rep(0L, 6), rep(1L, 6))),
          identical(as.integer(coding$time_post_t), c(rep(0L, 6), 1:6)))
write.csv(coding, file.path(OUT_DIR, "EXPL_07d_coding_table.csv"), row.names = FALSE)

its_outcomes <- c("log1p_deposits", "asinh_losses", "log1p_wagers", "log1p_play_hours")

# ---------------------------------------------------------------------------
# 2. Fits (nlme::lme, ML; identical control settings to 07 and 07c)
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

ci_row <- function(est, se, df, term) {
  q <- qt(.975, df)
  data.frame(term = term, est = est, se = se, ci_lo = est - q * se, ci_hi = est + q * se,
             p = 2 * pt(abs(est / se), df, lower.tail = FALSE),
             pct = 100 * (exp(est) - 1), pct_lo = 100 * (exp(est - q * se) - 1),
             pct_hi = 100 * (exp(est + q * se) - 1), row.names = NULL)
}

est_rows <- list(); cmp_rows <- list(); saved <- list()
for (v in its_outcomes) {
  for (spec in c("primary", "ar1")) {
    fml <- as.formula(sprintf("%s ~ time_t + reg_t + intervention_t + time_post_t", v))
    stamp(sprintf("nlme two-interruption fit: %s [%s]", v, spec))
    m  <- fit_lme(fml, pm_two, ar1 = (spec == "ar1"))
    cf <- nlme::fixed.effects(m); V <- vcov(m); df <- m$fixDF$terms[["time_t"]]
    se <- sqrt(diag(V))
    L  <- setNames(rep(0, length(cf)), names(cf)); L[c("intervention_t", "time_post_t")] <- 1
    oct <- sum(L * cf); oct_se <- sqrt(as.numeric(t(L) %*% V %*% L))
    rows <- bind_rows(
      ci_row(cf[["time_t"]],         se[["time_t"]],         df, "beta_1 baseline trend"),
      ci_row(cf[["reg_t"]],          se[["reg_t"]],          df, "beta_reg September step"),
      ci_row(cf[["intervention_t"]], se[["intervention_t"]], df, "beta_2 level change"),
      ci_row(cf[["time_post_t"]],    se[["time_post_t"]],    df, "beta_3 slope change"),
      ci_row(oct,                    oct_se,                 df, "beta_2 + beta_3 October departure"))
    rows$outcome <- v; rows$spec <- spec; rows$df <- df
    if (v == "asinh_losses") rows[, c("pct", "pct_lo", "pct_hi")] <- NA_real_  # % not valid for asinh
    est_rows[[length(est_rows) + 1]] <- rows
    saved[[paste(v, spec, sep = "|")]] <- list(fixef = cf, vcov = V, df = df,
                                               logLik = as.numeric(logLik(m)), AIC = AIC(m), BIC = BIC(m))

    # Nested comparison with the saved pre-registered fit (same data, ML):
    # the registered model is this model with beta_reg = 0.
    m0 <- if (spec == "primary") orig$freq[[v]]$model else orig$freq[[v]]$model_ar1
    if (!is.null(m0)) {
      cf0 <- nlme::fixed.effects(m0)
      oct0 <- cf0[["intervention_t"]] + cf0[["time_post_t"]]
      lr  <- 2 * (as.numeric(logLik(m)) - as.numeric(logLik(m0)))
      cmp_rows[[length(cmp_rows) + 1]] <- data.frame(
        outcome = v, spec = spec,
        logLik_registered = as.numeric(logLik(m0)), logLik_two = as.numeric(logLik(m)),
        AIC_registered = AIC(m0), AIC_two = AIC(m), BIC_registered = BIC(m0), BIC_two = BIC(m),
        LR_chisq_1df = lr, p_LR = pchisq(lr, 1, lower.tail = FALSE),
        beta1_registered = cf0[["time_t"]], beta1_two = cf[["time_t"]],
        october_departure_registered = oct0, october_departure_two = oct,
        share_of_registered_departure_remaining = oct / oct0, row.names = NULL)
    }
    rm(m, m0); invisible(gc(verbose = FALSE))
  }
}

est_tab <- bind_rows(est_rows) |> select(outcome, spec, term, everything())
cmp_tab <- bind_rows(cmp_rows)
write.csv(est_tab, file.path(OUT_DIR, "EXPL_07d_estimates.csv"), row.names = FALSE)
write.csv(cmp_tab, file.path(OUT_DIR, "EXPL_07d_model_comparison.csv"), row.names = FALSE)
saveRDS(list(fits = saved, estimates = est_tab, comparison = cmp_tab, coding = coding,
             reg_start = REG_START, written = Sys.time()),
        file.path(OUT_DIR, "EXPL_07d_results.rds"))

options(width = 250)
cat("\n=== Two-interruption estimates ===\n"); print(est_tab, digits = 4)
cat("\n=== Registered vs two-interruption model ===\n"); print(cmp_tab, digits = 5)
stamp("07d_poc_its_two_interruptions.R complete")
