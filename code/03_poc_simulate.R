# =============================================================================
# 03_poc_simulate.R
# Purpose : Step 3 — generate a synthetic (simulated) dataset for
#           pre-registration-phase analysis development
# Project : A Safe Bet (ASB) — Proof-of-Concept evaluation.
#           Putting Design into Practice: A Safe Bet — A Proof-of-Concept
#           Evaluation of a Tailored Online Responsible Gambling Framework
# Inputs  : poc_analytical_LOCKED.rds
# Outputs : poc_synthetic.rds; synthetic hash appended to poc_manifest.txt
# =============================================================================
# PURPOSE:
#   Fit marginal distributions and key covariance structure from
#   poc_analytical_LOCKED.rds, then generate a synthetic dataset of the same
#   size that preserves:
#     - N (5,808 players, 2 periods; 5,808 × 12 months)
#     - Marginal distributions of all primary outcomes
#     - Within-player correlation across periods
#     - Risk tier distributions and transition probabilities
#     - Young Adult and baseline risk stratification
#     - Realistic zero-inflation (gambling is highly zero-inflated)
#
#   The synthetic dataset is used ONLY for:
#     (a) Developing analysis scripts (06+)
#     (b) Writing the detailed pre-registration on OSF
#     (c) Confirming that analysis scripts run without errors
#
#   The synthetic dataset does NOT need to reproduce the exact effect sizes
#   from preliminary explorations — it just needs to be plausible and
#   structurally valid.
#
# APPROACH:
#   Uses synthpop (Nowok et al., 2016) for the player-period data.
#   For the player-month data (ITS/Markov), a custom simulation approach
#   preserves the temporal structure using fitted transition matrices.
#
# STATUS: Preparatory – permitted before OSF registration.
#
# OUTPUT:
#   poc_synthetic.rds
#     $player_period_synth – synthetic player × period data
#     $player_month_synth  – synthetic player × month data (incl. engagement covariates)
#     $sim_params          – all parameters used for simulation
#     $validation          – marginal comparison real vs synthetic
# =============================================================================

library(dplyr)
library(tidyr)
library(lubridate)
library(synthpop)   # install if needed: install.packages("synthpop")
library(MASS)       # mvrnorm for correlated outcomes

source("00_config.R")

set.seed(GLOBAL_SEED)

OUTPUT_DIR <- ROOT_OUT

cat("=== 03_poc_simulate.R ===\n")
cat("Seed:", GLOBAL_SEED, "\n")
cat("Started:", format(Sys.time()), "\n\n")

# -----------------------------------------------------------------------------
# 1. LOAD LOCKED DATASET
# -----------------------------------------------------------------------------

locked_path <- file.path(OUTPUT_DIR, "poc_analytical_LOCKED.rds")

if (!file.exists(locked_path)) {
  stop("Locked dataset not found. Run 02_poc_lock.R first.")
}

poc <- readRDS(locked_path)
pp  <- poc$player_period   # player × period
pm  <- poc$player_month    # player × month

cat("Loaded locked dataset.\n")
cat("  player_period rows:", nrow(pp), "\n")
cat("  player_month rows:", nrow(pm), "\n\n")

# -----------------------------------------------------------------------------
# 2. FIT PARAMETERS FROM REAL DATA
#    Fit separate parameters for baseline and intervention periods
#    All parameters are logged for reproducibility
# -----------------------------------------------------------------------------

cat("Fitting parameters from real data...\n")

# --- 2a. Player-period: primary outcomes ---

# Work in wide format: one row per player
# Extract player-level constants before pivoting (one value per player)
player_constants <- pp |>
  dplyr::filter(period == "baseline") |>
  dplyr::select(participant_id,
                dplyr::any_of(c("age_years", "engaged_cohort", "young_adult_flag"))) |>
  dplyr::distinct()

pp_wide <- pp |>
  dplyr::select(participant_id, period, total_deposits, total_losses, total_wagers,
                play_hours_total, night_hours_total, agd_count,
                dominant_risk) |>
  pivot_wider(names_from = period,
              values_from = c(total_deposits, total_losses, total_wagers,
                              play_hours_total, night_hours_total, agd_count,
                              dominant_risk)) |>
  dplyr::left_join(player_constants, by = "participant_id")

N_players <- nrow(pp_wide)
cat("  N players for simulation:", N_players, "\n")

# Fit log-normal parameters for each outcome × period
# (deposit_amount, net_loss, wagers are right-skewed; log-transform first)
fit_lognormal <- function(x) {
  # Zero-inflate: many players have zero deposit_amount on some days
  # but at period level most will have >0; check and handle
  x_pos <- x[x > 0 & !is.na(x)]
  if (length(x_pos) < 10) return(list(mu = 0, sigma = 1, zero_prop = 1))
  list(
    mu        = mean(log(x_pos)),
    sigma     = sd(log(x_pos)),
    zero_prop = mean(x == 0, na.rm = TRUE)
  )
}

fit_params <- list()
outcomes <- c("total_deposits", "total_losses", "total_wagers",
              "play_hours_total", "night_hours_total", "agd_count")

for (outcome in outcomes) {
  for (period in c("baseline", "intervention")) {
    col <- paste0(outcome, "_", period)
    if (col %in% names(pp_wide)) {
      fit_params[[col]] <- fit_lognormal(pp_wide[[col]])
    }
  }
}

cat("  Fitted lognormal params for", length(fit_params), "outcome × period cells\n")

# --- 2b. Within-player correlation matrix across periods ---
# Key: baseline outcome is correlated with intervention outcome
# Fit Pearson correlation on log-scale

outcome_cols <- c(
  "total_deposits_baseline",   "total_deposits_intervention",
  "total_losses_baseline",     "total_losses_intervention",
  "total_wagers_baseline",     "total_wagers_intervention",
  "play_hours_total_baseline", "play_hours_total_intervention"
)

# Log-transform for correlation fitting
pp_log <- pp_wide |>
  dplyr::select(all_of(outcome_cols)) |>
  mutate(across(everything(), ~ log(pmax(., 1))))  # log(max(x,1)) avoids -Inf

cor_matrix <- cor(pp_log, use = "pairwise.complete.obs")

cat("  Fitted within-player correlation matrix (", ncol(cor_matrix), "×",
    nrow(cor_matrix), ")\n")

# --- 2c. Risk tier marginals ---

risk_marginals <- pp |>
  filter(!is.na(dominant_risk)) |>
  group_by(period, dominant_risk) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(period) |>
  mutate(prop = n / sum(n))

cat("  Risk tier marginals fitted\n")
print(risk_marginals)

# --- 2d. Markov transition matrices (from player_month) ---
# Fit 4×4 transition matrix for baseline and intervention periods separately

compute_transition_matrix <- function(pm_subset) {
  states <- c("L", "M", "H", "I")
  trans  <- matrix(0, nrow = 4, ncol = 4, dimnames = list(states, states))
  
  # For each player, look at consecutive months
  pm_ordered <- pm_subset |>
    arrange(participant_id, year_month)
  
  for (pid in unique(pm_ordered$participant_id)) {
    p_states <- pm_ordered$markov_state[pm_ordered$participant_id == pid]
    p_states <- na.omit(p_states)
    if (length(p_states) < 2) next
    for (t in seq_len(length(p_states) - 1)) {
      s_from <- p_states[t]
      s_to   <- p_states[t + 1]
      if (s_from %in% states && s_to %in% states) {
        trans[s_from, s_to] <- trans[s_from, s_to] + 1
      }
    }
  }
  
  # Row-normalise to get probabilities
  row_sums <- rowSums(trans)
  trans_prob <- sweep(trans, 1, pmax(row_sums, 1), "/")
  trans_prob
}

pm_baseline     <- pm |> filter(year_month <  INTERVENTION_START)
pm_intervention <- pm |> filter(year_month >= INTERVENTION_START)

trans_baseline     <- compute_transition_matrix(pm_baseline)
trans_intervention <- compute_transition_matrix(pm_intervention)

cat("\n  Baseline transition matrix:\n")
print(round(trans_baseline, 3))
cat("\n  Intervention transition matrix:\n")
print(round(trans_intervention, 3))

# --- 2e. Young Adult flag distribution ---
ya_prop <- mean(pp_wide$young_adult_flag, na.rm = TRUE)
cat("\n  Young Adult proportion:", round(ya_prop, 4), "\n")

# -----------------------------------------------------------------------------
# 3. GENERATE SYNTHETIC PLAYER-PERIOD DATA
# -----------------------------------------------------------------------------

cat("\nGenerating synthetic player-period data...\n")

# Use synthpop on the wide player-period data
# Include key predictors and outcomes; synthpop will preserve
# marginals and correlations automatically

pp_for_synth <- pp_wide |>
  dplyr::select(
    total_deposits_baseline,    total_deposits_intervention,
    total_losses_baseline,      total_losses_intervention,
    total_wagers_baseline,      total_wagers_intervention,
    play_hours_total_baseline,  play_hours_total_intervention,
    agd_count_baseline,         agd_count_intervention,
    dominant_risk_baseline,     dominant_risk_intervention,
    young_adult_flag,
    # Include all columns needed by analysis scripts
    dplyr::any_of(c("age_years", "engaged_cohort",
                    "night_hours_total_baseline", "night_hours_total_intervention"))
  ) |>
  mutate(
    across(where(is.character), as.factor),
    young_adult_flag = as.integer(young_adult_flag),
    dplyr::across(dplyr::any_of(c("engaged_cohort", "young_adult_flag")), as.integer)
  )

# synthpop: parametric synthesis preserving correlations
synth_result <- syn(
  pp_for_synth,
  method = "parametric",   # Use parametric for speed; "cart" for more fidelity
  m      = 1,              # One synthetic dataset
  seed   = GLOBAL_SEED,
  print.flag = FALSE
)

synth_pp_wide <- synth_result$syn

# Assign synthetic player IDs
synth_pp_wide <- synth_pp_wide |>
  mutate(participant_id = paste0("SYNTH_", sprintf("%05d", row_number())))

# Reshape to long format (matching real data structure)
player_period_synth <- synth_pp_wide |>
  pivot_longer(
    cols = matches("_(baseline|intervention)$"),
    names_to = c(".value", "period"),
    names_pattern = "(.+)_(baseline|intervention)"
  ) |>
  rename(
    dominant_risk    = dominant_risk,
    agd_count        = agd_count
  ) |>
  mutate(
    period_factor = factor(period, levels = c("baseline", "intervention")),
    period_num    = if_else(period == "baseline", 0L, 1L)
  )

cat("  Synthetic player_period rows:", nrow(player_period_synth), "\n")
cat("  Synthetic N players:", n_distinct(player_period_synth$participant_id), "\n")

# -----------------------------------------------------------------------------
# 4. GENERATE SYNTHETIC PLAYER-MONTH DATA
#    Simulate 12 months of Markov states then attach outcome distributions
# -----------------------------------------------------------------------------

cat("\nGenerating synthetic player-month data...\n")

states     <- c("L", "M", "H", "I")
all_months <- MONTH_DATES   # from config: April 2024 through March 2025

n_synth <- N_players  # match real N

simulate_markov_sequence <- function(n_players, trans_bl, trans_int, months) {
  n_months_bl  <- sum(months <  INTERVENTION_START)
  n_months_int <- sum(months >= INTERVENTION_START)
  
  # Starting state distribution from baseline marginals
  start_dist <- c(L = 0.70, M = 0.25, H = 0.04, I = 0.01)  # approximate from risk_marginals
  
  result <- vector("list", n_players)
  
  for (i in seq_len(n_players)) {
    pid    <- paste0("SYNTH_", sprintf("%05d", i))
    states_seq <- character(length(months))
    
    # Starting state
    states_seq[1] <- sample(names(start_dist), 1, prob = start_dist)
    
    for (t in seq_along(months)[-1]) {
      is_int    <- months[t] >= INTERVENTION_START
      trans_use <- if (is_int) trans_int else trans_bl
      prev      <- states_seq[t - 1]
      if (is.na(prev) || !prev %in% rownames(trans_use)) {
        states_seq[t] <- "L"
      } else {
        states_seq[t] <- sample(states, 1, prob = trans_use[prev, ])
      }
    }
    
    result[[i]] <- data.frame(
      participant_id    = pid,
      year_month   = months,
      month_number = seq_along(months),
      markov_state = states_seq,
      is_intervention = as.integer(months >= INTERVENTION_START),
      months_post = pmax(0L, cumsum(months >= INTERVENTION_START))
    )
  }
  
  bind_rows(result)
}

player_month_synth <- simulate_markov_sequence(
  n_synth, trans_baseline, trans_intervention, all_months
)

# Attach simulated outcome values per player-month
# (Simulate from lognormal distributions fitted in Step 2.)
# Note: fit_params is fit at PERIOD level (6-month totals) so per-month draws
# are biased upward in magnitude. Synthetic player_month outcomes capture the
# DIRECTION and RELATIVE LEVEL CHANGE of the intervention but should not be
# read as preview estimates of effect magnitude. See methods.
simulate_outcome <- function(period_vec, fit_params, outcome_name) {
  result <- numeric(length(period_vec))
  for (p in unique(period_vec)) {
    params <- fit_params[[paste0(outcome_name, "_", p)]]
    if (is.null(params)) next
    idx   <- which(period_vec == p)
    n_p   <- length(idx)
    zeros <- rbinom(n_p, 1, params$zero_prop)
    vals  <- rlnorm(n_p, meanlog = params$mu, sdlog = params$sigma)
    result[idx] <- vals * (1 - zeros)
  }
  result
}

player_month_synth <- player_month_synth |>
  mutate(
    period = if_else(is_intervention == 0, "baseline", "intervention"),
    total_deposits   = simulate_outcome(period, fit_params, "total_deposits"),
    total_losses     = simulate_outcome(period, fit_params, "total_losses"),
    total_wagers     = simulate_outcome(period, fit_params, "total_wagers"),
    play_hours_total = simulate_outcome(period, fit_params, "play_hours_total"),
    # Monthly risk label from Markov state
    monthly_risk     = case_when(
      markov_state == "L" ~ "low",
      markov_state == "M" ~ "medium",
      markov_state == "H" ~ "high",
      markov_state == "I" ~ NA_character_
    ),
    agd_count = if_else(markov_state == "I", 0L,
                        as.integer(round(runif(n(), 3, 20))))
  )

# Simulate four engagement covariates at player-month grain.
# Parameters are fit empirically from the locked dataset's intervention period.
# Baseline engagement covariates are forced to zero, matching real data
# (Kafka events extracted intervention-period-only by design in 00_poc_pull.R).

# Fit zero-inflation proportions from real intervention data
fit_engagement_params <- function(pm_real) {
  pm_int <- pm_real |> dplyr::filter(is_intervention == 1)
  
  fit_hurdle_lognormal <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(list(zero_prop = 1, mu = 0, sigma = 1))
    zero_prop <- mean(x == 0)
    nz <- x[x > 0]
    if (length(nz) < 10) return(list(zero_prop = zero_prop, mu = 0, sigma = 1))
    list(zero_prop = zero_prop,
         mu        = mean(log(nz)),
         sigma     = sd(log(nz)))
  }
  
  fit_zip <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(list(zero_prop = 1, lambda = 0))
    zero_prop <- mean(x == 0)
    # E[X] = (1 - zero_prop) * lambda, so lambda = mean / (1 - zero_prop)
    overall_mean <- mean(x)
    lambda <- if (zero_prop < 1) overall_mean / (1 - zero_prop) else 0
    list(zero_prop = zero_prop, lambda = lambda)
  }
  
  fit_hurdle_beta <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(list(zero_prop = 1, shape1 = 1, shape2 = 1))
    zero_prop <- mean(x == 0)
    nz <- x[x > 0 & x < 1]
    if (length(nz) < 10) return(list(zero_prop = zero_prop, shape1 = 1, shape2 = 1))
    # Method-of-moments fit for Beta(α, β)
    m  <- mean(nz);  v <- var(nz)
    if (v >= m * (1 - m)) {  # invalid for beta — fallback
      return(list(zero_prop = zero_prop, shape1 = 2, shape2 = 5))
    }
    common <- m * (1 - m) / v - 1
    list(zero_prop = zero_prop,
         shape1   = m * common,
         shape2   = (1 - m) * common)
  }
  
  list(
    msg_response_rate = fit_hurdle_beta(pm_int$msg_response_rate),
    n_limit_intents   = fit_zip(pm_int$n_limit_intents),
    limit_page_dwell  = fit_hurdle_lognormal(pm_int$limit_page_dwell),
    rg_support_dwell  = fit_hurdle_lognormal(pm_int$rg_support_dwell),
    protective_share  = if (sum(pm_int$n_limit_intents, na.rm = TRUE) > 0) {
      sum(pm_int$n_protective_intents, na.rm = TRUE) /
        sum(pm_int$n_limit_intents,    na.rm = TRUE)
    } else 0.5
  )
}

ENGAGEMENT_SIM_PARAMS <- fit_engagement_params(pm)
cat("\nFitted ENGAGEMENT_SIM_PARAMS from locked intervention-period data:\n")
str(ENGAGEMENT_SIM_PARAMS)

zip_sample <- function(n, zero_prop, lambda) {
  zeros <- rbinom(n, 1, zero_prop)
  vals  <- rpois(n, lambda)
  ifelse(zeros == 1, 0L, as.integer(pmax(vals, 1L)))
}

hurdle_lognormal_sample <- function(n, zero_prop, mu, sigma) {
  zeros <- rbinom(n, 1, zero_prop)
  vals  <- rlnorm(n, meanlog = mu, sdlog = sigma)
  ifelse(zeros == 1, 0, vals)
}

hurdle_beta_sample <- function(n, zero_prop, shape1, shape2) {
  zeros <- rbinom(n, 1, zero_prop)
  vals  <- rbeta(n, shape1, shape2)
  ifelse(zeros == 1, 0, vals)
}

n_pm <- nrow(player_month_synth)
player_month_synth <- player_month_synth |>
  mutate(
    # Simulate intervention-period values from fitted distributions
    msg_response_rate = hurdle_beta_sample(n_pm,
                                           ENGAGEMENT_SIM_PARAMS$msg_response_rate$zero_prop,
                                           ENGAGEMENT_SIM_PARAMS$msg_response_rate$shape1,
                                           ENGAGEMENT_SIM_PARAMS$msg_response_rate$shape2),
    n_limit_intents   = zip_sample(n_pm,
                                   ENGAGEMENT_SIM_PARAMS$n_limit_intents$zero_prop,
                                   ENGAGEMENT_SIM_PARAMS$n_limit_intents$lambda),
    limit_page_dwell  = hurdle_lognormal_sample(n_pm,
                                                ENGAGEMENT_SIM_PARAMS$limit_page_dwell$zero_prop,
                                                ENGAGEMENT_SIM_PARAMS$limit_page_dwell$mu,
                                                ENGAGEMENT_SIM_PARAMS$limit_page_dwell$sigma),
    rg_support_dwell  = hurdle_lognormal_sample(n_pm,
                                                ENGAGEMENT_SIM_PARAMS$rg_support_dwell$zero_prop,
                                                ENGAGEMENT_SIM_PARAMS$rg_support_dwell$mu,
                                                ENGAGEMENT_SIM_PARAMS$rg_support_dwell$sigma),
    # FORCE baseline to zero — matches real data structure (Kafka events
    # are intervention-period only per 00_poc_pull.R)
    msg_response_rate = if_else(is_intervention == 0L, 0,  msg_response_rate),
    n_limit_intents   = if_else(is_intervention == 0L, 0L, n_limit_intents),
    limit_page_dwell  = if_else(is_intervention == 0L, 0,  limit_page_dwell),
    rg_support_dwell  = if_else(is_intervention == 0L, 0,  rg_support_dwell),
    # Descriptive supplement (FA5b direction breakdown) — NOT engagement covariates
    n_protective_intents = ifelse(n_limit_intents == 0, 0L,
                                  rbinom(n_pm, n_limit_intents,
                                         ENGAGEMENT_SIM_PARAMS$protective_share)),
    n_increase_intents   = n_limit_intents - n_protective_intents
  )

cat("  Engagement covariates simulated (msg_response_rate, n_limit_intents,\n")
cat("    limit_page_dwell, rg_support_dwell + descriptive supplement)\n")
cat("  Non-zero n_limit_intents:  ", sum(player_month_synth$n_limit_intents > 0),
    "of", n_pm, "player-months\n")
cat("  Non-zero limit_page_dwell: ", sum(player_month_synth$limit_page_dwell > 0),
    "of", n_pm, "player-months\n")
cat("  Non-zero rg_support_dwell: ", sum(player_month_synth$rg_support_dwell > 0),
    "of", n_pm, "player-months\n")

cat("  Synthetic player_month rows:", nrow(player_month_synth), "\n")

# -----------------------------------------------------------------------------
# 5. SAVE SYNTHETIC DATASET
# -----------------------------------------------------------------------------

poc_synthetic <- list(
  player_period_synth = player_period_synth,
  player_month_synth  = player_month_synth,
  sim_params = list(
    seed                  = GLOBAL_SEED,
    n_players             = N_players,
    fit_params            = fit_params,
    cor_matrix            = cor_matrix,
    trans_baseline        = trans_baseline,
    trans_intervention    = trans_intervention,
    ya_prop               = ya_prop,
    risk_marginals        = risk_marginals,
    engagement_sim_params = ENGAGEMENT_SIM_PARAMS,
    simulated_at          = Sys.time(),
    r_version             = R.version.string,
    synthpop_version      = as.character(packageVersion("synthpop"))
  )
)

synth_path <- file.path(OUTPUT_DIR, "poc_synthetic.rds")
saveRDS(poc_synthetic, synth_path)

# Hash the synthetic file too (for complete reproducibility)
synth_hash <- digest::digest(synth_path, algo = "sha256", file = TRUE)
cat("\nSaved poc_synthetic.rds\n")
cat("SHA-256:", synth_hash, "\n")

# Append synthetic hash to manifest
manifest_path <- file.path(OUTPUT_DIR, "poc_manifest.txt")
cat("\n## Synthetic dataset\n", file = manifest_path, append = TRUE)
cat("synth_file: poc_synthetic.rds\n", file = manifest_path, append = TRUE)
cat(paste0("synth_sha256: ", synth_hash, "\n"), file = manifest_path, append = TRUE)
cat(paste0("synth_seed: ", GLOBAL_SEED, "\n"), file = manifest_path, append = TRUE)

cat("\n=== 03_poc_simulate.R complete ===\n")
cat("NEXT: Run 04_poc_validate.R to compare synthetic vs real marginals\n")