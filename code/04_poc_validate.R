# =============================================================================
# 04_poc_validate.R
# Purpose : Step 4 — validate the synthetic dataset against the locked
#           real dataset (marginals, risk tiers, transitions, engagement)
# Project : A Safe Bet (ASB) — Proof-of-Concept evaluation.
#           Putting Design into Practice: A Safe Bet — A Proof-of-Concept
#           Evaluation of a Tailored Online Responsible Gambling Framework
# Inputs  : poc_analytical_LOCKED.rds; poc_synthetic.rds
# Outputs : poc_validation_report.txt; poc_validation_plots.pdf
# =============================================================================
# PURPOSE:
#   Compare marginals, distributions, and key structural features between
#   poc_analytical_LOCKED.rds and poc_synthetic.rds.
#   Produces a validation report that should be reviewed before using the
#   synthetic dataset for analysis development.
#
#   KEY QUESTION: Is the synthetic data plausible enough to develop analysis
#   scripts and write the pre-registration? It does NOT need to reproduce
#   exact effect sizes — it needs realistic structure.
#
# OUTPUT:
#   poc_validation_report.txt   – tabular comparison of key statistics
#   poc_validation_plots.pdf    – distribution comparison plots
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

source("00_config.R")

# Resolve namespace conflicts (conflicted may be loaded from .Rprofile)
if (requireNamespace("conflicted", quietly = TRUE)) {
  library(conflicted)
  conflicts_prefer(dplyr::filter)
  conflicts_prefer(dplyr::select)
  conflicts_prefer(dplyr::lag)
}

OUTPUT_DIR <- ROOT_OUT

cat("=== 04_poc_validate.R ===\n")
cat("Started:", format(Sys.time()), "\n\n")

# -----------------------------------------------------------------------------
# 1. LOAD BOTH DATASETS
# -----------------------------------------------------------------------------

poc_real  <- readRDS(file.path(OUTPUT_DIR, "poc_analytical_LOCKED.rds"))
poc_synth <- readRDS(file.path(OUTPUT_DIR, "poc_synthetic.rds"))

pp_real  <- poc_real$player_period
pm_real  <- poc_real$player_month

pp_synth <- poc_synth$player_period_synth
pm_synth <- poc_synth$player_month_synth

cat("Real:      ", nrow(pp_real), "player-period rows,",
    n_distinct(pp_real$participant_id), "players\n")
cat("Synthetic: ", nrow(pp_synth), "player-period rows,",
    n_distinct(pp_synth$participant_id), "players\n\n")

# -----------------------------------------------------------------------------
# 2. COMPARE MARGINALS – PRIMARY OUTCOMES
# -----------------------------------------------------------------------------

compare_outcome <- function(real_vec, synth_vec, label) {
  real_pos  <- real_vec[real_vec > 0 & !is.na(real_vec)]
  synth_pos <- synth_vec[synth_vec > 0 & !is.na(synth_vec)]
  
  data.frame(
    outcome    = label,
    stat       = c("mean", "median", "sd", "p25", "p75", "zero_pct"),
    real_val = c(
      mean(real_vec,  na.rm = TRUE),
      median(real_vec, na.rm = TRUE),
      sd(real_vec,    na.rm = TRUE),
      quantile(real_vec,  0.25, na.rm = TRUE),
      quantile(real_vec,  0.75, na.rm = TRUE),
      mean(real_vec == 0, na.rm = TRUE) * 100
    ),
    synth_val = c(
      mean(synth_vec,  na.rm = TRUE),
      median(synth_vec, na.rm = TRUE),
      sd(synth_vec,    na.rm = TRUE),
      quantile(synth_vec,  0.25, na.rm = TRUE),
      quantile(synth_vec,  0.75, na.rm = TRUE),
      mean(synth_vec == 0, na.rm = TRUE) * 100
    )
  )
}

outcomes <- c("total_deposits", "total_losses", "total_wagers",
              "play_hours_total", "agd_count")

# Compare baseline period
validation_bl <- bind_rows(lapply(outcomes, function(o) {
  real_v  <- pp_real[[o]][pp_real$period == "baseline"]
  synth_v <- pp_synth[[o]][pp_synth$period == "baseline"]
  compare_outcome(real_v, synth_v, paste0(o, "_baseline"))
}))

# Compare intervention period
validation_int <- bind_rows(lapply(outcomes, function(o) {
  real_v  <- pp_real[[o]][pp_real$period == "intervention"]
  synth_v <- pp_synth[[o]][pp_synth$period == "intervention"]
  compare_outcome(real_v, synth_v, paste0(o, "_intervention"))
}))

validation_marginals <- bind_rows(validation_bl, validation_int) |>
  dplyr::mutate(
    pct_diff = abs(real_val - synth_val) / (abs(real_val) + 0.01) * 100,
    flag = if_else(pct_diff > 20, "*** CHECK ***", "OK")
  )

cat("=== MARGINAL COMPARISON ===\n")
print(as.data.frame(validation_marginals))

# -----------------------------------------------------------------------------
# 3. COMPARE RISK TIER DISTRIBUTIONS
# -----------------------------------------------------------------------------

risk_real <- pp_real |>
  dplyr::filter(!is.na(dominant_risk)) |>
  count(period, dominant_risk) |>
  group_by(period) |>
  dplyr::mutate(prop_real = n / sum(n)) |>
  dplyr::select(-n)

risk_synth <- pp_synth |>
  dplyr::filter(!is.na(dominant_risk)) |>
  count(period, dominant_risk) |>
  group_by(period) |>
  dplyr::mutate(prop_synth = n / sum(n)) |>
  dplyr::select(-n)

risk_comparison <- left_join(risk_real, risk_synth, by = c("period", "dominant_risk")) |>
  dplyr::mutate(diff = abs(prop_real - prop_synth))

cat("\n=== RISK TIER COMPARISON ===\n")
print(risk_comparison)

# -----------------------------------------------------------------------------
# 4. COMPARE MARKOV TRANSITION MATRICES
# -----------------------------------------------------------------------------

compute_trans_from_data <- function(pm_data, period_filter) {
  states <- c("L", "M", "H", "I")
  trans  <- matrix(0, nrow = 4, ncol = 4, dimnames = list(states, states))
  
  pm_f <- pm_data |>
    filter(is_intervention == period_filter) |>
    arrange(participant_id, year_month)
  
  for (pid in unique(pm_f$participant_id)) {
    p_states <- pm_f$markov_state[pm_f$participant_id == pid]
    p_states <- na.omit(p_states)
    if (length(p_states) < 2) next
    for (t in seq_len(length(p_states) - 1)) {
      s_from <- p_states[t]; s_to <- p_states[t + 1]
      if (s_from %in% states && s_to %in% states)
        trans[s_from, s_to] <- trans[s_from, s_to] + 1
    }
  }
  sweep(trans, 1, pmax(rowSums(trans), 1), "/")
}

trans_real_int  <- compute_trans_from_data(pm_real,  1)
trans_synth_int <- compute_trans_from_data(pm_synth, 1)

cat("\n=== TRANSITION MATRIX COMPARISON (Intervention period) ===\n")
cat("Real:\n"); print(round(trans_real_int, 3))
cat("Synthetic:\n"); print(round(trans_synth_int, 3))
cat("Max absolute difference:", round(max(abs(trans_real_int - trans_synth_int)), 3), "\n")

# -----------------------------------------------------------------------------
# 4b. ENGAGEMENT COVARIATE COMPARISON (player_month grain)
#     msg_response_rate, n_limit_intents, limit_page_dwell, rg_support_dwell
# -----------------------------------------------------------------------------

cat("\n=== ENGAGEMENT COVARIATE COMPARISON (player_month) ===\n")

# Engagement covariates exist at player_month level
validation_eng_bl <- bind_rows(lapply(ENGAGEMENT_COVARIATES, function(o) {
  real_v  <- pm_real[[o]][pm_real$is_intervention == 0]
  synth_v <- pm_synth[[o]][pm_synth$is_intervention == 0]
  compare_outcome(real_v, synth_v, paste0(o, "_baseline"))
}))

validation_eng_int <- bind_rows(lapply(ENGAGEMENT_COVARIATES, function(o) {
  real_v  <- pm_real[[o]][pm_real$is_intervention == 1]
  synth_v <- pm_synth[[o]][pm_synth$is_intervention == 1]
  compare_outcome(real_v, synth_v, paste0(o, "_intervention"))
}))

validation_engagement <- bind_rows(validation_eng_bl, validation_eng_int) |>
  dplyr::mutate(
    pct_diff = abs(real_val - synth_val) / (abs(real_val) + 0.01) * 100,
    flag = dplyr::case_when(
      stat == "zero_pct" & abs(real_val - synth_val) > 10 ~ "*** ZERO-INFLATION MISMATCH ***",
      pct_diff > 50                                       ~ "*** CHECK ***",
      TRUE                                                ~ "OK"
    )
  )

print(as.data.frame(validation_engagement))

# Specifically flag zero-inflation mismatch (most likely failure mode for the
# fitted ENGAGEMENT_SIM_PARAMS in 03_poc_simulate.R)
zero_mismatches <- validation_engagement |>
  dplyr::filter(stat == "zero_pct", flag != "OK")
if (nrow(zero_mismatches) > 0) {
  cat("\n*** ZERO-INFLATION MISMATCH DETECTED ***\n")
  cat("Update ENGAGEMENT_SIM_PARAMS in 03_poc_simulate.R using these zero proportions:\n")
  for (i in seq_len(nrow(zero_mismatches))) {
    cat(sprintf("  %-40s real=%.1f%%  synth=%.1f%%\n",
                zero_mismatches$outcome[i],
                zero_mismatches$real_val[i],
                zero_mismatches$synth_val[i]))
  }
  cat("Recommend re-running 03_poc_simulate.R after recalibration if synthetic\n")
  cat("data will be used for non-trivial development of analysis scripts.\n")
}

# -----------------------------------------------------------------------------
# 5. DISTRIBUTION PLOTS
# -----------------------------------------------------------------------------

make_dist_plot <- function(real_vec, synth_vec, label) {
  df <- bind_rows(
    data.frame(value = log1p(real_vec),  source = "Real"),
    data.frame(value = log1p(synth_vec), source = "Synthetic")
  )
  ggplot(df, aes(x = value, fill = source)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c("Real" = "#1f77b4", "Synthetic" = "#ff7f0e")) +
    labs(title = label, x = "log(1 + value)", y = "Density") +
    theme_minimal(base_size = 10) +
    theme(legend.position = "bottom")
}

plots_outcomes <- list(
  make_dist_plot(
    pp_real$total_deposits[pp_real$period == "baseline"],
    pp_synth$total_deposits[pp_synth$period == "baseline"],
    "Total deposit_amount – Baseline"
  ),
  make_dist_plot(
    pp_real$total_deposits[pp_real$period == "intervention"],
    pp_synth$total_deposits[pp_synth$period == "intervention"],
    "Total deposit_amount – Intervention"
  ),
  make_dist_plot(
    pp_real$total_losses[pp_real$period == "baseline"],
    pp_synth$total_losses[pp_synth$period == "baseline"],
    "Total net_loss – Baseline"
  ),
  make_dist_plot(
    pp_real$agd_count[pp_real$period == "baseline"],
    pp_synth$agd_count[pp_synth$period == "baseline"],
    "AGD count – Baseline"
  )
)

# Engagement covariate distribution plots (intervention period)
plots_engagement <- lapply(ENGAGEMENT_COVARIATES, function(o) {
  make_dist_plot(
    pm_real[[o]][pm_real$is_intervention == 1],
    pm_synth[[o]][pm_synth$is_intervention == 1],
    paste0(o, " – Intervention")
  )
})

plots <- c(plots_outcomes, plots_engagement)

pdf(file.path(OUTPUT_DIR, "poc_validation_plots.pdf"), width = 12, height = 14)
patchwork::wrap_plots(plots, ncol = 2)
dev.off()

cat("\nValidation plots saved to poc_validation_plots.pdf\n")

# -----------------------------------------------------------------------------
# 6. WRITE VALIDATION REPORT
# -----------------------------------------------------------------------------

report_path <- file.path(OUTPUT_DIR, "poc_validation_report.txt")

report_lines <- c(
  "# POC Synthetic Dataset Validation Report",
  paste("# Generated:", format(Sys.time())),
  "",
  "## Sample sizes",
  paste("Real N players:      ", n_distinct(pp_real$participant_id)),
  paste("Synthetic N players: ", n_distinct(pp_synth$participant_id)),
  "",
  "## Marginal comparison",
  "  (pct_diff > 20% flagged for review)",
  "",
  capture.output(print(as.data.frame(validation_marginals))),
  "",
  "## Risk tier comparison",
  capture.output(print(risk_comparison)),
  "",
  "## Transition matrix (intervention period)",
  "Real:", capture.output(print(round(trans_real_int, 3))),
  "Synthetic:", capture.output(print(round(trans_synth_int, 3))),
  paste("Max absolute difference:", round(max(abs(trans_real_int - trans_synth_int)), 3)),
  "",
  "## Engagement covariate marginals",
  capture.output(print(as.data.frame(validation_engagement))),
  "",
  "## Verdict",
  "Review flagged rows (*** CHECK *** or *** ZERO-INFLATION MISMATCH ***)",
  "before using synthetic data for development.",
  "If outcome distributions are substantially different, re-fit synthpop",
  "with method='cart'. If engagement covariate distributions differ, update",
  "ENGAGEMENT_SIM_PARAMS in 03_poc_simulate.R using empirical zero_prop",
  "and lambda/mu/sigma values from the locked dataset.",
  "",
  "The synthetic dataset is fit for purpose if:",
  "  1. Risk tier proportions match within +/- 5 percentage points",
  "  2. Outcome medians match within +/- 30% (log scale)",
  "  3. Markov transition matrices differ by < 0.05 in each cell",
  "  4. Zero-inflation proportions are within +/- 5 percentage points",
  "  5. Engagement covariate zero-inflation matches within +/- 10 percentage points",
  "  6. Engagement covariate medians of non-zero values match within +/- 50%"
)

writeLines(report_lines, report_path)
cat("Validation report written to:", report_path, "\n")

cat("\n=== 04_poc_validate.R complete ===\n")
cat("Review poc_validation_report.txt before proceeding to analysis scripts.\n")
cat("NEXT: Develop analysis scripts (06+) using poc_synthetic.rds\n")