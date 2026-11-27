# =============================================================================
# 05_poc_descriptives.R
# Purpose : Descriptives, feasibility outcomes (FA1-FA6), and sample
#           characterisation
# Project : A Safe Bet (ASB) — Proof-of-Concept evaluation.
#           Putting Design into Practice: A Safe Bet — A Proof-of-Concept
#           Evaluation of a Tailored Online Responsible Gambling Framework
# Inputs  : poc_synthetic.rds or poc_analytical_LOCKED.rds (via 00_config.R);
#           poc_tool_assignments.rds
# Outputs : results/05_descriptives/descriptives_output.rds
# Flags   : USE_SYNTHETIC — TRUE runs on poc_synthetic.rds (pre-registration
#           phase); FALSE runs on poc_analytical_LOCKED.rds
#           (post-registration). Currently FALSE.
# =============================================================================
# PRE-REG:  OSF [URL to be added]
# =============================================================================
# Addresses: FA1-FA6 (feasibility outcomes; prereg 6.1) plus engagement
# covariate descriptives (prereg 5.4 / 6.6). All descriptive; no significance
# thresholds (prereg 6.1).
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

# Toggle here — synthetic during pre-reg, locked after OSF submission
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

tools <- readRDS(FILE_POC_TOOLS)

dir.create("results/05_descriptives", recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# FA1: Proportion of 6,000 players active during intervention
# =============================================================================

cat("\n--- FA1: Retention ---\n")

n_selected <- N_SELECTED

# FA1 numerator (prereg 6.1): count of the 6,000 selected players with
# >=1 AGD in intervention. The locked dataset's sample_flow holds this
# computed over the full enrolled cohort. On synthetic data sample_flow
# is absent and we fall back to deriving from pp (analytical sample only),
# which omits the 192 below-threshold players and so under-reports.
if (!is.null(dat$sample_flow$n_active_intervention_full)) {
  n_active_any <- dat$sample_flow$n_active_intervention_full
  source_note  <- "full 6,000 cohort"
} else {
  n_active_any <- sum(pp$agd_count[pp$period == "intervention"] > 0)
  source_note  <- "analytical sample only; 192 below-threshold players omitted"
}
pct_active <- n_active_any / n_selected * 100

cat("N with >= 1 AGD in intervention:", n_active_any,
    sprintf("(%.1f%% of %d; %s)\n", pct_active, n_selected, source_note))

# =============================================================================
# FA2: 6-month completion rate
# =============================================================================

cat("\n--- FA2: 6-month completion ---\n")

n_active_6m <- pm |>
  filter(year_month == floor_date(INTERVENTION_END, "month")) |>
  summarise(n = sum(agd_count > 0)) |>
  pull(n)

# FA2 headline denominator per prereg 6.1 / Table 4: active participants
# (>= 1 intervention AGD), computed here within the analytical sample carried
# in pm (the full-cohort FA1 numerator adds below-threshold participants and
# is not available at pm grain). The proportion of the 6,000 selected is
# retained as context.
n_active_participants <- pm |>
  filter(is_intervention == 1) |>
  group_by(participant_id) |>
  summarise(any_active = any(agd_count > 0), .groups = "drop") |>
  summarise(n = sum(any_active)) |>
  pull(n)

pct_6m_active   <- n_active_6m / n_active_participants * 100
pct_6m_selected <- n_active_6m / n_selected * 100
cat("Active in March 2025:", n_active_6m,
    sprintf("(%.1f%% of %d active participants [FA2 headline]; %.1f%% of %d selected)\n",
            pct_6m_active, n_active_participants, pct_6m_selected, n_selected))

# Active months per participant, computed over the full analytical sample
# carried in pm (zero-active participants included as zeros; prereg 6.1 FA2)
active_months <- pm |>
  filter(is_intervention == 1) |>
  group_by(participant_id) |>
  summarise(n_active_months = sum(agd_count > 0), .groups = "drop")
cat("Mean active intervention months:", round(mean(active_months$n_active_months), 1), "\n")

n_active_all6 <- sum(active_months$n_active_months == 6)
cat("Active in ALL six intervention months:", n_active_all6,
    sprintf("(%.1f%% of %d engaged cohort [FA2 headline]; %.1f%% of analytical sample; %.1f%% of %d selected)\n",
            100 * n_active_all6 / n_active_participants, n_active_participants,
            100 * n_active_all6 / nrow(active_months),
            100 * n_active_all6 / n_selected, n_selected))

# Engagement categories
engagement_cats <- active_months |>
  mutate(category = case_when(
    n_active_months == 6 ~ "Continuous (6/6)",
    n_active_months == 5 ~ "Highly engaged (5/6)",
    n_active_months %in% 3:4 ~ "Moderately engaged (3-4/6)",
    n_active_months %in% 1:2 ~ "Low engagement (1-2/6)",
    n_active_months == 0 ~ "Never active (0/6)"
  )) |>
  count(category) |>
  mutate(pct = round(n / sum(n) * 100, 1))

cat("\nEngagement categories:\n")
print(engagement_cats)

# =============================================================================
# FA3: Tool delivery fidelity
# =============================================================================

cat("\n--- FA3: Tool delivery ---\n")

tool_summary <- tools |>
  group_by(assignment_date, signal_class) |>
  summarise(n = n(), .groups = "drop") |>
  pivot_wider(names_from = signal_class, values_from = n, values_fill = 0)

cat("Tool assignment counts by month and signal_class:\n")
print(tool_summary)

pct_ghost <- sum(tools$signal_class == "GHOST") / nrow(tools) * 100
cat(sprintf("\nGhost tool proportion: %.1f%%\n", pct_ghost))

# =============================================================================
# FA4: Message response rate
# =============================================================================

# Young Adult exclusion for FA4/FA5 (prereg 4.4): the n = 168 participants
# under 25 at offer date (ages 18-24) received the system-enforced regulatory
# YA override of the Deposit and Loss Limits (YA-coded tool variants), making
# their limit and message engagement non-comparable with risk-tailored
# assignments. They remain in FA1-FA3, FA6, and the sample-characteristics
# table by design. Real-data branch only (the synthetic player_month does
# not carry the flag at this grain).
if (!USE_SYNTHETIC) {
  ya_excluded_ids <- pp |>
    dplyr::filter(young_adult_flag == 1) |>
    dplyr::pull(participant_id) |>
    unique()
  cat("\nFA4/FA5 Young Adult exclusion (prereg 4.4): excluding",
      length(ya_excluded_ids), "participants\n")
}

cat("\n--- FA4: Message response rate ---\n")

if (USE_SYNTHETIC) {
  # Synthetic player_month includes msg_response_rate and n_limit_intents
  # directly (simulated in 03_poc_simulate.R)
  msg_monthly <- pm |>
    dplyr::filter(is_intervention == 1) |>
    dplyr::group_by(month_number) |>
    dplyr::summarise(
      mean_msg_response_rate = mean(msg_response_rate, na.rm = TRUE),
      pct_with_response      = mean(msg_response_rate > 0, na.rm = TRUE) * 100,
      mean_n_limit_intents   = mean(n_limit_intents,   na.rm = TRUE),
      pct_with_intent        = mean(n_limit_intents > 0, na.rm = TRUE) * 100,
      .groups = "drop"
    )
  cat("Synthetic msg_response_rate and n_limit_intents by month:\n")
  print(msg_monthly)
} else {
  msg_monthly <- dat$engagement$msg_summary |>
    filter(!participant_id %in% ya_excluded_ids) |>   # prereg 4.4 YA exclusion
    filter(year_month >= INTERVENTION_START) |>
    group_by(year_month) |>
    summarise(
      n_players        = n(),
      mean_sent        = mean(n_messages_sent,    na.rm = TRUE),
      mean_responses   = mean(n_responses,        na.rm = TRUE),
      mean_rg_clicks   = mean(n_rg_button_clicks, na.rm = TRUE),
      # Macro-average per prereg 6.1 FA4: mean of participant-month rates
      # within the month; participant-months with no online_send event are
      # excluded from the rate denominator (prereg 6.1, v36). The same FA4
      # rule is applied to every reported rate in this table.
      response_rate    = mean(ifelse(n_messages_sent > 0,
                                     pmin(n_responses / n_messages_sent, 1),
                                     NA_real_), na.rm = TRUE),
      rg_click_rate    = mean(ifelse(n_messages_sent > 0,
                                     pmin(n_rg_button_clicks / n_messages_sent, 1),
                                     NA_real_), na.rm = TRUE),
      .groups = "drop"
    )
  cat("Message response rate by month:\n")
  print(msg_monthly)
}

# =============================================================================
# FA5: Threshold hits and limit modification intent
# =============================================================================

cat("\n--- FA5: Tool engagement ---\n")

if (USE_SYNTHETIC) {
  # Synthetic includes n_limit_intents and direction breakdown
  intent_monthly <- pm |>
    dplyr::filter(is_intervention == 1) |>
    dplyr::group_by(month_number) |>
    dplyr::summarise(
      n_players_any_intent      = sum(n_limit_intents > 0),
      mean_n_limit_intents      = mean(n_limit_intents,      na.rm = TRUE),
      mean_protective_intents   = mean(n_protective_intents, na.rm = TRUE),
      mean_increase_intents     = mean(n_increase_intents,   na.rm = TRUE),
      .groups = "drop"
    )
  cat("Synthetic limit intent counts by intervention month:\n")
  print(intent_monthly)
  cat("  (Threshold hits not simulated; available only on real data.)\n")
} else {
  threshold_monthly <- dat$engagement$threshold_summary |>
    filter(!participant_id %in% ya_excluded_ids) |>   # prereg 4.4 YA exclusion
    filter(year_month >= INTERVENTION_START) |>
    group_by(year_month, limit_type) |>
    summarise(
      n_players_hit    = n_distinct(participant_id),
      n_hits_100pct    = sum(hit_100pct_any, na.rm = TRUE),
      .groups = "drop"
    )
  cat("Threshold hit rate by month and tool:\n")
  print(threshold_monthly)
  
  intent_monthly <- dat$engagement$intent_summary |>
    filter(!participant_id %in% ya_excluded_ids) |>   # prereg 4.4 YA exclusion
    filter(year_month >= INTERVENTION_START) |>
    group_by(year_month) |>
    summarise(
      n_players_any_intent      = sum(n_limit_intents > 0),
      mean_n_limit_intents      = mean(n_limit_intents,      na.rm = TRUE),
      mean_protective_intents   = mean(n_protective_intents, na.rm = TRUE),
      mean_increase_intents     = mean(n_increase_intents,   na.rm = TRUE),
      .groups = "drop"
    )
  cat("\nLimit intent by month (n_limit_intents = primary engagement covariate;\n")
  cat("  protective/increase = descriptive supplement, system-driven excluded):\n")
  print(intent_monthly)
}

# =============================================================================
# ENGAGEMENT COVARIATE DESCRIPTIVES (prereg 5.4 / 6.6) - descriptive
# =============================================================================

cat("\n--- engagement covariate descriptives ---\n")

eng_cov_summary <- pm |>
  dplyr::mutate(period = if_else(is_intervention == 1, "intervention", "baseline")) |>
  dplyr::group_by(period) |>
  dplyr::summarise(
    n_player_months  = dplyr::n(),
    # msg_response_rate (proportion in [0, 1])
    msg_resp_mean    = mean(msg_response_rate,   na.rm = TRUE),
    msg_resp_median  = median(msg_response_rate, na.rm = TRUE),
    msg_resp_pct_nz  = mean(msg_response_rate > 0, na.rm = TRUE) * 100,
    # n_limit_intents (count)
    n_lim_int_mean   = mean(n_limit_intents,     na.rm = TRUE),
    n_lim_int_max    = max(n_limit_intents,      na.rm = TRUE),
    n_lim_int_pct_nz = mean(n_limit_intents > 0, na.rm = TRUE) * 100,
    # limit_page_dwell (seconds)
    lim_dwell_mean   = mean(limit_page_dwell,    na.rm = TRUE),
    lim_dwell_pct_nz = mean(limit_page_dwell > 0, na.rm = TRUE) * 100,
    # rg_support_dwell (seconds)
    rg_dwell_mean    = mean(rg_support_dwell,    na.rm = TRUE),
    rg_dwell_pct_nz  = mean(rg_support_dwell > 0, na.rm = TRUE) * 100,
    .groups = "drop"
  )

cat("Engagement covariate descriptives (means and % non-zero by period):\n")
print(t(eng_cov_summary))

# =============================================================================
# FA6: AGD change (feasibility / churn check — not a confirmatory outcome)
# =============================================================================

cat("\n--- FA6: AGD change check ---\n")

agd_summary <- pp |>
  group_by(period) |>
  summarise(
    mean_agd   = mean(agd_count, na.rm = TRUE),
    median_agd = median(agd_count, na.rm = TRUE),
    sd_agd     = sd(agd_count, na.rm = TRUE),
    .groups = "drop"
  )

cat("AGD by period:\n")
print(agd_summary)

# Within-person mean AGD difference (prereg 6.1 FA6): baseline minus
# intervention per participant, reported with a 95% confidence interval
agd_diff <- pp |>
  dplyr::select(participant_id, period, agd_count) |>
  tidyr::pivot_wider(names_from = period, values_from = agd_count) |>
  dplyr::mutate(d = baseline - intervention)
n_d    <- sum(!is.na(agd_diff$d))
mean_d <- mean(agd_diff$d, na.rm = TRUE)
se_d   <- sd(agd_diff$d, na.rm = TRUE) / sqrt(n_d)
ci_d   <- mean_d + c(-1, 1) * qt(0.975, df = n_d - 1) * se_d
cat(sprintf(
  "Within-person AGD difference (baseline - intervention): mean = %.2f, 95%% CI [%.2f, %.2f], n = %d\n",
  mean_d, ci_d[1], ci_d[2], n_d))

cat("Note: AGD is a feasibility/churn check, not a primary confirmatory outcome.\n")

# =============================================================================
# SAMPLE CHARACTERISTICS TABLE
# =============================================================================

cat("\n--- Sample characteristics ---\n")

# Note: age_years and engaged_cohort are carried into the synthetic
# player_period when present in the locked data (03_poc_simulate.R includes
# them via any_of()); the guards below cover older synthetic files without them.
pp_bl_data <- pp |> dplyr::filter(period == "baseline")

char_bl <- pp_bl_data |>
  dplyr::summarise(
    N                = n(),
    # age_years only available on real data
    mean_age         = if ("age_years" %in% names(pp_bl_data)) mean(age_years, na.rm=TRUE) else NA_real_,
    sd_age           = if ("age_years" %in% names(pp_bl_data)) sd(age_years, na.rm=TRUE)   else NA_real_,
    pct_ya           = mean(young_adult_flag, na.rm = TRUE) * 100,
    median_deposits  = median(total_deposits, na.rm = TRUE),
    iqr_deposits_lo  = quantile(total_deposits, 0.25, na.rm = TRUE),
    iqr_deposits_hi  = quantile(total_deposits, 0.75, na.rm = TRUE),
    median_losses    = median(total_losses, na.rm = TRUE),
    iqr_losses_lo    = quantile(total_losses, 0.25, na.rm = TRUE),
    iqr_losses_hi    = quantile(total_losses, 0.75, na.rm = TRUE),
    median_agd       = median(agd_count, na.rm = TRUE),
    mean_agd         = mean(agd_count, na.rm = TRUE),
    sd_agd           = sd(agd_count, na.rm = TRUE),
    pct_low_risk     = mean(dominant_risk == "low",    na.rm = TRUE) * 100,
    pct_medium_risk  = mean(dominant_risk == "medium", na.rm = TRUE) * 100,
    pct_high_risk    = mean(dominant_risk == "high",   na.rm = TRUE) * 100
  )

cat("Baseline characteristics:\n")
print(t(char_bl))

saveRDS(list(
  agd_summary      = agd_summary,
  char_bl          = char_bl,
  engagement_cats  = engagement_cats,
  tool_summary     = tool_summary,
  eng_cov_summary  = eng_cov_summary
), "results/05_descriptives/descriptives_output.rds")

cat("\n=== 05_poc_descriptives.R complete ===\n")