# =============================================================================
# 05b_poc_descriptives_followup.R
# Purpose : Follow-up to 05_poc_descriptives.R — completes the FA4/FA5
#           reporting that the 16 July 2026 run left truncated or
#           unimplemented, and expands the sample-characteristics table
#           for the manuscript (Table 1).
# Project : A Safe Bet (ASB) — Proof-of-Concept evaluation.
# Inputs  : poc_analytical_LOCKED.rds (via 00_config.R); poc_tool_assignments.rds;
#           poc_raw.rds (inter-session interval block only)
# Outputs : results/05_descriptives/descriptives_followup.rds
# Log     : run as
#           Rscript 05b_poc_descriptives_followup.R 2>&1 | tee logs/05b_real_YYYYMMDD.log
# =============================================================================
# STATUS: POST-REGISTRATION follow-up script (written 17 July 2026, after the
# 05 real-data run). It computes NO directional or hypothesis-test statistic.
# Everything here is descriptive (prereg 6.1: no significance thresholds).
# Blocks 3b, 4 and 5 implement items pre-registered as post-registration
# computations (prereg 10.4 item 4: FA4 stratified reporting, FA5a
# threshold-level breakdown, inter-session interval).
#
# CONTENTS
#   1. Full-width reprints of the FA4/FA5 tables the 05 log truncated
#      (msg_monthly incl. rg_click_rate; threshold_monthly all rows;
#      intent_monthly incl. mean_increase_intents)
#   2. FA5a as registered: monthly PROPORTIONS by tool, DL/LL/BL primary
#      scope, with the two candidate denominators printed side by side
#      (decision D7 in the results master: which one enters the manuscript)
#   3. FA5a supplement: non-registered tool types (FORCED_LOSS_LIMIT,
#      LOGIN_TIME_LIMIT) reported separately (decision D6)
#   3b. FA5a threshold-level breakdown (25/50/75/80%) — runs only if the
#      locked engagement summary carries a threshold-level field; otherwise
#      prints exactly what 01_poc_build.R must add (post-registration item)
#   4. FA4 stratified reporting scaffold — diagnostic of what the locked
#      data can support; computes nothing unless a message-type-grain
#      object exists (post-registration item)
#   5. Inter-session interval (prereg 5.3; post-registration item)
#   6. Expanded sample characteristics: baseline AND intervention, analytical
#      sample / engaged cohort / YA subgroup, all five outcome families
#      (feeds manuscript Table 1) + full sample_flow print
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

# Print helper: never allow tibble truncation in this script's log
print_full <- function(x, label = NULL) {
  if (!is.null(label)) cat(label, "\n")
  print(as.data.frame(x), row.names = FALSE)
}

# This is a post-lock, real-data-only follow-up; there is no synthetic branch.
dat <- readRDS(FILE_POC_LOCKED)
pp  <- dat$player_period
pm  <- dat$player_month
cat("Running on LOCKED real data\n")
stamp(sprintf("05b started; player_period %d rows, player_month %d rows",
              nrow(pp), nrow(pm)))

dir.create("results/05_descriptives", recursive = TRUE, showWarnings = FALSE)

# YA exclusion for FA4/FA5, identical to 05_poc_descriptives.R (prereg 4.4)
ya_excluded_ids <- pp |>
  dplyr::filter(young_adult_flag == 1) |>
  dplyr::pull(participant_id) |>
  unique()
cat("FA4/FA5 Young Adult exclusion (prereg 4.4): excluding",
    length(ya_excluded_ids), "participants\n")

n_nonya_analytical <- dplyr::n_distinct(pp$participant_id) - length(ya_excluded_ids)
cat("Non-YA analytical sample (fixed denominator candidate):", n_nonya_analytical, "\n")

# -----------------------------------------------------------------------------
# 0. SCHEMA DIAGNOSTIC — one-time print of what the locked engagement
#    summaries actually contain (drives blocks 3b and 4)
# -----------------------------------------------------------------------------
cat("\n=== 0. Engagement-object schema diagnostic ===\n")
for (nm in names(dat$engagement)) {
  obj <- dat$engagement[[nm]]
  if (is.data.frame(obj)) {
    cat(sprintf("  $engagement$%s: %d rows; columns: %s\n",
                nm, nrow(obj), paste(names(obj), collapse = ", ")))
  } else {
    cat(sprintf("  $engagement$%s: class %s\n", nm, paste(class(obj), collapse = "/")))
  }
}

# =============================================================================
# 1. FULL-WIDTH REPRINTS (identical pipelines to 05_poc_descriptives.R;
#    only the printing differs — no truncation)
# =============================================================================

cat("\n=== 1a. FA4 message response rate by month — FULL TABLE ===\n")
msg_monthly <- dat$engagement$msg_summary |>
  filter(!participant_id %in% ya_excluded_ids) |>
  filter(year_month >= INTERVENTION_START) |>
  group_by(year_month) |>
  summarise(
    n_players        = n(),
    mean_sent        = mean(n_messages_sent,    na.rm = TRUE),
    mean_responses   = mean(n_responses,        na.rm = TRUE),
    mean_rg_clicks   = mean(n_rg_button_clicks, na.rm = TRUE),
    response_rate    = mean(ifelse(n_messages_sent > 0,
                                   pmin(n_responses / n_messages_sent, 1),
                                   NA_real_), na.rm = TRUE),
    rg_click_rate    = mean(ifelse(n_messages_sent > 0,
                                   pmin(n_rg_button_clicks / n_messages_sent, 1),
                                   NA_real_), na.rm = TRUE),
    .groups = "drop"
  )
print_full(msg_monthly)

cat("\n=== 1b. FA5a threshold hits by month and tool — FULL TABLE (all rows) ===\n")
threshold_monthly <- dat$engagement$threshold_summary |>
  filter(!participant_id %in% ya_excluded_ids) |>
  filter(year_month >= INTERVENTION_START) |>
  group_by(year_month, limit_type) |>
  summarise(
    n_players_hit    = n_distinct(participant_id),
    n_hits_100pct    = sum(hit_100pct_any, na.rm = TRUE),
    .groups = "drop"
  )
print_full(threshold_monthly)

cat("\n=== 1c. FA5b limit intent by month — FULL TABLE (all columns) ===\n")
intent_monthly <- dat$engagement$intent_summary |>
  filter(!participant_id %in% ya_excluded_ids) |>
  filter(year_month >= INTERVENTION_START) |>
  group_by(year_month) |>
  summarise(
    n_rows_in_frame           = n(),   # denominator of the means below (D7)
    n_players_any_intent      = sum(n_limit_intents > 0),
    mean_n_limit_intents      = mean(n_limit_intents,      na.rm = TRUE),
    mean_protective_intents   = mean(n_protective_intents, na.rm = TRUE),
    mean_increase_intents     = mean(n_increase_intents,   na.rm = TRUE),
    .groups = "drop"
  )
print_full(intent_monthly)
cat("NOTE: n_rows_in_frame documents which participant-months the means average\n")
cat("over (results-master decision D7). If it equals the non-YA analytical\n")
cat("sample each month, the means are per-participant; if smaller, they are\n")
cat("per participant-with-records.\n")

# FA5b by tool, if the intent summary carries a tool/limit_type column
cat("\n=== 1d. FA5b by tool (DL/LL/BL) — registered breakdown ===\n")
tool_col <- intersect(c("limit_type", "tool", "tool_type"), names(dat$engagement$intent_summary))
if (length(tool_col) >= 1) {
  intent_by_tool <- dat$engagement$intent_summary |>
    filter(!participant_id %in% ya_excluded_ids) |>
    filter(year_month >= INTERVENTION_START) |>
    group_by(year_month, .data[[tool_col[1]]]) |>
    summarise(
      n_players_any_intent = sum(n_limit_intents > 0),
      mean_n_limit_intents = mean(n_limit_intents, na.rm = TRUE),
      .groups = "drop"
    )
  print_full(intent_by_tool)
} else {
  intent_by_tool <- NULL
  cat("NOT AVAILABLE: intent_summary has no tool/limit_type column — the\n")
  cat("participant-month intent counts were aggregated across tools at build\n")
  cat("time. The registered by-tool FA5b split (prereg 6.1) needs 01_poc_build.R\n")
  cat("to also save a participant-month x limit_type intent summary.\n")
}

# =============================================================================
# 2. FA5a AS REGISTERED — monthly PROPORTIONS by tool, DL/LL/BL primary scope
#    Two candidate denominators printed side by side (decision D7):
#      pct_analytical_nonya : fixed non-YA analytical sample
#      pct_active_nonya     : non-YA participants with >=1 AGD in that month
# =============================================================================

cat("\n=== 2. FA5a registered proportions (primary scope: DL / LL / BL) ===\n")

REGISTERED_LIMIT_TOOLS <- c("DEPOSIT_LIMIT", "LOSS_LIMIT", "BET_LIMIT")

active_nonya_by_month <- pm |>
  filter(is_intervention == 1, !participant_id %in% ya_excluded_ids) |>
  group_by(year_month) |>
  summarise(n_active_nonya = sum(agd_count > 0), .groups = "drop")

threshold_rates <- threshold_monthly |>
  filter(limit_type %in% REGISTERED_LIMIT_TOOLS) |>
  left_join(active_nonya_by_month, by = "year_month") |>
  mutate(
    pct_analytical_nonya = 100 * n_players_hit / n_nonya_analytical,
    pct_active_nonya     = 100 * n_players_hit / n_active_nonya
  )
print_full(threshold_rates)
cat(sprintf("Denominators: non-YA analytical sample = %d (fixed); n_active_nonya varies by month.\n",
            n_nonya_analytical))
cat("Which denominator enters the manuscript is decision D7 (results master, Annex A).\n")

# =============================================================================
# 3. FA5a SUPPLEMENT — tool types outside the registered DL/LL/BL set
#    (decision D6: exclude from FA5a or report as a labelled supplement)
# =============================================================================

cat("\n=== 3. FA5a supplement — non-registered tool types ===\n")
threshold_other <- threshold_monthly |>
  filter(!limit_type %in% REGISTERED_LIMIT_TOOLS)
if (nrow(threshold_other) > 0) {
  print_full(threshold_other)
  cat("These events sit outside the registered FA5a scope (prereg 6.1 names\n")
  cat("DL, LL, BL only). Reported separately pending decision D6.\n")
} else {
  cat("None found.\n")
}

# =============================================================================
# 3b. FA5a THRESHOLD-LEVEL BREAKDOWN (25/50/75/80%) — post-registration item
#     (prereg 10.4 item 4). Runs only if the locked summary carries a
#     threshold-level field; otherwise states exactly what is missing.
# =============================================================================

cat("\n=== 3b. FA5a threshold-level breakdown (25/50/75/80%) ===\n")
thr_col <- intersect(c("threshold", "threshold_level", "threshold_pct", "max_threshold"),
                     names(dat$engagement$threshold_summary))
if (length(thr_col) >= 1) {
  threshold_by_level <- dat$engagement$threshold_summary |>
    filter(!participant_id %in% ya_excluded_ids) |>
    filter(year_month >= INTERVENTION_START) |>
    group_by(year_month, limit_type, .data[[thr_col[1]]]) |>
    summarise(n_players_hit = n_distinct(participant_id), .groups = "drop")
  print_full(threshold_by_level)
  cat("Primary thresholds per prereg 6.1: 80% for DL; 25% for LL and BL.\n")
} else {
  threshold_by_level <- NULL
  cat("NOT AVAILABLE: threshold_summary carries no threshold-level column —\n")
  cat("the build aggregated across levels. To compute the registered breakdown,\n")
  cat("01_poc_build.R must save a participant-month x limit_type x threshold\n")
  cat("summary from evt_limit_threshold_reached (payload field: threshold).\n")
  cat("This remains open as results-master decision D4.\n")
}

# =============================================================================
# 4. FA4 STRATIFIED REPORTING SCAFFOLD — post-registration item (prereg 10.4
#    item 4): rates by message type x tool x baseline risk (roster
#    asb_risk_level_at_selection) x content variant (PPF/EPF), each as the
#    macro-average of participant-month rates within the stratum.
#    The locked msg_summary is participant-month grain with no message-type
#    fields, so this block is a diagnostic: it computes the stratification
#    only if a message-type-grain object exists.
# =============================================================================

cat("\n=== 4. FA4 stratified reporting — capability check ===\n")
msg_type_cols  <- c("message_stage", "template_id", "kafka_template_id",
                    "tool_name", "content_type", "message_type")
candidate_objs <- names(dat$engagement)[vapply(names(dat$engagement), function(nm) {
  obj <- dat$engagement[[nm]]
  is.data.frame(obj) && length(intersect(msg_type_cols, names(obj))) >= 1
}, logical(1))]

has_roster_risk <- "asb_risk_level_at_selection" %in% names(pp) ||
  ("roster" %in% names(dat) && is.data.frame(dat$roster) &&
     "asb_risk_level_at_selection" %in% names(dat$roster))

cat("Message-type-grain objects found:",
    if (length(candidate_objs)) paste(candidate_objs, collapse = ", ") else "NONE", "\n")
cat("Roster risk level (asb_risk_level_at_selection) available:",
    ifelse(has_roster_risk, "YES", "NO"), "\n")

if (length(candidate_objs) == 0) {
  cat("\nCANNOT COMPUTE HERE. The registered FA4 stratification needs a\n")
  cat("participant-month x message-type summary that the locked file does not\n")
  cat("carry. Required addition to 01_poc_build.R (no change to any registered\n")
  cat("definition; extraction only):\n")
  cat("  - evt_online_message events joined to the message_templates codebook\n")
  cat("    on kafka_template_id, keeping: message_stage, tool_name, content_type\n")
  cat("    (PPF/EPF/unified), phase (online_send / online_submit)\n")
  cat("  - the seven registered ASB message types identified per prereg 6.1 FA4\n")
  cat("    (incl. the breach-100% NULL-template match to evt_limit_threshold_reached\n")
  cat("    within +/-5 s), legacy/BetBuddy/registration messages excluded\n")
  cat("  - aggregated to participant-month x message type x tool x content variant\n")
  cat("    send and submit counts, joined to asb_risk_level_at_selection\n")
  cat("  - YA participants excluded as in FA4 (prereg 4.4)\n")
  cat("Rates are then macro-averages of participant-month rates within each\n")
  cat("stratum; participant-months with no send in a stratum are excluded from\n")
  cat("that stratum's denominator (prereg 6.1). Tracked as decision D3.\n")
  fa4_stratified <- NULL
} else {
  cat("\nA message-type-grain object exists — implement the stratification against\n")
  cat("it (extend this block once its schema is confirmed in the log above).\n")
  fa4_stratified <- NULL
}

# =============================================================================
# 5. INTER-SESSION INTERVAL (prereg 5.3; post-registration computation)
#    Mean days between consecutive AGDs within the intervention period.
#    Uses poc_raw$daily_stats (participant_id, evaluation_date, is_agd).
# =============================================================================

cat("\n=== 5. Inter-session interval (intervention period) ===\n")
if (file.exists(FILE_POC_RAW)) {
  stamp("loading poc_raw.rds (large; needed for daily AGD dates)")
  poc_raw     <- readRDS(FILE_POC_RAW)
  daily_stats <- poc_raw$daily_stats
  rm(poc_raw); invisible(gc(verbose = FALSE))
  stamp("poc_raw released; daily_stats retained")

  stopifnot(all(c("participant_id", "evaluation_date", "is_agd") %in% names(daily_stats)))

  isi <- daily_stats |>
    filter(participant_id %in% unique(pp$participant_id),
           is_agd == 1,
           evaluation_date >= INTERVENTION_START,
           evaluation_date <= INTERVENTION_END) |>
    arrange(participant_id, evaluation_date) |>
    group_by(participant_id) |>
    summarise(
      n_agd    = n(),
      mean_gap = if (n() >= 2) mean(as.numeric(diff(evaluation_date))) else NA_real_,
      .groups = "drop"
    )
  rm(daily_stats); invisible(gc(verbose = FALSE))

  isi_valid <- isi |> filter(!is.na(mean_gap))
  isi_summary <- isi_valid |>
    summarise(
      n_participants = n(),
      mean_of_means  = mean(mean_gap),
      sd_of_means    = sd(mean_gap),
      median         = median(mean_gap),
      q25            = quantile(mean_gap, .25),
      q75            = quantile(mean_gap, .75)
    )
  cat(sprintf("Participants with >=2 intervention AGDs: %d (of %d analytical)\n",
              nrow(isi_valid), dplyr::n_distinct(pp$participant_id)))
  print_full(isi_summary, "Inter-session interval (days between consecutive AGDs):")
} else {
  isi_summary <- NULL; isi <- NULL
  cat("poc_raw.rds not found at FILE_POC_RAW — block skipped; rerun when available.\n")
}

# =============================================================================
# 6. EXPANDED SAMPLE CHARACTERISTICS (feeds manuscript Table 1)
#    Baseline AND intervention; analytical sample, engaged cohort, YA subgroup.
#    Adds wagers, play hours and night hours, which 05's char_bl did not carry.
# =============================================================================

cat("\n=== 6. Expanded sample characteristics ===\n")

char_block <- function(pp_data, cohort_label) {
  out <- pp_data |>
    group_by(period) |>
    summarise(
      N              = n(),
      mean_age       = if ("age_years" %in% names(pp_data)) mean(age_years, na.rm = TRUE) else NA_real_,
      sd_age         = if ("age_years" %in% names(pp_data)) sd(age_years,  na.rm = TRUE) else NA_real_,
      n_ya           = sum(young_adult_flag, na.rm = TRUE),
      agd_mean       = mean(agd_count, na.rm = TRUE),
      agd_sd         = sd(agd_count,  na.rm = TRUE),
      agd_mdn        = median(agd_count, na.rm = TRUE),
      agd_q25        = quantile(agd_count, .25, na.rm = TRUE),
      agd_q75        = quantile(agd_count, .75, na.rm = TRUE),
      dep_mdn        = median(total_deposits, na.rm = TRUE),
      dep_q25        = quantile(total_deposits, .25, na.rm = TRUE),
      dep_q75        = quantile(total_deposits, .75, na.rm = TRUE),
      loss_mdn       = median(total_losses, na.rm = TRUE),
      loss_q25       = quantile(total_losses, .25, na.rm = TRUE),
      loss_q75       = quantile(total_losses, .75, na.rm = TRUE),
      wager_mdn      = median(total_wagers, na.rm = TRUE),
      wager_q25      = quantile(total_wagers, .25, na.rm = TRUE),
      wager_q75      = quantile(total_wagers, .75, na.rm = TRUE),
      hours_mdn      = median(play_hours_total, na.rm = TRUE),
      hours_q25      = quantile(play_hours_total, .25, na.rm = TRUE),
      hours_q75      = quantile(play_hours_total, .75, na.rm = TRUE),
      night_mdn      = if ("night_hours_total" %in% names(pp_data)) median(night_hours_total, na.rm = TRUE) else NA_real_,
      night_q25      = if ("night_hours_total" %in% names(pp_data)) quantile(night_hours_total, .25, na.rm = TRUE) else NA_real_,
      night_q75      = if ("night_hours_total" %in% names(pp_data)) quantile(night_hours_total, .75, na.rm = TRUE) else NA_real_,
      pct_low        = mean(dominant_risk == "low",    na.rm = TRUE) * 100,
      pct_medium     = mean(dominant_risk == "medium", na.rm = TRUE) * 100,
      pct_high       = mean(dominant_risk == "high",   na.rm = TRUE) * 100,
      n_risk_missing = sum(is.na(dominant_risk)),
      .groups = "drop"
    ) |>
    mutate(cohort = cohort_label, .before = 1)
  out
}

char_expanded <- bind_rows(
  char_block(pp, "Analytical sample (N = 5,808)"),
  char_block(pp |> filter(engaged_cohort == TRUE), "Engaged cohort"),
  char_block(pp |> filter(young_adult_flag == 1),  "Young Adult subgroup")
)
print_full(char_expanded)
cat("NOTE: intervention-period dominant risk uses the 6-month dominant rule;\n")
cat("n_risk_missing counts participants with no resolvable period state.\n")

# Sample flow — everything the locked file knows about the selected 6,000
cat("\n--- sample_flow (full print: the locked file's account of the 6,000) ---\n")
if (!is.null(dat$sample_flow)) {
  for (nm in names(dat$sample_flow)) {
    v <- dat$sample_flow[[nm]]
    if (is.atomic(v) && length(v) <= 20) {
      cat(sprintf("  %s: %s\n", nm, paste(v, collapse = ", ")))
    } else {
      cat(sprintf("  %s: <%s, length %d>\n", nm, class(v)[1], length(v)))
    }
  }
  cat("Characteristics of the 192 below-threshold selected participants are not\n")
  cat("in the locked analytical file; anything beyond these counts would need a\n")
  cat("separate pull (out of scope for the registered analyses).\n")
} else {
  cat("  sample_flow absent from locked file.\n")
}

# =============================================================================
# SAVE
# =============================================================================

saveRDS(list(
  msg_monthly        = msg_monthly,         # 1a full FA4 monthly table
  threshold_monthly  = threshold_monthly,   # 1b full FA5a counts table
  intent_monthly     = intent_monthly,      # 1c full FA5b table (+ frame size)
  intent_by_tool     = intent_by_tool,      # 1d or NULL
  threshold_rates    = threshold_rates,     # 2  registered proportions, 2 denominators
  threshold_other    = threshold_other,     # 3  non-registered tools supplement
  threshold_by_level = threshold_by_level,  # 3b or NULL
  fa4_stratified     = fa4_stratified,      # 4  NULL until build extension exists
  isi                = if (exists("isi")) isi else NULL,
  isi_summary        = if (exists("isi_summary")) isi_summary else NULL,   # 5
  char_expanded      = char_expanded,       # 6  manuscript Table 1 feed
  n_nonya_analytical = n_nonya_analytical,
  ya_excluded_n      = length(ya_excluded_ids)
), "results/05_descriptives/descriptives_followup.rds")

stamp("Results saved to results/05_descriptives/descriptives_followup.rds")
cat("\n=== 05b_poc_descriptives_followup.R complete ===\n")
