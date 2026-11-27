# =============================================================================
# 01_poc_build.R
# Purpose : Step 1 — build the analytical dataset, applying all
#           pre-registered rules
# Project : A Safe Bet (ASB) — Proof-of-Concept evaluation.
#           Putting Design into Practice: A Safe Bet — A Proof-of-Concept
#           Evaluation of a Tailored Online Responsible Gambling Framework
# Inputs  : poc_raw.rds; poc_tool_assignments.rds; internal rename crosswalk
#           (refresh-message template IDs only)
# Outputs : poc_analytical.rds; build log
# =============================================================================
# STATUS: Preparatory — permitted before OSF registration
#
# OUTPUT STRUCTURE: poc_analytical.rds
#   $player_period   — N=5,808 × 2 periods (primary analysis level)
#   $player_month    — N=5,808 × 12 months  (ITS + Markov)
#   $ghost_players   — 0 (all 5,808 analytical-sample players had ≥1 baseline AGD)
#   $engagement      — message + limit event + page-dwell summaries
#   $sample_flow     — exclusion waterfall
#   $build_params
#
# DATA NOTES:
#   - raw daily amounts: amount_wagered, amount_won, net_loss (period sums are total_*)
#   - is_agd: amount_wagered > 0 OR deposit_amount > 0
#   - YA flag from safebet.age (not separate metadata table)
#   - Dominant risk = percentage frequency of overall_risk_level
#   - Ghost tool stripping via signal_class
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
conflicts_prefer(dplyr::lag)
conflicts_prefer(dplyr::pull)

# Refresh-message template IDs — resolved from the internal rename crosswalk
# (server-side; kept out of the deposited config). Used only for the FA5b
# system-driven exclusion below; the real values are not persisted downstream.
.xw <- read.csv(file.path(ROOT_DATA, "ASB_rename_crosswalk.csv"), stringsAsFactors = FALSE)
REFRESH_EVENT_TEMPLATE_IDS <- unique(.xw$real_name[
  .xw$category == "template" &
  .xw$new_name %in% c("msg_refresh_dynamic_rules", "msg_refresh_dynamic_rules_2")
])
stopifnot("Refresh template IDs not resolved from crosswalk" =
  length(REFRESH_EVENT_TEMPLATE_IDS) == 2L)

cat("=== 01_poc_build.R ===\n")
cat("AGD threshold:", AGD_THRESHOLD, "\n")
cat("Started:", format(Sys.time()), "\n\n")

# -----------------------------------------------------------------------------
# 0. LOAD RAW DATA
# -----------------------------------------------------------------------------

poc_raw <- readRDS(FILE_POC_RAW)

safebet      <- poc_raw$safebet
daily_stats  <- poc_raw$daily_stats
risk_scores  <- poc_raw$model_results
crosswalk    <- poc_raw$crosswalk
kafka_online <- poc_raw$kafka_online_msg
kafka_thresh <- poc_raw$kafka_threshold
kafka_intent     <- poc_raw$kafka_intent
kafka_pages      <- poc_raw$kafka_account_tag      # page heartbeats

cat("Loaded poc_raw.rds\n")
cat("  Cohort players at selection:", n_distinct(safebet$participant_id[safebet$assignment_date == SELECTION_DATE]), "\n")

# -----------------------------------------------------------------------------
# 1. LOAD TOOL ASSIGNMENTS
#    From FILE_POC_TOOLS — the clean POC copy filtered by 00_poc_pull.R.
#    signal_class is pre-coded:
#      ACTIVE      = include in outcome analyses
#      LOW_SIGNAL  = descriptive counts only (page-dwell captured separately)
#      GHOST       = exclude from all analyses
# -----------------------------------------------------------------------------

poc_tools <- readRDS(FILE_POC_TOOLS)

cat("Tool assignments loaded from FILE_POC_TOOLS\n")
cat("  signal_class distribution:\n")
print(table(poc_tools$signal_class))

# Views used downstream
tools_active     <- poc_tools |> filter(signal_class == "ACTIVE")
tools_low_signal <- poc_tools |> filter(signal_class == "LOW_SIGNAL")
# GHOST rows simply not used anywhere

cat("  ACTIVE tools: ", nrow(tools_active), "rows,",
    n_distinct(tools_active$participant_id), "players\n")


# -----------------------------------------------------------------------------
# 2. LABEL PERIODS AND DERIVE ITS PREDICTORS IN DAILY STATS
# -----------------------------------------------------------------------------

daily_stats <- daily_stats |>
  mutate(
    period = case_when(
      evaluation_date >= BASELINE_START     & evaluation_date <= BASELINE_END     ~ "baseline",
      evaluation_date >= INTERVENTION_START & evaluation_date <= INTERVENTION_END ~ "intervention",
      TRUE ~ "outside_window"
    ),
    year_month   = floor_date(evaluation_date, "month"),
    # ITS month index: 1 = April 2024, ..., 12 = March 2025
    month_number = as.integer(
      interval(floor_date(BASELINE_START, "month"),
               floor_date(evaluation_date, "month")) / months(1)
    ) + 1L,
    # ITS predictors
    is_intervention   = as.integer(evaluation_date >= INTERVENTION_START),
    months_post       = pmax(0L, as.integer(
      floor(interval(INTERVENTION_START, evaluation_date) / months(1))
    ) + 1L) * as.integer(evaluation_date >= INTERVENTION_START)
  )

daily_study <- daily_stats |> filter(period %in% c("baseline", "intervention"))

# -----------------------------------------------------------------------------
# 3. COMPUTE AGDs PER PLAYER PER PERIOD
#    is_agd: amount_wagered > 0 OR deposit_amount > 0 (row already has is_agd column from 00_pull)
# -----------------------------------------------------------------------------

agd_by_period <- daily_study |>
  group_by(participant_id, period) |>
  summarise(
    agd_count = sum(is_agd, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_wider(names_from = period, values_from = agd_count,
              names_prefix = "agd_count_")

# Fill NA with 0 for players with no activity in a period
agd_by_period <- agd_by_period |>
  mutate(
    agd_count_baseline     = replace_na(agd_count_baseline,     0L),
    agd_count_intervention = replace_na(agd_count_intervention, 0L)
  )

# -----------------------------------------------------------------------------
# 4. SAMPLE FLOW — APPLY INCLUSION CRITERIA
#
# DESIGN DECISIONS:
#   - Activity threshold applied to BASELINE ONLY (>= AGD_THRESHOLD AGDs)
#   - Players with zero intervention activity are NOT excluded — they are
#     retained and coded as zeros / Inactive state in Markov model.
#     Excluding them would risk selecting out the intervention's effect.
#   - Ghost players = zero AGD in BOTH baseline AND intervention (never started)
#   - Two analytical cohorts:
#       Full cohort:     >= AGD_THRESHOLD baseline AGDs (primary)
#       Engaged cohort:  full cohort AND >= 1 AGD in intervention (secondary)
# -----------------------------------------------------------------------------

sample_flow <- list()

all_ids <- safebet |>
  filter(assignment_date == SELECTION_DATE) |>
  pull(participant_id) |>
  unique()

sample_flow$n_selected <- length(all_ids)

# FA1 numerator (prereg 6.1): count of the 6,000 selected players with
# >=1 AGD in the intervention period. Computed over the full enrolled
# cohort (all_ids), NOT the analytical sample (analytical_ids), so that
# below-threshold players who happen to gamble in intervention are still
# counted in the retention figure. Denominator is sample_flow$n_selected.
sample_flow$n_active_intervention_full <- agd_by_period |>
  dplyr::filter(participant_id %in% all_ids, agd_count_intervention >= 1) |>
  nrow()

# Ghost players: zero AGD in BOTH periods — never engaged at all
# These are structural non-participants, not intervention dropouts
ghost_ids <- agd_by_period |>
  filter(participant_id %in% all_ids,
         agd_count_baseline == 0,
         agd_count_intervention == 0) |>
  pull(participant_id)

ghost_players <- safebet |>
  filter(participant_id %in% ghost_ids, assignment_date == SELECTION_DATE) |>
  select(participant_id, age_years, gender, asb_risk_level, young_adult_flag)

sample_flow$n_ghost <- length(ghost_ids)
cat("Ghost players (zero AGD in both periods):", length(ghost_ids), "\n")

remaining_ids <- setdiff(all_ids, ghost_ids)
sample_flow$n_after_ghost <- length(remaining_ids)

# Activity threshold: >= AGD_THRESHOLD in BASELINE ONLY
# Ensures sufficient data for within-person comparison without
# excluding players based on intervention-period behaviour
below_threshold_ids <- agd_by_period |>
  filter(participant_id %in% remaining_ids) |>
  filter(agd_count_baseline < AGD_THRESHOLD) |>
  pull(participant_id)

sample_flow$n_below_threshold <- length(below_threshold_ids)
sample_flow$threshold_used    <- AGD_THRESHOLD

analytical_ids <- setdiff(remaining_ids, below_threshold_ids)
sample_flow$n_analytical <- length(analytical_ids)

cat("Analytical sample (>= ", AGD_THRESHOLD, " baseline AGDs): ",
    length(analytical_ids), " (expected ~5,808)\n", sep = "")

# Secondary cohort: engaged players (>= 1 AGD in intervention)
engaged_ids <- agd_by_period |>
  filter(participant_id %in% analytical_ids,
         agd_count_intervention >= 1) |>
  pull(participant_id)

sample_flow$n_engaged <- length(engaged_ids)
cat("Engaged cohort (>= 1 intervention AGD):", length(engaged_ids),
    "(expected ~5,103)\n")

# -----------------------------------------------------------------------------
# 5. PLAYER METADATA FOR ANALYTICAL SAMPLE
#    Source: tbl_study_cohort at selection date
#    YA flag from age_years field (not a separate DOB table)
# -----------------------------------------------------------------------------

player_meta <- safebet |>
  filter(participant_id %in% analytical_ids, assignment_date == SELECTION_DATE) |>
  select(
    participant_id,
    age_years,
    gender,
    young_adult_flag,
    asb_risk_level_at_selection = asb_risk_level
  )

cat("Player metadata rows:", nrow(player_meta), "\n")
cat("  Young Adult players in analytical sample:",
    sum(player_meta$young_adult_flag, na.rm = TRUE), "(expected ~110)\n")

# -----------------------------------------------------------------------------
# 6. BUILD PLAYER-PERIOD DATASET
#    One row per player × period (baseline / intervention)
#    Primary outcomes: deposit_amount, net_loss, amount_wagered, play hours, AGD
# -----------------------------------------------------------------------------

cat("Building player_period dataset...\n")

# Complete player x period grid — ensures every player has both periods.
# Players with zero intervention activity get a row with all zeros for
# the intervention period (not missing). This is essential for the
# within-subjects design: zero activity IS a valid observation.
period_grid <- tidyr::expand_grid(
  participant_id = analytical_ids,
  period    = c("baseline", "intervention")
) |>
  mutate(
    period_num    = if_else(period == "baseline", 0L, 1L),
    period_factor = factor(period, levels = c("baseline", "intervention"))
  )

# Aggregate daily stats per player x period
daily_period <- daily_study |>
  filter(participant_id %in% analytical_ids) |>
  group_by(participant_id, period) |>
  summarise(
    total_deposits    = sum(deposit_amount,               na.rm = TRUE),
    total_losses      = sum(net_loss,                 na.rm = TRUE),
    total_bets        = sum(amount_wagered,                   na.rm = TRUE),
    play_hours_total  = sum(play_hours_total,        na.rm = TRUE),
    night_hours_total  = sum(night_hours_total,        na.rm = TRUE),
    agd_count         = sum(is_agd,                 na.rm = TRUE),
    n_days_observed   = n(),
    total_wins        = sum(amount_won,                   na.rm = TRUE),
    declined_deposits = sum(n_declined_deposits, na.rm = TRUE),
    n_days_post_recal = sum(post_recalibration,     na.rm = TRUE),
    .groups = "drop"
  )

# Join onto complete grid — zeros for missing intervention periods
player_period <- period_grid |>
  left_join(daily_period, by = c("participant_id", "period")) |>
  mutate(
    across(c(total_deposits, total_losses, total_bets, play_hours_total,
             night_hours_total, agd_count, n_days_observed, total_wins,
             declined_deposits, n_days_post_recal),
           ~ replace_na(.x, 0))
  )

# Attach player metadata and engaged cohort flag
player_period <- player_period |>
  left_join(player_meta, by = "participant_id") |>
  # engaged_cohort = secondary cohort; >= 1 AGD in intervention period
  mutate(engaged_cohort = participant_id %in% engaged_ids)

# Attach dominant risk tier per period (percentage-frequency dominant level
# with temporal tiebreak, prereg 5.2 v31: ties broken by the most recent 7
# assessed days, then by the final day's assessment, matching the monthly
# derivation in Section 5.4).
#
# SHARED HELPER - dominant level with recency tiebreak, used at BOTH the
# period grain (below) and the monthly grain (Section 7 of this script).
# The logic is a faithful transcription of the previous monthly-only
# pct_freq_risk_temporal(), with the window length parameterised (7 at both
# grains here); the monthly output is unchanged by construction. Semantics,
# preserved exactly from the monthly implementation:
#   1. Percentage frequency of each valid risk level over all assessed rows;
#      unique maximum wins.
#   2. Tie: the recent window is the most recent recent_n ROWS of the raw
#      input (invalid rows filtered after selection); frequencies are
#      recounted within the window and compared among the tied levels only.
#   3. Still tied: the most recent occurrence of a tied level anywhere in
#      the period wins (the "final day" tier).
#   4. NA if no valid assessments, if dates are unavailable, or if the
#      recent window contains none of the tied levels (in which case tier 3
#      is not reached; this mirrors the monthly code exactly).
# The previous period-level mode_risk() had no tiebreak and resolved ties
# to the alphabetically first tied level as a table() ordering side effect;
# it is removed by this patch (prereg 5.2 v31 disclosure).
dominant_level_impl <- function(x, dates = NULL, recent_n = 7L) {
  valid <- RISK_LEVELS   # c("low", "medium", "high")
  x_clean <- x[!is.na(x) & x %in% valid]
  if (length(x_clean) == 0) {
    return(list(level = NA_character_, tier = "no_valid_assessments"))
  }

  # Tier 1: percentage frequency over the full period
  tbl <- table(x_clean) / length(x_clean)
  max_pct <- max(tbl)
  winners <- names(tbl)[tbl == max_pct]
  if (length(winners) == 1) return(list(level = winners, tier = "unique_mode"))

  # Tier 2: tie - most recent recent_n rows
  if (!is.null(dates) && length(dates) == length(x)) {
    recent_idx <- order(dates, decreasing = TRUE)[1:min(recent_n, length(dates))]
    x_recent <- x[recent_idx]
    x_recent <- x_recent[!is.na(x_recent) & x_recent %in% valid]
    if (length(x_recent) > 0) {
      tbl2 <- table(x_recent) / length(x_recent)
      # Only consider the tied levels
      tbl2 <- tbl2[names(tbl2) %in% winners]
      if (length(tbl2) > 0) {
        max_pct2 <- max(tbl2)
        winners2 <- names(tbl2)[tbl2 == max_pct2]
        if (length(winners2) == 1) {
          return(list(level = winners2, tier = "recent_window"))
        }

        # Tier 3: still tied - most recent occurrence of a tied level
        last_valid <- x[order(dates, decreasing = TRUE)]
        last_valid <- last_valid[!is.na(last_valid) & last_valid %in% winners2]
        if (length(last_valid) > 0) {
          return(list(level = last_valid[1], tier = "final_day"))
        }
      }
    }
  }

  # Fallback - NA if unresolvable
  list(level = NA_character_, tier = "unresolved_na")
}

dominant_level <- function(x, dates = NULL, recent_n = 7L) {
  dominant_level_impl(x, dates, recent_n)$level
}

dominant_level_tier <- function(x, dates = NULL, recent_n = 7L) {
  dominant_level_impl(x, dates, recent_n)$tier
}

risk_by_period <- risk_scores |>
  filter(participant_id %in% analytical_ids) |>
  mutate(period = case_when(
    evaluation_date >= BASELINE_START     & evaluation_date <= BASELINE_END     ~ "baseline",
    evaluation_date >= INTERVENTION_START & evaluation_date <= INTERVENTION_END ~ "intervention",
    TRUE ~ NA_character_
  )) |>
  filter(!is.na(period)) |>
  group_by(participant_id, period) |>
  summarise(
    dominant_risk = dominant_level(overall_risk_level, evaluation_date),
    tie_tier      = dominant_level_tier(overall_risk_level, evaluation_date),
    n_risk_days   = n(),
    .groups = "drop"
  )

# Tie disclosure counts for the pre-registration (prereg 5.2 v31): how each
# participant-period's dominant level was resolved. Printed every run.
cat("  Period-level dominant risk tie disclosure (prereg 5.2 v31):\n")
cat("    participant-periods with any assessment:",
    nrow(risk_by_period), "\n")
cat("    frequency ties:                         ",
    sum(risk_by_period$tie_tier %in%
          c("recent_window", "final_day", "unresolved_na")), "\n")
cat("    resolved by most recent 7 days:         ",
    sum(risk_by_period$tie_tier == "recent_window"), "\n")
cat("    resolved by final-day rule:             ",
    sum(risk_by_period$tie_tier == "final_day"), "\n")
cat("    unresolved, set to NA:                  ",
    sum(risk_by_period$tie_tier == "unresolved_na"), "\n")
cat("    no valid assessments (NA):              ",
    sum(risk_by_period$tie_tier == "no_valid_assessments"), "\n")

# tie_tier is a run-log diagnostic only; dropped so the locked dataset
# schema is unchanged apart from the corrected dominant_risk values
player_period <- player_period |>
  left_join(risk_by_period |> dplyr::select(-tie_tier),
            by = c("participant_id", "period"))

cat("  player_period rows:", nrow(player_period),
    "(expected:", length(analytical_ids) * 2, ")\n")

# Verify pre-reg variable names match
# Pre-reg uses "total_wagers" but tbl_daily_behaviour field is "amount_wagered"
# Rename for consistency with pre-reg
player_period <- player_period |>
  rename(total_wagers = total_bets)

# -----------------------------------------------------------------------------
# 7. BUILD PLAYER-MONTH DATASET
#    One row per player × calendar month (ITS and Markov model)
# -----------------------------------------------------------------------------

cat("Building player_month dataset...\n")

# Build complete player x month grid first.
# tbl_daily_behaviour only has rows for active days — zero-activity months
# would be absent if we aggregate directly. The complete grid ensures all
# analytical-cohort players have all 12 months, with zeros for inactive months.
all_months <- tibble::tibble(
  year_month      = seq(floor_date(BASELINE_START,    "month"),
                        floor_date(INTERVENTION_END,  "month"),
                        by = "month"),
  month_number    = seq_len(12L),
  is_intervention = as.integer(
    seq(floor_date(BASELINE_START,   "month"),
        floor_date(INTERVENTION_END, "month"),
        by = "month") >= INTERVENTION_START),
  months_post     = pmax(0L, cumsum(
    seq(floor_date(BASELINE_START,   "month"),
        floor_date(INTERVENTION_END, "month"),
        by = "month") >= INTERVENTION_START))
)

player_month_grid <- tidyr::expand_grid(
  participant_id  = analytical_ids,
  year_month = all_months$year_month
) |>
  left_join(all_months, by = "year_month")

# Aggregate daily stats to monthly
daily_monthly <- daily_study |>
  filter(participant_id %in% analytical_ids) |>
  group_by(participant_id, year_month) |>
  summarise(
    total_deposits    = sum(deposit_amount,         na.rm = TRUE),
    total_losses      = sum(net_loss,           na.rm = TRUE),
    total_wagers      = sum(amount_wagered,             na.rm = TRUE),
    play_hours_total  = sum(play_hours_total, na.rm = TRUE),
    night_hours_total  = sum(night_hours_total, na.rm = TRUE),
    agd_count         = sum(is_agd,           na.rm = TRUE),
    n_days_active     = n(),
    n_days_post_recal = sum(post_recalibration, na.rm = TRUE),
    .groups = "drop"
  )

# Join onto complete grid — zeros for missing (inactive) months
player_month <- player_month_grid |>
  left_join(daily_monthly, by = c("participant_id", "year_month")) |>
  mutate(
    across(c(total_deposits, total_losses, total_wagers,
             play_hours_total, night_hours_total, agd_count,
             n_days_active, n_days_post_recal),
           ~ replace_na(.x, 0)),
    is_inactive = as.integer(agd_count == 0)
  )


# Attach monthly risk tier using percentage frequency of overall_risk_level
# Pre-reg rationale: consistent methodology across baseline and intervention.
# For each player x month, the dominant risk level is the one with the
# highest percentage frequency of daily BetBuddy assessments.
# This mirrors the asb_risk_level computation but is applied consistently
# to both periods, avoiding a methodological break at October 2024.

# Dominant risk level by percentage frequency with temporal tiebreaking.
# Implemented by the shared dominant_level() helper defined in Section 6
# (prereg 5.2 and 5.4, v31): unique percentage-frequency winner; ties broken
# within the most recent 7 days, compared among tied levels only; then the
# most recent occurrence of a tied level; NA if unresolvable. The helper is
# a faithful transcription of the pct_freq_risk_temporal() function that
# previously lived here, so the monthly output is unchanged.
# Rationale: temporal recency reflects the player's current trajectory
# and is the closest analogue to how asb_risk_level is computed
# (30-day lookback ending at the refresh date).

risk_by_month <- risk_scores |>
  dplyr::filter(participant_id %in% analytical_ids) |>
  dplyr::mutate(year_month = floor_date(evaluation_date, "month")) |>
  dplyr::filter(
    year_month >= floor_date(BASELINE_START,   "month") &
      year_month <= floor_date(INTERVENTION_END, "month")
  ) |>
  dplyr::group_by(participant_id, year_month) |>
  dplyr::summarise(
    monthly_risk       = dominant_level(overall_risk_level, evaluation_date),
    n_risk_assessments = n(),
    .groups = "drop"
  )

# Markov state: L / M / H / I (I = inactive = zero AGD in month)
player_month <- player_month |>
  left_join(risk_by_month, by = c("participant_id", "year_month")) |>
  dplyr::mutate(
    markov_state = dplyr::case_when(
      is_inactive == 1         ~ "I",
      monthly_risk == "low"    ~ "L",
      monthly_risk == "medium" ~ "M",
      monthly_risk == "high"   ~ "H",
      TRUE ~ NA_character_
    )
  )

cat("  Monthly risk computed via percentage frequency of overall_risk_level\n")
cat("  Markov states: L=", sum(player_month$markov_state == "L", na.rm=TRUE),
    "M=", sum(player_month$markov_state == "M", na.rm=TRUE),
    "H=", sum(player_month$markov_state == "H", na.rm=TRUE),
    "I=", sum(player_month$markov_state == "I", na.rm=TRUE), "\n")

# Attach player metadata
player_month <- player_month |>
  left_join(player_meta, by = "participant_id")

cat("  player_month rows:", nrow(player_month), "\n")

# -----------------------------------------------------------------------------
# 8. ENGAGEMENT SUMMARIES (prereg 6.6 engagement analyses; 6.1 FA4/FA5)
# -----------------------------------------------------------------------------

cat("Building engagement summaries...\n")

# 8a. Message response rates by player × month × type
msg_summary <- kafka_online |>
  filter(participant_id %in% analytical_ids) |>
  mutate(year_month = floor_date(as.Date(decision_month), "month")) |>
  group_by(participant_id, year_month) |>
  summarise(
    n_messages_sent      = sum(is_sent,     na.rm = TRUE),
    n_responses          = sum(is_response, na.rm = TRUE),
    n_rg_button_clicks   = sum(is_rg_button & is_response, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    # Cap at 1: cross-month timing of send/response can produce ratios > 1
    # (e.g. response in month M to a message sent in month M-1 with own M-1 send count).
    # Conceptually a "response rate" cannot exceed 100%.
    response_rate  = pmin(n_responses        / pmax(n_messages_sent, 1), 1),
    rg_click_rate  = pmin(n_rg_button_clicks / pmax(n_messages_sent, 1), 1)
  )

# 8b. Limit threshold hits by player × month × tool type
threshold_summary <- kafka_thresh |>
  filter(participant_id %in% analytical_ids) |>
  mutate(year_month = floor_date(as.Date(decision_month), "month")) |>
  group_by(participant_id, year_month, limit_type) |>
  summarise(
    n_threshold_hits = n(),
    hit_100pct_any   = as.integer(any(hit_100pct, na.rm = TRUE)),
    .groups = "drop"
  )

# 8c. Limit modification engagement - n_limit_intents (prereg 6.1 FA5b, 5.4)
#     Inclusion rule:
#       - limit_type IN {DEPOSIT_LIMIT, LOSS_LIMIT, BET_LIMIT}     (already in extraction)
#       - limit_period = MONTHLY (is_monthly == TRUE; framework-aligned scope)
#       - NOT system-driven: no evt_online_message with refresh template_id
#         within ±SYSTEM_DRIVEN_TIME_TOLERANCE_SECS for same participant_uid
#     NOT filtered by activation type. n_protective_intents kept as DESCRIPTIVE
#     supplement (FA5b direction breakdown).

# Step 1: identify refresh-event timestamps per player from kafka_online
refresh_dispatch <- kafka_online |>
  filter(
    !is.na(participant_id),
    template_id %in% REFRESH_EVENT_TEMPLATE_IDS
  ) |>
  distinct(participant_id, refresh_ts = processing_time)

cat("  Refresh-event dispatches identified:", nrow(refresh_dispatch), "\n")

# Step 2: tag intent events as system-driven if any refresh dispatch is within ±5s
intents_with_match <- kafka_intent |>
  filter(participant_id %in% analytical_ids) |>
  mutate(intent_row = dplyr::row_number()) |>
  inner_join(refresh_dispatch, by = "participant_id", relationship = "many-to-many") |>
  filter(
    abs(as.numeric(processing_time - refresh_ts, units = "secs")) <=
      SYSTEM_DRIVEN_TIME_TOLERANCE_SECS
  ) |>
  distinct(intent_row) |>
  pull(intent_row)

kafka_intent_filtered <- kafka_intent |>
  filter(participant_id %in% analytical_ids) |>
  mutate(
    intent_row       = dplyr::row_number(),
    is_system_driven = intent_row %in% intents_with_match
  )

cat("  Intent events flagged as system-driven:",
    sum(kafka_intent_filtered$is_system_driven, na.rm = TRUE),
    "of", nrow(kafka_intent_filtered), "\n")

# Step 3: aggregate n_limit_intents (engagement covariate) per player-month,
# applying MONTHLY-window filter and system-driven exclusion. Direction
# breakdown (n_protective_intents, n_increase_intents) preserved as
# DESCRIPTIVE supplement for FA5b — NOT used as engagement covariate filter.
intent_summary <- kafka_intent_filtered |>
  mutate(year_month = floor_date(as.Date(decision_month), "month")) |>
  filter(
    is_monthly       == TRUE,         # MONTHLY-window only
    is_system_driven == FALSE         # exclude refresh-event executions
  ) |>
  group_by(participant_id, year_month) |>
  summarise(
    n_limit_intents       = dplyr::n(),                              # PRIMARY engagement covariate
    n_protective_intents  = sum(is_protective,  na.rm = TRUE),       # descriptive supplement
    n_increase_intents    = sum(!is_protective, na.rm = TRUE),       # descriptive supplement
    .groups = "drop"
  )


# 8d. Page dwell - limit_page_dwell + rg_support_dwell (prereg 5.4)
#     Heartbeat reconstruction from evt_account_tag_update records:
#       - cluster heartbeats per player with 180s gap rule (SESSION_GAP_SECS)
#       - visit duration = max minus min processing_time within cluster
#       - assign visit to the calendar month of its start time
#       - sum visit durations per (participant_id, year_month) for each page set
#     Source: RG Page Dwell Time metric specification v0.1
compute_page_dwell <- function(events, page_set, dwell_var_name) {
  events |>
    filter(
      participant_id %in% analytical_ids,
      page_name %in% page_set
    ) |>
    arrange(participant_id, processing_time) |>
    group_by(participant_id) |>
    mutate(
      gap_secs  = as.numeric(
        difftime(processing_time, lag(processing_time), units = "secs")
      ),
      new_visit = is.na(gap_secs) | gap_secs > SESSION_GAP_SECS,
      visit_id  = cumsum(new_visit)
    ) |>
    group_by(participant_id, visit_id) |>
    summarise(
      visit_start    = min(processing_time),
      visit_duration = as.numeric(
        difftime(max(processing_time), min(processing_time), units = "secs")
      ),
      .groups = "drop"
    ) |>
    mutate(year_month = floor_date(as.Date(visit_start), "month")) |>
    group_by(participant_id, year_month) |>
    summarise(
      !!dwell_var_name := sum(visit_duration),
      .groups = "drop"
    )
}

limit_dwell_summary    <- compute_page_dwell(kafka_pages, PAGE_LIMIT_MANAGEMENT, "limit_page_dwell")
rg_support_dwell_summary <- compute_page_dwell(kafka_pages, PAGE_RG_SUPPORT,       "rg_support_dwell")

cat("  Player-months with limit_page_dwell > 0:",
    nrow(limit_dwell_summary), "\n")
cat("  Player-months with rg_support_dwell > 0:",
    nrow(rg_support_dwell_summary), "\n")

# Attach engagement to player_month - four covariates per prereg 5.4:
#   msg_response_rate, n_limit_intents, limit_page_dwell, rg_support_dwell
# Plus descriptive supplement variables (n_protective_intents, n_increase_intents).

msg_by_month <- msg_summary |>
  group_by(participant_id, year_month) |>
  summarise(
    n_messages_sent    = sum(n_messages_sent,    na.rm = TRUE),
    n_responses        = sum(n_responses,        na.rm = TRUE),
    n_rg_button_clicks = sum(n_rg_button_clicks, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    # Cap at 1 (see note in §8a) — proportion bounded above by 100%
    msg_response_rate = pmin(n_responses        / pmax(n_messages_sent, 1), 1),
    rg_click_rate     = pmin(n_rg_button_clicks / pmax(n_messages_sent, 1), 1)
  )

player_month <- player_month |>
  left_join(
    intent_summary |> dplyr::select(participant_id, year_month,
                                    n_limit_intents,
                                    n_protective_intents,
                                    n_increase_intents),
    by = c("participant_id", "year_month")
  ) |>
  left_join(
    msg_by_month |> dplyr::select(participant_id, year_month,
                                  n_messages_sent, n_responses,
                                  msg_response_rate, rg_click_rate),
    by = c("participant_id", "year_month")
  ) |>
  left_join(limit_dwell_summary,      by = c("participant_id", "year_month")) |>
  left_join(rg_support_dwell_summary, by = c("participant_id", "year_month")) |>
  mutate(
    # Engagement covariates (prereg 5.4) - zero where no events recorded
    n_limit_intents      = replace_na(n_limit_intents,      0L),
    msg_response_rate    = replace_na(msg_response_rate,    0),
    limit_page_dwell     = replace_na(limit_page_dwell,     0),
    rg_support_dwell     = replace_na(rg_support_dwell,     0),
    # Descriptive supplement (FA5b direction breakdown) — not engagement covariates
    n_protective_intents = replace_na(n_protective_intents, 0L),
    n_increase_intents   = replace_na(n_increase_intents,   0L),
    # Other auxiliaries
    n_messages_sent      = replace_na(n_messages_sent,      0L),
    n_responses          = replace_na(n_responses,          0L),
    rg_click_rate        = replace_na(rg_click_rate,        0)
  )

cat("  Engagement covariates added to player_month (prereg 5.4):\n")
cat("    msg_response_rate, n_limit_intents, limit_page_dwell, rg_support_dwell\n")
cat("  Non-zero n_limit_intents:  ", sum(player_month$n_limit_intents > 0),    "player-months\n")
cat("  Non-zero msg_response_rate:", sum(player_month$msg_response_rate > 0), "player-months\n")
cat("  Non-zero limit_page_dwell: ", sum(player_month$limit_page_dwell > 0),  "player-months\n")
cat("  Non-zero rg_support_dwell: ", sum(player_month$rg_support_dwell > 0),  "player-months\n")

# -----------------------------------------------------------------------------
# 8e. SCHEMA VALIDATION ASSERTIONS
#     Fail loudly on any structural mismatch with pre-reg expectations.
# -----------------------------------------------------------------------------

cat("\nValidating analytical dataset schema...\n")

# player_period: expected rows = N analytical × 2 periods
expected_pp_rows <- length(analytical_ids) * 2L
stopifnot(
  "player_period row count differs from N analytical × 2" =
    nrow(player_period) == expected_pp_rows
)
cat("  player_period rows OK:", nrow(player_period), "\n")

# player_month: expected rows = N analytical × 12 months
expected_pm_rows <- length(analytical_ids) * 12L
stopifnot(
  "player_month row count differs from N analytical × 12" =
    nrow(player_month) == expected_pm_rows
)
cat("  player_month rows OK:", nrow(player_month), "\n")

# All 4 engagement covariates present and non-NA in player_month
required_cols <- ENGAGEMENT_COVARIATES
missing_cols <- setdiff(required_cols, names(player_month))
stopifnot(
  "Missing engagement covariate columns in player_month" =
    length(missing_cols) == 0
)
na_counts <- sapply(player_month[, required_cols], function(x) sum(is.na(x)))
stopifnot(
  "NA values present in engagement covariates after replace_na" =
    all(na_counts == 0)
)
cat("  Engagement covariates present and complete:",
    paste(required_cols, collapse = ", "), "\n")

# Markov state values are in {L, M, H, I} (or NA for unresolved ties)
unexpected_states <- setdiff(unique(player_month$markov_state),
                             c(MARKOV_STATES, NA_character_))
stopifnot(
  "Unexpected markov_state values" =
    length(unexpected_states) == 0
)
cat("  markov_state values OK:", paste(MARKOV_STATES, collapse = ", "),
    "(NAs:", sum(is.na(player_month$markov_state)), ")\n")

# Engagement covariates non-negative (log1p will be applied downstream)
stopifnot(
  "Negative values in n_limit_intents"  = all(player_month$n_limit_intents  >= 0),
  "Negative values in limit_page_dwell" = all(player_month$limit_page_dwell >= 0),
  "Negative values in rg_support_dwell" = all(player_month$rg_support_dwell >= 0),
  "msg_response_rate outside [0, 1]"    = all(
    player_month$msg_response_rate >= 0 & player_month$msg_response_rate <= 1
  )
)
cat("  Engagement covariate ranges OK\n")

cat("  All schema checks passed.\n")

# -----------------------------------------------------------------------------
# 9. PACK AND SAVE
# -----------------------------------------------------------------------------

poc_analytical <- list(
  player_period     = player_period,
  player_month      = player_month,
  ghost_players     = ghost_players,
  engagement = list(
    msg_summary       = msg_summary,
    threshold_summary = threshold_summary,
    intent_summary    = intent_summary
  ),
  sample_flow = sample_flow,
  build_params = list(
    agd_threshold      = AGD_THRESHOLD,
    baseline_start     = BASELINE_START,
    baseline_end       = BASELINE_END,
    intervention_start = INTERVENTION_START,
    intervention_end   = INTERVENTION_END,
    recal_date         = BETBUDDY_RECALIB_DATE,
    ghost_tool_codes   = GHOST_TOOL_CODES,
    built_at           = Sys.time(),
    r_version          = R.version.string
  )
)

saveRDS(poc_analytical, FILE_POC_ANALYTICAL)
cat("\nSaved:", FILE_POC_ANALYTICAL, "\n")

# Print sample flow
cat("\n--- SAMPLE FLOW ---\n")
cat("  Selected:                       ", sample_flow$n_selected, "\n")
cat("  Ghost (zero in both periods):  -", sample_flow$n_ghost,
    "(expected 0 by selection criteria; see prereg 4.2)\n")
cat("  After ghost removal:            ", sample_flow$n_after_ghost, "\n")
cat("  Below AGD threshold:           -", sample_flow$n_below_threshold,
    "(threshold =", sample_flow$threshold_used, ")\n")
cat("  ANALYTICAL SAMPLE:              ", sample_flow$n_analytical, "(expected ~5,808)\n")
cat("  Zero intervention activity:     ", sample_flow$n_analytical - sample_flow$n_engaged,
    "(expected ~705; retained, coded as zeros)\n")
cat("  Engaged cohort:                 ", sample_flow$n_engaged, "(expected ~5,103)\n")

# Log
log_lines <- c(
  paste("Run time:", format(Sys.time())),
  paste("AGD threshold:", AGD_THRESHOLD),
  paste("N selected:", sample_flow$n_selected),
  paste("N active in intervention (full cohort, FA1 numerator):",
        sample_flow$n_active_intervention_full),
  paste("N ghost:", sample_flow$n_ghost),
  paste("N below threshold:", sample_flow$n_below_threshold),
  paste("N analytical:", sample_flow$n_analytical),
  paste("player_period rows:", nrow(player_period)),
  paste("player_month rows:", nrow(player_month))
)
writeLines(log_lines, file.path(DIR_LOGS, "01_poc_build_log.txt"))

cat("\n=== 01_poc_build.R complete ===\n")