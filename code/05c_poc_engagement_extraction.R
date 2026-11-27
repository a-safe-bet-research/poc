# =============================================================================
# 05c_poc_engagement_extraction.R   (v4: FA4 by month added 25 November 2026;
#                                     v3 patched 21 July 2026)
# Purpose : Post-registration engagement extraction — the three FA4/FA5
#           strata that prereg 10.4 item 4 registered as post-registration
#           computations and that the locked analytical file cannot supply:
#             A. FA4 stratified message response rates
#                (message type x tool x baseline risk level x content variant)
#             B. FA5a threshold-level breakdown (25/50/75/80%)
#             C. FA5b limit-modification engagement by tool (DL/LL/BL)
# Project : A Safe Bet (ASB) — Proof-of-Concept evaluation.
# Inputs  : poc_raw.rds (event-level Kafka extracts saved by 00_poc_pull.R);
#           poc_analytical_LOCKED.rds (READ-ONLY);
#           internal rename crosswalk (template rows only)
# Outputs : results/05_descriptives/engagement_extraction.rds  (OWN file)
# Log     : Rscript 05c_poc_engagement_extraction.R 2>&1 | tee logs/05c_real_YYYYMMDD.log
# =============================================================================
# STATUS  : POST-REGISTRATION implementation of pre-registered work
#           (prereg 6.1 FA4/FA5a; 10.4 item 4). Descriptive only.
#
# V2 PATCH (21 July 2026; per results-master decisions D3 and D16):
#
#   (1) CODEBOOK DERIVATION (D3 resolution): the crosswalk template rows
#       carry NO metadata columns. The neutral template names ARE the
#       message_templates codebook, via the naming grammar
#           msg_safebet_<family>_<content>_<tier>[_YA]
#       message_stage is derived from <family>:
#         lossthreshold, betthreshold            -> threshold_25_popup
#         depositthreshold80                     -> threshold_80_popup
#         depositincrease / lossincrease /
#           betincrease / loginincrease          -> increase_request_popup
#         <family> ending in 'pending'           -> pending_increase_popup
#         <family> ending in 'decrease'          -> decrease_toaster
#       tool_name from the family prefix (deposit -> Deposit Limit,
#       loss -> Loss Limit, bet -> Bet Limit, login -> Login/Playtime Limit);
#       content_type from <content> (ppf -> PPF, epf -> EPF, all -> unified;
#       'all' co-occurs with tier _H — the unified high-risk style, reported
#       without a PPF/EPF split per prereg 6.1). _YA-suffixed
#       depositthreshold80 templates classify normally: their recipients are
#       removed by the FA4/FA5 YA exclusion, so the rows are inert.
#       Non-safebet and unparsed names -> NA (the registered excluded-legacy
#       class). This grammar derivation IS the codebook implementation
#       referenced in prereg 6.1.
#
#   (2) FIRST-REFRESH TEMPLATE (D16): a third refresh template (neutral
#       msg_refresh_dynamic_rules_3; the 1 October first Refresh Event
#       dispatched via IMS; ~11,336 events) was unknown to 01_poc_build.R
#       and absent from its FA5b system-driven exclusion. Consequences
#       implemented here:
#         a. FA5b uses ALL THREE refresh template IDs for the ±5 s
#            system-driven exclusion — the registered RULE ("within ±5 s of
#            a refresh-template dispatch") implemented completely. The
#            three-ID vs two-ID (locked-build) excluded-event comparison is
#            printed by month and saved (difference expected to concentrate
#            in October).
#         b. FA4's refresh_event_notification type keeps the registered
#            two-ID definition; the first-refresh template is reported as
#            its own labelled row ("first Refresh Event notification —
#            identified post-registration").
#         c. The refresh-ID consistency check is 2 registered + 1
#            post-registration = 3 within this script ONLY; 00_config.R and
#            01_poc_build.R are NOT touched.
#       The locked engagement table's October FA5b figures (and the
#       n_limit_intents covariate) retain the two-ID contamination — logged
#       as D16 in the results master; 05c's three-ID figures are the
#       reporting basis for FA5b.
#
# V3 PATCH (21 July 2026, after the v2 real-data run; D18):
#   THRESHOLD FORMAT: the extracted threshold_pct values carry NO percent
#   sign (observed: 25/50/75/80/95/98/100). v2's registered-level filter
#   ("25%" etc.) therefore matched nothing (empty primary table), and the
#   same format mismatch means the pull-stage hit_100pct comparison
#   (threshold_pct == "100%") can never have matched — the locked build's
#   hit_100pct_any zeros, and 05b's "zero 100% hits", are format artefacts,
#   not findings: level-100 events are frequent in the raw extract.
#   v3 normalises the level (strip a trailing % if present) before the
#   registered-level filter AND before the FA4 breach-100 matching, and
#   prints the level table both raw and normalised. No other change.
#
# V4 ADDITION (25 November 2026):
#   FA4 BY MONTH on ASB-classified messages (msg_asb), with the same
#   registered macro-average rule as the stratified tables. Added because
#   the monthly rate in the locked build (01_poc_build.R, msg_summary)
#   counts all online message events, while prereg 6.1 restricts FA4 to ASB
#   messages. Printed with and without the first Refresh Event template
#   (D16), plus the overall rate. No existing computation is changed.
#
# LOCKED-FILE PROTECTION: reads only; own outputs only; mtime assertion at end.
# RUN NOTE: loads poc_raw.rds (several GB); do not run alongside 07b's HMC.
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

stamp <- function(msg) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), msg))
print_full <- function(x, label = NULL) {
  if (!is.null(label)) cat(label, "\n")
  print(as.data.frame(x), row.names = FALSE)
}

dir.create("results/05_descriptives", recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 0. LOCKED FILE (READ-ONLY)
# -----------------------------------------------------------------------------
locked_mtime_at_load <- file.info(FILE_POC_LOCKED)$mtime
dat <- readRDS(FILE_POC_LOCKED)
pp  <- dat$player_period
pm  <- dat$player_month
cat("Running on LOCKED real data (read-only)\n")

analytical_ids <- unique(pp$participant_id)
ya_ids <- pp |> filter(young_adult_flag == 1) |> pull(participant_id) |> unique()
cat("Analytical sample:", length(analytical_ids),
    "| YA excluded from FA4/FA5 (prereg 4.4):", length(ya_ids), "\n")
nonya_ids          <- setdiff(analytical_ids, ya_ids)
n_nonya_analytical <- length(nonya_ids)

roster_risk <- pp |>
  filter(period == "baseline") |>
  distinct(participant_id, asb_risk_level_at_selection)
stopifnot("asb_risk_level_at_selection missing from locked player_period" =
            "asb_risk_level_at_selection" %in% names(roster_risk))
cat("Roster risk distribution (non-YA analytical sample):\n")
print(table(roster_risk$asb_risk_level_at_selection[
  roster_risk$participant_id %in% nonya_ids], useNA = "ifany"))

active_nonya_by_month <- pm |>
  filter(is_intervention == 1, participant_id %in% nonya_ids) |>
  group_by(year_month) |>
  summarise(n_active_nonya = sum(agd_count > 0), .groups = "drop")

rm(pm); invisible(gc(verbose = FALSE))

# -----------------------------------------------------------------------------
# 1. TEMPLATE CODEBOOK — derived from the crosswalk's NEUTRAL template names
#    (v2: the naming grammar is the codebook; see header patch note 1 / D3)
# -----------------------------------------------------------------------------
CROSSWALK_PATH <- Sys.getenv("ASB_CROSSWALK",
                             unset = file.path(ROOT_DATA, "ASB_rename_crosswalk.csv"))
stopifnot("Rename crosswalk not found" = file.exists(CROSSWALK_PATH))
XW   <- read.csv(CROSSWALK_PATH, stringsAsFactors = FALSE, na.strings = "")
tmpl <- XW |> filter(category == "template")
cat("\nTemplate rows in crosswalk:", nrow(tmpl), "\n")

# --- Refresh templates: 2 registered + 1 identified post-registration (D16) ---
REFRESH_NEUTRAL_2ID <- c("msg_refresh_dynamic_rules", "msg_refresh_dynamic_rules_2")
REFRESH_NEUTRAL_3RD <- "msg_refresh_dynamic_rules_3"
REFRESH_IDS_2ID <- unique(tmpl$real_name[tmpl$new_name %in% REFRESH_NEUTRAL_2ID])
REFRESH_ID_3RD  <- unique(tmpl$real_name[tmpl$new_name == REFRESH_NEUTRAL_3RD])
stopifnot(
  "Registered refresh template IDs (rules, rules_2) not resolved from crosswalk" =
    length(REFRESH_IDS_2ID) == 2L,
  "First-refresh template (msg_refresh_dynamic_rules_3; 1 Oct IMS dispatch, identified post-registration) not in crosswalk — add its row before running" =
    length(REFRESH_ID_3RD) == 1L
)
REFRESH_IDS_ALL3 <- c(REFRESH_IDS_2ID, REFRESH_ID_3RD)
cat("Refresh templates resolved: 2 registered + 1 post-registration (first Refresh Event) = 3\n")

# --- ASB message templates: parse msg_safebet_<family>_<content>_<tier>[_YA] ---
safebet_tmpl <- tmpl |>
  filter(grepl("^msg_safebet_", new_name)) |>
  mutate(
    core       = sub("^msg_safebet_", "", new_name),
    ya_variant = grepl("_YA$", core),
    core_noya  = sub("_YA$", "", core),
    n_tokens   = lengths(strsplit(core_noya, "_", fixed = TRUE))
  )

parsed <- safebet_tmpl |>
  filter(n_tokens == 3) |>
  separate(core_noya, into = c("family", "content", "tier"), sep = "_", remove = FALSE) |>
  mutate(
    message_stage = case_when(
      family %in% c("lossthreshold", "betthreshold")   ~ "threshold_25_popup",
      family == "depositthreshold80"                   ~ "threshold_80_popup",
      family %in% c("depositincrease", "lossincrease",
                    "betincrease", "loginincrease")    ~ "increase_request_popup",
      grepl("pending$",  family)                       ~ "pending_increase_popup",
      grepl("decrease$", family)                       ~ "decrease_toaster",
      TRUE                                             ~ NA_character_
    ),
    tool_name = case_when(
      grepl("^deposit", family) ~ "Deposit Limit",
      grepl("^loss",    family) ~ "Loss Limit",
      grepl("^bet",     family) ~ "Bet Limit",
      grepl("^login",   family) ~ "Login/Playtime Limit",
      TRUE                      ~ NA_character_
    ),
    content_type = case_when(
      content == "ppf" ~ "PPF",
      content == "epf" ~ "EPF",
      content == "all" ~ "unified",   # co-occurs with tier _H: blended high-risk style
      TRUE             ~ NA_character_
    )
  )

# Derived classification table — NEUTRAL names only (real IDs never printed)
cat("\nDerived template codebook (grammar msg_safebet_<family>_<content>_<tier>[_YA]):\n")
print_full(parsed |>
             select(template_neutral = new_name, family, content, tier,
                    ya_variant, message_stage, tool_name, content_type) |>
             arrange(message_stage, tool_name, content_type))

unparsed <- c(
  safebet_tmpl$new_name[safebet_tmpl$n_tokens != 3],
  parsed$new_name[is.na(parsed$message_stage) | is.na(parsed$content_type)]
)
if (length(unparsed) > 0) {
  cat("\nUnparsed / unclassifiable msg_safebet_* templates -> NA (excluded legacy class):\n ",
      paste(unique(unparsed), collapse = "\n  "), "\n")
}
nonasb_tmpl <- tmpl$new_name[!grepl("^msg_safebet_", tmpl$new_name) &
                               !tmpl$new_name %in% c(REFRESH_NEUTRAL_2ID, REFRESH_NEUTRAL_3RD)]
cat("Non-ASB template names in crosswalk (excluded legacy class):",
    length(nonasb_tmpl), "\n")

codebook <- parsed |>
  filter(!is.na(message_stage), !is.na(content_type)) |>
  transmute(template_id_real = real_name, template_neutral = new_name,
            message_stage, tool_name, content_type)
cat("Codebook rows usable for the FA4 join:", nrow(codebook), "\n")

# -----------------------------------------------------------------------------
# 2. EVENT DATA — from poc_raw.rds
# -----------------------------------------------------------------------------
stamp("loading poc_raw.rds (large)")
poc_raw      <- readRDS(FILE_POC_RAW)
kafka_online <- poc_raw$kafka_online_msg
kafka_thresh <- poc_raw$kafka_threshold
kafka_intent <- poc_raw$kafka_intent
rm(poc_raw); invisible(gc(verbose = FALSE))
stamp("poc_raw released")

kafka_online <- kafka_online |> filter(participant_id %in% nonya_ids)
kafka_thresh <- kafka_thresh |> filter(participant_id %in% nonya_ids)
kafka_intent <- kafka_intent |> filter(participant_id %in% nonya_ids)
cat("Event rows (non-YA analytical): online", nrow(kafka_online),
    "| threshold", nrow(kafka_thresh), "| intent", nrow(kafka_intent), "\n")

n_first_refresh_events <- sum(kafka_online$template_id %in% REFRESH_ID_3RD, na.rm = TRUE)
cat("First-refresh (rules_3) online-message events in the extract:",
    n_first_refresh_events, "(finding reported ~11,336 across the full cohort;",
    "this count is after the YA filter)\n")

# =============================================================================
# A. FA4 STRATIFIED MESSAGE RESPONSE RATES (prereg 6.1 FA4; 10.4 item 4)
# =============================================================================
cat("\n=== A. FA4 stratified message response rates ===\n")

msg <- kafka_online |>
  mutate(year_month = floor_date(as.Date(decision_month), "month")) |>
  left_join(codebook, by = c("template_id" = "template_id_real"))

# Breach 100% pop-up rule (registered): NULL-template events matched to a 100%
# threshold event, same participant_uid, ±5 s
# v3: normalise the level — values carry no % sign in the extract (D18)
thresh_100 <- kafka_thresh |>
  filter(sub("%$", "", threshold_pct) == "100") |>
  distinct(participant_uid, t100_ts = processing_time)
cat("Level-100 threshold events available for breach matching:", nrow(thresh_100),
    "\n(NOTE, D18: 05b's zero hit_100pct months reflect the pull-stage '100%'\n",
    "string comparison, which the unsigned values can never match — a format\n",
    "artefact, not an absence of level-100 events.)\n")

null_tmpl <- msg |> filter(is.na(template_id)) |> mutate(msg_row = dplyr::row_number())
breach_rows <- null_tmpl |>
  inner_join(thresh_100, by = "participant_uid", relationship = "many-to-many") |>
  filter(abs(as.numeric(processing_time - t100_ts, units = "secs")) <=
           SYSTEM_DRIVEN_TIME_TOLERANCE_SECS) |>
  distinct(msg_row) |>
  pull(msg_row)
null_tmpl <- null_tmpl |> mutate(is_breach_100 = msg_row %in% breach_rows)

msg <- msg |>
  left_join(null_tmpl |> select(participant_uid, processing_time, is_breach_100) |>
              distinct(),
            by = c("participant_uid", "processing_time")) |>
  mutate(is_breach_100 = coalesce(is_breach_100, FALSE))

# Message-type classification. refresh_event_notification keeps the REGISTERED
# two-ID definition; the first-refresh template is its own labelled
# post-registration row (v2 patch note 2b / D16).
msg <- msg |>
  mutate(
    message_type = case_when(
      template_id %in% REFRESH_IDS_2ID            ~ "refresh_event_notification",
      template_id %in% REFRESH_ID_3RD             ~ "first_refresh_event_notification (identified post-registration)",
      message_stage == "threshold_25_popup"       ~ "threshold_25_popup",
      message_stage == "threshold_80_popup"       ~ "threshold_80_popup",
      message_stage == "increase_request_popup"   ~ "increase_request_popup",
      message_stage == "pending_increase_popup"   ~ "pending_increase_popup",
      message_stage == "decrease_toaster"         ~ "decrease_toaster",
      is_breach_100                               ~ "breach_100_popup",
      TRUE                                        ~ NA_character_
    ),
    content_variant = case_when(
      content_type %in% c("PPF", "EPF") ~ content_type,
      content_type == "unified"         ~ "unified (no PPF/EPF split)",
      TRUE                              ~ NA_character_
    )
  )

n_asb      <- sum(!is.na(msg$message_type))
n_excluded <- sum(is.na(msg$message_type))
cat(sprintf(
  "ASB-classified message events: %d | excluded as non-ASB/legacy/unmatched: %d (%.1f%%)\n",
  n_asb, n_excluded, 100 * n_excluded / max(nrow(msg), 1)))
cat("Events per message type (send + submit phases):\n")
print(table(msg$message_type, msg$phase, useNA = "ifany"))

msg_asb <- msg |>
  filter(!is.na(message_type)) |>
  left_join(roster_risk, by = "participant_id")

# Macro-average machinery (prereg 6.1 FA4, exact registered rule)
fa4_stratified_rate <- function(data, stratum_col) {
  data |>
    group_by(.data[[stratum_col]], participant_id, year_month) |>
    summarise(
      n_send   = sum(phase == MSG_PHASE_SENT,     na.rm = TRUE),
      n_submit = sum(phase == MSG_PHASE_RESPONSE, na.rm = TRUE),
      .groups = "drop"
    ) |>
    filter(n_send > 0) |>
    mutate(rate = pmin(n_submit / n_send, 1)) |>
    group_by(.data[[stratum_col]]) |>
    summarise(
      n_participant_months    = n(),
      n_participants          = n_distinct(participant_id),
      total_sends             = sum(n_send),
      total_submits           = sum(n_submit),
      macro_avg_response_rate = mean(rate),
      .groups = "drop"
    )
}

fa4_by_type    <- fa4_stratified_rate(msg_asb, "message_type")
fa4_by_tool    <- fa4_stratified_rate(msg_asb |> filter(!is.na(tool_name)), "tool_name")
fa4_by_risk    <- fa4_stratified_rate(msg_asb, "asb_risk_level_at_selection")
fa4_by_variant <- fa4_stratified_rate(msg_asb |> filter(!is.na(content_variant)),
                                      "content_variant")
fa4_type_by_risk <- msg_asb |>
  group_by(message_type, asb_risk_level_at_selection, participant_id, year_month) |>
  summarise(n_send   = sum(phase == MSG_PHASE_SENT, na.rm = TRUE),
            n_submit = sum(phase == MSG_PHASE_RESPONSE, na.rm = TRUE),
            .groups = "drop") |>
  filter(n_send > 0) |>
  mutate(rate = pmin(n_submit / n_send, 1)) |>
  group_by(message_type, asb_risk_level_at_selection) |>
  summarise(n_participant_months = n(),
            macro_avg_response_rate = mean(rate), .groups = "drop")

print_full(fa4_by_type,    "\nFA4 by message type (macro-averaged; first-refresh row labelled post-registration):")
print_full(fa4_by_tool,    "\nFA4 by tool (macro-averaged):")
print_full(fa4_by_risk,    "\nFA4 by roster risk level at selection (macro-averaged):")
print_full(fa4_by_variant, "\nFA4 by content variant (macro-averaged; unified unsplit):")
print_full(fa4_type_by_risk, "\nFA4 message type x risk level (macro-averaged):")

# -----------------------------------------------------------------------------
# FA4 BY MONTH, ASB MESSAGES ONLY (added 25 November 2026)
# Same registered macro-average rule as above, grouped by month. Printed twice:
# all ASB messages, and without the first Refresh Event template (1 October,
# identified post-registration; D16).
# -----------------------------------------------------------------------------
fa4_monthly_rate <- function(data) {
  data |>
    group_by(participant_id, year_month) |>
    summarise(
      n_send   = sum(phase == MSG_PHASE_SENT,     na.rm = TRUE),
      n_submit = sum(phase == MSG_PHASE_RESPONSE, na.rm = TRUE),
      .groups = "drop"
    ) |>
    filter(n_send > 0) |>
    mutate(rate = pmin(n_submit / n_send, 1)) |>
    group_by(year_month) |>
    summarise(
      n_participants          = n(),
      total_sends             = sum(n_send),
      total_submits           = sum(n_submit),
      macro_avg_response_rate = mean(rate),
      .groups = "drop"
    )
}

fa4_by_month <- fa4_monthly_rate(msg_asb)
fa4_by_month_no_first_refresh <-
  fa4_monthly_rate(msg_asb |> filter(!template_id %in% REFRESH_ID_3RD))
fa4_overall <- fa4_stratified_rate(msg_asb |> mutate(all = "all ASB messages"), "all")

print_full(fa4_by_month, "\nFA4 by month, ASB messages only (macro-averaged):")
print_full(fa4_by_month_no_first_refresh,
           "\nFA4 by month, ASB messages only, without the first Refresh Event template:")
print_full(fa4_overall, "\nFA4 overall, ASB messages only (macro-averaged):")

# =============================================================================
# B. FA5a THRESHOLD-LEVEL BREAKDOWN (prereg 6.1 FA5a; 10.4 item 4)
# =============================================================================
cat("\n=== B. FA5a threshold-level breakdown (25/50/75/80%) ===\n")

REGISTERED_LIMIT_TOOLS <- c("DEPOSIT_LIMIT", "LOSS_LIMIT", "BET_LIMIT")
REGISTERED_LEVELS      <- c("25", "50", "75", "80")   # v3: unsigned (D18)

thr <- kafka_thresh |>
  mutate(year_month    = floor_date(as.Date(decision_month), "month"),
         threshold_lvl = sub("%$", "", threshold_pct))   # v3 normalisation
cat("Observed threshold_pct values (raw):\n")
print(table(thr$threshold_pct, useNA = "ifany"))
cat("Observed threshold levels (normalised):\n")
print(table(thr$threshold_lvl, useNA = "ifany"))
unexpected_levels <- setdiff(unique(thr$threshold_lvl),
                             c(REGISTERED_LEVELS, "100", NA))
if (length(unexpected_levels) > 0) {
  cat("NOTE: non-registered threshold levels observed:",
      paste(unexpected_levels, collapse = ", "),
      "- reported in threshold_by_level_other, not dropped\n")
}

thr_level_all <- thr |>
  group_by(year_month, limit_type, threshold_lvl) |>
  summarise(n_players_hit = n_distinct(participant_id),
            n_events      = n(), .groups = "drop")

threshold_by_level <- thr_level_all |>
  filter(limit_type %in% REGISTERED_LIMIT_TOOLS,
         threshold_lvl %in% REGISTERED_LEVELS) |>
  left_join(active_nonya_by_month, by = "year_month") |>
  mutate(
    pct_analytical_nonya = 100 * n_players_hit / n_nonya_analytical,
    pct_active_nonya     = 100 * n_players_hit / n_active_nonya,
    is_primary_threshold = (limit_type == "DEPOSIT_LIMIT" & threshold_lvl == "80") |
      (limit_type %in% c("LOSS_LIMIT", "BET_LIMIT") & threshold_lvl == "25")
  ) |>
  arrange(year_month, limit_type, threshold_lvl)

threshold_by_level_other <- thr_level_all |>
  filter(!(limit_type %in% REGISTERED_LIMIT_TOOLS) |
           !(threshold_lvl %in% REGISTERED_LEVELS))

print_full(threshold_by_level,
           "FA5a by threshold level (registered tools and levels; both denominators):")
print_full(threshold_by_level_other,
           "\nFA5a outside registered scope (non-registered tools / levels incl. 100%):")

# =============================================================================
# C. FA5b BY TOOL (prereg 6.1 FA5b) — v2: complete three-ID system-driven
#    exclusion (registered rule implemented in full), with the two-ID
#    (locked-build) comparison printed and saved (D16)
# =============================================================================
cat("\n=== C. FA5b limit modifications by tool (DL/LL/BL) ===\n")

refresh_dispatch_3id <- kafka_online |>
  filter(!is.na(participant_id), template_id %in% REFRESH_IDS_ALL3) |>
  distinct(participant_id, refresh_ts = processing_time)
refresh_dispatch_2id <- kafka_online |>
  filter(!is.na(participant_id), template_id %in% REFRESH_IDS_2ID) |>
  distinct(participant_id, refresh_ts = processing_time)
cat("Refresh dispatches: three-ID rule", nrow(refresh_dispatch_3id),
    "| two-ID (locked-build) rule", nrow(refresh_dispatch_2id), "\n")

kafka_intent <- kafka_intent |> mutate(intent_row = dplyr::row_number())

mark_system <- function(dispatch) {
  kafka_intent |>
    inner_join(dispatch, by = "participant_id", relationship = "many-to-many") |>
    filter(abs(as.numeric(processing_time - refresh_ts, units = "secs")) <=
             SYSTEM_DRIVEN_TIME_TOLERANCE_SECS) |>
    distinct(intent_row) |>
    pull(intent_row)
}
system_rows_3id <- mark_system(refresh_dispatch_3id)
system_rows_2id <- mark_system(refresh_dispatch_2id)
cat("Intent events flagged system-driven: three-ID", length(system_rows_3id),
    "| two-ID", length(system_rows_2id),
    "| newly excluded by the first-refresh template:",
    length(setdiff(system_rows_3id, system_rows_2id)), "\n")

# Eligible events (registered FA5b scope) for the exclusion comparison
eligible <- kafka_intent |>
  filter(is_monthly == TRUE, limit_type %in% REGISTERED_LIMIT_TOOLS) |>
  mutate(year_month = floor_date(as.Date(decision_month), "month"))

refresh_exclusion_compare <- eligible |>
  mutate(
    excl_3id = intent_row %in% system_rows_3id,
    excl_2id = intent_row %in% system_rows_2id
  ) |>
  group_by(year_month) |>
  summarise(
    n_eligible_events   = n(),
    n_excluded_3id      = sum(excl_3id),
    n_excluded_2id      = sum(excl_2id),
    diff_first_refresh  = sum(excl_3id & !excl_2id),
    .groups = "drop"
  )
print_full(refresh_exclusion_compare,
           "System-driven exclusion by month — three-ID (complete registered rule) vs two-ID (locked build):")
cat("Expectation per the finding: the difference concentrates in October 2024\n")
cat("(first Refresh Event dispatched via IMS on 1 October).\n")

# Reporting basis: THREE-ID exclusion (the registered rule, complete)
intent_by_tool <- eligible |>
  filter(!intent_row %in% system_rows_3id) |>
  group_by(year_month, limit_type) |>
  summarise(
    n_players_any_intent = n_distinct(participant_id),
    n_intents            = n(),
    n_protective         = sum(is_protective,  na.rm = TRUE),
    n_increase           = sum(!is_protective, na.rm = TRUE),
    .groups = "drop"
  )
print_full(intent_by_tool,
           "FA5b by tool and month (REPORTING BASIS: user-initiated, MONTHLY window, three-ID exclusion):")

# Reconciliation to 05b (which used the locked build = two-ID rule)
intent_totals_2id <- eligible |>
  filter(!intent_row %in% system_rows_2id) |>
  group_by(year_month) |>
  summarise(n_intents_total_2id = n(), .groups = "drop")
intent_totals_3id <- intent_by_tool |>
  group_by(year_month) |>
  summarise(n_intents_total_3id = sum(n_intents), .groups = "drop")
recon <- intent_totals_2id |> left_join(intent_totals_3id, by = "year_month")
print_full(recon, "\nMonthly intent totals under both rules:")
cat("Two-ID totals should reconcile with 05b's intent_monthly\n")
cat("(2,714 / 2,072 / 1,422 / 1,546 / 1,213 / 900 expected from the 05b means);\n")
cat("three-ID totals are the FA5b reporting basis; the October gap between the\n")
cat("two columns is the D16 contamination quantified.\n")

# =============================================================================
# SAVE (own output file only) + locked-file integrity check
# =============================================================================
saveRDS(list(
  codebook_derived          = parsed |> select(template_neutral = new_name, family,
                                               content, tier, ya_variant,
                                               message_stage, tool_name, content_type),
  fa4_by_type               = fa4_by_type,
  fa4_by_tool               = fa4_by_tool,
  fa4_by_risk               = fa4_by_risk,
  fa4_by_variant            = fa4_by_variant,
  fa4_type_by_risk          = fa4_type_by_risk,
  fa4_by_month              = fa4_by_month,              # v4: ASB messages only
  fa4_by_month_no_first_refresh = fa4_by_month_no_first_refresh,
  fa4_overall               = fa4_overall,
  fa4_n_asb_events          = n_asb,
  fa4_n_excluded_events     = n_excluded,
  n_first_refresh_events    = n_first_refresh_events,
  threshold_by_level        = threshold_by_level,
  threshold_by_level_other  = threshold_by_level_other,
  intent_by_tool            = intent_by_tool,            # three-ID reporting basis
  refresh_exclusion_compare = refresh_exclusion_compare, # D16: 3-ID vs 2-ID by month
  intent_totals_recon       = recon,
  n_nonya_analytical        = n_nonya_analytical,
  ya_excluded_n             = length(ya_ids)
), "results/05_descriptives/engagement_extraction.rds")
stamp("Results saved to results/05_descriptives/engagement_extraction.rds")

stopifnot("LOCKED FILE MTIME CHANGED DURING RUN — investigate immediately" =
            identical(locked_mtime_at_load, file.info(FILE_POC_LOCKED)$mtime))
cat("Locked-file integrity check passed: poc_analytical_LOCKED.rds untouched.\n")

cat("\n=== 05c_poc_engagement_extraction.R (v4) complete ===\n")
