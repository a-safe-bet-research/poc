# =============================================================================
# 10_poc_supplementary_descriptives.R  (Item 10, per the analysis decisions
#                                       register, 22 July 2026)
# Purpose : Supplementary descriptives package S-D1 to S-D7 (transparency
#           layer). VALUES ONLY: no tests, no risk-stratified outcome panels,
#           no covariate-outcome linking. Does not reopen the exploratory
#           scope note (dose-response remains declined).
# Project : A Safe Bet (ASB) - Proof-of-Concept evaluation.
# Inputs  : poc_analytical_LOCKED.rds (READ-ONLY; player_month for S-D6 and
#           the engagement summaries for volume context);
#           poc_tool_assignments.rds (S-D1);
#           poc_raw.rds (kafka_online / kafka_intent for the newly computed
#           monthly grains in S-D2, S-D3 and S-D5);
#           internal rename crosswalk (template rows only; never printed);
#           results/05_descriptives/engagement_extraction.rds (v3 run;
#           verified) - assembled panels S-D4, S-D5;
#           results/05_descriptives/descriptives_output.rds and
#           results/05_descriptives/descriptives_followup.rds (verified) -
#           reconciliation targets and carried inter-session interval;
#           results/tables/T4_monthly_metrics.rds and
#           results/tables/T5_engagement_categories.rds (patched 09 run,
#           source of record) - carried unchanged (S-D7).
# Outputs : results/10_supplementary/supplementary_descriptives.rds (OWN file)
# Log     : Rscript 10_poc_supplementary_descriptives.R 2>&1 | tee logs/10_real_YYYYMMDD.log
# =============================================================================
# STATUS  : POST-REGISTRATION presentation layer. Each panel is registered in
#           table_register as ASSEMBLED (from an existing verified output,
#           reprinted with its caption) or NEWLY COMPUTED (new grain not
#           retained by an earlier run). Newly computed panels reconcile to
#           their verified aggregates before anything is saved:
#             S-D1  per-tool x month inventory      NEWLY COMPUTED (FA3 run
#                   retained month x signal_class only); reconciles to the
#                   retained tool_summary exactly.
#             S-D2  message activity x month        NEWLY COMPUTED monthly
#                   grain (05c v3 retained stratum totals only); the pooled
#                   recomputation must match fa4_by_type exactly.
#             S-D3  content style x month           NEWLY COMPUTED monthly
#                   grain; pooled recomputation must match fa4_by_variant.
#             S-D4  threshold exposure              ASSEMBLED (05c v3).
#             S-D5  limit modifications             ASSEMBLED (05c v3) plus
#                   NEWLY COMPUTED monthly unique-modifier counts and the
#                   labelled out-of-scope panel; per-tool monthly cells must
#                   match the saved intent_by_tool exactly.
#             S-D6  engagement covariates           summary NEWLY COMPUTED in
#                   full (SD and medians not retained); overlapping values
#                   must match eng_cov_summary; monthly series NEWLY COMPUTED.
#             S-D7  T4, T5, inter-session interval  CARRIED unchanged.
#           Denominator basis is stated in every caption. Rounding left raw;
#           half-up 2 dp applied once at manuscript assembly.
# LOCKED-FILE PROTECTION: reads only; own outputs only; mtime assertion at end.
# RUN NOTE: loads poc_raw.rds (several GB); do not run alongside HMC fits.
# Mismatches and open questions route to HRG; nothing is resolved silently.
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

dir.create("results/10_supplementary", recursive = TRUE, showWarnings = FALSE)

# Captions used for the two S-D denominator bases (D7):
CAP_FIXED  <- "Denominator: fixed non-YA analytical sample, n = 5,640 (headline, D7)"
CAP_ACTIVE <- "Denominator: non-YA participants active in month (quoted alongside, D7)"

# -----------------------------------------------------------------------------
# 0. LOCKED FILE (READ-ONLY) + verified inputs
# -----------------------------------------------------------------------------
locked_mtime_at_load <- file.info(FILE_POC_LOCKED)$mtime
dat <- readRDS(FILE_POC_LOCKED)
pp  <- dat$player_period
pm  <- dat$player_month
cat("Running on LOCKED real data (read-only)\n")
cat("Item 10 supplementary descriptives: values only; no tests; no",
    "risk-stratified outcome panels; no covariate-outcome linking\n")

analytical_ids <- unique(pp$participant_id)
ya_ids     <- pp |> filter(young_adult_flag == 1) |> pull(participant_id) |> unique()
nonya_ids  <- setdiff(analytical_ids, ya_ids)
n_nonya_analytical <- length(nonya_ids)
cat("Analytical:", length(analytical_ids), "| non-YA:", n_nonya_analytical, "\n")

active_nonya_by_month <- pm |>
  filter(is_intervention == 1, participant_id %in% nonya_ids) |>
  group_by(year_month) |>
  summarise(n_active_nonya = sum(agd_count > 0), .groups = "drop")

ENGEXT_PATH <- "results/05_descriptives/engagement_extraction.rds"
DESC05_PATH <- "results/05_descriptives/descriptives_output.rds"
DESC5B_PATH <- "results/05_descriptives/descriptives_followup.rds"
T4_PATH     <- "results/tables/T4_monthly_metrics.rds"
T5_PATH     <- "results/tables/T5_engagement_categories.rds"
for (p in c(ENGEXT_PATH, DESC05_PATH, DESC5B_PATH, T4_PATH, T5_PATH)) {
  stopifnot("Verified input file missing - route to HRG before running" = file.exists(p))
}
engext <- readRDS(ENGEXT_PATH)
desc05 <- readRDS(DESC05_PATH)
desc5b <- readRDS(DESC5B_PATH)
# Guard: the engagement extraction on disk must be the v3 run (v2's registered
# threshold table was empty under the pre-normalisation level filter, D18)
stopifnot("engagement_extraction.rds does not look like the v3 run (empty registered threshold table) - route to HRG" =
            nrow(engext$threshold_by_level) > 0)

table_register <- list()
register <- function(id, status, source, caption) {
  table_register[[id]] <<- data.frame(panel = id, status = status,
                                      source = source, caption = caption)
}

# =============================================================================
# S-D1. TOOL DELIVERY INVENTORY - per tool x month (NEWLY COMPUTED: the FA3
#       run retained month x signal_class only, so the per-tool split was not
#       retained and is tabulated here from poc_tool_assignments.rds)
# =============================================================================
cat("\n=== S-D1. Tool delivery inventory (per tool x month) ===\n")

tools <- readRDS(FILE_POC_TOOLS)

sd1_tool_month <- tools |>
  group_by(assignment_date, tool_code, tool_name, signal_class) |>
  summarise(n_assignments = n(), .groups = "drop") |>
  pivot_wider(names_from = signal_class, values_from = n_assignments,
              values_fill = 0) |>
  arrange(assignment_date, tool_code)
print_full(sd1_tool_month,
  "S-D1: assignments per tool x month, ACTIVE / GHOST / LOW_SIGNAL in own columns\n(Denominator basis: assignment counts, full assignment file as delivered; no participant denominator)")

# Reconciliation 1: monthly class totals must equal the retained FA3 table
sd1_month_totals <- tools |>
  count(assignment_date, signal_class) |>
  pivot_wider(names_from = signal_class, values_from = n, values_fill = 0) |>
  arrange(assignment_date)
print_full(sd1_month_totals, "\nS-D1 reconciliation: monthly totals by signal class:")

fa3_ref <- desc05$tool_summary |> arrange(assignment_date)
common_cols <- intersect(names(sd1_month_totals), names(fa3_ref))
stopifnot(
  "S-D1 monthly totals do not reconcile to the retained FA3 tool_summary - route to HRG" =
    isTRUE(all.equal(as.data.frame(sd1_month_totals[common_cols]),
                     as.data.frame(fa3_ref[common_cols]),
                     check.attributes = FALSE))
)
cat("Reconciled EXACTLY to descriptives_output.rds tool_summary (FA3 run).\n")

# Reconciliation 2: the verified FA3 headline constants
active_by_month <- sd1_month_totals |> pull(ACTIVE)
ghost_total     <- sum(sd1_month_totals$GHOST)
grand_total     <- sum(sd1_month_totals[setdiff(names(sd1_month_totals),
                                                "assignment_date")])
cat(sprintf("ACTIVE per cycle range: %d to %d (verified FA3: 14,673 to 14,841)\n",
            min(active_by_month), max(active_by_month)))
cat(sprintf("Ghost total: %d of %d (verified FA3: 23,167 of 199,052)\n",
            ghost_total, grand_total))
stopifnot(
  "S-D1 ACTIVE range does not reconcile to FA3 - route to HRG" =
    min(active_by_month) == 14673L && max(active_by_month) == 14841L,
  "S-D1 ghost/grand totals do not reconcile to FA3 - route to HRG" =
    ghost_total == 23167L && grand_total == 199052L
)

register("S-D1", "NEWLY COMPUTED",
         "poc_tool_assignments.rds; reconciled exactly to the FA3 tool_summary and headline totals",
         "Assignment counts per tool x month with ACTIVE, GHOST and LOW_SIGNAL in own columns; assignment-count basis, no participant denominator")

# =============================================================================
# S-D2 / S-D3 PREPARATION - monthly message grain (NEWLY COMPUTED).
# The classification below reproduces the 05c v3 derivation IDENTICALLY
# (grammar codebook, three refresh templates, v3 breach-100 normalisation);
# the pooled recomputation is asserted against the verified fa4_by_type /
# fa4_by_variant before the monthly panels are saved.
# =============================================================================
cat("\n=== S-D2/S-D3 preparation: monthly message classification (05c v3 derivation) ===\n")

CROSSWALK_PATH <- Sys.getenv("ASB_CROSSWALK",
                             unset = file.path(ROOT_DATA, "ASB_rename_crosswalk.csv"))
stopifnot("Rename crosswalk not found" = file.exists(CROSSWALK_PATH))
XW   <- read.csv(CROSSWALK_PATH, stringsAsFactors = FALSE, na.strings = "")
tmpl <- XW |> filter(category == "template")

REFRESH_NEUTRAL_2ID <- c("msg_refresh_dynamic_rules", "msg_refresh_dynamic_rules_2")
REFRESH_NEUTRAL_3RD <- "msg_refresh_dynamic_rules_3"
REFRESH_IDS_2ID <- unique(tmpl$real_name[tmpl$new_name %in% REFRESH_NEUTRAL_2ID])
REFRESH_ID_3RD  <- unique(tmpl$real_name[tmpl$new_name == REFRESH_NEUTRAL_3RD])
stopifnot(length(REFRESH_IDS_2ID) == 2L, length(REFRESH_ID_3RD) == 1L)

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
      content == "all" ~ "unified",
      TRUE             ~ NA_character_
    )
  )
codebook <- parsed |>
  filter(!is.na(message_stage), !is.na(content_type)) |>
  transmute(template_id_real = real_name, template_neutral = new_name,
            message_stage, tool_name, content_type)
cat("Codebook rows usable for the FA4 join:", nrow(codebook),
    "(05c v3 classified 48 usable template rows)\n")

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

msg <- kafka_online |>
  mutate(year_month = floor_date(as.Date(decision_month), "month")) |>
  left_join(codebook, by = c("template_id" = "template_id_real"))

# Breach 100% pop-up rule with the v3 level normalisation (D18)
thresh_100 <- kafka_thresh |>
  filter(sub("%$", "", threshold_pct) == "100") |>
  distinct(participant_uid, t100_ts = processing_time)
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
msg_asb <- msg |> filter(!is.na(message_type))
cat("ASB-classified message events:", nrow(msg_asb),
    "(05c v3 verified: 159,669)\n")
stopifnot("ASB-classified event count does not reconcile to 05c v3 (159,669) - route to HRG" =
            nrow(msg_asb) == 159669L)

# Shared macro-average machinery (prereg 6.1 FA4 registered rule), with an
# optional year_month dimension for the new monthly grain
fa4_rate_by <- function(data, ...) {
  data |>
    group_by(..., participant_id, year_month_pm = year_month) |>
    summarise(
      n_send   = sum(is_sent,     na.rm = TRUE),
      n_submit = sum(is_response, na.rm = TRUE),
      n_rg     = sum(is_rg_button & is_response, na.rm = TRUE),
      .groups = "drop"
    ) |>
    filter(n_send > 0) |>
    mutate(rate = pmin(n_submit / n_send, 1)) |>
    group_by(...) |>
    summarise(
      n_participant_months    = n(),
      n_participants          = n_distinct(participant_id),
      total_sends             = sum(n_send),
      total_submits           = sum(n_submit),
      total_rg_clicks         = sum(n_rg),
      macro_avg_response_rate = mean(rate),
      .groups = "drop"
    )
}

# Pooled recomputation asserted against the verified 05c v3 aggregates
check_type <- fa4_rate_by(msg_asb, message_type) |> arrange(message_type)
ref_type   <- engext$fa4_by_type |> arrange(message_type)
stopifnot(
  "Pooled by-type sends/submits do not reconcile to fa4_by_type (05c v3) - route to HRG" =
    isTRUE(all.equal(check_type[c("total_sends", "total_submits")],
                     ref_type[c("total_sends", "total_submits")],
                     check.attributes = FALSE)),
  "Pooled by-type macro rates do not reconcile to fa4_by_type (05c v3) - route to HRG" =
    isTRUE(all.equal(check_type$macro_avg_response_rate,
                     ref_type$macro_avg_response_rate, tolerance = 1e-10))
)
cat("Pooled by-type recomputation reconciles EXACTLY to the verified fa4_by_type.\n")

check_var <- fa4_rate_by(msg_asb |> filter(!is.na(content_variant)),
                         content_variant) |> arrange(content_variant)
ref_var   <- engext$fa4_by_variant |> arrange(content_variant)
stopifnot(
  "Pooled by-variant recomputation does not reconcile to fa4_by_variant (05c v3) - route to HRG" =
    isTRUE(all.equal(check_var[c("total_sends", "total_submits")],
                     ref_var[c("total_sends", "total_submits")],
                     check.attributes = FALSE)) &&
    isTRUE(all.equal(check_var$macro_avg_response_rate,
                     ref_var$macro_avg_response_rate, tolerance = 1e-10))
)
cat("Pooled by-variant recomputation reconciles EXACTLY to the verified fa4_by_variant.\n")

# =============================================================================
# S-D2. MESSAGE ACTIVITY - per message type x month (NEWLY COMPUTED grain)
# =============================================================================
cat("\n=== S-D2. Message activity per message type x month ===\n")

sd2_type_month <- fa4_rate_by(msg_asb, message_type, year_month) |>
  arrange(message_type, year_month)
print_full(sd2_type_month,
  "S-D2: sends, responses, macro-averaged response rate and RG click-throughs per type x month\n(Denominator basis: macro-average of participant-month rates within type x month; zero-send participant-months excluded, prereg 6.1 FA4; non-YA analytical participants)")

# Volume context: sends per participant-month from the locked engagement
# summary (same source and YA scope as the verified FA4 monthly table)
sd2_volume_context <- dat$engagement$msg_summary |>
  filter(!participant_id %in% ya_ids, year_month >= INTERVENTION_START) |>
  group_by(year_month) |>
  summarise(n_players = n(),
            mean_sends_per_participant_month = mean(n_messages_sent, na.rm = TRUE),
            .groups = "drop")
print_full(sd2_volume_context,
  "\nS-D2 volume context: mean ASB message sends per participant-month\n(Denominator basis: non-YA participant-months in the locked engagement summary; matches the verified FA4 monthly table)")

register("S-D2", "NEWLY COMPUTED (monthly grain)",
         "poc_raw kafka_online with the 05c v3 codebook; pooled totals reconciled exactly to fa4_by_type; volume context from the locked engagement summary",
         "Sends, responses, macro-averaged response rate and RG click-throughs per message type x month; macro-average basis per prereg 6.1 FA4; non-YA analytical participants")

# =============================================================================
# S-D3. CONTENT STYLE - response rates by sub-style x month (NEWLY COMPUTED)
# =============================================================================
cat("\n=== S-D3. Content style: response rates by sub-style x month ===\n")

sd3_variant_month <- fa4_rate_by(msg_asb |> filter(!is.na(content_variant)),
                                 content_variant, year_month) |>
  arrange(content_variant, year_month)
print_full(sd3_variant_month,
  "S-D3: macro-averaged response rates by content sub-style (PPF / EPF / unified) x month\n(Denominator basis: macro-average of participant-month rates within sub-style x month; zero-send participant-months excluded; non-YA analytical participants; unified reported without split as registered)")

register("S-D3", "NEWLY COMPUTED (monthly grain)",
         "poc_raw kafka_online with the 05c v3 codebook; pooled totals reconciled exactly to fa4_by_variant",
         "Response rates by content sub-style (personalised PPF / enhanced EPF / unified) x month; macro-average basis; non-YA analytical participants")

# Kept for S-D5 (computed while kafka_online is still in memory; avoids a
# second poc_raw load): the three-ID refresh dispatch timestamps
refresh_dispatch_3id <- kafka_online |>
  filter(!is.na(participant_id),
         template_id %in% c(REFRESH_IDS_2ID, REFRESH_ID_3RD)) |>
  distinct(participant_id, refresh_ts = processing_time)
cat("Refresh dispatches (three-ID rule):", nrow(refresh_dispatch_3id), "\n")

rm(kafka_online, kafka_thresh, msg, msg_asb, null_tmpl, thresh_100)
invisible(gc(verbose = FALSE))

# =============================================================================
# S-D4. THRESHOLD EXPOSURE - full FA5a tool x level x month (ASSEMBLED from
#       the verified 05c v3 output; reprinted unchanged)
# =============================================================================
cat("\n=== S-D4. Threshold exposure: tool x level x month (assembled, 05c v3) ===\n")

print_full(engext$threshold_by_level,
  paste0("S-D4: registered tools and levels (25/50/75/80), both denominators; primary thresholds flagged\n(",
         CAP_FIXED, "; ", CAP_ACTIVE, ")"))
print_full(engext$threshold_by_level_other,
  "\nS-D4 supplement: outside registered scope - non-registered levels incl. 100 and non-registered tools (labelled per D6)\n(Denominator basis: participant and event counts only; no rate denominator)")
cat("\nS-D4 breach pop-up counts (from the verified fa4_by_type):\n")
print_full(engext$fa4_by_type |>
             filter(message_type == "breach_100_popup") |>
             select(message_type, n_participant_months, n_participants,
                    total_sends, total_submits, macro_avg_response_rate))

register("S-D4", "ASSEMBLED",
         "engagement_extraction.rds (05c v3): threshold_by_level, threshold_by_level_other, fa4_by_type breach row; unchanged",
         "FA5a tool x level x month with fixed n = 5,640 headline denominator and active-in-month alongside (D7); level-100 supplement rows and breach pop-up counts included; supplement labelled per D6")

# =============================================================================
# S-D5. LIMIT MODIFICATIONS - FA5b tool x direction x month (ASSEMBLED),
#       plus NEWLY COMPUTED monthly unique-modifier counts and the labelled
#       out-of-scope panel
# =============================================================================
cat("\n=== S-D5. Limit modifications by tool x direction x month ===\n")

print_full(engext$intent_by_tool,
  "S-D5: FA5b by tool x month, protective and increase intents in own columns (ASSEMBLED, 05c v3 reporting basis:\nuser-initiated, MONTHLY window, three-ID system-driven exclusion, registered DL/LL/BL scope)\n(Denominator basis: event and unique-participant counts among non-YA analytical participants)")

sd5_total_events <- sum(engext$intent_by_tool$n_intents)
cat(sprintf("\nRegistered-scope user-initiated events, total: %d (verified 05c v3 / 05e reconciliation: 9,497)\n",
            sd5_total_events))
stopifnot("S-D5 registered-scope event total does not reconcile to 9,497 - route to HRG" =
            sd5_total_events == 9497L)

# Newly computed: monthly unique modifiers across the three registered tools
# (distinct participants per month; not derivable from per-tool unique counts).
# Derivation reproduces the 05c v3 basis and is asserted against its cells.
REGISTERED_LIMIT_TOOLS <- c("DEPOSIT_LIMIT", "LOSS_LIMIT", "BET_LIMIT")
kafka_intent <- kafka_intent |> mutate(intent_row = dplyr::row_number())
system_rows_3id <- kafka_intent |>
  inner_join(refresh_dispatch_3id, by = "participant_id",
             relationship = "many-to-many") |>
  filter(abs(as.numeric(processing_time - refresh_ts, units = "secs")) <=
           SYSTEM_DRIVEN_TIME_TOLERANCE_SECS) |>
  distinct(intent_row) |>
  pull(intent_row)

eligible <- kafka_intent |>
  filter(is_monthly == TRUE, limit_type %in% REGISTERED_LIMIT_TOOLS,
         !intent_row %in% system_rows_3id) |>
  mutate(year_month = floor_date(as.Date(decision_month), "month"))

# Assert the re-derivation matches the verified per-tool monthly cells exactly
check_tool <- eligible |>
  group_by(year_month, limit_type) |>
  summarise(n_players_any_intent = n_distinct(participant_id),
            n_intents = n(), .groups = "drop") |>
  arrange(year_month, limit_type)
ref_tool <- engext$intent_by_tool |>
  select(year_month, limit_type, n_players_any_intent, n_intents) |>
  arrange(year_month, limit_type)
stopifnot("S-D5 re-derivation does not reconcile to the verified intent_by_tool cells - route to HRG" =
            isTRUE(all.equal(as.data.frame(check_tool), as.data.frame(ref_tool),
                             check.attributes = FALSE)))
cat("Three-ID re-derivation reconciles EXACTLY to the verified intent_by_tool.\n")

sd5_unique_modifiers <- eligible |>
  group_by(year_month) |>
  summarise(n_unique_modifiers = n_distinct(participant_id),
            n_intents = n(), .groups = "drop")
print_full(sd5_unique_modifiers,
  "\nS-D5 (NEWLY COMPUTED): monthly unique modifiers across the three registered tools\n(Denominator basis: distinct non-YA analytical participants with >= 1 user-initiated registered-scope intent that month)")

# Out-of-scope panel, labelled per the D6 supplement convention (D17 tool type)
sd5_out_of_scope <- kafka_intent |>
  filter(is_monthly == TRUE, !limit_type %in% REGISTERED_LIMIT_TOOLS,
         !intent_row %in% system_rows_3id) |>
  mutate(year_month = floor_date(as.Date(decision_month), "month")) |>
  group_by(year_month, limit_type) |>
  summarise(n_players = n_distinct(participant_id),
            n_intents = n(), .groups = "drop")
print_full(sd5_out_of_scope,
  "\nS-D5 out-of-scope panel (labelled per D6): user-initiated MONTHLY intents outside the registered DL/LL/BL scope\n(Denominator basis: event and unique-participant counts, non-YA analytical; the locked build's inclusion of this tool type is the disclosed D17 deviation)")
cat("Note: the D17 diagnostic expected roughly 370 out-of-scope user-initiated\n")
cat("events across the period; a materially different total routes to HRG.\n")

register("S-D5", "ASSEMBLED + NEWLY COMPUTED",
         "intent_by_tool assembled from engagement_extraction.rds (05c v3); monthly unique-modifier counts and out-of-scope panel newly computed on the identical three-ID basis, per-tool cells reconciled exactly",
         "FA5b by tool x direction x month, registered scope (9,497 events), with the out-of-scope panel labelled per D6 and monthly unique-modifier counts; non-YA analytical participants")

# =============================================================================
# S-D6. ENGAGEMENT COVARIATES - full summary + NEW monthly series
# =============================================================================
cat("\n=== S-D6. Engagement covariates (prereg 5.4 / 6.6) ===\n")

pm_iv <- pm |> filter(is_intervention == 1)
pm_bl <- pm |> filter(is_intervention != 1)

# Structural-zero footnote check (baseline)
bl_allzero <- sapply(ENGAGEMENT_COVARIATES, function(v) all(pm_bl[[v]] == 0, na.rm = TRUE))
print(bl_allzero)
stopifnot("Baseline engagement covariates are not all structurally zero - route to HRG" =
            all(bl_allzero))
cat("FOOTNOTE (verified): all four covariates are structurally zero at baseline\n")
cat("by design - ASB messaging and tailored limits existed only during the intervention.\n")

sd6_summary <- lapply(ENGAGEMENT_COVARIATES, function(v) {
  x <- pm_iv[[v]]
  data.frame(covariate = v,
             mean   = mean(x, na.rm = TRUE),
             median = median(x, na.rm = TRUE),
             sd     = sd(x, na.rm = TRUE),
             pct_nonzero = 100 * mean(x > 0, na.rm = TRUE),
             max    = max(x, na.rm = TRUE))
}) |> bind_rows()
print_full(sd6_summary,
  "S-D6: intervention-period summary per covariate (mean, median, SD, % non-zero, max)\n(Denominator basis: all 34,848 intervention participant-months, analytical sample, zeros included)")

# Reconciliation to the verified eng_cov_summary (05 run) on overlapping values
ecs <- desc05$eng_cov_summary |> filter(period == "intervention")
stopifnot(
  "S-D6 means do not reconcile to eng_cov_summary - route to HRG" =
    isTRUE(all.equal(sd6_summary$mean,
                     c(ecs$msg_resp_mean, ecs$n_lim_int_mean,
                       ecs$lim_dwell_mean, ecs$rg_dwell_mean),
                     tolerance = 1e-10)),
  "S-D6 % non-zero does not reconcile to eng_cov_summary - route to HRG" =
    isTRUE(all.equal(sd6_summary$pct_nonzero,
                     c(ecs$msg_resp_pct_nz, ecs$n_lim_int_pct_nz,
                       ecs$lim_dwell_pct_nz, ecs$rg_dwell_pct_nz),
                     tolerance = 1e-10))
)
cat("Overlapping values reconcile EXACTLY to eng_cov_summary (verified: means\n")
cat("0.162 / 0.289 / 23.3 s / 11.1 s; % non-zero 66.4 / 11.9 / 10.9 / 7.0).\n")

# NEW: monthly series (mean and % non-zero per covariate per intervention month)
sd6_monthly_series <- pm_iv |>
  group_by(year_month) |>
  summarise(across(all_of(ENGAGEMENT_COVARIATES),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        pct_nonzero = ~100 * mean(.x > 0, na.rm = TRUE))),
            .groups = "drop")
print_full(sd6_monthly_series,
  "\nS-D6 (NEWLY COMPUTED): monthly series - mean and % non-zero per covariate per month\n(Denominator basis: all analytical participant-months in that month, zeros included)")

register("S-D6", "NEWLY COMPUTED",
         "player_month (locked, read-only); overlapping summary values reconciled exactly to eng_cov_summary (05 run)",
         "Four registered engagement covariates: intervention mean, median, SD, % non-zero, max, plus the monthly mean and % non-zero series; all analytical participant-months, zeros included; all four covariates structurally zero at baseline (footnote)")

# =============================================================================
# S-D7. CARRIED TABLES - T4, T5 and the inter-session interval (unchanged)
# =============================================================================
cat("\n=== S-D7. Carried tables (unchanged) ===\n")

sd7_t4 <- readRDS(T4_PATH)
sd7_t5 <- readRDS(T5_PATH)
sd7_isi <- desc5b$isi_summary
print_full(sd7_t4,  "S-D7: T4 monthly metrics (CARRIED unchanged from the patched 09 run, source of record)")
print_full(sd7_t5,  "\nS-D7: T5 engagement categories (CARRIED unchanged from the patched 09 run)\n(Denominator basis: analytical sample N = 5,808)")
print_full(sd7_isi, "\nS-D7: inter-session interval (CARRIED unchanged from the verified 05b run)\n(Denominator basis: participants with >= 2 intervention AGDs, n = 4,976)")

register("S-D7", "CARRIED",
         "results/tables T4 and T5 (patched 09 run) and isi_summary (descriptives_followup.rds); unchanged",
         "T4 monthly metrics, T5 engagement categories (analytical N = 5,808) and the inter-session interval (n = 4,976 with >= 2 intervention AGDs)")

# =============================================================================
# SAVE (own output only) + LOCKED-FILE INTEGRITY
# =============================================================================
table_register_df <- bind_rows(table_register)
print_full(table_register_df, "\nPanel register (status per panel):")

out <- list(
  label = "Item 10 supplementary descriptives (transparency layer; values only; post-registration presentation layer)",
  scope_note = "No tests, no risk-stratified outcome panels, no covariate-outcome linking; the exploratory scope note is not reopened",
  rounding_note = "Values left raw; half-up 2 dp applied once at manuscript assembly",
  table_register       = table_register_df,
  sd1_tool_month       = sd1_tool_month,
  sd1_month_totals     = sd1_month_totals,
  sd2_type_month       = sd2_type_month,
  sd2_volume_context   = sd2_volume_context,
  sd3_variant_month    = sd3_variant_month,
  sd4_threshold_by_level       = engext$threshold_by_level,
  sd4_threshold_by_level_other = engext$threshold_by_level_other,
  sd4_breach_row       = engext$fa4_by_type |> filter(message_type == "breach_100_popup"),
  sd5_intent_by_tool   = engext$intent_by_tool,
  sd5_unique_modifiers = sd5_unique_modifiers,
  sd5_out_of_scope     = sd5_out_of_scope,
  sd6_summary          = sd6_summary,
  sd6_monthly_series   = sd6_monthly_series,
  sd6_baseline_all_zero = bl_allzero,
  sd7_t4               = sd7_t4,
  sd7_t5               = sd7_t5,
  sd7_isi              = sd7_isi
)
saveRDS(out, "results/10_supplementary/supplementary_descriptives.rds")
stamp("Saved results/10_supplementary/supplementary_descriptives.rds")

stopifnot("LOCKED FILE MTIME CHANGED DURING RUN - investigate immediately" =
            identical(locked_mtime_at_load, file.info(FILE_POC_LOCKED)$mtime))
cat("Locked-file integrity check passed: poc_analytical_LOCKED.rds untouched.\n")
stamp("10_poc_supplementary_descriptives.R complete - Item 10 package delivered.")
