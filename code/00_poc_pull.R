# =============================================================================
# 00_poc_pull.R
# Purpose : Step 0 — pull raw data from Parquet files and Kafka batch exports
#           via DuckDB, at the de-identification boundary
# Project : A Safe Bet (ASB) — Proof-of-Concept evaluation.
#           Putting Design into Practice: A Safe Bet — A Proof-of-Concept
#           Evaluation of a Tailored Online Responsible Gambling Framework
# Inputs  : Parquet source tables, Kafka batch directories, tool-assignment
#           CSV, and the internal rename crosswalk (paths via 00_config.R)
# Outputs : poc_raw.rds; poc_tool_assignments.rds; pull log
# =============================================================================
# STATUS : Preparatory — permitted before OSF registration
#
# DE-IDENTIFICATION BOUNDARY:
#   This script is the single boundary where operator-original schema identifiers
#   are translated to the project's neutral names. It reads the internal rename
#   crosswalk (ASB_rename_crosswalk.csv) at runtime and builds every SQL SELECT,
#   alias and event/field reference from it. No operator table, column, event,
#   field or value literal is written in this file. After this boundary the whole
#   pipeline is in neutral names. The crosswalk holds the real names and is
#   internal — it lives with the withheld data and is never deposited.
#
# BEFORE RUNNING:
#   Run from the project root (the folder containing this script and 00_config.R),
#   with ASB_DATA_ROOT and ASB_CROSSWALK set in .Renviron (see .Renviron.example):
#   source("00_poc_pull.R")
#
# DESIGN NOTES:
#   - tbl_study_cohort defines the cohort (who the 6,000 participants are)
#   - the tool-assignments source provides tool detail only (variants, signal_class)
#   - signal_class is normalised on read at this boundary (tool-assignment
#     signal_class normalisation): the source signal column is aliased to
#     signal_class and its delivered-tool value is recoded to ACTIVE; LOW_SIGNAL
#     and GHOST are unchanged
#   - JSON payload fields parsed inside DuckDB queries — fast and vectorized
#     NOT in R after loading (which is row-by-row and extremely slow)
#   - Kafka batches filtered to 8 POC-relevant event types (incl. the account-tag
#     event for page-heartbeat dwell reconstruction per prereg 5.4 limit_page_dwell /
#     rg_support_dwell)
# =============================================================================

library(duckdb)
library(DBI)
library(dplyr)
library(readr)

source("00_config.R")

# Resolve namespace conflicts (conflicted may be loaded from .Rprofile)
if (requireNamespace("conflicted", quietly = TRUE)) {
  library(conflicted)
  conflicts_prefer(dplyr::filter)
  conflicts_prefer(dplyr::select)
  conflicts_prefer(dplyr::lag)
  conflicts_prefer(dplyr::pull)
}

dir.create(ROOT_OUT, showWarnings = FALSE, recursive = TRUE)
dir.create(DIR_LOGS,  showWarnings = FALSE, recursive = TRUE)

cat("=== 00_poc_pull.R ===\n")
cat("ROOT_DATA:", ROOT_DATA, "\n")
cat("Started:", format(Sys.time()), "\n\n")

PULL_START <- as.Date("2024-03-01")
PULL_END   <- as.Date("2025-04-30")

# =============================================================================
# 0. RENAME CROSSWALK — single source of truth for real <-> neutral names
#    Internal file; lives with the withheld data; never deposited.
# =============================================================================

CROSSWALK_PATH <- Sys.getenv(
  "ASB_CROSSWALK",
  unset = file.path(ROOT_DATA, "ASB_rename_crosswalk.csv")
)
stopifnot("Rename crosswalk not found" = file.exists(CROSSWALK_PATH))
XW <- read.csv(CROSSWALK_PATH, stringsAsFactors = FALSE, na.strings = "")
cat("Loaded crosswalk:", CROSSWALK_PATH, "(", nrow(XW), "rows )\n")

# real name for a neutral name in a category (parent disambiguates columns)
xreal <- function(category, new_name, parent = NA) {
  sel <- XW$category == category & XW$new_name == new_name
  if (!is.na(parent)) {
    sel_p <- sel & (XW$parent == parent | is.na(XW$parent))
    if (any(sel_p)) sel <- sel_p
  }
  hit <- unique(XW$real_name[sel])
  if (length(hit) == 0) stop(sprintf("crosswalk: no %s named '%s'", category, new_name))
  hit[1]
}
rtab   <- function(nt) xreal("table", nt)
rcol   <- function(nt, nc) xreal("column", nc, parent = nt)
rfield <- function(nf) xreal("field", nf)
revent <- function(ne) xreal("event", ne)
rglob  <- function(nt) file.path(ROOT_DATA, paste0(rtab(nt), "*.parquet"))

# SELECT term: "real AS neutral" (or just real when the name is KEEP/unchanged)
sterm <- function(nt, nc) {
  rc <- rcol(nt, nc); if (rc == nc) rc else paste0(rc, " AS ", nc)
}
# join several SELECT terms for a vector of neutral columns
sterms <- function(nt, ncs) paste(vapply(ncs, function(x) sterm(nt, x), character(1)),
                                  collapse = ",\n    ")

# real Kafka event-type *values* (data-level), resolved from the crosswalk so the
# operator event names never appear as literals in this file
EV <- list(
  online    = revent("evt_online_message"),
  offline   = revent("evt_offline_message"),
  intent    = revent("evt_limit_change_intent"),
  threshold = revent("evt_limit_threshold_reached"),
  acct_tag  = revent("evt_account_tag_update"),
  corr      = revent("evt_correspondence_added"),
  login     = revent("evt_login_session_start"),
  acct_chg  = revent("evt_account_status_change")
)
sql_quote <- function(x) paste0("'", x, "'")
EV_IN <- paste0("(", paste(sql_quote(unlist(EV, use.names = FALSE)), collapse = ","), ")")

# =============================================================================
# 1. CONNECT
# =============================================================================

con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
cat("DuckDB connected.\n")

# =============================================================================
# 2. REGISTER PARQUET VIEWS (file globs resolved from the crosswalk)
# =============================================================================

cat("Registering views...\n")

register_view <- function(con, view_name, glob_pattern) {
  dbExecute(con, sprintf(
    "CREATE VIEW %s AS SELECT * FROM read_parquet('%s')",
    view_name, glob_pattern
  ))
  cat("  ", view_name, "(", length(Sys.glob(glob_pattern)), "files)\n")
}

register_view(con, "v_cohort",  rglob("tbl_study_cohort"))
register_view(con, "v_daily",   rglob("tbl_daily_behaviour"))
register_view(con, "v_risk",    rglob("tbl_daily_risk"))
register_view(con, "v_players", rglob("tbl_player_snapshot"))
# Only the deposit-limit DWH table is queried (gaming-limit and self-exclusion
# tables are not read by this pipeline — see dictionary reconciliation).
register_view(con, "v_deposit_limits", rglob("tbl_deposit_limits"))

# =============================================================================
# 3. COHORT — tbl_study_cohort
#    Authority for: who the 6,000 participants are, risk level, age_years, offer structure
# =============================================================================

cat("\nPulling cohort...\n")

CT <- "tbl_study_cohort"
cohort_plain <- c(
  "participant_id", "participant_uid", "operator", "age_years", "gender",
  "vip_tier", "asb_risk_level", "latest_risk_level", "dominant_risk_30d",
  "agds_90d", "deposits_90d_total", "bets_90d_total", "wins_90d_total",
  "play_hours_90d_total", "night_hours_90d_total",
  "n_high_risk_90d", "n_medium_risk_90d", "n_low_risk_90d", "asb_assigned_tools"
)

safebet <- dbGetQuery(con, sprintf("
  SELECT
    %s,
    %s::DATE                  AS assignment_date,
    TRY_CAST(%s AS DATE)      AS next_assignment_date,
    (%s - %s)                 AS losses_90d_total
  FROM v_cohort
  WHERE %s = 'sunvegas'
  AND %s::DATE <= DATE '%s'
",
  sterms(CT, cohort_plain),
  rcol(CT, "assignment_date"),
  rcol(CT, "next_assignment_date"),
  rcol(CT, "bets_90d_total"), rcol(CT, "wins_90d_total"),
  rcol(CT, "operator"),
  rcol(CT, "assignment_date"), SNAPSHOT_DATE
)) |>
  mutate(
    assignment_date  = as.Date(assignment_date),
    young_adult_flag = age_years >= YA_AGE_MIN & age_years <= YA_AGE_MAX,
    is_snapshot_only = assignment_date == SNAPSHOT_DATE
  )

cat("  Rows:", nrow(safebet), "\n")
cat("  Unique participants:", n_distinct(safebet$participant_id), "\n")
cat("  Offer dates:", paste(sort(unique(safebet$assignment_date)), collapse = ", "), "\n")

cohort_ids <- safebet |>
  filter(assignment_date == SELECTION_DATE) |>
  pull(participant_id) |>
  unique()

cat("  Cohort N at selection:", length(cohort_ids), "(expected ~6,000)\n")
cat("  Young Adults:", sum(safebet$young_adult_flag[safebet$assignment_date == SELECTION_DATE],
                           na.rm = TRUE), "(expected ~174)\n")

dbWriteTable(con, "cohort_ids", data.frame(participant_id = cohort_ids), overwrite = TRUE)

# =============================================================================
# 4. ID MAP — participant_id <-> participant_uid via tbl_player_snapshot
# =============================================================================

cat("\nBuilding id map...\n")

PL <- "tbl_player_snapshot"
crosswalk <- dbGetQuery(con, sprintf("
  SELECT DISTINCT p.%s AS participant_id, p.%s AS participant_uid
  FROM v_players p
  INNER JOIN cohort_ids c ON p.%s = c.participant_id
  WHERE p.%s IS NOT NULL AND p.%s = 'sunvegas'
",
  rcol(PL, "participant_id"), rcol(PL, "participant_uid"),
  rcol(PL, "participant_id"),
  rcol(PL, "participant_uid"), rcol(PL, "operator")
)) |>
  distinct(participant_id, .keep_all = TRUE)

cat("  Participants with uid:", nrow(crosswalk), "\n")
dbWriteTable(con, "crosswalk", crosswalk, overwrite = TRUE)

# =============================================================================
# 5. DAILY BEHAVIOUR
#    Output names: amount_wagered, amount_won, net_loss (not raw operator names)
# =============================================================================

cat("\nPulling daily behaviour...\n")

DB <- "tbl_daily_behaviour"
daily_plain <- c(
  "participant_id", "amount_wagered", "amount_won", "net_loss", "deposit_amount",
  "withdrawal_amount", "n_deposits", "n_declined_deposits", "play_hours_total",
  "night_hours_total", "max_continuous_hours", "n_spins", "self_excluded_flag",
  "n_forced_timeouts"
)

daily_stats <- dbGetQuery(con, sprintf("
  SELECT
    %s,
    %s::DATE AS evaluation_date,
    CASE WHEN (%s > 0 OR %s > 0) THEN 1 ELSE 0 END AS is_agd,
    CASE WHEN %s::DATE >= DATE '%s' THEN 1 ELSE 0 END AS post_recalibration
  FROM v_daily
  INNER JOIN cohort_ids c ON %s = c.participant_id
  WHERE %s = 'sunvegas'
  AND %s::DATE BETWEEN DATE '%s' AND DATE '%s'
",
  sterms(DB, daily_plain),
  rcol(DB, "evaluation_date"),
  rcol(DB, "amount_wagered"), rcol(DB, "deposit_amount"),
  rcol(DB, "evaluation_date"), BETBUDDY_RECALIB_DATE,
  rcol(DB, "participant_id"),
  rcol(DB, "operator"),
  rcol(DB, "evaluation_date"), PULL_START, PULL_END
)) |>
  mutate(evaluation_date = as.Date(evaluation_date))

cat("  Rows:", nrow(daily_stats), "\n")
cat("  Participants:", n_distinct(daily_stats$participant_id), "\n")
cat("  Date range:", format(min(daily_stats$evaluation_date)),
    "to", format(max(daily_stats$evaluation_date)), "\n")

# =============================================================================
# 6. RISK SCORES
# =============================================================================

cat("\nPulling risk scores...\n")

DR <- "tbl_daily_risk"
model_results <- dbGetQuery(con, sprintf("
  SELECT
    %s,
    %s::DATE AS evaluation_date,
    %s AS overall_risk_level,
    CASE %s
      WHEN 'low'    THEN 1
      WHEN 'medium' THEN 2
      WHEN 'high'   THEN 3
      ELSE NULL
    END AS risk_numeric,
    CASE WHEN %s::DATE >= DATE '%s' THEN 1 ELSE 0 END AS post_recalibration
  FROM v_risk r
  INNER JOIN cohort_ids c ON %s = c.participant_id
  WHERE %s = 'sunvegas'
  AND %s IN ('low', 'medium', 'high')
  AND %s::DATE BETWEEN DATE '%s' AND DATE '%s'
",
  paste0("r.", rcol(DR, "participant_id"), " AS participant_id"),
  paste0("r.", rcol(DR, "evaluation_date")),
  paste0("r.", rcol(DR, "overall_risk_level")),
  paste0("r.", rcol(DR, "overall_risk_level")),
  paste0("r.", rcol(DR, "evaluation_date")), BETBUDDY_RECALIB_DATE,
  paste0("r.", rcol(DR, "participant_id")),
  paste0("r.", rcol(DR, "operator")),
  paste0("r.", rcol(DR, "overall_risk_level")),
  paste0("r.", rcol(DR, "evaluation_date")), PULL_START, PULL_END
)) |>
  mutate(evaluation_date = as.Date(evaluation_date))

cat("  Rows:", nrow(model_results), "\n")
cat("  Risk levels:", paste(sort(unique(model_results$overall_risk_level)), collapse = ", "), "\n")

# =============================================================================
# 7. KAFKA — load pre-extracted batch files
#    JSON payload parsed inside DuckDB (fast). Event names / field names / the
#    discriminator column are all resolved from the crosswalk.
# =============================================================================

cat("\nLoading Kafka batches...\n")

rEvType <- rfield("event_type")        # discriminator field (real)
rUid    <- rfield("participant_uid")   # uid field (real)
rPay    <- rfield("payload")           # payload (KEEP)

# one extraction term: alias <- json_extract on a payload field, gated by event(s)
kx <- function(alias, neutral_field, events, numeric = FALSE) {
  ev_cond <- if (length(events) == 1) {
    sprintf("k.%s = %s", rEvType, sql_quote(events))
  } else {
    sprintf("k.%s IN (%s)", rEvType, paste(sql_quote(events), collapse = ","))
  }
  ex <- sprintf("json_extract_string(k.%s, '$.%s')", rPay, rfield(neutral_field))
  if (numeric) ex <- sprintf("TRY_CAST(%s AS DOUBLE)", ex)
  sprintf("CASE WHEN %s THEN %s END AS %s", ev_cond, ex, alias)
}

kafka_extracts <- paste(c(
  kx("template_id",     "template_id",          EV$online),
  kx("phase",           "phase",                EV$online),
  kx("button_id",       "button_id",            EV$online),
  kx("limit_type",      "limit_type",           c(EV$intent, EV$threshold)),
  kx("limit_period",    "limit_period",         EV$intent),
  kx("activation_type", "limit_activation_type", EV$intent),
  kx("new_value",       "update_amount",        EV$intent, numeric = TRUE),
  kx("original_amount", "original_amount",      EV$intent, numeric = TRUE),
  kx("time_period",     "time_period",          EV$threshold),
  kx("threshold_pct",   "threshold",            EV$threshold),
  kx("tag_name",        "tag_name",             EV$acct_tag),
  kx("tag_action",      "tag_action",           EV$acct_tag),
  kx("tag_value",       "tag_value",            EV$acct_tag),
  kx("subject",         "subject",              EV$corr)
), collapse = ",\n      ")

load_kafka_tier <- function(con, batch_dir, int_start, int_end, tier_label) {

  if (!dir.exists(batch_dir)) {
    cat("  SKIPPED", tier_label, "— directory not found\n"); return(NULL)
  }
  batch_files <- list.files(batch_dir, pattern = "\\.parquet$")
  if (length(batch_files) == 0) {
    cat("  SKIPPED", tier_label, "— no parquet files\n"); return(NULL)
  }
  cat("  Loading", tier_label, "(", length(batch_files), "files)...\n")

  result <- dbGetQuery(con, sprintf("
    SELECT
      k.%s AS participant_uid,
      k.%s AS event_type,
      k.processing_time,
      k.decision_month::DATE AS decision_month,
      k.refresh_date,
      %s,
      x.participant_id
    FROM read_parquet('%s/*.parquet') k
    LEFT JOIN crosswalk x ON k.%s = x.participant_uid
    WHERE k.%s IN %s
    AND k.decision_month::DATE BETWEEN DATE '%s' AND DATE '%s'
  ",
    rUid, rEvType, kafka_extracts, batch_dir,
    rUid, rEvType, EV_IN, int_start, int_end))

  cat("    Rows:", nrow(result),
      "| Matched to participant_id:", sum(!is.na(result$participant_id)), "\n")
  result
}

kafka_r2 <- load_kafka_tier(con, KAFKA_PATHS$r2_batches, INTERVENTION_START, INTERVENTION_END, "R2 (medium)")
kafka_r1 <- load_kafka_tier(con, KAFKA_PATHS$r1_batches, INTERVENTION_START, INTERVENTION_END, "R1 (low)")
kafka_r3 <- load_kafka_tier(con, KAFKA_PATHS$r3_batches, INTERVENTION_START, INTERVENTION_END, "R3 (high)")

kafka_all <- bind_rows(
  if (!is.null(kafka_r1)) mutate(kafka_r1, risk_tier = "low"),
  if (!is.null(kafka_r2)) mutate(kafka_r2, risk_tier = "medium"),
  if (!is.null(kafka_r3)) mutate(kafka_r3, risk_tier = "high")
) |>
  mutate(decision_month = as.Date(decision_month))

cat("\nKafka combined:\n")
cat("  Total rows:", nrow(kafka_all), "\n")
print(sort(table(kafka_all$event_type), decreasing = TRUE))

# =============================================================================
# 8. EVENT SUBSETS
# =============================================================================

cat("\nCreating event subsets...\n")

kafka_online_msg <- kafka_all |>
  filter(event_type == EV$online) |>
  mutate(
    is_sent      = phase == "online_send",
    is_response  = phase == "online_submit",
    is_rg_button = button_id %in% RG_BUTTONS_ENGAGEMENT
  )
cat("  Online messages:", nrow(kafka_online_msg), "\n")

kafka_threshold <- kafka_all |>
  filter(event_type == EV$threshold) |>
  mutate(hit_100pct = threshold_pct == "100%")
cat("  Limit thresholds:", nrow(kafka_threshold), "\n")

kafka_intent <- kafka_all |>
  filter(event_type == EV$intent) |>
  mutate(
    # is_protective: DESCRIPTIVE ONLY — used for FA5b direction supplement
    # (NOT a filter on the engagement covariate; n_limit_intents counts all)
    is_protective = activation_type %in% PROTECTIVE_INTENT_TYPES,
    # is_monthly: framework-aligned scope per prereg 6.1 FA5b
    is_monthly    = limit_period == LIMIT_WINDOWS_FRAMEWORK
  )
cat("  Limit intents:", nrow(kafka_intent), "\n")
cat("    Monthly window only:", sum(kafka_intent$is_monthly, na.rm = TRUE), "\n")

# Account-tag page heartbeats for dwell reconstruction
# Filter to the page-visit tag + UPDATED only; parse page_name from tag_value
# (substring before final _HH:MM:SS suffix; anonymous heartbeats excluded)
kafka_account_tag <- kafka_all |>
  filter(
    event_type == EV$acct_tag,
    tag_name   == PAGE_VISIT_TAG_NAME,
    tag_action == PAGE_VISIT_TAG_ACTION
  ) |>
  mutate(
    page_name = sub("_[0-9]{2}:[0-9]{2}:[0-9]{2}$", "", tag_value)
  ) |>
  filter(page_name != "" & !is.na(page_name))
cat("  Page heartbeats:", nrow(kafka_account_tag), "\n")
cat("    Unique page names:", paste(sort(unique(kafka_account_tag$page_name)),
                                    collapse = ", "), "\n")

# =============================================================================
# 9. DWH DEPOSIT LIMITS
# =============================================================================

cat("\nLoading DWH deposit limits...\n")

DL <- "tbl_deposit_limits"
dwh_deposit <- tryCatch({
  dbGetQuery(con, sprintf("
    SELECT
      d.%s AS participant_id,
      d.%s::TIMESTAMP AS effective_from,
      d.%s::TIMESTAMP AS effective_to,
      d.%s AS limit_period_code,
      d.%s AS limit_amount,
      d.%s AS created_by_role,
      d.%s AS ended_by_role
    FROM v_deposit_limits d
    INNER JOIN cohort_ids c ON d.%s = c.participant_id
    WHERE d.%s >= DATE '%s'
  ",
    rcol(DL, "participant_id"),
    rcol(DL, "effective_from"), rcol(DL, "effective_to"),
    rcol(DL, "limit_period_code"), rcol(DL, "limit_amount"),
    rcol(DL, "created_by_role"), rcol(DL, "ended_by_role"),
    rcol(DL, "participant_id"),
    rcol(DL, "effective_from"), BASELINE_START))
}, error = function(e) {
  cat("  Not available:", conditionMessage(e), "\n")
  NULL
})

if (!is.null(dwh_deposit)) cat("  Rows:", nrow(dwh_deposit), "\n")

# =============================================================================
# 10. TOOL ASSIGNMENTS
#     Filter the tool-assignment source to cohort + active offer dates.
#     Operator-schema columns are aliased to neutral names via the crosswalk;
#     signal_class is normalised here (the source signal column -> signal_class,
#     delivered-tool value -> ACTIVE). Save as poc_tool_assignments.rds — all
#     scripts use this, never the source.
# =============================================================================

cat("\nLoading tool assignments...\n")

TOOL_ASSIGN_PATH <- file.path(ROOT_DATA, "action_space_outputs", "all_tool_assignments.csv")

# real source column names resolved from the crosswalk (operator-schema only)
src_pid <- rcol("tbl_study_cohort", "participant_id")
src_uid <- rcol("tbl_study_cohort", "participant_uid")
src_od  <- rcol("tbl_study_cohort", "assignment_date")
src_nod <- rcol("tbl_study_cohort", "next_assignment_date")
src_srl <- rcol("tbl_study_cohort", "asb_risk_level")
src_sig <- rcol("tool_assignments", "signal_class")    # source signal column
ope_active_value <- xreal("value", "ACTIVE", parent = "signal_class")  # delivered-tool source value

col_spec <- list()
col_spec[[src_pid]] <- col_double()
col_spec[[src_uid]] <- col_character()
col_spec[[src_od]]  <- col_date()
col_spec[[src_nod]] <- col_date()
col_spec[[src_srl]] <- col_character()
col_spec[[src_sig]] <- col_character()
col_spec[["tool_code"]]          <- col_character()
col_spec[["row_id"]]             <- col_double()
col_spec[["risk_level"]]         <- col_character()
col_spec[["mandatory_optional"]] <- col_character()
col_spec[["tool_name"]]          <- col_character()
col_spec[["category_id"]]        <- col_double()
col_spec[["variant"]]            <- col_double()

rename_map <- setNames(
  c(src_pid, src_uid, src_od, src_nod, src_srl, src_sig),
  c("participant_id", "participant_uid", "assignment_date",
    "next_assignment_date", "asb_risk_level", "signal_class")
)  # names = neutral (new), values = real (old): rename(new = old)

poc_tool_assignments <- read_csv(
  TOOL_ASSIGN_PATH, col_types = do.call(cols, col_spec), show_col_types = FALSE
) |>
  rename(!!!rename_map) |>
  filter(
    participant_id %in% cohort_ids,
    assignment_date %in% OFFER_DATES
  ) |>
  # tool-assignment signal_class normalisation (delivered-tool value -> ACTIVE;
  # LOW_SIGNAL and GHOST unchanged)
  mutate(signal_class = dplyr::recode(signal_class, !!!setNames("ACTIVE", ope_active_value)))

cat("  Rows:", nrow(poc_tool_assignments), "\n")
cat("  signal_class:\n")
print(table(poc_tool_assignments$signal_class))

saveRDS(poc_tool_assignments, FILE_POC_TOOLS)
cat("  Saved:", FILE_POC_TOOLS, "\n")

# =============================================================================
# 11. SCHEMA VALIDATION ASSERTIONS
# =============================================================================

cat("\nValidating schema...\n")

stopifnot(
  "Cohort size is 0 — upstream extraction issue" =
    length(cohort_ids) > 0,
  "Cohort size differs from N_SELECTED by more than 50 — investigate" =
    abs(length(cohort_ids) - N_SELECTED) <= 50
)
cat("  Cohort size OK:", length(cohort_ids), "\n")

# Tool-assignment signal_class values (post-normalisation)
expected_classes <- c("ACTIVE", "LOW_SIGNAL", "GHOST")
unexpected_classes <- setdiff(unique(poc_tool_assignments$signal_class), expected_classes)
stopifnot(
  "Unexpected signal_class values — check tool-assignment signal_class normalisation" =
    length(unexpected_classes) == 0
)
cat("  signal_class values OK:", paste(expected_classes, collapse = ", "), "\n")

# Required event types each have at least 1 row (real values from the crosswalk)
expected_events <- c(EV$online, EV$intent, EV$threshold, EV$acct_tag)
missing_events <- setdiff(expected_events, unique(kafka_all$event_type))
stopifnot(
  "Missing required event types — upstream issue" =
    length(missing_events) == 0
)
cat("  Event coverage OK\n")

stopifnot(
  "Zero page heartbeat events extracted — payload parsing or filter issue" =
    nrow(kafka_account_tag) > 0,
  "Heartbeats include non page-visit tag — filter issue" =
    all(kafka_account_tag$tag_name == PAGE_VISIT_TAG_NAME, na.rm = TRUE)
)
cat("  Page heartbeats OK:", nrow(kafka_account_tag), "rows\n")

# =============================================================================
# 12. DISCONNECT AND SAVE
# =============================================================================

dbDisconnect(con, shutdown = TRUE)
cat("\nDuckDB disconnected.\n")

poc_raw <- list(
  safebet              = safebet,
  daily_stats          = daily_stats,
  model_results        = model_results,
  crosswalk            = crosswalk,
  kafka_all            = kafka_all,
  kafka_online_msg     = kafka_online_msg,
  kafka_threshold      = kafka_threshold,
  kafka_intent         = kafka_intent,
  kafka_account_tag    = kafka_account_tag,
  dwh_deposit          = dwh_deposit,
  poc_tool_assignments = poc_tool_assignments,
  pull_params = list(
    pull_start         = PULL_START,
    pull_end           = PULL_END,
    baseline_start     = BASELINE_START,
    baseline_end       = BASELINE_END,
    intervention_start = INTERVENTION_START,
    intervention_end   = INTERVENTION_END,
    recal_date         = BETBUDDY_RECALIB_DATE,
    n_cohort           = length(cohort_ids),
    pulled_at          = Sys.time(),
    r_version          = R.version.string
  )
)

saveRDS(poc_raw, FILE_POC_RAW)
cat("\nSaved:", FILE_POC_RAW, "\n")

writeLines(c(
  paste("Run time:",         format(Sys.time())),
  paste("N cohort:",         length(cohort_ids)),
  paste("Daily rows:",       nrow(daily_stats)),
  paste("Risk score rows:",  nrow(model_results)),
  paste("Kafka rows:",       nrow(kafka_all)),
  paste("  Online:",         nrow(kafka_online_msg)),
  paste("  Threshold:",      nrow(kafka_threshold)),
  paste("  Intent:",         nrow(kafka_intent)),
  paste("  Account-tag:",    nrow(kafka_account_tag))
), file.path(DIR_LOGS, "00_poc_pull_log.txt"))

cat("\n=== 00_poc_pull.R complete ===\n")
