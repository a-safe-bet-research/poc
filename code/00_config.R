# =============================================================================
# 00_config.R
# Purpose : Single source of truth for all paths, constants, and seeds
# Project : A Safe Bet (ASB) — Proof-of-Concept evaluation.
#           Putting Design into Practice: A Safe Bet — A Proof-of-Concept
#           Evaluation of a Tailored Online Responsible Gambling Framework
# Inputs  : none (constants only; sourced by every pipeline script)
# Outputs : none (defines paths, constants, seeds, and the brms helpers)
# =============================================================================
# Source  : ASB_DataDictionary_OSF.xlsx  (single project data dictionary)
# =============================================================================
# NOTE ON NAMES: operator-original schema identifiers (table, column, event,
# field, value names) are not written in this file. They are resolved at the
# data-contact boundary (00_poc_pull.R) from the internal rename crosswalk
# (ASB_rename_crosswalk.csv), which is internal and never deposited. Everything
# here is in the project's neutral names or in kept descriptive values.
# =============================================================================

# -----------------------------------------------------------------------------
# 0. Reproducibility seed
# -----------------------------------------------------------------------------
GLOBAL_SEED <- 20240401

# -----------------------------------------------------------------------------
# 1. Data paths (root taken from the ASB_DATA_ROOT environment variable; see .Renviron.example)
# -----------------------------------------------------------------------------
ROOT_DATA <- Sys.getenv("ASB_DATA_ROOT")
if (!nzchar(ROOT_DATA)) stop("ASB_DATA_ROOT is not set. Define it in .Renviron (see .Renviron.example).")

# Parquet table file globs are resolved from the crosswalk in 00_poc_pull.R
# (rglob()), so no source table names are written here.

# Kafka — pre-extracted per-risk-tier batch directories
# Output of the period-based join extraction (Kafka pipeline script).
KAFKA_PATHS <- list(
  r1_batches = file.path(ROOT_DATA, "r1_kafka_batches"),
  r2_batches = file.path(ROOT_DATA, "r2_kafka_batches"),
  r3_batches = file.path(ROOT_DATA, "r3_kafka_batches")
)

# Pre-computed lookup tables (saved by Kafka pipeline script)
LOOKUP_PATHS <- list(
  r1_lookup        = file.path(ROOT_DATA, "r1_lookup_periods.rds"),
  r2_lookup        = file.path(ROOT_DATA, "r2_lookup_periods.rds"),
  r3_lookup        = file.path(ROOT_DATA, "r3_lookup_periods.rds"),
  refresh_calendar = file.path(ROOT_DATA, "refresh_calendar.rds"),
  kafka_file_index = file.path(ROOT_DATA, "kafka_file_index.rds")
)

# -----------------------------------------------------------------------------
# 2. Output paths
# -----------------------------------------------------------------------------
ROOT_OUT  <- file.path(ROOT_DATA, "poc_output_lock_2026-07-17")
DIR_LOGS  <- file.path(ROOT_DATA, "logs")

FILE_POC_RAW        <- file.path(ROOT_OUT, "poc_raw.rds")
FILE_POC_ANALYTICAL <- file.path(ROOT_OUT, "poc_analytical.rds")
FILE_POC_LOCKED     <- file.path(ROOT_OUT, "poc_analytical_LOCKED.rds")
FILE_POC_MANIFEST   <- file.path(ROOT_OUT, "poc_manifest.txt")
FILE_POC_SYNTHETIC  <- file.path(ROOT_OUT, "poc_synthetic.rds")
FILE_POC_VALIDATION  <- file.path(ROOT_OUT, "poc_validation_report.txt")
FILE_POC_TOOLS      <- file.path(ROOT_OUT, "poc_tool_assignments.rds")  # clean copy; source of truth for all analyses

# -----------------------------------------------------------------------------
# 3. Column names
#    Column selection and aliasing are crosswalk-driven at the pull boundary
#    (00_poc_pull.R). No operator column-name lists are held here.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# 4. Study period constants
# -----------------------------------------------------------------------------
BASELINE_START      <- as.Date("2024-04-01")
BASELINE_END        <- as.Date("2024-09-30")
INTERVENTION_START  <- as.Date("2024-10-01")
INTERVENTION_END    <- as.Date("2025-03-31")
SELECTION_DATE      <- as.Date("2024-10-01")

# BetBuddy algorithm recalibration — UK only; creates structural break in risk series
BETBUDDY_RECALIB_DATE <- as.Date("2024-12-01")

# UK offer dates (monthly refresh events)
OFFER_DATES <- as.Date(c(
  "2024-10-01", "2024-11-01", "2024-12-01",
  "2025-01-01", "2025-02-01", "2025-03-01"
))
# April 2025 is end-of-study snapshot only — no tool assignments, no Kafka data
SNAPSHOT_DATE <- as.Date("2025-04-01")

# ITS month mapping (month 1 = April 2024, month 12 = March 2025)
MONTH_DATES  <- seq(BASELINE_START, INTERVENTION_END, by = "month")
MONTH_LABELS <- format(MONTH_DATES, "%b-%y")

# -----------------------------------------------------------------------------
# 5. Sample size constants
# -----------------------------------------------------------------------------
N_SELECTED          <- 6000L
N_EXPECTED_ANALYTIC <- 5808L   # baseline-only threshold
N_EXPECTED_ENGAGED  <- 5103L   # engaged cohort (>=1 intervention AGD)
N_EXPECTED_GHOST    <- 0L      # zero AGD in both periods; none in this cohort

AGD_THRESHOLD         <- 15L   # primary; sensitivity: 10, 20, 30
SELECTION_AGD_90d     <- 1L
SELECTION_AGD_180d    <- 20L

# Young Adult flag: under 25 at offer date (ages 18-24, completed years); regulatory YA rule
YA_AGE_MIN  <- 18L
YA_AGE_MAX  <- 24L
# 168 YA participants in analytical sample received the system-enforced regulatory YA override
# of the Deposit and Loss Limits (YA-coded tool variants) regardless of risk level

# -----------------------------------------------------------------------------
# 6. Tool codes
# -----------------------------------------------------------------------------
# Ghost tools: assigned but never built on SunVegas — strip before all analysis
GHOST_TOOL_CODES <- c("1.3", "1.6", "1.8")   # AWL, Scheduled Breaks, Deposit Count Limit

# Low-signal tools: descriptive counts only; excluded from outcome analyses
LOW_SIGNAL_TOOLS <- c("reality_check", "rg_homepage", "positive_play", "dark_flow")

# Tool string format: UK.PA.{risk_code}.M.{feature_code}
RISK_TIER_CODES <- c(low = "1", medium = "2", high = "3")

# -----------------------------------------------------------------------------
# 7. Risk / Markov states
# -----------------------------------------------------------------------------
RISK_LEVELS   <- c("low", "medium", "high")
MARKOV_STATES <- c("L", "M", "H", "I")   # I = inactive (zero AGD in month)

# -----------------------------------------------------------------------------
# 8. Kafka value constants (phases, buttons, intent types)
#    Event-type and field names are resolved from the crosswalk in 00_poc_pull.R.
#    The values below are kept descriptive states, not operator schema identifiers.
# -----------------------------------------------------------------------------

# CAUTION: the platform limit-change event is unreliable on the UK side — it is
# never used for confirmed limit changes. The limit-change-intent event is used
# instead, together with the DWH deposit / gaming limit tables.

# Online-message phases
MSG_PHASE_SENT     <- "online_send"    # dispatched by system
MSG_PHASE_RESPONSE <- "online_submit"  # participant responded; button_id only populated here

# buttons indicating RG engagement (engagement-outcome buttons)
RG_BUTTONS_ENGAGEMENT <- c(
  "Your Limits", "Yourlimits", "Setlimits", "openRgSupport",
  "ReviewLimits", "TIME TIPS", "ContinuePlay", "Logout",
  "ThanksGotIt", "IncreaseLimits", "CONTACT US", "VisitRgSupport"
)
RG_BUTTONS_DECLINE <- c("No Thanks", "NoThanks")

# Limit-change-intent activation types
INTENT_IMMEDIATE  <- "IMMEDIATELY_EFFECTIVE"   # usually protective decreases
INTENT_PENDING    <- "PENDING"                 # used in some records
INTENT_REQUIRES   <- "REQUIRES_ACTIVATION"    # increases; cooling-off applies

# Protective intent types — used for DESCRIPTIVE breakdown of FA5b modifications only
# (direction supplement: protective decreases vs escalating increases).
# NOT a filter on the engagement covariate. The engagement covariate n_limit_intents
# counts ALL limit modifications regardless of activation type. Direction-conditional
# (protective vs escalating) analysis is out of scope for this paper.
PROTECTIVE_INTENT_TYPES <- c(INTENT_IMMEDIATE, INTENT_PENDING)

# -----------------------------------------------------------------------------
# 8b. Refresh Event identification (for system-driven exclusion in n_limit_intents)
#     Limit-change-intent events within ±5s of an online-message event carrying a
#     refresh-message template are classified as system-driven and excluded from
#     the engagement covariate per prereg 6.1 FA5b inclusion rule.
#     The refresh-message template IDs are resolved from the crosswalk in
#     01_poc_build.R (kept out of the deposited config).
# -----------------------------------------------------------------------------
SYSTEM_DRIVEN_TIME_TOLERANCE_SECS <- 5L

# -----------------------------------------------------------------------------
# 8c. Limit window scope (for FA5b and engagement covariate)
#     Primary engagement scope = MONTHLY only (matches ASB intervention scope:
#     the framework actions only MONTHLY limits; prereg 6.1 FA5b). Prereg v26
#     Section 9 does not include an all-windows sensitivity re-run.
# -----------------------------------------------------------------------------
LIMIT_WINDOWS_FRAMEWORK <- "MONTHLY"

# -----------------------------------------------------------------------------
# 8d. Page dwell — account-tag heartbeat reconstruction
#     Participants generate a heartbeat ~once per minute while on a named RG page.
#     Visits are clustered with a 180s gap rule; visit duration = max-min
#     processing_time per cluster. Pooled separately for limit-management vs
#     RG-support page sets.
#     Source: RG Page Dwell Time metric specification v0.1.
# -----------------------------------------------------------------------------
PAGE_VISIT_TAG_NAME   <- "Safebet/PageVisit"
PAGE_VISIT_TAG_ACTION <- "UPDATED"
PAGE_LIMIT_MANAGEMENT <- "set-sg-limits"
PAGE_RG_SUPPORT       <- c("play-responsibly", "smart-tips")
SESSION_GAP_SECS      <- 180L

# -----------------------------------------------------------------------------
# 8e. Engagement covariates - used as time-varying covariates in prereg 6.7 Markov
#     All four are framed as engagement (reactive / active / information-seeking).
#     Direction-conditional analysis (protective vs escalating limit modifications)
#     is out of scope for this paper.
# -----------------------------------------------------------------------------
ENGAGEMENT_COVARIATES <- c(
  "msg_response_rate",
  "n_limit_intents",
  "limit_page_dwell",
  "rg_support_dwell"
)

# -----------------------------------------------------------------------------
# 9. Sensitivity analysis parameters (prereg Section 9)
# -----------------------------------------------------------------------------
SENSITIVITY_AGD_THRESHOLDS <- c(10L, 15L, 20L, 30L)
OUTLIER_PERCENTILE         <- 0.99   # top 1% of financial outcomes excluded in sensitivity run

# =============================================================================

# -----------------------------------------------------------------------------
# 10. brms helpers — conflicted compatibility
#     conflicted intercepts function lookups inside brms, causing errors.
#     Call brms_safe_start() before any brm() call and brms_safe_end() after.
# -----------------------------------------------------------------------------

brms_safe_start <- function() {
  if ("conflicted" %in% loadedNamespaces()) {
    detach("package:conflicted", unload = TRUE, force = TRUE)
    .brms_conflicted_was_loaded <<- TRUE
  } else {
    .brms_conflicted_was_loaded <<- FALSE
  }
}

brms_safe_end <- function() {
  if (isTRUE(.brms_conflicted_was_loaded)) {
    library(conflicted)
    conflicts_prefer(dplyr::filter)
    conflicts_prefer(dplyr::select)
    conflicts_prefer(dplyr::mutate)
    conflicts_prefer(dplyr::summarise)
    conflicts_prefer(dplyr::arrange)
    conflicts_prefer(dplyr::count)
    conflicts_prefer(dplyr::group_by)
  }
}

cat("[config] 00_config.R loaded. ROOT_DATA:", ROOT_DATA, "\n")
