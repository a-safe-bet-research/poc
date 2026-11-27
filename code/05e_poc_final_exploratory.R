# =============================================================================
# 05e_poc_final_exploratory.R  (Item 9, per the analysis decisions register, 21 July 2026: FINAL exploratory set)
# ASB POC — EXPLORATORY / post-hoc descriptives (NOT pre-registered)
#
# The exploratory set CLOSES with items 7 and 9. Scope note for the record:
# dose-response analyses linking tool exposure to outcome magnitude were
# considered and DECLINED (exposure confounded by risk-tailored assignment;
# the registered instrument for the covariate question was the msm model,
# reported not estimable). No further exploratory analyses.
#
# 9a — FIRST-TIME LIMIT SETTING: ON HOLD (analysis decisions register, 21 July, after the schema
#   check). The intent stream carries NO event-level first-time-set
#   category: event_type is uniformly LimitModificationIntentEvent, phase
#   and tag_action are empty, original_amount is populated on all eligible
#   events, and activation_type (IMMEDIATELY_EFFECTIVE / PENDING /
#   REQUIRES_ACTIVATION) encodes effect timing, not firstness — consistent
#   with mandatory system-assigned limits at every Refresh Event: a
#   literal first-time set cannot exist operator-side. Redefinition
#   (first user-initiated intent per participant-tool) is with the analysis team;
#   nothing is computed for 9a in this version.
#
# 9b — BASELINE PROFILE OF THE NEVER-ACTIVE (705 vs engaged 5,103).
#   Comparison from the locked player_period characteristics only:
#   baseline dominant risk level, baseline AGDs, baseline deposits
#   (median [IQR]), age. NO significance tests — descriptive contrast
#   only, per the feasibility-outcome convention. Purpose: D1 evidence
#   (who disengaged); reports beside the item 7a inactivity outputs.
#
# Data    : poc_analytical_LOCKED.rds only (READ-ONLY; 9b needs nothing
#           else in this version). Locked dataset untouched. Fast.
# Output  : results/05_descriptives/final_exploratory.rds
# Log     : Rscript 05e_poc_final_exploratory.R 2>&1 | tee logs/05e_real_YYYYMMDD.log
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
})
source("00_config.R")
library(conflicted)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::summarise)
conflicts_prefer(dplyr::arrange)
conflicts_prefer(dplyr::group_by)
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
cat("Running on LOCKED real data (read-only)\n")
cat("EXPLORATORY / post-hoc: nothing in this script is pre-registered",
    "(labelling convention as D10 / item 7)\n")

analytical_ids <- unique(pp$participant_id)
ya_ids     <- pp |> filter(young_adult_flag == 1) |> pull(participant_id) |> unique()
nonya_ids  <- setdiff(analytical_ids, ya_ids)
n_nonya    <- length(nonya_ids)
cat("Analytical:", length(analytical_ids), "| non-YA:", n_nonya, "\n")

# =============================================================================
# 9b FIRST (locked file only, cheap): never-active vs engaged baseline profile
# =============================================================================
cat("\n=== 9b. Baseline profile: never-active (zero-intervention) vs engaged ===\n")
stopifnot("engaged_cohort column missing from player_period" =
            "engaged_cohort" %in% names(pp))

bl <- pp |> filter(period == "baseline") |>
  mutate(group = ifelse(engaged_cohort == TRUE, "engaged", "never_active"))
cat("Group sizes (expect engaged 5,103 / never-active 705):\n")
print(table(bl$group))

profile_9b <- bl |>
  group_by(group) |>
  summarise(
    N          = n(),
    age_mean   = mean(age_years, na.rm = TRUE),
    age_sd     = sd(age_years,   na.rm = TRUE),
    agd_mdn    = median(agd_count, na.rm = TRUE),
    agd_q25    = quantile(agd_count, .25, na.rm = TRUE),
    agd_q75    = quantile(agd_count, .75, na.rm = TRUE),
    dep_mdn    = median(total_deposits, na.rm = TRUE),
    dep_q25    = quantile(total_deposits, .25, na.rm = TRUE),
    dep_q75    = quantile(total_deposits, .75, na.rm = TRUE),
    pct_low    = mean(dominant_risk == "low",    na.rm = TRUE) * 100,
    pct_medium = mean(dominant_risk == "medium", na.rm = TRUE) * 100,
    pct_high   = mean(dominant_risk == "high",   na.rm = TRUE) * 100,
    n_risk_missing = sum(is.na(dominant_risk)),
    .groups = "drop"
  )
print_full(profile_9b, "Baseline profile (descriptive contrast only; NO tests):")
cat("Adjudication relevance (D1): if the never-active risk distribution matches\n")
cat("the engaged cohort's -> benign-disengagement reading; if moderates are\n")
cat("over-represented among the never-active -> material to the adjudication\n")
cat("sentence and the Discussion's differential-reach limitation.\n")

# =============================================================================
# SAVE (own output only) + LOCKED-FILE INTEGRITY
# =============================================================================
out <- list(
  label            = "EXPLORATORY / post-hoc — not pre-registered (convention as D10/item 7); exploratory set closes with items 7 and 9",
  scope_note       = "dose-response analyses considered and declined: exposure confounded by risk-tailored assignment; registered covariate instrument (msm) not estimable",
  note_9a          = "9a ON HOLD (analysis decisions register, 21 July): the intent stream carries NO event-level first-time-set category (single event_type LimitModificationIntentEvent; original_amount always populated; activation_type encodes effect timing) - consistent with mandatory system-assigned limits at every Refresh Event, a literal first-time set cannot exist operator-side. Redefinition options with the analysis team.",
  profile_9b       = profile_9b,
  group_sizes_9b   = table(bl$group)
)
saveRDS(out, "results/05_descriptives/final_exploratory.rds")
stamp("Saved results/05_descriptives/final_exploratory.rds")

stopifnot("LOCKED FILE MTIME CHANGED DURING RUN — investigate immediately" =
            identical(locked_mtime_at_load, file.info(FILE_POC_LOCKED)$mtime))
cat("Locked-file integrity check passed: poc_analytical_LOCKED.rds untouched.\n")
stamp("05e (Item 9b) complete. 9a on hold per the analysis decisions register; the exploratory set closes with items 7 and 9.")
