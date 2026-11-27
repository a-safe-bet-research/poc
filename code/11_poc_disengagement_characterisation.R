# =============================================================================
# 11_poc_disengagement_characterisation.R  (Item 11, per the analysis
#                                           decisions register, 22 July 2026)
# ASB POC - EXPLORATORY / post-hoc descriptives (NOT pre-registered;
#           labelling convention as D10). Extends items 7a and 9b; reports
#           into the D1 evidence block. THE ADOPTED D1 ADJUDICATION SENTENCE
#           IS NOT ALTERED BY ANY BRANCH BELOW - the branches decide wording
#           and limitation framing only.
# Project : A Safe Bet (ASB) - Proof-of-Concept evaluation.
#
# 11a - PLATFORM CONTACT AMONG THE 705 ZERO-AGD PARTICIPANTS.
#   For each of the four registered engagement covariates (prereg 5.4):
#   count of the 705 with ANY non-zero intervention-month value; count with
#   all four flat zero across all six intervention months.
#   PRE-STATED BRANCHES (recorded verbatim, before the numbers exist):
#     - essentially all flat zero -> manuscript wording "recorded no
#       platform interaction of any kind during the intervention",
#       consistent with pre-launch attrition;
#     - a non-trivial share non-zero -> wording becomes "reached but did
#       not gamble or engage further" and the differential-reach limitation
#       is restated as differential engagement.
#   ENTRY NOTE: flat zero is consistent with never logging in but does not
#   prove it (login events are not among the four covariates).
#
# 11b - BASELINE PROFILE OF THE DISENGAGEMENT GROUPS.
#   The 9b comparison extended to three groups from the 7a accounting:
#   not-returned-by-study-end (expected 1,042), returners (expected 711),
#   never-broke (expected 3,030), on baseline dominant risk, baseline AGDs,
#   baseline deposits median [IQR], age. DESCRIPTIVE ONLY - no tests.
#   The 320 participants whose break IS March 2025 belong to no group
#   (no observable return window; 7a rule) and are accounted for explicitly.
#   PRE-STATED BRANCHES (verbatim):
#     - not-returned resemble the never-active (heavier, more moderate) ->
#       the differential-loss concern extends across the disengagement
#       component;
#     - not-returned resemble the never-broke -> mid-study leaving reads as
#       general churn and the concern stays confined to the 705.
#
# 11c - MONTHLY INFLOW SERIES (A -> I per month-pair).
#   Month-specific probability of an active month being followed by an
#   inactive month, both periods where computable - the counterpart of the
#   delivered I -> I series. Observed sequences only.
#   PRE-STATED READING (verbatim):
#     - flat doubled inflow -> entry began immediately at a constant
#       elevated rate;
#     - rising series -> part of the second-half story relocates to inflow.
#
# 11d - PERSISTENCE BY SPELL AGE: NOT BUILT IN THIS SCRIPT. Gated: build
#   only after the 11c result is delivered, and only if 11c shows a stable
#   inflow. Purpose (for the record): separate the pool-composition account
#   of the Dec -> Jan persistence step from a boundary shift - flat within
#   age strata across the boundary -> composition explains the step; a jump
#   within strata -> something happened at December. Observed-sequence
#   counting only; no model.
#
# STRUCTURAL RULE (as item 7): NO chained-matrix quantities anywhere; every
#   statistic computed directly from observed sequences (homogeneity
#   rejected, chi-square(48) = 760.07).
# WORDING RULE: censored spells are "had not returned by study end", never
#   "never returned".
#
# Data    : poc_analytical_LOCKED.rds only (READ-ONLY); player_month and
#           player_period; locked dataset untouched; own outputs only;
#           mtime assertion at end.
# Output  : results/11_disengagement/disengagement_characterisation.rds
# Log     : Rscript 11_poc_disengagement_characterisation.R 2>&1 | tee logs/11_real_YYYYMMDD.log
# Mismatches and open questions route to HRG; nothing is resolved silently.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})
source("00_config.R")
library(conflicted)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::summarise)
conflicts_prefer(dplyr::arrange)
conflicts_prefer(dplyr::group_by)
conflicts_prefer(dplyr::lag)
conflicts_prefer(dplyr::pull)

stamp <- function(msg) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), msg))
print_full <- function(x, label = NULL) {
  if (!is.null(label)) cat(label, "\n")
  print(as.data.frame(x), row.names = FALSE)
}

dir.create("results/11_disengagement", recursive = TRUE, showWarnings = FALSE)

BRANCHES_11A <- paste(
  "11a pre-stated branches (verbatim): essentially all flat zero ->",
  "manuscript wording 'recorded no platform interaction of any kind during",
  "the intervention', consistent with pre-launch attrition; a non-trivial",
  "share non-zero -> wording becomes 'reached but did not gamble or engage",
  "further' and the differential-reach limitation is restated as",
  "differential engagement. Entry note: flat zero is consistent with never",
  "logging in but does not prove it.")
BRANCHES_11B <- paste(
  "11b pre-stated branches (verbatim): not-returned resemble the",
  "never-active (heavier, more moderate) -> the differential-loss concern",
  "extends across the disengagement component; not-returned resemble the",
  "never-broke -> mid-study leaving reads as general churn and the concern",
  "stays confined to the 705.")
READING_11C <- paste(
  "11c pre-stated reading (verbatim): flat doubled inflow -> entry began",
  "immediately at a constant elevated rate; rising series -> part of the",
  "second-half story relocates to inflow.")
GATE_11D <- paste(
  "11d gate: NOT built in this script. Build only after the 11c result is",
  "delivered, and only if 11c shows a stable inflow. Observed-sequence",
  "counting only; no model.")

# -----------------------------------------------------------------------------
# 0. LOCKED FILE (READ-ONLY)
# -----------------------------------------------------------------------------
locked_mtime_at_load <- file.info(FILE_POC_LOCKED)$mtime
dat <- readRDS(FILE_POC_LOCKED)
pm  <- dat$player_month
pp  <- dat$player_period
cat("Running on LOCKED real data (read-only)\n")
cat("EXPLORATORY / post-hoc: nothing in this script is pre-registered",
    "(labelling convention as D10)\n")
cat("Item 11 reports into the D1 evidence block; the adopted D1 sentence is",
    "not altered by any branch.\n")
cat(BRANCHES_11A, "\n")
cat(BRANCHES_11B, "\n")
cat(READING_11C, "\n")
cat(GATE_11D, "\n")

analytical_ids <- unique(pp$participant_id)
cat("Analytical sample:", length(analytical_ids), "\n")

# =============================================================================
# 11a. PLATFORM CONTACT AMONG THE 705 ZERO-AGD PARTICIPANTS
# =============================================================================
cat("\n=== 11a. Platform contact among the zero-AGD participants ===\n")

stopifnot("engaged_cohort column missing from player_period" =
            "engaged_cohort" %in% names(pp))
never_active_ids <- pp |>
  filter(period == "baseline", engaged_cohort == FALSE) |>
  pull(participant_id) |>
  unique()
cat("Never-active (zero intervention AGD) participants:",
    length(never_active_ids), "(expected 705)\n")
stopifnot("Never-active group size does not match the verified 705 - route to HRG" =
            length(never_active_ids) == 705L)

pm_na <- pm |>
  filter(is_intervention == 1, participant_id %in% never_active_ids)
cat("Intervention participant-months carried for the group:", nrow(pm_na),
    "(expect 705 x 6 = 4,230 on the complete grid; a shortfall is grid",
    "absence and is treated as zero contact)\n")

# Covariate values are as locked (the n_limit_intents basis is the locked
# build's; its known D17 out-of-scope tool inclusion only widens the contact
# definition, which is conservative for the flat-zero branch)
contact_by_cov <- lapply(ENGAGEMENT_COVARIATES, function(v) {
  any_nz <- pm_na |>
    group_by(participant_id) |>
    summarise(nz = any(.data[[v]] > 0, na.rm = TRUE), .groups = "drop")
  data.frame(covariate = v, n_any_nonzero = sum(any_nz$nz))
}) |> bind_rows()
print_full(contact_by_cov,
  "11a: of the 705, participants with ANY non-zero intervention-month value, per covariate\n(Denominator basis: the 705 zero-AGD analytical participants)")

flat_zero <- pm_na |>
  group_by(participant_id) |>
  summarise(all_flat_zero = all(msg_response_rate == 0 & n_limit_intents == 0 &
                                  limit_page_dwell == 0 & rg_support_dwell == 0),
            .groups = "drop")
# Participants entirely absent from the intervention grid have no non-zero
# value by construction; count them as flat zero explicitly
n_absent_grid <- length(setdiff(never_active_ids, flat_zero$participant_id))
n_flat_zero   <- sum(flat_zero$all_flat_zero) + n_absent_grid
n_na_group    <- length(never_active_ids)
cat(sprintf("\n11a HEADLINE: %d of %d flat zero on ALL FOUR covariates across all six months (%d of these absent from the intervention grid entirely); %d with any recorded contact\n",
            n_flat_zero, n_na_group, n_absent_grid, n_na_group - n_flat_zero))
cat("Branch resolution is a wording call for the entry, made against the\n")
cat("pre-stated branches printed above; recorded here, decided in the master.\n")

# =============================================================================
# 11b. BASELINE PROFILE OF THE DISENGAGEMENT GROUPS (7a accounting)
# =============================================================================
cat("\n=== 11b. Baseline profile of the disengagement groups ===\n")

# --- 7a break accounting, reproduced identically to the verified 05d run ---
pm_a <- pm |>
  filter(participant_id %in% analytical_ids) |>
  select(participant_id, year_month, is_intervention, agd_count)
iv_months <- pm_a |> filter(is_intervention == 1) |> pull(year_month) |> unique() |> sort()
stopifnot("expected 6 intervention months" = length(iv_months) == 6)

ivg <- expand_grid(participant_id = analytical_ids, year_month = iv_months) |>
  left_join(pm_a |> filter(year_month %in% iv_months) |>
              select(participant_id, year_month, agd_count),
            by = c("participant_id", "year_month")) |>
  mutate(active = as.integer(coalesce(agd_count, 0L) > 0),
         m_idx  = match(year_month, iv_months)) |>
  arrange(participant_id, year_month)

per_p <- ivg |>
  group_by(participant_id) |>
  summarise(
    n_active     = sum(active),
    first_active = if (any(active == 1)) min(m_idx[active == 1]) else NA_integer_,
    .groups = "drop"
  )
breaks <- ivg |>
  inner_join(per_p |> filter(!is.na(first_active)), by = "participant_id") |>
  filter(m_idx > first_active, active == 0) |>
  group_by(participant_id) |>
  summarise(break_idx = min(m_idx), .groups = "drop")
returns <- ivg |>
  inner_join(breaks, by = "participant_id") |>
  filter(m_idx > break_idx, active == 1) |>
  group_by(participant_id) |>
  summarise(first_return_idx = min(m_idx), .groups = "drop")

cohort <- per_p |>
  left_join(breaks,  by = "participant_id") |>
  left_join(returns, by = "participant_id") |>
  mutate(group = case_when(
    n_active > 0 & is.na(break_idx)                          ~ "never_broke",
    !is.na(break_idx) & break_idx < 6 & !is.na(first_return_idx) ~ "returner",
    !is.na(break_idx) & break_idx < 6 &  is.na(first_return_idx) ~ "not_returned_by_study_end",
    !is.na(break_idx) & break_idx == 6                       ~ "march_break_no_group",
    TRUE                                                     ~ "zero_active"
  ))

acct <- cohort |> count(group)
print_full(acct, "11b group accounting (from the 7a rules):")
n_of <- function(g) sum(cohort$group == g)
stopifnot(
  "11b never-broke size does not match the verified 7a accounting (3,030) - route to HRG" =
    n_of("never_broke") == 3030L,
  "11b returner size does not match the verified 7a accounting (711) - route to HRG" =
    n_of("returner") == 711L,
  "11b not-returned-by-study-end size does not match the verified 7a accounting (1,042) - route to HRG" =
    n_of("not_returned_by_study_end") == 1042L,
  "11b March-break exclusion does not match the verified 7a accounting (320) - route to HRG" =
    n_of("march_break_no_group") == 320L,
  "11b zero-active remainder does not match the verified 705 - route to HRG" =
    n_of("zero_active") == 705L
)
cat("Accounting reconciles EXACTLY to the verified 7a run (3,030 / 2,073 ->\n")
cat("1,753 + 320 / 711 / 1,042) and the 705 never-active.\n")

# --- Baseline profile, identical measure set to the verified 9b profile ---
profile_groups <- pp |>
  filter(period == "baseline") |>
  inner_join(cohort |> select(participant_id, group), by = "participant_id") |>
  filter(group %in% c("not_returned_by_study_end", "returner",
                      "never_broke", "zero_active")) |>
  mutate(group = if_else(group == "zero_active",
                         "never_active_705 (9b context row)", group)) |>
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
print_full(profile_groups,
  "11b: baseline profile by disengagement group (descriptive contrast only; NO tests)\n(Denominator basis: baseline player_period rows of each group; the never-active row is the 9b context row and must match the verified 9b values)")
cat("The 320 March-break participants belong to no group (no observable\n")
cat("return window) and are excluded from the profile, as accounted above.\n")
cat("Branch resolution against the pre-stated 11b branches is made in the\n")
cat("master entry, not in this script.\n")

# =============================================================================
# 11c. MONTHLY INFLOW SERIES - A -> I per month-pair, both periods
# =============================================================================
cat("\n=== 11c. Monthly inflow series (A -> I per month-pair) ===\n")

all_months <- pm_a |> pull(year_month) |> unique() |> sort()
cat("Months found in player_month:", format(all_months), "\n")
stopifnot("expected 12 study months" = length(all_months) == 12)

grid12 <- expand_grid(participant_id = analytical_ids, year_month = all_months) |>
  left_join(pm_a |> select(participant_id, year_month, agd_count),
            by = c("participant_id", "year_month")) |>
  mutate(active = as.integer(coalesce(agd_count, 0L) > 0)) |>
  arrange(participant_id, year_month)
n_pm_rows <- nrow(pm_a |> filter(year_month %in% all_months))
cat("player_month rows:", n_pm_rows, "| grid rows:", nrow(grid12),
    "| rows filled as inactive (absent from player_month):",
    nrow(grid12) - n_pm_rows, "\n")

wide12 <- grid12 |>
  select(participant_id, year_month, active) |>
  pivot_wider(names_from = year_month, values_from = active)
mcols12 <- as.character(all_months)

iv_start_idx <- match(min(iv_months), all_months)   # first intervention month
ai_series <- lapply(seq_len(length(mcols12) - 1), function(k) {
  from <- wide12[[mcols12[k]]]; to <- wide12[[mcols12[k + 1]]]
  n_A  <- sum(from == 1)
  data.frame(
    pair  = sprintf("%s -> %s", format(all_months[k], "%b %Y"),
                    format(all_months[k + 1], "%b %Y")),
    scope = if (k + 1 < iv_start_idx) "baseline" else
            if (k + 1 == iv_start_idx) "ENTRY PAIR (last baseline -> first intervention; separately labelled)" else
            "within intervention window",
    n_active_from = n_A,
    p_A_to_I = round(sum(from == 1 & to == 0) / n_A, 3)
  )
}) |> bind_rows()
print_full(ai_series,
  "11c: month-specific A -> I probabilities (all analytical participants; active = agd_count > 0;\nobserved sequences only; the counterpart of the delivered I -> I series)\n(Denominator basis: participants active in the FROM month of each pair)")
cat("Reading is made against the pre-stated 11c reading printed above;\n")
cat("recorded here, decided in the master entry.\n")
cat("\n", GATE_11D, "\n")

# =============================================================================
# SAVE (own output only) + LOCKED-FILE INTEGRITY
# =============================================================================
out <- list(
  label = "EXPLORATORY / post-hoc - not pre-registered (convention as D10); Item 11 disengagement characterisation; extends items 7a and 9b; reports into the D1 evidence block; the adopted D1 sentence is not altered by any branch",
  structural_rule = "no chained-matrix quantities; observed sequences only (homogeneity rejected, chi-square(48) = 760.07)",
  wording_rule    = "censored spells are 'had not returned by study end', never 'never returned'",
  branches_11a    = BRANCHES_11A,
  branches_11b    = BRANCHES_11B,
  reading_11c     = READING_11C,
  gate_11d        = GATE_11D,
  note_11a        = "flat zero is consistent with never logging in but does not prove it; covariate values as locked (the locked n_limit_intents basis includes the D17 out-of-scope tool type, which only widens the contact definition and is conservative for the flat-zero branch)",
  contact_by_cov_11a  = contact_by_cov,
  n_flat_zero_11a     = n_flat_zero,
  n_absent_grid_11a   = n_absent_grid,
  n_any_contact_11a   = n_na_group - n_flat_zero,
  accounting_11b      = acct,
  profile_groups_11b  = profile_groups,
  ai_series_11c       = ai_series
)
saveRDS(out, "results/11_disengagement/disengagement_characterisation.rds")
stamp("Saved results/11_disengagement/disengagement_characterisation.rds")

stopifnot("LOCKED FILE MTIME CHANGED DURING RUN - investigate immediately" =
            identical(locked_mtime_at_load, file.info(FILE_POC_LOCKED)$mtime))
cat("Locked-file integrity check passed: poc_analytical_LOCKED.rds untouched.\n")
stamp("11_poc_disengagement_characterisation.R complete - Item 11 (11a-11c) delivered; 11d remains gated on the 11c result.")
