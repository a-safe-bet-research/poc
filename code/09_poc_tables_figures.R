# Run from the project root; 00_config.R resolves all paths.
# =============================================================================
# 09_poc_tables_figures.R  (PATCHED, post-registration corrections, 21 July 2026)
#
# PATCH SUMMARY (all approved via the analysis decisions register, 21 July):
#   P1 — T2: test-statistic column mapped explicitly to its outcome rows
#        (previously misaligned: H1-H4 stats were assigned by position and
#        landed on the wrong rows; the AGD row has no registered test and
#        now shows a dash; H3/wagers has no T2 row by design).
#   P2 — pp_wide: pivot_wider now uses explicit id_cols. The unpatched
#        pivot split each participant across two rows (period-varying
#        columns in player_period), which emptied T6a (joint
#        baseline x intervention filter matched nothing) and left F5
#        rendering from an empty frame. Fixes T6a AND F5.
#   P3 — T9/T10/F9: exploratory recalibration set re-anchored on DECEMBER
#        2024 (the recalibration month, where the observed escalation
#        spike is: 32.4%), not January 2025. T10's excess decomposition
#        recomputed against the December anchor.
#   P4 — NEW T6b: point-in-time risk migration by baseline state (last
#        classified active month per period), approved in the decisions
#        register; T6c and TS1-TS3 are DROPPED from the plan.
#   P5 — NEW F10: regulatory-context timeline (prereg Section 8 descriptive
#        mapping) — monthly median deposits with the operator-verified
#        1 Sept 2024 implementation date and public UKGC effective dates
#        (30 Aug 2024 FVC in force at GBP 500; 28 Feb 2025 threshold
#        lowered to GBP 150) annotated. ASB launch 1 Oct 2024 marked.
#   P6 — USE_SYNTHETIC set FALSE and OVERWRITE set TRUE for the correction
#        re-run (stale synthetic/defective outputs must not be kept).
#   R  — RESTYLE (30 July 2026): figure section visually restyled via
#        asb_style.R (shared theme, semantic palettes, dual manuscript +
#        slide outputs). Display labels aligned to locked terminology.
#        Data construction for every figure unchanged; change log R1-R8
#        at the FIGURES section header below. R7 applies the Moderate
#        display label to tables T1/T6a/T6b/T8/T10 (values/pipelines
#        unchanged); R8 adds outcome-hue shading and F2b (presentation
#        companion, NOT in the registered figure list); R9 slims headings
#        and recolours F8/F9/F10 annotations.
# =============================================================================
# Purpose : All publication tables and figures
# Project : A Safe Bet (ASB) — Proof-of-Concept evaluation.
#           Putting Design into Practice: A Safe Bet — A Proof-of-Concept
#           Evaluation of a Tailored Online Responsible Gambling Framework
# Inputs  : poc_synthetic.rds or poc_analytical_LOCKED.rds (via 00_config.R);
#           saved results from 06/07/08 where available
# Outputs : results/tables/   — .csv and .rds for each table
#           results/figures/  — .pdf and .png for each figure
# Flags   : USE_SYNTHETIC — TRUE runs on poc_synthetic.rds (pre-registration
#           phase); FALSE runs on poc_analytical_LOCKED.rds
#           (post-registration). Currently FALSE.
#           OVERWRITE — FALSE skips re-rendering figures whose outputs exist
#           (crash-resumable); TRUE re-renders everything. Currently TRUE.
#           (Full flag documentation at the flag definitions below.)
# =============================================================================
# PRE-REG:  OSF [URL to be added] - publication tables and figures for all
#           sections (script table, prereg Section 10.1)
# =============================================================================
# PRODUCES:
#
# TABLES
#   T1  — Baseline demographic and gambling characteristics
#   T2  — Primary outcomes: baseline vs intervention (full + engaged cohorts)
#   T3  — Gambling intensity per AGD (engaged cohort only)
#   T4  — Monthly metrics across intervention period
#   T5  — Player engagement categories
#   T6a — Risk migration matrix: 6-month modal classification
#   T7  — Player risk trajectory patterns
#   T8  — ASB monthly risk distribution
#   T9  — December 2024 escalation spike analysis (recalibration sensitivity)
#         [EXPLORATORY / post-hoc; not pre-registered]
#   T10 — Algorithm impact decomposition
#         [EXPLORATORY / post-hoc; not pre-registered]
#   T6b — Point-in-time risk migration by baseline state (last classified
#         active month per period) [built by patch P4]
#   DROPPED FROM THE PLAN (decisions register, 21 July): T6c (risk method
#   comparison), TS1-TS3 (Bayesian, ITS, and Markov supplementary tables)
#
# FIGURES
#   F1  — CONSORT-style sample flow diagram
#   F2  — Primary outcomes: paired pre-post distributions (violin + box)
#   F3  — Monthly trends: all four outcomes across 12 months (ITS)
#   F4  — Risk tier distribution: baseline vs intervention (stacked bar)
#   F5  — Risk transition heatmap (baseline -> intervention)
#   F6  — Markov transition matrix heatmap
#   F7  — Engagement over time: response rate and intent count by month
#   F8  — ITS posterior distributions (beta_2 / beta_3 densities per outcome;
#         requires results/07_its/its_results.rds from 07_poc_its.R)
#   F9  — BetBuddy recalibration: monthly escalation rates, December 2024
#         spike (re-anchored by patch P3)
#   F10 — Regulatory-context timeline: monthly median deposits with
#         regulatory effective dates annotated (prereg Section 8 mapping)
# =============================================================================

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(scales)
library(patchwork)

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

# Post-registration correction run: locked real data.
USE_SYNTHETIC <- FALSE

# OVERWRITE = TRUE for this patched correction run (decisions register,
# 21 July): every table and figure must be rebuilt — several prior outputs
# were defective (T2, T6a, F5) or mis-anchored (T9/T10/F9), so nothing
# stale may be kept. (FALSE restores crash-resumable skipping of figures
# whose outputs exist; do not use FALSE until after this run completes.)
OVERWRITE <- TRUE

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

# Load pre-computed results if available
prepost_res <- tryCatch(readRDS("results/06_prepost/prepost_results.rds"), error = function(e) NULL)
its_res     <- tryCatch(readRDS("results/07_its/its_results.rds"),         error = function(e) NULL)
markov_res  <- tryCatch(readRDS("results/08_markov/markov_results.rds"),   error = function(e) NULL)

dir.create("results/tables",  recursive = TRUE, showWarnings = FALSE)
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# STYLE — theme, palettes and figure output are sourced from asb_style.R
# (restyle note R; the registered theme_pub/save_figure block lived here)
# =============================================================================

source("asb_style.R")

save_table <- function(df, name) {
  write.csv(df, file.path("results/tables", paste0(name, ".csv")), row.names = FALSE)
  saveRDS(df, file.path("results/tables", paste0(name, ".rds")))
  cat("  Saved:", name, "\n")
}

# =============================================================================
# HELPERS
# =============================================================================

fmt_n_pct <- function(n, total) sprintf("%d (%.1f%%)", n, n/total*100)
fmt_mdn_iqr <- function(x) {
  sprintf("%.2f [%.2f\u2013%.2f]",
          median(x, na.rm=TRUE),
          quantile(x, 0.25, na.rm=TRUE),
          quantile(x, 0.75, na.rm=TRUE))
}
fmt_mean_sd <- function(x) sprintf("%.1f (%.1f)", mean(x, na.rm=TRUE), sd(x, na.rm=TRUE))

# PATCH P2: explicit id_cols — player_period carries period-varying columns
# beyond the value set, so the unpatched pivot produced TWO rows per
# participant (one per period, complementary NAs). Median-based tables
# tolerated that (na.rm), but T6a/F5's joint filter on
# dominant_risk_baseline AND dominant_risk_intervention matched no rows.
pp_wide <- pp |>
  pivot_wider(id_cols = c(participant_id, engaged_cohort, young_adult_flag),
              names_from = period,
              values_from = c(total_deposits, total_losses, total_wagers,
                              play_hours_total, night_hours_total, agd_count,
                              dominant_risk))
stopifnot("pp_wide must be one row per participant" =
            nrow(pp_wide) == dplyr::n_distinct(pp$participant_id))

N_full     <- n_distinct(pp$participant_id)
N_engaged  <- sum(pp$engaged_cohort[pp$period == "baseline"], na.rm = TRUE)

# =============================================================================
# TABLE 1 — Baseline demographic and gambling characteristics
# =============================================================================

cat("\nBuilding T1...\n")

pp_bl <- pp |> filter(period == "baseline")
pm_bl <- pm |> filter(is_intervention == 0)

T1 <- data.frame(
  Variable = c(
    "N",
    "Age, M (SD)",
    "Young adult (18–24), n (%)",
    "Active Gambling Days, M (SD)",
    "Active Gambling Days, Mdn [IQR]",
    "Total deposits (GBP), M (SD)",
    "Total deposits (GBP), Mdn [IQR]",
    "Total net_loss (GBP), M (SD)",
    "Total net_loss (GBP), Mdn [IQR]",
    "Total play hours, M (SD)",
    "Total play hours, Mdn [IQR]",
    "Risk: low, n (%)",
    "Risk: moderate, n (%)",
    "Risk: high, n (%)"
  ),
  Full_cohort = c(
    as.character(N_full),
    fmt_mean_sd(pp_bl$age_years),
    fmt_n_pct(sum(pp_bl$young_adult_flag, na.rm=TRUE), N_full),
    fmt_mean_sd(pp_bl$agd_count),
    fmt_mdn_iqr(pp_bl$agd_count),
    fmt_mean_sd(pp_bl$total_deposits),
    fmt_mdn_iqr(pp_bl$total_deposits),
    fmt_mean_sd(pp_bl$total_losses),
    fmt_mdn_iqr(pp_bl$total_losses),
    fmt_mean_sd(pp_bl$play_hours_total),
    fmt_mdn_iqr(pp_bl$play_hours_total),
    fmt_n_pct(sum(pp_bl$dominant_risk == "low",    na.rm=TRUE), N_full),
    fmt_n_pct(sum(pp_bl$dominant_risk == "medium", na.rm=TRUE), N_full),
    fmt_n_pct(sum(pp_bl$dominant_risk == "high",   na.rm=TRUE), N_full)
  )
)

save_table(T1, "T1_baseline_characteristics")
print(T1)

# =============================================================================
# TABLE 2 — Primary outcomes: pre-post comparison
# =============================================================================

cat("\nBuilding T2...\n")

make_prepost_row <- function(pp_data, var_bl, var_int, label, cohort_n) {
  bl  <- pp_data[[var_bl]]
  int <- pp_data[[var_int]]
  
  pct_change <- (median(int, na.rm=TRUE) - median(bl, na.rm=TRUE)) /
    median(bl, na.rm=TRUE) * 100
  
  data.frame(
    Metric       = label,
    Baseline     = fmt_mdn_iqr(bl),
    Intervention = fmt_mdn_iqr(int),
    Pct_change   = sprintf("%.1f%%", pct_change),
    N            = cohort_n,
    stringsAsFactors = FALSE
  )
}

outcomes_t2 <- list(
  list(bl = "agd_count_baseline",       int = "agd_count_intervention",       label = "AGD, Mdn [IQR]"),
  list(bl = "total_deposits_baseline",  int = "total_deposits_intervention",  label = "Deposits (GBP), Mdn [IQR]"),
  list(bl = "total_losses_baseline",    int = "total_losses_intervention",    label = "Net loss (GBP), Mdn [IQR]"),
  list(bl = "play_hours_total_baseline",int = "play_hours_total_intervention",label = "Play hours, Mdn [IQR]")
)

T2_full <- bind_rows(lapply(outcomes_t2, function(o) {
  make_prepost_row(pp_wide, o$bl, o$int, o$label, N_full)
}))
T2_full$Cohort <- "Full cohort"

T2_engaged <- bind_rows(lapply(outcomes_t2, function(o) {
  make_prepost_row(
    pp_wide |> filter(engaged_cohort == TRUE),
    o$bl, o$int, o$label, N_engaged
  )
}))
T2_engaged$Cohort <- "Engaged cohort"

# Add frequentist test statistics if available
# PATCH P1: explicit outcome->hypothesis mapping. The unpatched code
# assigned H1-H4 by row position, so H1 (deposits) landed on the AGD row,
# H2 on deposits, etc. Mapping is now by outcome label: deposits = H1,
# net loss = H2, play hours = H4. AGD has no registered pre-post test
# (it is FA6 material) and shows a dash; H3 (wagers) has no T2 row.
if (!is.null(prepost_res)) {
  t2_hyp_map <- c("AGD, Mdn [IQR]"              = NA_character_,
                  "Deposits (GBP), Mdn [IQR]"   = "H1",
                  "Net loss (GBP), Mdn [IQR]"   = "H2",
                  "Play hours, Mdn [IQR]"       = "H4")
  stopifnot("T2 rows and hypothesis map out of sync" =
              setequal(T2_full$Metric, names(t2_hyp_map)))
  T2_full$V_stat <- vapply(T2_full$Metric, function(m) {
    h <- t2_hyp_map[[m]]
    if (is.na(h)) return("\u2014")
    r <- prepost_res$freq_full[[h]]
    if (!is.null(r)) sprintf("V = %.0f, p %s, r = %.3f",
                             r$stat_V,
                             ifelse(r$p_value < .001, "< .001", sprintf("= %.3f", r$p_value)),
                             r$r_rb) else NA_character_
  }, character(1))
}

T2 <- bind_rows(T2_full, T2_engaged)
save_table(T2, "T2_primary_outcomes")
print(T2)

# =============================================================================
# TABLE 3 — Gambling intensity per AGD
# =============================================================================

cat("\nBuilding T3...\n")

pp_intensity <- pp_wide |>
  filter(engaged_cohort == TRUE) |>
  mutate(
    dep_per_agd_bl  = total_deposits_baseline  / pmax(agd_count_baseline, 1),
    dep_per_agd_int = total_deposits_intervention / pmax(agd_count_intervention, 1),
    loss_per_agd_bl  = total_losses_baseline   / pmax(agd_count_baseline, 1),
    loss_per_agd_int = total_losses_intervention / pmax(agd_count_intervention, 1),
    hrs_per_agd_bl   = play_hours_total_baseline / pmax(agd_count_baseline, 1),
    hrs_per_agd_int  = play_hours_total_intervention / pmax(agd_count_intervention, 1)
  )

T3 <- data.frame(
  Metric_per_AGD = c("Deposits (GBP)", "Net loss (GBP)", "Play hours"),
  Baseline_Mdn   = c(
    sprintf("%.2f", median(pp_intensity$dep_per_agd_bl,  na.rm=TRUE)),
    sprintf("%.2f", median(pp_intensity$loss_per_agd_bl, na.rm=TRUE)),
    sprintf("%.2f", median(pp_intensity$hrs_per_agd_bl,  na.rm=TRUE))
  ),
  Intervention_Mdn = c(
    sprintf("%.2f", median(pp_intensity$dep_per_agd_int,  na.rm=TRUE)),
    sprintf("%.2f", median(pp_intensity$loss_per_agd_int, na.rm=TRUE)),
    sprintf("%.2f", median(pp_intensity$hrs_per_agd_int,  na.rm=TRUE))
  )
)

save_table(T3, "T3_intensity_per_agd")
print(T3)

# =============================================================================
# TABLE 4 — Monthly metrics across intervention period
# =============================================================================

cat("\nBuilding T4...\n")

T4 <- pm |>
  filter(is_intervention == 1) |>
  group_by(year_month) |>
  summarise(
    N_active          = sum(agd_count > 0),
    participation_pct = mean(agd_count > 0) * 100,
    mean_agd          = mean(agd_count[agd_count > 0], na.rm = TRUE),
    mdn_dep_per_agd   = median(
      total_deposits[agd_count > 0] / pmax(agd_count[agd_count > 0], 1),
      na.rm = TRUE),
    mdn_loss_per_agd  = median(
      total_losses[agd_count > 0] / pmax(agd_count[agd_count > 0], 1),
      na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    Month = format(year_month, "%b %Y"),
    across(where(is.numeric), ~ round(.x, 1))
  ) |>
  select(Month, N_active, participation_pct, mean_agd, mdn_dep_per_agd, mdn_loss_per_agd)

names(T4) <- c("Month", "Active players", "Participation (%)",
               "Mean AGD", "Mdn deposit_amount/AGD (GBP)", "Mdn loss/AGD (GBP)")

save_table(T4, "T4_monthly_metrics")
print(T4)

# =============================================================================
# TABLE 5 — Engagement categories
# =============================================================================

cat("\nBuilding T5...\n")

active_months_by_player <- pm |>
  filter(is_intervention == 1) |>
  group_by(participant_id) |>
  summarise(n_active = sum(agd_count > 0), .groups = "drop")

T5 <- active_months_by_player |>
  mutate(category = case_when(
    n_active == 6   ~ "Continuous (6/6 months)",
    n_active == 5   ~ "Highly engaged (5/6)",
    n_active %in% 3:4 ~ "Moderately engaged (3\u20134/6)",
    n_active %in% 1:2 ~ "Low engagement (1\u20132/6)",
    n_active == 0   ~ "Never active (0/6)"
  )) |>
  count(category) |>
  mutate(
    pct = sprintf("%.1f%%", n / sum(n) * 100),
    category = factor(category, levels = c(
      "Continuous (6/6 months)", "Highly engaged (5/6)",
      "Moderately engaged (3\u20134/6)", "Low engagement (1\u20132/6)",
      "Never active (0/6)"))
  ) |>
  arrange(category) |>
  rename(`Engagement pattern` = category, `n` = n, `%` = pct)

save_table(T5, "T5_engagement_categories")
print(T5)

# =============================================================================
# TABLE 6a — Risk migration matrix: 6-month modal
# =============================================================================

cat("\nBuilding T6a...\n")

risk_levels <- c("low", "medium", "high")

T6a_raw <- pp_wide |>
  filter(!is.na(dominant_risk_baseline), !is.na(dominant_risk_intervention)) |>
  count(dominant_risk_baseline, dominant_risk_intervention) |>
  group_by(dominant_risk_baseline) |>
  mutate(
    row_total = sum(n),
    pct = sprintf("%d (%.1f%%)", n, n/row_total*100)
  ) |>
  ungroup()

# Also compute dropout (no intervention dominant risk)
dropout_counts <- pp_wide |>
  filter(!is.na(dominant_risk_baseline), is.na(dominant_risk_intervention)) |>
  count(dominant_risk_baseline, name = "n_dropout")

T6a <- T6a_raw |>
  dplyr::mutate(
    dominant_risk_baseline     = as.character(dominant_risk_baseline),
    dominant_risk_intervention = as.character(dominant_risk_intervention)
  ) |>
  select(dominant_risk_baseline, dominant_risk_intervention, pct) |>
  pivot_wider(names_from = dominant_risk_intervention, values_from = pct, values_fill = "0 (0.0%)") |>
  left_join(
    dropout_counts |> dplyr::mutate(dominant_risk_baseline = as.character(dominant_risk_baseline)),
    by = "dominant_risk_baseline"
  ) |>
  mutate(Dropped = fmt_n_pct(replace_na(n_dropout, 0),
                             sum(replace_na(n_dropout, 0)) + sum(T6a_raw$n))) |>
  select(dominant_risk_baseline, any_of(risk_levels), Dropped) |>
  dplyr::rename_with(~ dplyr::case_when(
    . == "dominant_risk_baseline" ~ "Baseline risk",
    . == "low"    ~ "→ Low",
    . == "medium" ~ "→ Moderate",
    . == "high"   ~ "→ High",
    TRUE          ~ .
  )) |>
  mutate(`Baseline risk` = unname(ASB_RISK_LABELS[`Baseline risk`]))  # R7 display labels

save_table(T6a, "T6a_risk_migration_modal")
print(T6a)

# =============================================================================
# TABLE 6b — Point-in-time risk migration by baseline state  [PATCH P4]
# Approved in the decisions register (21 July) in place of the planned
# T6c/TS1-TS3, which are dropped. Point-in-time state = the participant's
# LAST CLASSIFIED ACTIVE month (markov_state in L/M/H) within each period;
# inactive months are skipped, not imputed. Participants with no classified
# active intervention month fall in the "No classified month" column —
# they are the disengaged, not a risk state. The December 2024 BetBuddy
# recalibration caveat attaches to any month-level classification.
# =============================================================================

cat("\nBuilding T6b (point-in-time migration; patch P4)...\n")

last_state <- function(pm_data, intervention_flag) {
  pm_data |>
    filter(is_intervention == intervention_flag,
           markov_state %in% c("L","M","H")) |>
    group_by(participant_id) |>
    slice_max(year_month, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(participant_id, state = markov_state)
}

t6b_pairs <- last_state(pm, 0) |>
  rename(state_bl = state) |>
  left_join(last_state(pm, 1) |> rename(state_int = state),
            by = "participant_id") |>
  mutate(state_int = ifelse(is.na(state_int), "No classified month", state_int))

T6b <- t6b_pairs |>
  count(state_bl, state_int) |>
  group_by(state_bl) |>
  mutate(pct = sprintf("%d (%.1f%%)", n, n / sum(n) * 100)) |>
  ungroup() |>
  select(state_bl, state_int, pct) |>
  pivot_wider(names_from = state_int, values_from = pct,
              values_fill = "0 (0.0%)") |>
  mutate(state_bl = factor(state_bl, levels = c("L","M","H"),
                           labels = c("Low", "Moderate", "High"))) |>
  arrange(state_bl) |>
  select(`Baseline state (last classified month)` = state_bl,
         any_of(c("L","M","H")), any_of("No classified month")) |>
  dplyr::rename_with(~ dplyr::case_when(
    . == "L" ~ "\u2192 Low", . == "M" ~ "\u2192 Moderate", . == "H" ~ "\u2192 High",
    TRUE ~ .))

save_table(T6b, "T6b_risk_migration_point_in_time")
print(T6b)

# =============================================================================
# TABLE 7 — Risk trajectory patterns
# =============================================================================

cat("\nBuilding T7...\n")

risk_traj <- pm |>
  filter(!is.na(markov_state)) |>
  group_by(participant_id) |>
  summarise(
    n_changes = sum(diff(as.integer(factor(markov_state,
                                           levels = c("L","M","H","I")))) != 0,
                    na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(pattern = case_when(
    n_changes == 0 ~ "Stable (no changes)",
    n_changes == 1 ~ "Single change",
    n_changes == 2 ~ "Two changes",
    n_changes >= 3 ~ "Volatile (3+ changes)"
  ))

T7 <- risk_traj |>
  count(pattern) |>
  mutate(pct = sprintf("%.1f%%", n / sum(n) * 100)) |>
  rename(`Pattern` = pattern, `n` = n, `%` = pct)

save_table(T7, "T7_risk_trajectories")
print(T7)

# =============================================================================
# TABLE 8 — ASB monthly risk distribution
# =============================================================================

cat("\nBuilding T8...\n")

T8 <- pm |>
  group_by(year_month) |>
  summarise(
    pct_low    = mean(markov_state == "L", na.rm=TRUE) * 100,
    pct_medium = mean(markov_state == "M", na.rm=TRUE) * 100,
    pct_high   = mean(markov_state == "H", na.rm=TRUE) * 100,
    N          = n(),
    .groups = "drop"
  ) |>
  mutate(
    Month  = format(year_month, "%b %Y"),
    Low    = sprintf("%.1f%%", pct_low),
    Moderate = sprintf("%.1f%%", pct_medium),
    High   = sprintf("%.1f%%", pct_high)
  ) |>
  select(Month, Low, Moderate, High, N)

save_table(T8, "T8_monthly_risk_distribution")
print(T8)

# =============================================================================
# TABLE 9 & 10 — Algorithm recalibration sensitivity
# EXPLORATORY / post-hoc analysis. Not among the prereg Section 9 sensitivities
# or Section 2 hypotheses. Report as exploratory in the manuscript.
# =============================================================================

cat("\nBuilding T9/T10...\n")

# PATCH P3: single anchor for the exploratory recalibration set.
SPIKE_MONTH <- floor_date(BETBUDDY_RECALIB_DATE, "month")  # December 2024
stopifnot("Spike anchor must be December 2024" =
            SPIKE_MONTH == as.Date("2024-12-01"))

# Month-to-month escalation from low risk
escalation_monthly <- pm |>
  arrange(participant_id, year_month) |>
  group_by(participant_id) |>
  mutate(
    prev_state = lag(markov_state),
    escalated  = prev_state == "L" & markov_state %in% c("M","H")
  ) |>
  ungroup() |>
  filter(!is.na(prev_state), prev_state == "L") |>
  group_by(year_month) |>
  summarise(
    n_low_prev  = n(),
    n_escalated = sum(escalated, na.rm = TRUE),
    pct_escalated = n_escalated / n_low_prev * 100,
    .groups = "drop"
  ) |>
  filter(year_month >= BASELINE_START) |>
  mutate(
    Month   = format(year_month, "%b %Y"),
    # PATCH P3: the observed escalation spike is DECEMBER 2024 — the
    # recalibration month itself — not January 2025. Labels re-anchored.
    period_label = case_when(
      year_month <  SPIKE_MONTH ~ "Pre-recalibration",
      year_month == SPIKE_MONTH ~ "Recalibration month (observed spike)",
      year_month >  SPIKE_MONTH ~ "Post-recalibration"
    )
  )

T9 <- escalation_monthly |>
  select(Month, n_low_prev, n_escalated, pct_escalated, period_label) |>
  mutate(pct_escalated = sprintf("%.1f%%", pct_escalated)) |>
  rename(`Month` = Month, `N low-risk` = n_low_prev,
         `N escalated` = n_escalated, `% escalated` = pct_escalated,
         `Classification` = period_label)

# T10: decomposition  [PATCH P3: anchored on the December recalibration month]
expected_rate <- escalation_monthly |>
  filter(year_month != SPIKE_MONTH,
         year_month >= INTERVENTION_START) |>
  summarise(mean_rate = mean(pct_escalated / 100, na.rm = TRUE)) |>
  pull(mean_rate)

spike_row      <- escalation_monthly |> filter(year_month == SPIKE_MONTH)
spike_n_low    <- spike_row$n_low_prev
spike_n_esc    <- spike_row$n_escalated
expected_esc   <- round(spike_n_low * expected_rate)
excess_esc     <- spike_n_esc - expected_esc
total_esc_all  <- sum(escalation_monthly$n_escalated[escalation_monthly$year_month >= INTERVENTION_START], na.rm = TRUE)

T10 <- data.frame(
  Component = c("Total low\u2192moderate/high transitions",
                "Excess December escalations (attributable to recalibration)",
                "Estimated true behavioural change"),
  n         = c(total_esc_all, excess_esc, total_esc_all - excess_esc),
  pct       = c("100%",
                sprintf("%.1f%%", excess_esc / total_esc_all * 100),
                sprintf("%.1f%%", (total_esc_all - excess_esc) / total_esc_all * 100))
)

save_table(T9,  "T9_escalation_spike")
save_table(T10, "T10_algorithm_decomposition")

# =============================================================================
# FIGURES — restyled section (restyle note R in the header)
# =============================================================================
# CHANGES RELATIVE TO THE REGISTERED FIGURE CODE (all display-level):
#   R1 — Shared style module (asb_style.R): one theme, semantic colour roles,
#        consistent month axes, dual output (manuscript PDF+TIFF without
#        in-figure titles at Addiction column widths; 16:9 titled slide PNG).
#   R2 — Locked-terminology alignment in DISPLAY LABELS only: risk level
#        (not state/tier), Moderate (not Medium), Net loss (not Losses).
#        Underlying variable codes ("medium", markov_state) untouched.
#   R3 — F2 y-axis now uses a pseudo-log (asinh-family) transform on the raw
#        values instead of plotting log1p(value). This is a display fix with
#        a substantive edge: log1p produced NaN for net losses below -1, so
#        players in profit by more than GBP 1 were silently dropped from the
#        net-loss panel. pseudo_log keeps them. Axis breaks are now clean
#        powers of ten rather than back-transformed positions.
#   R4 — F10 annotation labels staggered/anchored per label (the registered
#        code drew all four at one height with hjust = 0, which overlapped);
#        operator-verified line recoloured from the High-risk red to the
#        annotation accent so risk semantics stay unique.
#   R5 — F6 legends collected into one (the registered patchwork drew the
#        identical probability legend twice).
#   R6 — F8/F9 recoloured off the baseline/intervention pair (the registered
#        code reused those colours for unrelated contrasts).
#   R7 — Moderate display label applied in tables T1, T6a, T6b, T8, T10.
#   R8 — Outcome-hue scheme: each outcome has one hue; shade encodes
#        period (light = baseline, dark = intervention) in F2/F2b/F3;
#        F10 deposits series inherits the deposits hue. F2b added as a
#        presentation companion (pre-post medians + IQR bars); it is
#        EXPLORATORY-DISPLAY ONLY and not in the registered figure list.
#   R9 — Heading weight reduced (plain facet strips, smaller titles).
#        F8 recoloured to the outcome hues with shade = parameter (dark =
#        level change beta_2, light = slope change beta_3). F9 made
#        monochrome (near-black spike bar among grey bars). F10 annotation
#        lines differentiated by BOTH colour and line pattern (solid orange
#        = ASB launch, dashed slate #4F6D8C = operator-verified, dotted
#        grey = public dates).
# DEPENDS ON (from 09 tables section + 00_config.R, unchanged): pm, pp,
#   pp_wide, dat, N_full, markov_res, its_res, escalation_monthly,
#   SPIKE_MONTH, OVERWRITE, INTERVENTION_START, BETBUDDY_RECALIB_DATE,
#   N_SELECTED, N_EXPECTED_ANALYTIC, N_EXPECTED_ENGAGED.
# =============================================================================

# Convenience: text sizes that scale on slides
asb_txt <- function(pt, slides) (pt / .pt) * (if (slides) 1.7 else 1)

# =============================================================================
# FIGURE 1 — CONSORT-style sample flow
# =============================================================================

cat("\nBuilding F1 (CONSORT flow)...\n")

sf        <- dat$sample_flow
n_sel     <- if (!is.null(sf$n_selected))        sf$n_selected        else N_SELECTED
n_analyt  <- if (!is.null(sf$n_analytical))      sf$n_analytical      else N_EXPECTED_ANALYTIC
n_excl    <- if (!is.null(sf$n_below_threshold)) sf$n_below_threshold else (n_sel - n_analyt)
n_engaged <- if (!is.null(sf$n_engaged))         sf$n_engaged         else N_EXPECTED_ENGAGED
n_no_int  <- n_analyt - n_engaged

flow_data <- data.frame(
  label = c(
    sprintf("Selected at baseline\nn = %s", format(n_sel, big.mark = ",")),
    sprintf("Excluded: <15 baseline AGDs\nn = %s", format(n_excl, big.mark = ",")),
    sprintf("Analytical sample (full cohort)\nN = %s", format(n_analyt, big.mark = ",")),
    sprintf("No intervention-period AGDs\nn = %s (coded as zeros)", format(n_no_int, big.mark = ",")),
    sprintf("Engaged cohort\nn = %s", format(n_engaged, big.mark = ","))
  ),
  x    = c(0.34, 0.76, 0.34, 0.76, 0.34),
  y    = c(0.88, 0.74, 0.60, 0.44, 0.26),
  type = c("main", "excl", "main", "excl", "main")
)

make_F1 <- function(slides = FALSE) {
  bx <- 0.21; bh <- 0.07
  ggplot() +
    geom_rect(data = flow_data,
              aes(xmin = x - bx, xmax = x + bx, ymin = y - bh, ymax = y + bh),
              fill = "white", colour = "grey30", linewidth = 0.4) +
    geom_text(data = flow_data, aes(x = x, y = y, label = label),
              size = asb_txt(8, slides), lineheight = 1.15,
              family = "sans", colour = "grey5") +
    # Main flow arrows
    annotate("segment", x = 0.34, xend = 0.34, y = 0.81, yend = 0.675,
             arrow = arrow(length = unit(0.14, "cm"), type = "closed"),
             linewidth = 0.35, colour = "grey30") +
    annotate("segment", x = 0.34, xend = 0.34, y = 0.53, yend = 0.335,
             arrow = arrow(length = unit(0.14, "cm"), type = "closed"),
             linewidth = 0.35, colour = "grey30") +
    # Exclusion arrows (branch at the exclusion's height, into the box edge)
    annotate("segment", x = 0.34, xend = 0.76 - bx, y = 0.74, yend = 0.74,
             arrow = arrow(length = unit(0.12, "cm"), type = "closed"),
             linewidth = 0.3, colour = "grey40") +
    annotate("segment", x = 0.34, xend = 0.76 - bx, y = 0.44, yend = 0.44,
             arrow = arrow(length = unit(0.12, "cm"), type = "closed"),
             linewidth = 0.3, colour = "grey40") +
    annotate("text", x = 0.34, y = 0.13, size = asb_txt(7, slides),
             colour = "grey40", family = "sans",
             label = "Players with no intervention-period AGDs retained in full\ncohort analyses (coded as 0); excluded from engaged cohort") +
    scale_x_continuous(limits = c(0, 1.02)) +
    scale_y_continuous(limits = c(0.04, 0.98)) +
    coord_fixed(ratio = 1.05) +
    theme_void(base_family = "sans") +
    theme(plot.margin = margin(8, 8, 8, 8),
          plot.title = element_text(face = "bold", size = if (slides) 20 else 11)) +
    asb_titles(slides, "Sample flow")
}

save_figure_all(make_F1, "F1_consort_flow", 110, 120)
rm(flow_data)

# =============================================================================
# FIGURE 2 — Primary outcomes: paired distributions
# =============================================================================

cat("\nBuilding F2 (pre-post distributions)...\n")

pp_long <- pp |>
  select(participant_id, period, total_deposits, total_losses, total_wagers, play_hours_total) |>
  pivot_longer(cols = c(total_deposits, total_losses, total_wagers, play_hours_total),
               names_to = "outcome", values_to = "value") |>
  mutate(
    outcome = factor(ASB_OUTCOME_LABELS[outcome], levels = ASB_OUTCOME_ORDER),
    period  = factor(period, levels = c("baseline", "intervention"),
                     labels = c("Baseline", "Intervention"))
  )

make_F2 <- function(slides = FALSE) {
  ggplot(pp_long, aes(x = period, y = value,
                      fill = interaction(outcome, period))) +
    geom_violin(alpha = 1, linewidth = 0.3, trim = TRUE, colour = "grey30") +
    geom_boxplot(width = 0.12, outlier.shape = NA, linewidth = 0.3,
                 fill = "white", colour = "grey20") +
    stat_summary(fun = median, geom = "point", size = if (slides) 2.2 else 1.3,
                 colour = "grey15") +
    scale_fill_manual(values = ASB_OUTCOME_PERIOD, guide = "none") +
    asb_scale_pseudolog(name = "Value (pseudo-log scale)") +
    facet_wrap(~ outcome, scales = "free_y", nrow = 1) +
    labs(x = NULL) +
    theme_asb(slides = slides) +
    asb_titles(slides, "Primary outcomes: baseline vs intervention",
               sprintf("Full cohort (N = %s). Lighter = baseline, darker = intervention.",
                       format(N_full, big.mark = ",")))
}

save_figure_all(make_F2, "F2_prepost_distributions", MS_2COL, 75)

# -----------------------------------------------------------------------------
# FIGURE 2b — Pre-post medians with IQR (presentation companion)  [R8]
# EXPLORATORY-DISPLAY ONLY: not in the registered figure list. Same data as
# F2 (pp_long); bars = medians, whiskers = IQR. Built primarily for slides;
# inclusion in the manuscript (e.g. supplementary) is an editorial decision.
# -----------------------------------------------------------------------------

cat("\nBuilding F2b (pre-post medians; presentation companion)...\n")

f2b_df <- pp_long |>
  group_by(outcome, period) |>
  summarise(mdn = median(value, na.rm = TRUE),
            q1  = quantile(value, 0.25, na.rm = TRUE),
            q3  = quantile(value, 0.75, na.rm = TRUE),
            .groups = "drop")

make_F2b <- function(slides = FALSE) {
  ggplot(f2b_df, aes(x = period, y = mdn,
                     fill = interaction(outcome, period))) +
    geom_col(width = 0.62, colour = "grey30", linewidth = 0.25) +
    geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.16,
                  linewidth = 0.35, colour = "grey20") +
    geom_text(aes(label = scales::comma(round(mdn, 1)), y = q3),
              vjust = -0.6, size = asb_txt(8, slides), colour = "grey5") +
    scale_fill_manual(values = ASB_OUTCOME_PERIOD, guide = "none") +
    scale_y_continuous(labels = scales::label_comma(),
                       expand = expansion(mult = c(0, 0.14))) +
    facet_wrap(~ outcome, scales = "free_y", nrow = 1) +
    labs(x = NULL, y = "Median (bars) with IQR (whiskers)") +
    theme_asb(slides = slides) +
    asb_titles(slides, "Primary outcomes: pre-post medians",
               sprintf("Full cohort (N = %s). Lighter = baseline, darker = intervention.",
                       format(N_full, big.mark = ",")))
}

save_figure_all(make_F2b, "F2b_prepost_medians", MS_2COL, 70)
rm(f2b_df)
rm(pp_long)

# =============================================================================
# FIGURE 3 — Monthly trends (ITS)
# =============================================================================

cat("\nBuilding F3 (ITS monthly trends)...\n")

pm_monthly_agg <- pm |>
  group_by(year_month, month_number) |>
  summarise(
    total_deposits   = median(total_deposits,   na.rm = TRUE),
    total_losses     = median(total_losses,     na.rm = TRUE),
    total_wagers     = median(total_wagers,     na.rm = TRUE),
    play_hours_total = median(play_hours_total, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(c(total_deposits, total_losses, total_wagers, play_hours_total),
               names_to = "outcome", values_to = "median_value") |>
  mutate(
    outcome = factor(ASB_OUTCOME_LABELS[outcome], levels = ASB_OUTCOME_ORDER),
    period  = ifelse(year_month < INTERVENTION_START, "Baseline", "Intervention")
  )

make_F3 <- function(slides = FALSE) {
  p <- ggplot(pm_monthly_agg,
              aes(x = year_month, y = median_value,
                  colour = interaction(outcome, period))) +
    geom_vline(xintercept = as.numeric(INTERVENTION_START),
               linetype = "dashed", colour = "grey45", linewidth = 0.4) +
    geom_line(aes(group = 1), linewidth = if (slides) 1 else 0.55) +
    geom_point(size = if (slides) 2.6 else 1.5) +
    scale_colour_manual(values = ASB_OUTCOME_PERIOD, guide = "none") +
    facet_wrap(~ outcome, scales = "free_y", nrow = 2) +
    labs(x = NULL, y = "Monthly median") +
    asb_scale_month("2 months") +
    theme_asb(slides = slides) +
    asb_titles(slides, "Monthly median outcomes across the 12-month study window",
               "Dashed line = intervention start (October 2024). Lighter = baseline, darker = intervention.")
  if (slides) {
    p <- p + geom_text(
      data = data.frame(year_month = INTERVENTION_START + 8,
                        outcome = factor(ASB_OUTCOME_ORDER[1],
                                         levels = ASB_OUTCOME_ORDER)),
      aes(x = year_month, y = -Inf), inherit.aes = FALSE,
      label = "Intervention start", vjust = -0.6, hjust = 0,
      size = asb_txt(11, FALSE), colour = "grey45")
  }
  p
}

save_figure_all(make_F3, "F3_monthly_trends_ITS", MS_2COL, 110)
rm(pm_monthly_agg)

# =============================================================================
# FIGURE 4 — Risk level distribution: stacked bar
# =============================================================================

cat("\nBuilding F4 (risk distribution)...\n")

risk_dist <- pm |>
  filter(!is.na(markov_state)) |>
  group_by(year_month) |>
  summarise(
    L = mean(markov_state == "L") * 100,
    M = mean(markov_state == "M") * 100,
    H = mean(markov_state == "H") * 100,
    I = mean(markov_state == "I") * 100,
    .groups = "drop"
  ) |>
  pivot_longer(c(L, M, H, I), names_to = "state", values_to = "pct") |>
  mutate(
    state = factor(state, levels = c("H", "M", "L", "I"),
                   labels = c("High", "Moderate", "Low", "Inactive"))
  )

make_F4 <- function(slides = FALSE) {
  ggplot(risk_dist, aes(x = year_month, y = pct, fill = state)) +
    geom_col(width = 25, colour = "white", linewidth = 0.2) +
    geom_vline(xintercept = as.numeric(INTERVENTION_START),
               linetype = "dashed", colour = "grey20", linewidth = 0.4) +
    scale_fill_manual(values = ASB_RISK, name = "Risk level") +
    scale_y_continuous(labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, 0.02))) +
    labs(x = NULL, y = "Proportion of players") +
    asb_scale_month("2 months") +
    theme_asb(slides = slides) +
    asb_titles(slides, "Monthly risk level distribution",
               "Dashed line = intervention start (October 2024)")
}

save_figure_all(make_F4, "F4_risk_distribution", MS_2COL, 85)
rm(risk_dist)

# =============================================================================
# FIGURE 5 — Risk level transition heatmap
# =============================================================================

cat("\nBuilding F5 (transition heatmap)...\n")

risk_trans_df <- pp_wide |>
  filter(!is.na(dominant_risk_baseline), !is.na(dominant_risk_intervention)) |>
  count(dominant_risk_baseline, dominant_risk_intervention) |>
  group_by(dominant_risk_baseline) |>
  mutate(pct = n / sum(n) * 100) |>
  ungroup() |>
  mutate(
    From = factor(ASB_RISK_LABELS[as.character(dominant_risk_baseline)],
                  levels = c("Low", "Moderate", "High")),
    To   = factor(ASB_RISK_LABELS[as.character(dominant_risk_intervention)],
                  levels = c("Low", "Moderate", "High"))
  )

make_F5 <- function(slides = FALSE) {
  ggplot(risk_trans_df, aes(x = To, y = From, fill = pct)) +
    geom_tile(colour = "white", linewidth = 0.6) +
    geom_text(aes(label = sprintf("%.1f%%\n(n = %s)", pct,
                                  format(n, big.mark = ",")),
                  colour = pct > 50),
              size = asb_txt(8, slides), lineheight = 1.1) +
    scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey10"),
                        guide = "none") +
    asb_scale_heat("% of row", limits = c(0, 100),
                   labels = function(x) paste0(x, "%")) +
    scale_x_discrete(position = "top") +
    scale_y_discrete(limits = rev) +
    labs(x = "Intervention risk level", y = "Baseline risk level") +
    theme_asb_heatmap(slides = slides) +
    asb_titles(slides, "Risk level transitions: baseline to intervention",
               "Row percentages; 6-month modal classification")
}

save_figure_all(make_F5, "F5_risk_transition_heatmap", 110, 90)
rm(risk_trans_df)

# =============================================================================
# FIGURE 6 — Markov transition heatmaps
# =============================================================================

cat("\nBuilding F6 (Markov heatmap)...\n")

if (!is.null(markov_res)) {
  states <- c("L", "M", "H", "I")

  make_markov_panel <- function(prob_mat, panel_title, slides) {
    df <- expand.grid(From = states, To = states) |>
      mutate(prob = as.vector(prob_mat[cbind(as.character(From),
                                             as.character(To))]))
    ggplot(df, aes(x = To, y = From, fill = prob)) +
      geom_tile(colour = "white", linewidth = 0.5) +
      geom_text(aes(label = sprintf("%.3f", prob), colour = prob > 0.5),
                size = asb_txt(7.5, slides)) +
      scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey10"),
                          guide = "none") +
      asb_scale_heat("Probability", limits = c(0, 1)) +
      scale_x_discrete(position = "top") +
      scale_y_discrete(limits = rev) +
      labs(title = panel_title, x = "Next state", y = "Current state") +
      theme_asb_heatmap(slides = slides) +
      theme(plot.title = element_text(face = "plain", colour = "grey10",
                                      size = if (slides) 16 else 9.5))
  }

  make_F6 <- function(slides = FALSE) {
    (make_markov_panel(markov_res$prob_baseline,     "Baseline",     slides) +
     make_markov_panel(markov_res$prob_intervention, "Intervention", slides)) +
      plot_layout(guides = "collect") &
      theme(legend.position = "right") &
      if (slides) plot_annotation(
        title    = "Monthly Markov transition matrices",
        subtitle = "States: L = low, M = moderate, H = high, I = inactive",
        theme = theme(plot.title = element_text(face = "bold", size = 20),
                      plot.subtitle = element_text(colour = "grey35", size = 15))
      ) else plot_annotation()
  }

  save_figure_all(make_F6, "F6_markov_heatmaps", MS_2COL, 80)
}

# =============================================================================
# FIGURE 7 — Engagement over time
# =============================================================================

cat("\nBuilding F7 (engagement trends)...\n")

engagement_monthly <- pm |>
  filter(is_intervention == 1) |>
  group_by(year_month) |>
  summarise(
    pct_with_intent = mean(n_limit_intents > 0, na.rm = TRUE) * 100,
    mean_intents    = mean(n_limit_intents,      na.rm = TRUE),
    .groups = "drop"
  )

make_F7 <- function(slides = FALSE) {
  panel_theme <- theme(plot.title = element_text(face = "plain", colour = "grey10",
                                                 size = if (slides) 15 else 9))
  p7a <- ggplot(engagement_monthly, aes(x = year_month, y = pct_with_intent)) +
    geom_line(colour = ASB_INTERVENTION, linewidth = if (slides) 1 else 0.6) +
    geom_point(colour = ASB_INTERVENTION, size = if (slides) 2.6 else 1.6) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, NA)) +
    labs(title = "Players with any limit modification (user-initiated)",
         x = NULL, y = "% of players") +
    asb_scale_month("1 month") +
    theme_asb(slides = slides) + panel_theme
  p7b <- ggplot(engagement_monthly, aes(x = year_month, y = mean_intents)) +
    geom_line(colour = ASB_INTERVENTION, linewidth = if (slides) 1 else 0.6) +
    geom_point(colour = ASB_INTERVENTION, size = if (slides) 2.6 else 1.6) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(title = "Mean limit modifications per player",
         x = NULL, y = "Mean count") +
    asb_scale_month("1 month") +
    theme_asb(slides = slides) + panel_theme
  (p7a / p7b) +
    if (slides) plot_annotation(
      title = "Engagement during the intervention period",
      theme = theme(plot.title = element_text(face = "bold", size = 20))
    ) else plot_annotation()
}

save_figure_all(make_F7, "F7_engagement_trends", 110, 130)
rm(engagement_monthly)

# =============================================================================
# FIGURE 8 — ITS posterior distributions (prereg 6.4)
# Draw-loading logic unchanged from the registered script.
# =============================================================================

cat("\nBuilding F8 (ITS posterior distributions)...\n")

if (!is.null(its_res) && !is.null(its_res$bayes)) {

  f8_outcome_labels <- c(
    log1p_deposits   = "Deposits (log1p)",
    asinh_losses     = "Net loss (asinh)",
    log1p_wagers     = "Wagers (log1p)",
    log1p_play_hours = "Play hours (log1p)"
  )
  f8_outcome_order <- unname(f8_outcome_labels[
    c("log1p_deposits", "asinh_losses", "log1p_play_hours", "log1p_wagers")])

  f8_get_draws <- function(entry) {
    if (is.null(entry)) return(NULL)
    if (!is.null(entry$b2) && !is.null(entry$b3)) {
      return(list(b2 = as.numeric(entry$b2), b3 = as.numeric(entry$b3)))
    }
    if (!is.null(entry$model_file) && file.exists(entry$model_file) &&
        requireNamespace("brms", quietly = TRUE)) {
      fit  <- readRDS(entry$model_file)
      post <- brms::as_draws_df(fit,
                                variable = c("b_intervention_t", "b_time_post_t"))
      out  <- list(b2 = post$b_intervention_t, b3 = post$b_time_post_t)
      rm(fit, post)
      invisible(gc(verbose = FALSE))
      return(out)
    }
    NULL
  }

  f8_df <- dplyr::bind_rows(lapply(names(its_res$bayes), function(v) {
    d <- f8_get_draws(its_res$bayes[[v]])
    if (is.null(d)) {
      cat("  F8: no posterior draws available for", v, "- panel skipped\n")
      return(NULL)
    }
    data.frame(
      outcome   = if (v %in% names(f8_outcome_labels)) f8_outcome_labels[[v]] else v,
      parameter = rep(c("b2", "b3"), times = c(length(d$b2), length(d$b3))),
      value     = c(d$b2, d$b3)
    )
  }))

  if (!is.null(f8_df) && nrow(f8_df) > 0) {
    f8_df$outcome <- factor(f8_df$outcome,
                            levels = intersect(f8_outcome_order,
                                               unique(f8_df$outcome)))
    # R9: outcome hues with shade = parameter (dark = beta_2 level change,
    # light = beta_3 slope change). Manuscript caption should state the
    # shade convention; the slide subtitle states it below.
    f8_hue_of <- c("Deposits (log1p)"   = "Deposits (GBP)",
                   "Net loss (asinh)"   = "Net loss (GBP)",
                   "Play hours (log1p)" = "Play hours",
                   "Wagers (log1p)"     = "Wagers (GBP)")
    f8_levels <- levels(f8_df$outcome)
    f8_fill_values <- c(
      setNames(unname(ASB_OUTCOME_DARK[f8_hue_of[f8_levels]]),
               paste0(f8_levels, ".b2")),
      setNames(unname(ASB_OUTCOME_LIGHT[f8_hue_of[f8_levels]]),
               paste0(f8_levels, ".b3"))
    )
    make_F8 <- function(slides = FALSE) {
      ggplot(f8_df, aes(x = value,
                        fill = interaction(outcome, parameter))) +
        geom_density(alpha = 1, linewidth = 0.3, colour = "grey30") +
        geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40",
                   linewidth = 0.4) +
        scale_fill_manual(values = f8_fill_values, guide = "none") +
        facet_wrap(~ outcome, scales = "free", nrow = 2) +
        labs(x = "Coefficient value (transformed outcome scale)",
             y = "Posterior density") +
        theme_asb(slides = slides) +
        asb_titles(slides, "ITS posterior distributions",
                   "Dashed line = 0. Darker = level change (beta_2); lighter = slope change (beta_3).")
    }
    save_figure_all(make_F8, "F8_its_posteriors", MS_2COL, 110)
  } else {
    cat("  F8 skipped: no posterior draws found in its_results.rds\n")
  }
  rm(f8_df)
  invisible(gc(verbose = FALSE))
} else {
  cat("  F8 skipped: results/07_its/its_results.rds not available (run 07_poc_its.R first)\n")
}

# =============================================================================
# FIGURE 9 — Recalibration escalation spike
# EXPLORATORY / post-hoc; companion to T9/T10. Not pre-registered.
# =============================================================================

cat("\nBuilding F9 (recalibration spike)...\n")

f9_df <- escalation_monthly |> filter(year_month >= INTERVENTION_START)
f9_expected <- mean(f9_df$pct_escalated[f9_df$year_month != SPIKE_MONTH],
                    na.rm = TRUE)

make_F9 <- function(slides = FALSE) {
  ggplot(f9_df, aes(x = year_month, y = pct_escalated,
                    fill = year_month == SPIKE_MONTH)) +
    geom_col(width = 25) +
    geom_hline(yintercept = f9_expected, linetype = "dashed",
               colour = "grey35", linewidth = 0.45) +
    scale_fill_manual(values = c(`FALSE` = "grey72", `TRUE` = "grey15"),
                      guide = "none") +
    scale_y_continuous(labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, 0.12))) +
    annotate("text", x = SPIKE_MONTH, y = max(f9_df$pct_escalated) * 1.06,
             label = "Recalibration month", size = asb_txt(8.5, slides),
             colour = "grey10", fontface = "bold", hjust = 0.5) +
    annotate("text", x = max(f9_df$year_month) + 10, y = f9_expected,
             label = "Expected rate", size = asb_txt(7.5, slides),
             colour = "grey35", hjust = 1, vjust = -0.7) +
    labs(x = NULL, y = "% escalating from low risk") +
    asb_scale_month("1 month") +
    theme_asb(slides = slides) +
    asb_titles(slides, "Monthly low-risk escalation rate",
               "Dark bar = December 2024 (recalibration month, observed spike). Dashed line = expected rate.")
}

save_figure_all(make_F9, "F9_recalibration_spike", 110, 85)
rm(f9_df)
invisible(gc(verbose = FALSE))

# =============================================================================
# FIGURE 10 — Regulatory-context timeline  [PATCH P5]
# Date provenance unchanged from the registered script (see 09 header).
# 27 Oct 2026 (co-author review): the two public UKGC date lines (30 Aug 2024
# FVC in force; 28 Feb 2025 FVC threshold) and their legend entry were removed;
# only the operator implementation (dashed) and ASB launch (solid) lines remain.
# =============================================================================

cat("\nBuilding F10 (regulatory timeline)...\n")

reg_dates <- data.frame(
  date   = as.Date(c("2024-09-01", "2024-10-01")),
  label  = c("Operator implementation of UKGC\nfinancial-risk requirements (1 Sep 2024)",
             "A Safe Bet launch\n(1 Oct 2024)"),
  # legend keys named as the line labels, ordered by date
  kind   = factor(c("Operator implementation", "ASB launch"),
                  levels = c("Operator implementation", "ASB launch")),
  # Staggered anchors so labels never overlap (R4): y as a fraction of the
  # series maximum; operator label left of its line (hjust 1), launch label
  # right of its line (hjust 0); no legend, the labels carry the description
  y_frac = c(1.14, 1.14),
  hjust  = c(1,    0),
  nudge  = c(-6,   6)
)

dep_monthly <- pm |>
  group_by(year_month) |>
  summarise(mdn_deposits = median(total_deposits, na.rm = TRUE), .groups = "drop")

make_F10 <- function(slides = FALSE) {
  y_max <- max(dep_monthly$mdn_deposits)
  ggplot(dep_monthly, aes(x = year_month, y = mdn_deposits)) +
    geom_vline(data = reg_dates,
               aes(xintercept = as.numeric(date), linetype = kind,
                   colour = kind),
               linewidth = 0.45) +
    geom_line(linewidth = if (slides) 1 else 0.6,
              colour = ASB_OUTCOME_DARK[["Deposits (GBP)"]]) +
    geom_point(size = if (slides) 2.6 else 1.7,
               colour = ASB_OUTCOME_DARK[["Deposits (GBP)"]]) +
    geom_text(data = reg_dates,
              aes(x = date + nudge, y = y_max * y_frac, label = label,
                  hjust = hjust, colour = kind),
              size = asb_txt(7.5, slides),
              lineheight = 1.05, vjust = 1, show.legend = FALSE) +
    scale_linetype_manual(values = c("Operator implementation" = "dashed",
                                     "ASB launch"              = "solid"),
                          name = NULL) +
    scale_colour_manual(values = c("Operator implementation" = "#4F6D8C",
                                   "ASB launch"              = ASB_INTERVENTION),
                        name = NULL) +
    scale_y_continuous(labels = scales::label_comma(prefix = "\u00a3"),
                       limits = c(0, y_max * 1.2),
                       expand = expansion(mult = c(0, 0))) +
    labs(x = NULL, y = "Median monthly deposits") +
    asb_scale_month("1 month") +
    theme_asb(slides = slides) +
    theme(legend.position = "none") +
    asb_titles(slides, "Monthly median deposits with regulatory timeline",
               "Operator implemented UKGC-related changes 1 Sept 2024; ASB launched 1 Oct 2024 (scheduled one month after).")
}

save_figure_all(make_F10, "F10_regulatory_timeline", MS_2COL, 95)
rm(dep_monthly)
invisible(gc(verbose = FALSE))

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n====================================\n")
cat("Tables saved to results/tables/\n")
cat("Figures saved to results/figures/manuscript/ (pdf + tiff, untitled)\n")
cat("  and results/figures/slides/ (16:9 titled png)\n")
cat("====================================\n")
cat("\nTables:\n")
cat("  T1  Baseline characteristics\n")  # (patched run: see PATCH SUMMARY in header)
cat("  T2  Primary outcomes (pre-post)\n")
cat("  T3  Intensity per AGD\n")
cat("  T4  Monthly metrics\n")
cat("  T5  Engagement categories\n")
cat("  T6a Risk migration (modal)\n")
cat("  T6b Risk migration (point-in-time; patch P4)\n")
cat("  T7  Risk trajectories\n")
cat("  T8  Monthly risk distribution\n")
cat("  T9  Escalation spike analysis (December anchor)\n")
cat("  T10 Algorithm decomposition\n")
cat("\nFigures:\n")
cat("  F1  CONSORT flow diagram\n")
cat("  F2  Pre-post distributions (violin + box)\n")
cat("  F2b Pre-post medians + IQR (presentation companion; not registered)\n")
cat("  F3  Monthly trends (ITS)\n")
cat("  F4  Risk distribution (stacked bar)\n")
cat("  F5  Risk transition heatmap\n")
cat("  F6  Markov heatmaps\n")
cat("  F7  Engagement trends\n")
cat("  F8  ITS posteriors\n")
cat("  F9  Recalibration spike (December anchor)\n")
cat("  F10 Regulatory timeline (patch P5)\n")
cat("\nNote: F8 is built from results/07_its/its_results.rds and is skipped\n")
cat("with a message when that file is absent (run 07_poc_its.R first)\n")
cat("\n=== 09_poc_tables_figures.R complete ===\n")
message("=== 09 COMPLETE ===")
