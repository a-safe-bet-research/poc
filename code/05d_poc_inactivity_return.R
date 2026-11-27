# =============================================================================
# 05d_poc_inactivity_return.R  (v2, Item 7 revised per the analysis decisions register, 21 July 2026)
# ASB POC — EXPLORATORY / post-hoc descriptives (NOT pre-registered)
#
# Item 7a: inactivity return and spell structure, computed DIRECTLY FROM
#          OBSERVED SEQUENCES in the player_month grid. NO chained-matrix
#          quantities anywhere in this script: the homogeneity rejection
#          (chi-square(48) = 760.07) forbids powering the period-average
#          matrices into multi-month trajectory claims.
#
# Item 7b (risk trajectories for baseline moderates) is CONDITIONAL on the
#          D11 decision and is deliberately NOT implemented here.
#
# BREAK DEFINITION (stated verbatim in the results entry): a participant's
#   FIRST inactive intervention month FOLLOWING at least one active
#   intervention month, within the intervention window only
#   (Oct 2024 - Mar 2025). Active month = agd_count > 0; inactive = 0
#   (absent player_month rows in window months count as inactive via a
#   full participant x month grid; fill count printed).
#
# Population: analytical participants (N = 5,808; no YA exclusion) with
#   >= 1 active intervention month whose break month precedes March 2025.
#
# Reported (7a):
#   (i)   proportion with >= 1 active month after the break;
#   (ii)  median and IQR of first inactive spell length — completed spells
#         (returners) and observed-including-censored shown separately;
#   (iii) proportion of first spells right-censored at March 2025 —
#         WORDING RULE: "had not returned by study end", never "never
#         returned";
#   (iv)  month-specific I -> I probabilities as a series (observed
#         sequences; the same inputs as the homogeneity test).
#         NOTE: a six-month window contains FIVE month-pairs
#         (Oct->Nov ... Feb->Mar); the spec says "six" — the script prints
#         the five that exist within the window plus, separately labelled,
#         the Sep->Oct entry pair (last baseline month -> first
#         intervention month) in case that was the intended sixth point.
#
# ICRG NOTE: reconciliation with the ICRG presentation's break-and-return
#   figures (65% one-month return) WAIVED per the analysis decisions
#   register, 21 July 2026: the construction is unavailable and the
#   figures are not treated as
#   comparable. The next-month-return proportion under THIS construction
#   is still printed as context; the entry definition is the sole basis
#   for any quoted number.
#
# Data    : player_month from poc_analytical_LOCKED.rds (READ-ONLY);
#           locked dataset untouched; own outputs only; mtime assertion.
# Output  : results/05_descriptives/inactivity_return.rds
# Log     : Rscript 05d_poc_inactivity_return.R 2>&1 | tee logs/05d_real_YYYYMMDD.log
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

dir.create("results/05_descriptives", recursive = TRUE, showWarnings = FALSE)

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
cat("NO chained-matrix quantities: all statistics computed directly from",
    "observed sequences (homogeneity rejected, chi-square(48) = 760.07)\n")

analytical_ids <- unique(pp$participant_id)
cat("Analytical sample:", length(analytical_ids), "\n")

# -----------------------------------------------------------------------------
# 1. FULL-YEAR ACTIVITY GRID (baseline + intervention)
#    Intervention months drive the break logic; the last baseline month is
#    kept only for the separately-labelled Sep->Oct entry pair in (iv).
# -----------------------------------------------------------------------------
pm_a <- pm |>
  filter(participant_id %in% analytical_ids) |>
  select(participant_id, year_month, is_intervention, agd_count)

iv_months <- pm_a |> filter(is_intervention == 1) |> pull(year_month) |> unique() |> sort()
cat("Intervention months found:", format(iv_months), "\n")
stopifnot("expected 6 intervention months" = length(iv_months) == 6)
sep_month <- max(pm_a$year_month[pm_a$is_intervention != 1])
cat("Last baseline month (entry-pair only):", format(sep_month), "\n")

grid_months <- c(sep_month, iv_months)
n_rows_before <- nrow(pm_a |> filter(year_month %in% grid_months))
grid <- expand_grid(participant_id = analytical_ids, year_month = grid_months) |>
  left_join(pm_a |> filter(year_month %in% grid_months) |>
              select(participant_id, year_month, agd_count),
            by = c("participant_id", "year_month")) |>
  mutate(active = as.integer(coalesce(agd_count, 0L) > 0)) |>
  arrange(participant_id, year_month)
cat("player_month rows in grid months:", n_rows_before,
    "| grid rows:", nrow(grid),
    "| rows filled as inactive (absent from player_month):",
    nrow(grid) - n_rows_before, "\n")

# intervention-only sub-grid with month index 1..6 (Oct = 1 ... Mar = 6)
ivg <- grid |> filter(year_month %in% iv_months) |>
  mutate(m_idx = match(year_month, iv_months))

# -----------------------------------------------------------------------------
# 2. BREAKS, RETURNS, SPELLS (7a i-iii) — observed sequences only
# -----------------------------------------------------------------------------
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
  left_join(returns, by = "participant_id")

cat("\n--- Population accounting ---\n")
cat("Analytical participants:                         ", nrow(cohort), "\n")
cat(">= 1 active intervention month:                  ", sum(cohort$n_active > 0), "\n")
cat("  no inactive month after activity (no break):   ",
    sum(cohort$n_active > 0 & is.na(cohort$break_idx)), "\n")
cat("  break exists:                                  ", sum(!is.na(cohort$break_idx)), "\n")
cat("  break precedes March 2025 (POPULATION):        ",
    sum(!is.na(cohort$break_idx) & cohort$break_idx < 6), "\n")
cat("  excluded: break IS March 2025 (no observable return window):",
    sum(!is.na(cohort$break_idx) & cohort$break_idx == 6), "\n")

pop <- cohort |>
  filter(!is.na(break_idx), break_idx < 6) |>
  mutate(
    returned  = as.integer(!is.na(first_return_idx)),
    # first inactive spell length in months:
    #   returners: consecutive inactive months = first_return_idx - break_idx
    #   censored : observed inactive months through March = 7 - break_idx,
    #              right-censored at study end
    spell_len = ifelse(returned == 1, first_return_idx - break_idx, 7L - break_idx),
    censored  = 1L - returned
  )

n_pop <- nrow(pop); n_ret <- sum(pop$returned)
cat("\n--- 7a HEADLINE (EXPLORATORY / post-hoc) ---\n")
cat(sprintf("(i)   Return rate: %d / %d = %.3f had >= 1 active month after the first break\n",
            n_ret, n_pop, n_ret / n_pop))
cat("(ii)  First inactive spell length, COMPLETED spells (returners only):\n")
print(summary(pop$spell_len[pop$returned == 1]))
cat(sprintf("      median %s [IQR %s, %s], n = %d\n",
            median(pop$spell_len[pop$returned == 1]),
            quantile(pop$spell_len[pop$returned == 1], .25),
            quantile(pop$spell_len[pop$returned == 1], .75), n_ret))
cat("      Observed spell length INCLUDING right-censored spells (lower bounds for censored):\n")
print(summary(pop$spell_len))
cat(sprintf("(iii) Right-censored at March 2025: %d / %d = %.3f HAD NOT RETURNED BY STUDY END\n",
            n_pop - n_ret, n_pop, (n_pop - n_ret) / n_pop))
cat("      (WORDING RULE: 'had not returned by study end' — never 'never returned')\n")

# ICRG reconciliation support: closest analogue under THIS construction
next_month <- sum(pop$returned == 1 & pop$spell_len == 1)
cat(sprintf("\nContext: %d / %d = %.3f of breaks were followed by activity in the VERY NEXT month.\n",
            next_month, n_pop, next_month / n_pop))
cat("  (ICRG presentation's 65% one-month figure: different construction, unavailable;\n")
cat("   reconciliation WAIVED per the analysis decisions register, 21 July: not comparable, not a gate.)\n")

# -----------------------------------------------------------------------------
# 3. (iv) MONTH-SPECIFIC I -> I SERIES — observed sequences, same inputs as
#    the homogeneity test. Five pairs exist within the window; the Sep->Oct
#    entry pair is printed separately in case it was the intended sixth.
# -----------------------------------------------------------------------------
wide <- grid |>
  select(participant_id, year_month, active) |>
  pivot_wider(names_from = year_month, values_from = active)
mcols <- as.character(grid_months)   # Sep, Oct, ..., Mar

ii_series <- lapply(seq_len(length(mcols) - 1), function(k) {
  from <- wide[[mcols[k]]]; to <- wide[[mcols[k + 1]]]
  n_I  <- sum(from == 0)
  data.frame(
    pair       = sprintf("%s -> %s", format(grid_months[k], "%b %Y"),
                         format(grid_months[k + 1], "%b %Y")),
    scope      = ifelse(k == 1, "ENTRY PAIR (last baseline -> first intervention; separately labelled)",
                        "within intervention window"),
    n_inactive_from = n_I,
    p_I_to_I   = round(sum(from == 0 & to == 0) / n_I, 3)
  )
}) |> bind_rows()
print_full(ii_series,
  "\n(iv) Month-specific I -> I probabilities (all analytical participants; active = agd_count > 0):")
cat("NOTE: the six-month window contains five month-pairs; the spec's 'six' is satisfied only if the\n")
cat("Sep->Oct entry pair counts; it is printed above, separately labelled, for the decisions register to include or drop.\n")

# -----------------------------------------------------------------------------
# 4. RETURN RATE BY BREAK MONTH (censoring made visible)
# -----------------------------------------------------------------------------
by_break <- pop |>
  mutate(break_month = format(iv_months[break_idx], "%b %Y"),
         months_observable = 6 - break_idx) |>
  group_by(break_idx, break_month, months_observable) |>
  summarise(n = n(), n_returned = sum(returned),
            return_rate = round(mean(returned), 3),
            n_not_returned_by_study_end = n() - sum(returned),
            median_spell_completed = median(spell_len[returned == 1]),
            .groups = "drop") |>
  arrange(break_idx)
print_full(by_break, "\nReturn rate by break month (later breaks are right-censored by the window end):")

# -----------------------------------------------------------------------------
# 5. SAVE (own output only) + LOCKED-FILE INTEGRITY
# -----------------------------------------------------------------------------
out <- list(
  label            = "EXPLORATORY / post-hoc — not pre-registered (convention as D10); no chained-matrix quantities",
  break_definition = "first inactive intervention month following >= 1 active intervention month; intervention window only (Oct 2024 - Mar 2025); active = agd_count > 0",
  population_rule  = "analytical participants with >= 1 active intervention month whose break month precedes March 2025",
  wording_rule     = "censored spells are 'had not returned by study end', never 'never returned'",
  icrg_note = "reconciliation with the ICRG presentation figures WAIVED (analysis decisions register, 21 July 2026): construction unavailable, not comparable; this entry's definition is the sole basis",
  accounting = cohort |> summarise(
    analytical              = n(),
    any_active              = sum(n_active > 0),
    no_break                = sum(n_active > 0 & is.na(break_idx)),
    break_exists            = sum(!is.na(break_idx)),
    population              = sum(!is.na(break_idx) & break_idx < 6),
    break_in_march_excluded = sum(!is.na(break_idx) & break_idx == 6)),
  n_population       = n_pop,
  n_returned         = n_ret,
  return_rate        = n_ret / n_pop,
  p_censored         = (n_pop - n_ret) / n_pop,
  p_next_month_return = next_month / n_pop,
  spell_completed_summary = summary(pop$spell_len[pop$returned == 1]),
  spell_completed_median_iqr = c(median = median(pop$spell_len[pop$returned == 1]),
                                 q25 = quantile(pop$spell_len[pop$returned == 1], .25),
                                 q75 = quantile(pop$spell_len[pop$returned == 1], .75)),
  spell_observed_summary  = summary(pop$spell_len),
  ii_series          = ii_series,
  by_break_month     = by_break,
  per_participant    = pop |> select(participant_id, break_idx, returned, censored, spell_len)
)
saveRDS(out, "results/05_descriptives/inactivity_return.rds")
stamp("Saved results/05_descriptives/inactivity_return.rds")

stopifnot("LOCKED FILE MTIME CHANGED DURING RUN — investigate immediately" =
            identical(locked_mtime_at_load, file.info(FILE_POC_LOCKED)$mtime))
cat("Locked-file integrity check passed: poc_analytical_LOCKED.rds untouched.\n")
stamp("05d (v2, Item 7a) complete. Item 7b NOT implemented — conditional on D11.")
