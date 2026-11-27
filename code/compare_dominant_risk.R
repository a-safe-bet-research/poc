# =============================================================================
# compare_dominant_risk.R
# Purpose : Pipeline verification utility — compare the pre-patch locked
#           dataset against the rebuilt dataset after the period-level
#           dominant-risk tiebreak patch
# Project : A Safe Bet (ASB) — Proof-of-Concept evaluation.
#           Putting Design into Practice: A Safe Bet — A Proof-of-Concept
#           Evaluation of a Tailored Online Responsible Gambling Framework
# Inputs  : pre-patch poc_analytical_LOCKED.rds (OLD_PATH); rebuilt
#           poc_analytical.rds (NEW_PATH)
# Outputs : none (read-only; prints comparison to console)
# =============================================================================
# PURPOSE: after rebuilding poc_analytical.rds with the prereg 5.2 (v31)
# period-level dominant-risk tiebreak patch (01_poc_build.R), compare the
# pre-patch locked dataset against the rebuilt dataset:
#   1. participant-period totals, number of dominant_risk changes, and the
#      old-to-new cross-table (the pre-registration disclosure figures);
#   2. confirm the monthly derivation (monthly_risk, markov_state) is
#      bit-identical, since the refactor must not change the monthly grain.
# READ-ONLY: loads two rds files and prints; writes and modifies nothing.
#
# RUN: from the scripts working directory, AFTER re-running 01_poc_build.R
# post-patch. 02_poc_lock.R refuses to overwrite the locked file, so the
# locked rds remains the pre-patch copy.
# =============================================================================

# OLD_PATH = archived pre-patch locked dataset; NEW_PATH = rebuilt post-patch
# poc_analytical.rds under ROOT_OUT as defined in 00_config.R
ROOT_DATA <- Sys.getenv("ASB_DATA_ROOT")
if (!nzchar(ROOT_DATA)) stop("ASB_DATA_ROOT is not set. Define it in .Renviron (see .Renviron.example).")
OLD_PATH <- file.path(ROOT_DATA, "archive_prelock", "poc_output_final_2026-06-24b", "poc_analytical_LOCKED.rds")  # pre-patch (locked)
NEW_PATH <- file.path(ROOT_DATA, "poc_output_lock_2026-07-15", "poc_analytical.rds")                                # rebuilt, post-patch

stopifnot(
  "Pre-patch locked file not found"   = file.exists(OLD_PATH),
  "Rebuilt analytical file not found" = file.exists(NEW_PATH)
)

old <- readRDS(OLD_PATH)
new <- readRDS(NEW_PATH)

pp_old <- old$player_period
pp_new <- new$player_period

# -----------------------------------------------------------------------------
# 1. Period-level dominant_risk comparison
# -----------------------------------------------------------------------------

key_old <- paste(pp_old$participant_id, pp_old$period)
key_new <- paste(pp_new$participant_id, pp_new$period)
stopifnot("participant-period keys differ between the two files" =
            identical(sort(key_old), sort(key_new)))

m        <- match(key_old, key_new)
old_risk <- as.character(pp_old$dominant_risk)
new_risk <- as.character(pp_new$dominant_risk[m])

n_total <- length(old_risk)
changed <- xor(is.na(old_risk), is.na(new_risk)) |
  (!is.na(old_risk) & !is.na(new_risk) & old_risk != new_risk)
n_changed <- sum(changed)

cat("Participant-periods compared:", n_total, "\n")
cat("dominant_risk changed:       ", n_changed,
    sprintf("(%.2f%%)\n", 100 * n_changed / n_total))

cat("\nOld-to-new cross-table (rows = pre-patch, cols = post-patch):\n")
print(table(old = old_risk, new = new_risk, useNA = "ifany"))

if (n_changed > 0) {
  cat("\nChanged participant-periods only:\n")
  print(table(old = old_risk[changed], new = new_risk[changed], useNA = "ifany"))
}

# -----------------------------------------------------------------------------
# 2. Monthly derivation must be bit-identical (refactor is shared-helper only)
# -----------------------------------------------------------------------------

pm_old <- old$player_month
pm_new <- new$player_month

key_m_old <- paste(pm_old$participant_id, pm_old$year_month)
key_m_new <- paste(pm_new$participant_id, pm_new$year_month)
stopifnot("participant-month keys differ between the two files" =
            identical(sort(key_m_old), sort(key_m_new)))

mm <- match(key_m_old, key_m_new)
same_monthly <- identical(as.character(pm_old$monthly_risk),
                          as.character(pm_new$monthly_risk[mm]))
same_state   <- identical(as.character(pm_old$markov_state),
                          as.character(pm_new$markov_state[mm]))

cat("\nMonthly derivation check (both must be TRUE):\n")
cat("  monthly_risk identical:", same_monthly, "\n")
cat("  markov_state identical:", same_state, "\n")
if (!same_monthly || !same_state) {
  cat("  WARNING: monthly output changed. The refactor was meant to be\n")
  cat("  behaviour-preserving at the monthly grain; investigate before\n")
  cat("  using the rebuilt dataset.\n")
}

cat("\ncompare_dominant_risk.R complete (read-only; nothing written).\n")
