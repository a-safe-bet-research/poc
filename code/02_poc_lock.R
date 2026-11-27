# =============================================================================
# 02_poc_lock.R
# Purpose : Step 2 — lock the analytical dataset with a reproducibility
#           manifest (SHA-256 hash + build parameters)
# Project : A Safe Bet (ASB) — Proof-of-Concept evaluation.
#           Putting Design into Practice: A Safe Bet — A Proof-of-Concept
#           Evaluation of a Tailored Online Responsible Gambling Framework
# Inputs  : poc_analytical.rds
# Outputs : poc_analytical_LOCKED.rds; poc_manifest.txt
# =============================================================================
# PURPOSE:
#   Save poc_analytical.rds as a LOCKED version with:
#     - SHA-256 hash of the file for tamper detection
#     - A manifest recording all build parameters
#     - A README note making the lock intent explicit
#
#   After this script runs, the locked .rds is the single source of truth.
#   Do NOT re-run scripts 00 or 01 after locking without renaming outputs.
#
# STATUS: Preparatory script – permitted to run before OSF registration.
#         Lock before developing ANY directional analysis scripts.
#
# OUTPUT:
#   poc_analytical_LOCKED.rds   (copy of poc_analytical.rds, immutable)
#   poc_manifest.txt            (hash + build params; commit this to git)
# =============================================================================

library(digest)  # for SHA-256

ROOT_DATA  <- Sys.getenv("ASB_DATA_ROOT")
if (!nzchar(ROOT_DATA)) stop("ASB_DATA_ROOT is not set. Define it in .Renviron (see .Renviron.example).")
OUTPUT_DIR <- file.path(ROOT_DATA, "poc_output_lock_2026-07-17")
LOG_DIR    <- file.path(ROOT_DATA, "logs")

cat("=== 02_poc_lock.R ===\n")
cat("Locking analytical dataset...\n")
cat("Time:", format(Sys.time()), "\n\n")

# -----------------------------------------------------------------------------
# 1. LOAD AND VERIFY poc_analytical.rds EXISTS
# -----------------------------------------------------------------------------

input_path  <- file.path(OUTPUT_DIR, "poc_analytical.rds")
locked_path <- file.path(OUTPUT_DIR, "poc_analytical_LOCKED.rds")

if (!file.exists(input_path)) {
  stop("poc_analytical.rds not found. Run 01_poc_build.R first.")
}

if (file.exists(locked_path)) {
  stop(
    "poc_analytical_LOCKED.rds already exists!\n",
    "Do NOT overwrite the locked dataset. If you need to rebuild from scratch,\n",
    "rename or archive the existing locked file first and document why."
  )
}

poc_analytical <- readRDS(input_path)

# Print sample flow for confirmation
sf <- poc_analytical$sample_flow
cat("Confirming build before locking:\n")
cat("  N selected:        ", sf$n_selected, "\n")
cat("  N ghost:           ", sf$n_ghost, "\n")
cat("  N analytical:      ", sf$n_analytical, "\n")
cat("  AGD threshold:     ", sf$threshold_used, "\n")
cat("  player_period rows:", nrow(poc_analytical$player_period), "\n")
cat("  player_month rows: ", nrow(poc_analytical$player_month), "\n\n")

# Prompt confirmation (skip if running non-interactively in batch)
if (interactive()) {
  ans <- readline("Type YES to lock this dataset: ")
  if (ans != "YES") stop("Lock cancelled by user.")
}

# -----------------------------------------------------------------------------
# 2. COPY TO LOCKED FILE
# -----------------------------------------------------------------------------

file.copy(input_path, locked_path, overwrite = FALSE)

# Compute SHA-256 of the locked file
hash_locked <- digest(locked_path, algo = "sha256", file = TRUE)

cat("Locked file SHA-256:", hash_locked, "\n")

# -----------------------------------------------------------------------------
# 3. WRITE MANIFEST
# -----------------------------------------------------------------------------

bp <- poc_analytical$build_params

manifest_lines <- c(
  "# A Safe Bet – POC Analytical Dataset Manifest",
  paste("# Generated:", format(Sys.time())),
  "",
  "## File",
  paste("file:", basename(locked_path)),
  paste("sha256:", hash_locked),
  paste("size_bytes:", file.size(locked_path)),
  "",
  "## Build parameters",
  paste("agd_threshold:", bp$agd_threshold),
  paste("baseline_start:", bp$baseline_start),
  paste("baseline_end:", bp$baseline_end),
  paste("intervention_start:", bp$intervention_start),
  paste("intervention_end:", bp$intervention_end),
  paste("recalibration_flag_date:", bp$recal_date),
  paste("ghost_tool_codes:", paste(bp$ghost_tool_codes, collapse = ", ")),
  paste("r_version:", bp$r_version),
  paste("built_at:", bp$built_at),
  "",
  "## Sample",
  paste("n_selected:", sf$n_selected),
  paste("n_ghost:", sf$n_ghost),
  paste("n_below_threshold:", sf$n_below_threshold),
  paste("n_analytical:", sf$n_analytical),
  paste("player_period_rows:", nrow(poc_analytical$player_period)),
  paste("player_month_rows:", nrow(poc_analytical$player_month)),
  "",
  "## Pre-registration status",
  "pre_reg_submitted: FALSE",
  "osf_url: [TO BE ADDED]",
  "",
  "## Instructions",
  "# This manifest should be committed to version control.",
  "# Verify dataset integrity before analysis:",
  "#   digest('poc_analytical_LOCKED.rds', algo='sha256', file=TRUE)",
  "# The hash above must match. If it does not, the file has been modified.",
  "# Directional analysis scripts (06+) must NOT be run on real data",
  "# until the pre-registration is submitted and the OSF URL is recorded above."
)

manifest_path <- file.path(OUTPUT_DIR, "poc_manifest.txt")
writeLines(manifest_lines, manifest_path)

cat("\nManifest written to:", manifest_path, "\n")
cat("\n--- NEXT STEPS ---\n")
cat("1. Commit poc_manifest.txt to your git repository\n")
cat("2. Run 03_poc_simulate.R to generate the synthetic dataset\n")
cat("3. Develop and run analysis scripts (06+) on poc_SYNTHETIC.rds ONLY\n")
cat("4. Submit pre-registration to OSF\n")
cat("5. Update poc_manifest.txt with OSF URL\n")
cat("6. Only then run analysis scripts on poc_analytical_LOCKED.rds\n")

cat("\n=== 02_poc_lock.R complete ===\n")
cat("LOCKED:", locked_path, "\n")
cat("SHA-256:", hash_locked, "\n")

