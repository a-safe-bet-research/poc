# A Safe Bet proof-of-concept evaluation: analysis code

R pipeline for the SunVegas UK proof-of-concept evaluation (pre-registration:
asb_poc_01_1). Data are read with DuckDB from Parquet tables and Kafka
batch exports in the secure EUR research environment and are not shareable. The internal
rename crosswalk read at the de-identification boundary holds operator-original schema
identifiers and is withheld with the data (asb_poc_01_1, Section 10.1; asb_poc_03_2). Nothing in this folder contains participant-level data, operator extracts,
the crosswalk, credentials or server paths.

## Environment

Recorded R version: 4.3.3. Package versions are in `renv.lock`, generated on the EUR
ECDA server from the R library that ran the locked pipeline (137 packages; CRAN); restore
with `renv::restore()`.

Two environment variables are required, read with `Sys.getenv()` from a `.Renviron`
file in the folder you start R from. `.Renviron.example` shows the format. The real
`.Renviron` names server paths and is never committed (`.gitignore` excludes it).

| Variable | Meaning |
|---|---|
| `ASB_DATA_ROOT` | Data root on the server. `00_config.R` builds every input and output path from it: the Kafka batch folders, the lookup tables, `poc_output_lock_2026-07-17/` (pipeline outputs, including the locked analytical dataset) and `logs/`. |
| `ASB_CROSSWALK` | Full path to the internal rename crosswalk. If unset, `00_poc_pull.R`, `05c` and `10` look for `ASB_rename_crosswalk.csv` under `ASB_DATA_ROOT`. |

Scripts do not call `setwd()`. Run them with the working directory set to this folder
(`code/`), which is where `00_config.R` is sourced from and where the `results/`
subfolders are written. On a server whose site Rprofile changes the working directory
for non-interactive runs (as EUR ECDA does), start each script through a wrapper that
sets it first: `Rscript -e 'setwd("<code folder>"); source("05_poc_descriptives.R")'`.

## Run order

| Script | Purpose | Produces |
|---|---|---|
| `00_config.R` | Paths (from `ASB_DATA_ROOT`), constants, seeds, brms helpers; sourced by every script | nothing |
| `00_poc_pull.R` | Step 0. Extraction from Parquet and Kafka batches via DuckDB at the de-identification boundary; `signal_class` normalisation on read | `poc_raw.rds`, `poc_tool_assignments.rds`, pull log |
| `01_poc_build.R` | Step 1. Analytical dataset under the pre-registered rules; Markov states by the percentage-frequency method | `poc_analytical.rds`, build log |
| `02_poc_lock.R` | Step 2. Locks the analytical dataset with a SHA-256 manifest; refuses to overwrite an existing lock | `poc_analytical_LOCKED.rds`, `poc_manifest.txt` |
| `03_poc_simulate.R` | Step 3. Synthetic development dataset fitted to the locked data (not deposited) | `poc_synthetic.rds`, hash appended to the manifest |
| `04_poc_validate.R` | Step 4. Validates the synthetic dataset against the locked data | `poc_validation_report.txt`, `poc_validation_plots.pdf` |
| `05_poc_descriptives.R` | Baseline characteristics; feasibility outcomes FA1 to FA6 | `results/05_descriptives/descriptives_output.rds` |
| `05b_poc_descriptives_followup.R` | Post-registration follow-up to `05`: full FA4 and FA5 reprints, FA5a proportions and the non-registered limit types, inter-session interval, and the expanded sample-characteristics table (manuscript Table 1) | `results/05_descriptives/descriptives_followup.rds` |
| `05c_poc_engagement_extraction.R` | Post-registration FA4 and FA5 strata registered in asb_poc_01_1, Section 10.4: message response by type, tool, risk level and content variant; threshold levels; limit modifications by tool (reported in the main text and Supporting Information S1 and S2) | `results/05_descriptives/engagement_extraction.rds` |
| `05d_poc_inactivity_return.R` | Exploratory, not pre-registered (item 7a): first periods of inactivity, return after inactivity, their length and month-specific inactive-to-inactive probabilities, counted from observed sequences (main-text return figures; Supporting Information S5, Table S5.4) | `results/05_descriptives/inactivity_return.rds` |
| `05e_poc_final_exploratory.R` | Exploratory, not pre-registered (item 9b): baseline profile of participants never active during the intervention against the engaged cohort (Supporting Information S5, Table S5.5) | `results/05_descriptives/final_exploratory.rds` |
| `05f_poc_configuration_summary.R` | Post-registration descriptive (FA3 detail): add-ons per monthly configuration overall, by risk level and by Refresh Event, and distinct variant codes assigned at least once (Results FA3 paragraph; Table 5; Supporting Information S2, Table S2.3). Written 27 October 2026 to replace a script that was not saved; reproduces the saved summary exactly | `results/10_supplementary/addon_distribution_summary.rds` |
| `06_poc_prepost.R` | Pre-post Wilcoxon tests and Bayes factors (H1 to H5, H8) | `results/06_prepost/prepost_results.rds` |
| `06b_poc_sensitivity.R` | Pre-specified sensitivity analyses (asb_poc_01_1, Section 9) | `results/06b_sensitivity/sensitivity_results.rds` |
| `07_poc_its.R` | Interrupted time series, frequentist and Bayesian (H6, H7) | `results/07_its/its_results.rds` and cached brms fits |
| `07b_poc_its_prior_sensitivity.R` | Pre-registered prior-sensitivity refits with Normal(0, 5) priors (asb_poc_01_1, Section 10.4 item 1; reported in Supporting Information S3) and the slim export of the 07 results | `results/07_its/its_prior_sensitivity.rds`, `results/07_its/its_results_slim.rds` |
| `07c_poc_its_reparameterisation.R` | Post-registration exploratory analysis of the ITS coding: computes the October 2024 departure from the extrapolated baseline (β₂ + β₃) with its standard error from the saved registered fits, and refits the same models with post-intervention time recoded so that the level-change coefficient is that departure directly (Results, interrupted time-series paragraph; Table 8 note; Supporting Information S1) | `results/07_its/expl/EXPL_*.csv`, `EXPL_freq_results.rds` |
| `07d_poc_its_two_interruptions.R` | Post-registration exploratory analysis: the registered segmented model with an added step at the operator's 1 September 2024 implementation of the Gambling Commission financial vulnerability checks, with the AR(1) sensitivity fit and a comparison against the registered model (Figure 4B; Supporting Information S1 and S5, Table S5.7) | `EXPL_outputs/EXPL_07d_*.csv`, `EXPL_outputs/EXPL_07d_results.rds` |
| `08_poc_markov.R` | Markov risk-state transition models, monthly and weekly. This is the 21 July 2026 version run for the reported results: the continuous-time msm fits use the bobyqa optimiser (`minqa`) after the BFGS fits failed to converge (Supporting Information S1, Item 8) | `results/08_markov/markov_results.rds` and cached msm fits |
| `09_poc_tables_figures.R` | Publication tables and figures. The final version of the registered script of the same name (30 July 2026 restyle; data pipelines unchanged from the 21 July patched run); this version produced the tables in the manuscript; the published figures are produced by `12_poc_manuscript_figures.R` | `results/tables/` (csv and rds), `results/figures/manuscript/` (pdf and tiff), `results/figures/slides/` (png) |
| `asb_style.R` | Shared figure style module sourced by `09_poc_tables_figures.R` and `12_poc_manuscript_figures.R`: theme, palettes and the dual manuscript and slide output | nothing on its own |
| `10_poc_supplementary_descriptives.R` | Supplementary descriptive panels S-D1 to S-D7: per-tool assignment inventory, message activity and content style by month, threshold exposure, limit modifications, engagement covariates (Supporting Information S2, Tables S2.4 to S2.10) | `results/10_supplementary/supplementary_descriptives.rds` |
| `11_poc_disengagement_characterisation.R` | Exploratory, not pre-registered: platform contact among participants never active, baseline profile of the disengagement groups, monthly inflow to inactivity (Supporting Information S5, Tables S5.5 and S5.6) | `results/11_disengagement/disengagement_characterisation.rds` |
| `12_poc_manuscript_figures.R` | Figures for the manuscript and its Supporting Information. Figures only: no table is rebuilt and no model is re-estimated. Reads the locked dataset and the saved results of `07`, `07d` and `08` | `results/figures/manuscript/` (pdf and tiff), `results/figures/slides/` (png), two verification csv files in `results/tables/` |
| `compare_dominant_risk.R` | Verification utility: compares the pre-patch locked dataset with the rebuilt dataset after the dominant-risk tie-break patch (asb_poc_01_1, Section 10.3); read-only | console output only |

Dependencies between scripts: `05b` and `05c` need `poc_raw.rds` and the locked dataset;
`05d`, `05e` and `11` need only the locked dataset; `05f` needs only `poc_tool_assignments.rds`; `07b` and `07c` need the saved `07` results;
`10` needs the saved outputs of `05`, `05b`, `05c` and `09`; `09` needs `asb_style.R` in the same folder;
`07d` needs the locked dataset and the saved `07` results; `12` needs the locked dataset, the saved `07`, `07d` and `08`
results and `asb_style.R`.

Script 12 output names and published figure numbers: Fig1 to Fig5 are Figures 1 to 5; FigS8 is Figure S2.1;
FigS9, FigS1, FigS2, FigS3 and FigS10 are Figures S3.1 to S3.5; FigS4 is Figure S4.1; FigS7 is Figure S5.1.
FigS5 and FigS6 are not used.

## Run flags

Six scripts carry a `USE_SYNTHETIC` flag near the top: `05`, `06`, `06b`, `07`, `08`
and `09`. `TRUE` runs the script on `poc_synthetic.rds` (the pre-registration
development phase); `FALSE` runs it on `poc_analytical_LOCKED.rds`. All six are set to
`FALSE` here, which is the setting that produced the reported results. `07`, `07b`,
`08`, `09` and `12` also carry an `OVERWRITE` flag controlling whether cached model fits or
rendered figures are reused. `07c` carries `RUN_BAYES` (default `FALSE`): `TRUE` adds the
exploratory brms refits, which take about three hours and are not reported. `07d` and `12` read four
optional environment variables with defaults (`EXPL_DIR`, `EXPL_ITS_RESULTS`, `EXPL_DATA_SOURCE`,
`EXPL_07D_RESULTS`).

## Relation to the registered OSF deposit

These are the script versions that produced the manuscript and its Supporting
Information. They post-date the analysis code registered with the OSF deposit
(https://osf.io/x6bfd): the deposited scripts were the pre-registration-phase versions,
and the versions here carry the post-registration corrections and the post-registration
scripts (`05b`, `05c`, `05d`, `05e`, `05f`, `07b`, `07c`, `07d`, `10`, `11`, `12`). Differences that change an
analysis decision are reported in Supporting Information S1.

## Dataset lock

The registered dataset lock (16 July 2026) is recorded in `poc_manifest.txt` on the
server and in asb_poc_01_1, Section 10.3. `02_poc_lock.R` will not overwrite an existing
locked file.
