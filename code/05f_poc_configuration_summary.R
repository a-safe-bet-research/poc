# =============================================================================
# 05f_poc_configuration_summary.R
# Purpose : Configuration-level summary of the tool assignments (FA3 detail):
#           add-ons per monthly configuration (mean, SD, median; counts of
#           configurations with 0, 1, 2 and 3 add-ons), overall, by risk level
#           at assignment and by Refresh Event; distinct variant codes assigned
#           at least once, overall and by risk segment.
# Project : A Safe Bet (ASB) - Proof-of-Concept evaluation.
# Inputs  : poc_tool_assignments.rds (via 00_config.R)
# Outputs : results/10_supplementary/addon_distribution_summary.rds
#           (elements overall, by_risk, by_month, variants, counting_rules)
# Log     : Rscript -e 'setwd("<code folder>"); source("05f_poc_configuration_summary.R")' 2>&1 | tee logs/05f_real_YYYYMMDD.log
# =============================================================================
# STATUS  : POST-REGISTRATION descriptive (FA3 detail), values only; no tests.
#           Reported descriptively alongside FA3.
# COUNTING RULES:
#   - A configuration is one participant at one Refresh Event (assignment_date).
#   - Mandatory tools: Deposit Limit, Loss Limit, Reality Check, RG Homepage
#     (one record each per configuration; young-adult overlay rows for the
#     Deposit and Loss Limits are additional records of the same tools and do
#     not count as add-ons).
#   - Add-ons: the six add-on tools of the POC register: Bet Limit,
#     Login/Playtime Limit, Positive Play, Dark Flow, Deposit Count Limit and
#     Scheduled Breaks. Ghost add-ons (Deposit Count Limit, Scheduled Breaks)
#     count as add-ons because the Randomised Tool Selector drew them. The
#     Automatic Withdrawal Limit code (outside the ten-tool register, recorded
#     in error) is NOT an add-on and is excluded from the add-on count.
#   - Risk level: asb_risk_level at the Refresh Event.
# LOCKED-FILE PROTECTION: reads the assignment file only; writes its own output.
# =============================================================================

suppressPackageStartupMessages({ library(dplyr); library(tidyr) })
source("00_config.R")
if (requireNamespace("conflicted", quietly = TRUE)) {
  library(conflicted)
  conflicts_prefer(dplyr::filter, dplyr::select, dplyr::mutate, dplyr::summarise,
                   dplyr::arrange, dplyr::count, dplyr::group_by, dplyr::lag, dplyr::pull)
}
dir.create("results/10_supplementary", recursive = TRUE, showWarnings = FALSE)
OUT_PATH <- "results/10_supplementary/addon_distribution_summary.rds"

ADDON_TOOLS <- c("bet_limit", "login_playtime_limit", "positive_play_selfassess",
                 "dark_flow_selfassess", "deposit_count_limit", "scheduled_breaks")
MANDATORY_TOOLS <- c("deposit_limit", "loss_limit", "reality_check", "rg_homepage")
EXCLUDED_CODES  <- c("awl")   # outside the ten-tool register

cat("=== 05f_poc_configuration_summary.R ===\n")
ta <- readRDS(FILE_POC_TOOLS) |>
  mutate(assignment_date = as.Date(assignment_date)) |>
  filter(assignment_date %in% OFFER_DATES)
cat("Assignment records:", nrow(ta), "| participants:", n_distinct(ta$participant_id),
    "| Refresh Events:", n_distinct(ta$assignment_date), "\n")
stopifnot("Unexpected tool_name values" =
            all(ta$tool_name %in% c(ADDON_TOOLS, MANDATORY_TOOLS, EXCLUDED_CODES)))

cfg <- ta |>
  group_by(participant_id, assignment_date, asb_risk_level) |>
  summarise(n_records   = n(),
            n_mandatory = n_distinct(tool_name[tool_name %in% MANDATORY_TOOLS]),
            n_addons    = sum(tool_name %in% ADDON_TOOLS),
            n_excluded  = sum(tool_name %in% EXCLUDED_CODES),
            .groups = "drop")
cat("Configurations:", nrow(cfg), "\n")
cat("Configurations with all four mandatory tools:", sum(cfg$n_mandatory == 4), "\n")

summarise_cfg <- function(d) {
  d |> summarise(n_cfg = n(), mean = mean(n_addons), sd = sd(n_addons), median = median(n_addons),
                 n_0 = sum(n_addons == 0), n_1 = sum(n_addons == 1),
                 n_2 = sum(n_addons == 2), n_3 = sum(n_addons == 3), n_4plus = sum(n_addons >= 4),
                 .groups = "drop") |>
    mutate(pct_0 = 100 * n_0 / n_cfg, pct_1 = 100 * n_1 / n_cfg,
           pct_2 = 100 * n_2 / n_cfg, pct_3 = 100 * n_3 / n_cfg)
}
overall  <- summarise_cfg(cfg)
by_risk  <- cfg |> group_by(asb_risk_level) |> summarise_cfg() |> arrange(asb_risk_level)
by_month <- cfg |> group_by(assignment_date) |> summarise_cfg() |> arrange(assignment_date)

# Distinct variant codes assigned at least once, overall and by risk segment of the code
risk_seg <- function(code) sub("^UK\\.PA\\.([^.]+)\\..*$", "\\1", code)
variants <- ta |>
  filter(!tool_name %in% EXCLUDED_CODES) |>
  distinct(tool_code) |>
  mutate(segment = risk_seg(tool_code),
         risk_label = case_when(segment == "1" ~ "low", segment == "2" ~ "moderate",
                                segment == "3" ~ "high", TRUE ~ "young_adult")) |>
  count(risk_label, name = "n_variants_assigned") |>
  bind_rows(tibble(risk_label = "total",
                   n_variants_assigned = n_distinct(ta$tool_code[!ta$tool_name %in% EXCLUDED_CODES])))

print_df <- function(x, label) { cat("\n", label, "\n", sep = ""); print(as.data.frame(x), row.names = FALSE, digits = 4) }
print_df(overall,  "Add-ons per configuration, overall:")
print_df(by_risk,  "Add-ons per configuration, by risk level at the Refresh Event:")
print_df(by_month, "Add-ons per configuration, by Refresh Event:")
print_df(variants, "Distinct variant codes assigned at least once (AWL codes excluded):")
cat("\nAutomatic Withdrawal Limit records excluded from the add-on count:", sum(cfg$n_excluded), "\n")

# --- Structural checks (design constants from 00_config.R) --------------------
stopifnot(
  "Configuration count differs from N_SELECTED x number of Refresh Events" =
    overall$n_cfg == N_SELECTED * length(OFFER_DATES),
  "Add-on counts do not partition the configurations" =
    overall$n_0 + overall$n_1 + overall$n_2 + overall$n_3 + overall$n_4plus == overall$n_cfg,
  "A configuration lacks one of the four mandatory tools" = all(cfg$n_mandatory == 4)
)
cat("\nStructural checks passed.\n")

saveRDS(list(overall = overall, by_risk = by_risk, by_month = by_month, variants = variants,
             counting_rules = list(addon_tools = ADDON_TOOLS, mandatory_tools = MANDATORY_TOOLS,
                                   excluded_codes = EXCLUDED_CODES),
             produced_at = Sys.time(), r_version = R.version.string),
        OUT_PATH)
cat("\nSaved:", OUT_PATH, "\n=== 05f complete ===\n")
