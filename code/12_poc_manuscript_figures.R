# Run from the project root; 00_config.R resolves all paths.
# =============================================================================
# 12_poc_manuscript_figures.R  (19 November 2026; revised 24 November 2026)
#   24 Nov 2026: panel order follows the manuscript (deposits, net loss,
#   wagers, play hours) in every figure; Fig 4A axis in GBP like the other
#   figures; OVERWRITE set TRUE so a re-run always re-renders; TIFF output
#   at 600 dpi (asb_style.R). No data step or model is changed.
#
# Purpose : Figures for the manuscript and its Supporting Information.
#
# Scope   : FIGURES ONLY. No table is rebuilt and no model is re-estimated.
#           Reads the LOCKED analytical dataset read-only and the saved
#           results from 07 and 08. Writes figure files and two small
#           verification CSVs. Does not modify any input.
#
# Depends : 00_config.R, asb_style.R (theme, palettes, save_figure_all,
#           asb_scale_pseudolog, asb_scale_month, asb_scale_heat,
#           theme_asb_heatmap, asb_titles), results/07_its/its_results.rds,
#           results/08_markov/markov_results.rds.
#
# Outputs : via save_figure_all(): manuscript PDF + TIFF (untitled, journal
#           widths) and titled 16:9 slide PNGs, as in 09.
#           results/tables/FIG_monthly_medians_bootstrap_ci.csv
#           results/tables/FIG_h8_change_by_baseline_risk.csv
#
# FIGURE MAP (manuscript numbering, in Results order)
#   MAIN TEXT
#   Fig 1  Participant flow                                   (09 F1, wording fixed)
#   Fig 2  Primary outcomes: (A) distributions by period;
#          (B) within-participant change                      (09 F2 + new B)
#   Fig 3  H8: within-participant change by baseline risk
#          level (low vs moderate)                            (NEW)
#   Fig 4  Interrupted time series, deposits: (A) monthly
#          medians, bootstrap 95% CIs, regulatory and launch
#          dates; (B) registered model: observed means, fitted
#          segments, projected baseline, 95% credible bands   (09 F10 + NEW)
#   Fig 5  Dominant risk level, baseline to intervention      (09 F5, moved to main)
#   SUPPORTING INFORMATION
#   Fig S1 Monthly medians, all four outcomes, 95% CIs         (09 F3 + CIs)
#   Fig S2 Interrupted time-series model, all four outcomes    (NEW)
#   Fig S3 Posterior densities, level and slope change         (09 F8, labels fixed)
#   Fig S4 Monthly risk-level distribution                     (09 F4, labels fixed)
#   Fig S5 Monthly Markov transition matrices                  (09 F6, labels fixed)
#   Fig S6 Limit modifications during the intervention         (09 F7, labels fixed)
#   Fig S7 Monthly low-risk escalation, December 2024          (09 F9, labels fixed)
#   Fig S8 Active intervention months per participant (FA1-2)  (NEW)
#   Fig S9 Change per active gambling day, engaged cohort      (NEW)
#   Fig S10 ITS sensitivity analyses, forest plot              (NEW, from saved 07 fits)
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

source("asb_style.R")

# Always re-render: save_figure_all() skips existing files when OVERWRITE is FALSE.
OVERWRITE <- TRUE

# Locked real data only. This script is never run on synthetic data.
dat <- readRDS(FILE_POC_LOCKED)
pp  <- dat$player_period
pm  <- dat$player_month
cat("Running on LOCKED real data (read-only)\n")

its_res    <- tryCatch(readRDS("results/07_its/its_results.rds"),       error = function(e) NULL)
markov_res <- tryCatch(readRDS("results/08_markov/markov_results.rds"), error = function(e) NULL)

dir.create("results/tables",  recursive = TRUE, showWarnings = FALSE)
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)

set.seed(20261119)          # bootstrap reproducibility
B_BOOT <- 2000              # bootstrap resamples per month and outcome

asb_txt <- function(pt, slides) (pt / .pt) * (if (slides) 1.7 else 1)

# Pseudo-log y scale with powers-of-ten breaks and nothing between zero and
# min_p, so the ticks near zero do not collide (review fix, 19 Nov). sigma
# sets the linear zone around zero; use a small sigma for small-valued data.
ms_pseudolog <- function(name, sigma = 1, min_p = 10) {
  brk <- function(lims) {
    m   <- max(abs(lims), min_p)
    top <- ceiling(log10(m))
    p   <- 10^(seq(floor(log10(min_p)), top))
    p   <- p[p >= max(min_p, 10^(top - 4))]
    b   <- c(-rev(p), 0, p)
    b[b >= lims[1] & b <= lims[2]]
  }
  scale_y_continuous(name = name,
                     trans = scales::pseudo_log_trans(sigma = sigma),
                     breaks = brk, labels = scales::label_comma(accuracy = min(1, min_p)))
}
rot_x <- theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))

# =============================================================================
# SHARED DATA AND CHECKS
# =============================================================================

pp_wide <- pp |>
  pivot_wider(id_cols = c(participant_id, engaged_cohort, young_adult_flag),
              names_from = period,
              values_from = c(total_deposits, total_losses, total_wagers,
                              play_hours_total, agd_count, dominant_risk))
stopifnot("pp_wide must be one row per participant" =
            nrow(pp_wide) == dplyr::n_distinct(pp$participant_id))

N_full <- n_distinct(pp$participant_id)

# Within-participant change, intervention minus baseline (negative = reduction).
outcome_vars <- c("total_deposits", "total_losses", "total_wagers", "play_hours_total")

change_long <- pp_wide |>
  transmute(
    participant_id,
    baseline_risk  = dominant_risk_baseline,
    total_deposits   = total_deposits_intervention   - total_deposits_baseline,
    total_losses     = total_losses_intervention     - total_losses_baseline,
    total_wagers     = total_wagers_intervention     - total_wagers_baseline,
    play_hours_total = play_hours_total_intervention - play_hours_total_baseline
  ) |>
  pivot_longer(all_of(outcome_vars), names_to = "outcome", values_to = "change") |>
  mutate(outcome = factor(ASB_OUTCOME_LABELS[outcome], levels = ASB_OUTCOME_ORDER))

# Bootstrap percentile 95% CI for a median (participants resampled).
boot_median_ci <- function(x, B = B_BOOT) {
  x <- x[!is.na(x)]
  n <- length(x)
  meds <- vapply(seq_len(B), function(i) median(x[sample.int(n, n, replace = TRUE)]),
                 numeric(1))
  c(mdn = median(x), lo = unname(quantile(meds, 0.025)),
    hi = unname(quantile(meds, 0.975)), n = n)
}

# Monthly medians with bootstrap CIs, all four outcomes (Fig 2 and Fig S1).
cat("\nBootstrapping monthly medians (", B_BOOT, " resamples)...\n", sep = "")
monthly_ci <- pm |>
  select(year_month, all_of(outcome_vars)) |>
  pivot_longer(all_of(outcome_vars), names_to = "outcome", values_to = "value") |>
  group_by(year_month, outcome) |>
  summarise(ci = list(boot_median_ci(value)), .groups = "drop") |>
  mutate(mdn = vapply(ci, `[[`, numeric(1), "mdn"),
         lo  = vapply(ci, `[[`, numeric(1), "lo"),
         hi  = vapply(ci, `[[`, numeric(1), "hi"),
         n   = vapply(ci, `[[`, numeric(1), "n")) |>
  select(-ci) |>
  mutate(outcome_label = factor(ASB_OUTCOME_LABELS[outcome], levels = ASB_OUTCOME_ORDER),
         period = ifelse(year_month < INTERVENTION_START, "Baseline", "Intervention"))

write.csv(monthly_ci |> mutate(month = format(year_month, "%b %Y")) |>
            select(month, outcome, n, mdn, lo, hi),
          "results/tables/FIG_monthly_medians_bootstrap_ci.csv", row.names = FALSE)
cat("  Saved: FIG_monthly_medians_bootstrap_ci.csv (check against F10 values)\n")

# =============================================================================
# FIGURE 1 — Participant flow
# =============================================================================

cat("\nBuilding Fig 1 (participant flow)...\n")

sf        <- dat$sample_flow
n_sel     <- if (!is.null(sf$n_selected))        sf$n_selected        else N_SELECTED
n_analyt  <- if (!is.null(sf$n_analytical))      sf$n_analytical      else N_EXPECTED_ANALYTIC
n_excl    <- if (!is.null(sf$n_below_threshold)) sf$n_below_threshold else (n_sel - n_analyt)
n_engaged <- if (!is.null(sf$n_engaged))         sf$n_engaged         else N_EXPECTED_ENGAGED
n_no_int  <- n_analyt - n_engaged

flow_data <- data.frame(
  label = c(
    sprintf("Selected at launch\nn = %s", format(n_sel, big.mark = ",")),
    sprintf("Excluded: fewer than 15\nbaseline AGDs\nn = %s", format(n_excl, big.mark = ",")),
    sprintf("Analytical sample\nN = %s", format(n_analyt, big.mark = ",")),
    sprintf("No intervention-period AGDs\nn = %s\n(retained with zero values)", format(n_no_int, big.mark = ",")),
    sprintf("Engaged cohort\nn = %s", format(n_engaged, big.mark = ","))
  ),
  x  = c(0.25, 0.75, 0.25, 0.75, 0.25),
  y  = c(0.88, 0.73, 0.58, 0.43, 0.28),
  bx = c(0.20, 0.235, 0.20, 0.235, 0.20),
  bh = c(0.06, 0.075, 0.06, 0.075, 0.06)
)

make_Fig1 <- function(slides = FALSE) {
  ggplot() +
    geom_rect(data = flow_data,
              aes(xmin = x - bx, xmax = x + bx, ymin = y - bh, ymax = y + bh),
              fill = "white", colour = "grey30", linewidth = 0.4) +
    geom_text(data = flow_data, aes(x = x, y = y, label = label),
              size = asb_txt(7.5, slides), lineheight = 1.1,
              family = "sans", colour = "grey5") +
    annotate("segment", x = 0.25, xend = 0.25, y = 0.82, yend = 0.64,
             arrow = arrow(length = unit(0.14, "cm"), type = "closed"),
             linewidth = 0.35, colour = "grey30") +
    annotate("segment", x = 0.25, xend = 0.25, y = 0.52, yend = 0.34,
             arrow = arrow(length = unit(0.14, "cm"), type = "closed"),
             linewidth = 0.35, colour = "grey30") +
    annotate("segment", x = 0.25, xend = 0.515, y = 0.73, yend = 0.73,
             arrow = arrow(length = unit(0.12, "cm"), type = "closed"),
             linewidth = 0.3, colour = "grey40") +
    annotate("segment", x = 0.25, xend = 0.515, y = 0.43, yend = 0.43,
             arrow = arrow(length = unit(0.12, "cm"), type = "closed"),
             linewidth = 0.3, colour = "grey40") +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0.2, 0.96)) +
    theme_void(base_family = "sans") +
    theme(plot.margin = margin(6, 6, 6, 6),
          plot.title = element_text(face = "bold", size = if (slides) 20 else 11)) +
    asb_titles(slides, "Participant flow")
}

save_figure_all(make_Fig1, "Fig1_participant_flow", 130, 100)
rm(flow_data)

# =============================================================================
# FIGURE 4, PANEL A — Monthly median deposits, bootstrap 95% CIs, timeline
# =============================================================================

cat("\nBuilding Fig 2 (monthly deposits with 95% CIs)...\n")

reg_dates <- data.frame(
  date   = as.Date(c("2024-08-30", "2024-09-01", "2024-10-01", "2025-02-28")),
  label  = c("Financial vulnerability\nchecks (GBP 500)",
             "Operator\nimplementation",
             "ASB launch",
             "Threshold lowered\nto GBP 150"),
  kind   = c("Regulatory (public date)", "Regulatory (operator-verified)",
             "Intervention", "Regulatory (public date)"),
  y_frac = c(1.16, 0.98, 1.16, 1.16),
  hjust  = c(1,    0,    0,    1),
  nudge  = c(-6,   6,    6,    -6)
)

dep_ci <- monthly_ci |> filter(outcome == "total_deposits")

make_Fig2 <- function(slides = FALSE) {
  y_max <- max(dep_ci$hi)
  ggplot(dep_ci, aes(x = year_month, y = mdn)) +
    geom_vline(data = reg_dates,
               aes(xintercept = as.numeric(date), linetype = kind, colour = kind),
               linewidth = 0.45) +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 9,
                  linewidth = 0.4, colour = ASB_OUTCOME_DARK[["Deposits (GBP)"]]) +
    geom_line(linewidth = if (slides) 1 else 0.6,
              colour = ASB_OUTCOME_DARK[["Deposits (GBP)"]]) +
    geom_point(size = if (slides) 2.6 else 1.7,
               colour = ASB_OUTCOME_DARK[["Deposits (GBP)"]]) +
    geom_text(data = reg_dates,
              aes(x = date + nudge, y = y_max * y_frac, label = label,
                  hjust = hjust, colour = kind),
              size = asb_txt(7, slides), lineheight = 1.05, vjust = 1,
              show.legend = FALSE) +
    scale_linetype_manual(values = c("Intervention"                   = "solid",
                                     "Regulatory (operator-verified)" = "22",
                                     "Regulatory (public date)"       = "dotted"),
                          name = NULL) +
    scale_colour_manual(values = c("Intervention"                   = ASB_INTERVENTION,
                                   "Regulatory (operator-verified)" = "#4F6D8C",
                                   "Regulatory (public date)"       = "grey45"),
                        name = NULL) +
    scale_y_continuous(labels = scales::label_comma(),
                       limits = c(0, y_max * 1.22),
                       expand = expansion(mult = c(0, 0))) +
    labs(x = NULL, y = "Median monthly deposits (GBP)") +
    asb_scale_month("1 month") +
    theme_asb(slides = slides) +
    theme(legend.position = "bottom",
          legend.key.width = unit(1.3, "cm")) +
    asb_titles(slides, "Monthly median deposits with 95% intervals",
               "Bootstrap 95% confidence intervals. Regulatory and intervention dates marked.")
}

# Not saved on its own: this is panel A of Figure 4 (built in the ITS section).

# =============================================================================
# FIGURE 2 — Primary outcomes: (A) distributions by period; (B) change
# =============================================================================

cat("\nBuilding Fig 3 (distributions and within-participant change)...\n")

pp_long <- pp |>
  select(participant_id, period, all_of(outcome_vars)) |>
  pivot_longer(all_of(outcome_vars), names_to = "outcome", values_to = "value") |>
  mutate(outcome = factor(ASB_OUTCOME_LABELS[outcome], levels = ASB_OUTCOME_ORDER),
         period  = factor(period, levels = c("baseline", "intervention"),
                          labels = c("Baseline", "Intervention")))

make_Fig3 <- function(slides = FALSE) {
  pA <- ggplot(pp_long, aes(x = period, y = value,
                            fill = interaction(outcome, period))) +
    geom_violin(linewidth = 0.3, trim = TRUE, colour = "grey30") +
    geom_boxplot(width = 0.12, outlier.shape = NA, linewidth = 0.3,
                 fill = "white", colour = "grey20") +
    stat_summary(fun = median, geom = "point",
                 size = if (slides) 2.2 else 1.3, colour = "grey15") +
    scale_fill_manual(values = ASB_OUTCOME_PERIOD, guide = "none") +
    ms_pseudolog(name = "Period total (pseudo-log scale)") +
    facet_wrap(~ outcome, scales = "free_y", nrow = 1) +
    labs(x = NULL) +
    theme_asb(slides = slides) + rot_x

  pB <- ggplot(change_long, aes(x = outcome, y = change, fill = outcome)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40",
               linewidth = 0.4) +
    geom_violin(linewidth = 0.3, trim = TRUE, colour = "grey30") +
    geom_boxplot(width = 0.12, outlier.shape = NA, linewidth = 0.3,
                 fill = "white", colour = "grey20") +
    stat_summary(fun = median, geom = "point",
                 size = if (slides) 2.2 else 1.3, colour = "grey15") +
    scale_fill_manual(values = ASB_OUTCOME_DARK, guide = "none") +
    ms_pseudolog(name = "Change, intervention minus\nbaseline (pseudo-log scale)") +
    facet_wrap(~ outcome, scales = "free", nrow = 1) +
    labs(x = NULL) +
    theme_asb(slides = slides) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

  ann <- if (slides) {
    plot_annotation(
      title = "Primary outcomes: baseline, intervention and within-participant change",
      subtitle = sprintf("Full cohort (N = %s). Values below zero in B are reductions.",
                         format(N_full, big.mark = ",")),
      tag_levels = "A",
      theme = theme(plot.title = element_text(face = "bold", size = 20),
                    plot.subtitle = element_text(colour = "grey35", size = 15)))
  } else {
    plot_annotation(tag_levels = "A")
  }
  (pA / pB) + ann
}

save_figure_all(make_Fig3, "Fig2_primary_outcomes_distribution_change", MS_2COL, 150)
rm(pp_long)

# =============================================================================
# FIGURE 3 — H8: within-participant change by baseline risk level
# Low vs moderate baseline dominant risk level; the two high-risk participants
# are excluded, as in the registered H8 analysis.
# =============================================================================

cat("\nBuilding Fig 4 (H8 change by baseline risk level)...\n")

h8_df <- change_long |>
  filter(baseline_risk %in% c("low", "medium")) |>
  mutate(baseline_risk = factor(ASB_RISK_LABELS[as.character(baseline_risk)],
                                levels = c("Low", "Moderate")))

h8_n <- h8_df |> distinct(participant_id, baseline_risk) |> count(baseline_risk)
print(h8_n)

h8_check <- h8_df |>
  group_by(outcome, baseline_risk) |>
  summarise(n = n(), median_change = median(change, na.rm = TRUE),
            q1 = quantile(change, 0.25, na.rm = TRUE),
            q3 = quantile(change, 0.75, na.rm = TRUE), .groups = "drop")
write.csv(h8_check, "results/tables/FIG_h8_change_by_baseline_risk.csv", row.names = FALSE)
cat("  Saved: FIG_h8_change_by_baseline_risk.csv (check against Table 7)\n")

h8_labels <- setNames(
  sprintf("%s\n(n = %s)", h8_n$baseline_risk, format(h8_n$n, big.mark = ",")),
  as.character(h8_n$baseline_risk))

make_Fig4 <- function(slides = FALSE) {
  ggplot(h8_df, aes(x = baseline_risk, y = change, fill = baseline_risk)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40",
               linewidth = 0.4) +
    geom_violin(linewidth = 0.3, trim = TRUE, colour = "grey30") +
    geom_boxplot(width = 0.12, outlier.shape = NA, linewidth = 0.3,
                 fill = "white", colour = "grey20") +
    stat_summary(fun = median, geom = "point",
                 size = if (slides) 2.2 else 1.3, colour = "grey15") +
    scale_fill_manual(values = ASB_RISK[c("Low", "Moderate")], guide = "none") +
    ms_pseudolog(name = "Change, intervention minus\nbaseline (pseudo-log scale)") +
    facet_wrap(~ outcome, scales = "free_y", nrow = 1) +
    labs(x = "Baseline risk level") +
    theme_asb(slides = slides) + rot_x +
    asb_titles(slides, "Within-participant change by baseline risk level (H8)",
               "Values below zero are reductions. Dashed line = no change.")
}

save_figure_all(make_Fig4, "Fig3_h8_change_by_baseline_risk", MS_2COL, 85)

# =============================================================================
# FIGURE 4 — Interrupted time series for deposits
#   (A) monthly median deposits with bootstrap 95% CIs and dates (above);
#   (B) the registered segmented model on its own scale: observed monthly
#       means of the transformed outcome with 95% CIs, the fitted baseline
#       and intervention segments, and the baseline segment projected into
#       the intervention period (the counterfactual), each with 95% credible
#       bands from the companion Bayesian model.
# FIGURE S2 — the same model panel for all four outcomes.
#
# Nothing is re-estimated: fitted lines come from the posterior draws of the
# saved brms models in its_results.rds. Coding as registered (prereg 6.4):
# time = month 1-12; intervention = 1 from October 2024; time since onset =
# 0 before, 1-6 October to March. The fixed effects are matched by name:
# b_Intercept, the coefficient containing "intervention" (beta_2), the one
# containing "post" (beta_3), and the remaining one (beta_1). The script
# stops with the names it found if that matching is ambiguous.
# Population-level lines are drawn (random intercepts at zero), so they sit
# on the scale of the monthly mean of the transformed outcome.
# =============================================================================

cat("\nBuilding Fig 4 and Fig S2 (interrupted time-series fits)...\n")

its_vars <- list(
  log1p_deposits   = list(col = "total_deposits",   fwd = log1p, inv = expm1,
                          label = "Deposits (GBP)", scale = "log(1 + deposits)"),
  asinh_losses     = list(col = "total_losses",     fwd = asinh, inv = sinh,
                          label = "Net loss (GBP)", scale = "asinh(net loss)"),
  log1p_wagers     = list(col = "total_wagers",     fwd = log1p, inv = expm1,
                          label = "Wagers (GBP)",   scale = "log(1 + wagers)"),
  log1p_play_hours = list(col = "play_hours_total", fwd = log1p, inv = expm1,
                          label = "Play hours",     scale = "log(1 + hours)")
)

REG_START_FIG <- as.Date("2024-09-01")   # operator implementation (07d step)

month_key <- pm |>
  distinct(year_month, month_number) |>
  arrange(month_number) |>
  mutate(intervention = as.integer(month_number >= 7),
         time_post    = pmax(month_number - 6, 0))
stopifnot("Expected 12 study months" = nrow(month_key) == 12,
          "October 2024 must be month 7" =
            month_key$year_month[month_key$month_number == 7] == INTERVENTION_START)

# Exploratory two-interruption model (07d, post-registration): if its
# saved results exist, the panel also shows its projection (baseline trend
# plus the September 2024 regulatory step, without ASB). Drawn from a
# multivariate normal on the saved fixed effects and covariance (nlme fit;
# 4,000 draws) for the 95% band. Labelled exploratory in the legend text.
EXPL_07D <- Sys.getenv("EXPL_07D_RESULTS", unset = "EXPL_outputs/EXPL_07d_results.rds")
expl07d <- if (file.exists(EXPL_07D)) readRDS(EXPL_07D) else NULL
if (is.null(expl07d)) cat("  EXPL_07d results not found at", EXPL_07D,
                          "- Fig 4B/S2 drawn without the September-step projection\n")

its_draws <- function(entry) {
  if (is.null(entry) || is.null(entry$model_file) || !file.exists(entry$model_file) ||
      !requireNamespace("brms", quietly = TRUE)) return(NULL)
  fit  <- readRDS(entry$model_file)
  post <- as.data.frame(brms::as_draws_df(fit))
  rm(fit); invisible(gc(verbose = FALSE))
  b <- grep("^b_", names(post), value = TRUE)
  b0 <- "b_Intercept"
  b2 <- grep("intervention", b, value = TRUE)
  b3 <- grep("post", b, value = TRUE)
  b1 <- setdiff(b, c(b0, b2, b3))
  if (!(b0 %in% b) || length(b1) != 1 || length(b2) != 1 || length(b3) != 1)
    stop("Cannot match ITS fixed effects unambiguously. Found: ",
         paste(b, collapse = ", "))
  list(b0 = post[[b0]], b1 = post[[b1]], b2 = post[[b2]], b3 = post[[b3]])
}

its_panel_data <- function(key) {
  spec <- its_vars[[key]]
  d <- its_draws(its_res$bayes[[key]])
  if (is.null(d)) return(NULL)
  q <- function(m) apply(m, 2, quantile, probs = c(0.5, 0.025, 0.975))
  t  <- month_key$month_number; I <- month_key$intervention; tp <- month_key$time_post
  fitted <- sapply(seq_along(t), function(j) d$b0 + d$b1 * t[j] + d$b2 * I[j] + d$b3 * tp[j])
  cf     <- sapply(seq_along(t), function(j) d$b0 + d$b1 * t[j])
  fq <- q(fitted); cq <- q(cf)
  lines <- bind_rows(
    data.frame(year_month = month_key$year_month, segment = ifelse(I == 1, "Fitted, intervention", "Fitted, baseline"),
               est = fq[1, ], lo = fq[2, ], hi = fq[3, ]),
    data.frame(year_month = month_key$year_month, segment = "Projected baseline",
               est = cq[1, ], lo = cq[2, ], hi = cq[3, ]) |>
      filter(year_month >= INTERVENTION_START - months(1))
  )
  fit2 <- if (!is.null(expl07d)) expl07d$fits[[paste(key, "primary", sep = "|")]] else NULL
  if (!is.null(fit2)) {
    cf <- fit2$fixef; V <- fit2$vcov
    dr <- MASS::mvrnorm(4000, mu = cf, Sigma = V)
    reg <- as.integer(t >= 6)
    cf2 <- sapply(seq_along(t), function(j)
      dr[, "(Intercept)"] + dr[, "time_t"] * t[j] + dr[, "reg_t"] * reg[j])
    c2 <- q(cf2)
    lines <- bind_rows(lines,
      data.frame(year_month = month_key$year_month, segment = "Projected, with September step",
                 est = c2[1, ], lo = c2[2, ], hi = c2[3, ]) |>
        filter(year_month >= REG_START_FIG))
  }
  obs <- pm |>
    transmute(year_month, y = spec$fwd(.data[[spec$col]])) |>
    group_by(year_month) |>
    summarise(mean = mean(y, na.rm = TRUE),
              se   = sd(y, na.rm = TRUE) / sqrt(sum(!is.na(y))), .groups = "drop") |>
    mutate(lo = mean - 1.96 * se, hi = mean + 1.96 * se)
  list(lines = lines, obs = obs, spec = spec)
}

make_its_panel <- function(pd, slides = FALSE, show_legend = TRUE) {
  seg_cols <- c("Fitted, baseline" = "grey10", "Fitted, intervention" = "grey10",
                "Projected baseline" = "grey45",
                "Projected, with September step" = "#4F6D8C")
  seg_lty  <- c("Fitted, baseline" = "solid", "Fitted, intervention" = "solid",
                "Projected baseline" = "dashed",
                "Projected, with September step" = "dotdash")
  seg_brk  <- intersect(c("Fitted, baseline", "Projected baseline", "Projected, with September step"),
                        unique(pd$lines$segment))
  seg_lab  <- c("Fitted, baseline" = "Fitted model (registered)",
                "Projected baseline" = "Projected baseline trend (registered)",
                "Projected, with September step" = "Projected, with September step (exploratory)")[seg_brk]
  ggplot() +
    geom_ribbon(data = pd$lines,
                aes(x = year_month, ymin = lo, ymax = hi, group = segment),
                fill = "grey70", alpha = 0.35) +
    geom_vline(xintercept = as.numeric(INTERVENTION_START),
               colour = ASB_INTERVENTION, linewidth = 0.45) +
    geom_line(data = pd$lines,
              aes(x = year_month, y = est, colour = segment, linetype = segment,
                  group = segment), linewidth = if (slides) 1 else 0.55) +
    geom_errorbar(data = pd$obs, aes(x = year_month, ymin = lo, ymax = hi),
                  width = 9, linewidth = 0.35, colour = "grey20") +
    geom_point(data = pd$obs, aes(x = year_month, y = mean),
               size = if (slides) 2.4 else 1.4, colour = "grey10") +
    scale_colour_manual(values = seg_cols, name = NULL, breaks = seg_brk, labels = unname(seg_lab)) +
    scale_linetype_manual(values = seg_lty, name = NULL, breaks = seg_brk, labels = unname(seg_lab)) +
    guides(colour = guide_legend(ncol = 1), linetype = guide_legend(ncol = 1)) +
    scale_y_continuous(name = paste0("Monthly mean of ", pd$spec$scale)) +
    labs(x = NULL) +
    asb_scale_month("2 months") +
    theme_asb(slides = slides) +
    theme(legend.position = if (show_legend) "bottom" else "none")
}

if (!is.null(its_res) && !is.null(its_res$bayes)) {
  its_pd <- lapply(names(its_vars), function(k) {
    out <- its_panel_data(k)
    if (is.null(out)) cat("  ITS fit: no saved brms model for", k, "- skipped\n")
    out
  })
  names(its_pd) <- names(its_vars)

  if (!is.null(its_pd$log1p_deposits)) {
    make_Fig4 <- function(slides = FALSE) {
      (make_Fig2(slides) / make_its_panel(its_pd$log1p_deposits, slides)) +
        plot_annotation(tag_levels = "A")
    }
    save_figure_all(make_Fig4, "Fig4_its_deposits_medians_and_model", MS_2COL, 175)
  }

  its_ok <- Filter(Negate(is.null), its_pd)
  if (length(its_ok) > 0) {
    make_FigS2 <- function(slides = FALSE) {
      panels <- lapply(seq_along(its_ok), function(i)
        make_its_panel(its_ok[[i]], slides, show_legend = (i == length(its_ok))) +
          ggtitle(sprintf("%s: %s", sub(" \\(GBP\\)", "", its_ok[[i]]$spec$label),
                          its_ok[[i]]$spec$scale)) +
          labs(y = "Monthly mean (95% CI)") +
          theme(plot.title = element_text(face = "plain", size = if (slides) 15 else 9),
                plot.title.position = "panel"))
      wrap_plots(panels, ncol = 2) + plot_layout(guides = "collect") &
        theme(legend.position = "bottom")
    }
    save_figure_all(make_FigS2, "FigS2_its_model_fit_all_outcomes", MS_2COL, 150)
  }
  rm(its_pd, its_ok); invisible(gc(verbose = FALSE))
} else {
  cat("  Fig 4 and Fig S2 skipped: results/07_its/its_results.rds not available\n")
}

# =============================================================================
# FIGURE S1 — Monthly medians, all four outcomes, with bootstrap 95% CIs
# =============================================================================

cat("\nBuilding Fig S1 (monthly medians, all outcomes, 95% CIs)...\n")

make_FigS1 <- function(slides = FALSE) {
  ggplot(monthly_ci, aes(x = year_month, y = mdn,
                         colour = interaction(outcome_label, period))) +
    geom_vline(xintercept = as.numeric(INTERVENTION_START),
               linetype = "dashed", colour = "grey45", linewidth = 0.4) +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 9, linewidth = 0.35) +
    geom_line(aes(group = 1), linewidth = if (slides) 1 else 0.55) +
    geom_point(size = if (slides) 2.6 else 1.5) +
    scale_colour_manual(values = ASB_OUTCOME_PERIOD, guide = "none") +
    facet_wrap(~ outcome_label, scales = "free_y", nrow = 2) +
    labs(x = NULL, y = "Monthly median (95% CI)") +
    asb_scale_month("2 months") +
    theme_asb(slides = slides) +
    asb_titles(slides, "Monthly medians with 95% intervals, all primary outcomes",
               "Dashed line = A Safe Bet launch (October 2024). Lighter = baseline, darker = intervention.")
}

save_figure_all(make_FigS1, "FigS1_monthly_medians_ci_all_outcomes", MS_2COL, 110)

# =============================================================================
# FIGURE S3 — Posterior densities of the level and slope changes
# Draw-loading logic unchanged from 09 F8.
# =============================================================================

cat("\nBuilding Fig S2 (posterior densities)...\n")

if (!is.null(its_res) && !is.null(its_res$bayes)) {
  s2_outcome_labels <- c(log1p_deposits   = "Deposits (log1p)",
                         asinh_losses     = "Net loss (asinh)",
                         log1p_wagers     = "Wagers (log1p)",
                         log1p_play_hours = "Play hours (log1p)")
  s2_order <- unname(s2_outcome_labels[
    c("log1p_deposits", "asinh_losses", "log1p_wagers", "log1p_play_hours")])

  s2_get_draws <- function(entry) {
    if (is.null(entry)) return(NULL)
    if (!is.null(entry$b2) && !is.null(entry$b3))
      return(list(b2 = as.numeric(entry$b2), b3 = as.numeric(entry$b3)))
    if (!is.null(entry$model_file) && file.exists(entry$model_file) &&
        requireNamespace("brms", quietly = TRUE)) {
      fit  <- readRDS(entry$model_file)
      post <- brms::as_draws_df(fit, variable = c("b_intervention_t", "b_time_post_t"))
      out  <- list(b2 = post$b_intervention_t, b3 = post$b_time_post_t)
      rm(fit, post); invisible(gc(verbose = FALSE))
      return(out)
    }
    NULL
  }

  s2_df <- dplyr::bind_rows(lapply(names(its_res$bayes), function(v) {
    d <- s2_get_draws(its_res$bayes[[v]])
    if (is.null(d)) { cat("  Fig S2: no draws for", v, "- panel skipped\n"); return(NULL) }
    data.frame(outcome   = if (v %in% names(s2_outcome_labels)) s2_outcome_labels[[v]] else v,
               parameter = rep(c("Level change", "Slope change"),
                               times = c(length(d$b2), length(d$b3))),
               value     = c(d$b2, d$b3))
  }))

  if (!is.null(s2_df) && nrow(s2_df) > 0) {
    s2_df$outcome <- factor(s2_df$outcome,
                            levels = intersect(s2_order, unique(s2_df$outcome)))
    make_FigS2 <- function(slides = FALSE) {
      ggplot(s2_df, aes(x = value, fill = parameter)) +
        geom_density(linewidth = 0.3, colour = "grey30", alpha = 0.85) +
        geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40",
                   linewidth = 0.4) +
        scale_fill_manual(values = c("Level change" = "grey25",
                                     "Slope change" = "grey70"),
                          name = NULL) +
        facet_wrap(~ outcome, scales = "free", nrow = 2) +
        labs(x = "Coefficient (transformed outcome scale)",
             y = "Posterior density") +
        theme_asb(slides = slides) +
        theme(legend.position = "bottom") +
        asb_titles(slides, "Posterior densities of the level and slope changes",
                   "Dashed line = 0.")
    }
    save_figure_all(make_FigS2, "FigS3_posterior_level_slope", MS_2COL, 110)
  }
  rm(s2_df); invisible(gc(verbose = FALSE))
} else {
  cat("  Fig S2 skipped: results/07_its/its_results.rds not available\n")
}

# =============================================================================
# FIGURE S4 — Monthly risk-level distribution
# =============================================================================

cat("\nBuilding Fig S3 (monthly risk-level distribution)...\n")

risk_dist <- pm |>
  filter(!is.na(markov_state)) |>
  group_by(year_month) |>
  summarise(L = mean(markov_state == "L") * 100,
            M = mean(markov_state == "M") * 100,
            H = mean(markov_state == "H") * 100,
            I = mean(markov_state == "I") * 100, .groups = "drop") |>
  pivot_longer(c(L, M, H, I), names_to = "state", values_to = "pct") |>
  mutate(state = factor(state, levels = c("H", "M", "L", "I"),
                        labels = c("High", "Moderate", "Low", "Inactive")))

make_FigS3 <- function(slides = FALSE) {
  ggplot(risk_dist, aes(x = year_month, y = pct, fill = state)) +
    geom_col(width = 25, colour = "white", linewidth = 0.2) +
    geom_vline(xintercept = as.numeric(INTERVENTION_START),
               linetype = "dashed", colour = "grey20", linewidth = 0.4) +
    scale_fill_manual(values = ASB_RISK, name = "Risk level") +
    scale_y_continuous(labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, 0.02))) +
    labs(x = NULL, y = "Participants (%)") +
    asb_scale_month("2 months") +
    theme_asb(slides = slides) +
    asb_titles(slides, "Monthly risk-level distribution",
               "Dashed line = A Safe Bet launch (October 2024)")
}

save_figure_all(make_FigS3, "FigS4_monthly_risk_distribution", MS_2COL, 85)
rm(risk_dist)

# =============================================================================
# FIGURE 5 — Dominant risk level, baseline to intervention (main text)
# =============================================================================

cat("\nBuilding Fig S4 (dominant risk level transitions)...\n")

risk_trans_df <- pp_wide |>
  filter(!is.na(dominant_risk_baseline), !is.na(dominant_risk_intervention)) |>
  count(dominant_risk_baseline, dominant_risk_intervention) |>
  group_by(dominant_risk_baseline) |>
  mutate(pct = n / sum(n) * 100) |>
  ungroup() |>
  mutate(From = factor(ASB_RISK_LABELS[as.character(dominant_risk_baseline)],
                       levels = c("Low", "Moderate", "High")),
         To   = factor(ASB_RISK_LABELS[as.character(dominant_risk_intervention)],
                       levels = c("Low", "Moderate", "High")))

make_FigS4 <- function(slides = FALSE) {
  ggplot(risk_trans_df, aes(x = To, y = From, fill = pct)) +
    geom_tile(colour = "white", linewidth = 0.6) +
    geom_text(aes(label = sprintf("%.1f%%\n(n = %s)", pct, format(n, big.mark = ",", trim = TRUE)),
                  colour = pct > 50),
              size = asb_txt(8, slides), lineheight = 1.1) +
    scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey10"), guide = "none") +
    asb_scale_heat("Row %", limits = c(0, 100), labels = function(x) paste0(x, "%")) +
    guides(fill = "none") +
    scale_x_discrete(position = "top") +
    scale_y_discrete(limits = rev) +
    labs(x = "Dominant risk level, intervention", y = "Dominant risk level, baseline") +
    theme_asb_heatmap(slides = slides) +
    asb_titles(slides, "Dominant risk level: baseline to intervention",
               "Row percentages; participants classified in both periods")
}

save_figure_all(make_FigS4, "Fig5_dominant_risk_transitions", 110, 90)
rm(risk_trans_df)

# =============================================================================
# FIGURE S5 — Monthly Markov transition matrices
# =============================================================================

cat("\nBuilding Fig S5 (Markov transition matrices)...\n")

if (!is.null(markov_res)) {
  states <- c("L", "M", "H", "I")
  state_lab <- c(L = "Low", M = "Moderate", H = "High", I = "Inactive")

  make_markov_panel <- function(prob_mat, panel_title, slides) {
    df <- expand.grid(From = states, To = states) |>
      mutate(prob = as.vector(prob_mat[cbind(as.character(From), as.character(To))]),
             From = factor(state_lab[as.character(From)], levels = state_lab),
             To   = factor(state_lab[as.character(To)],   levels = state_lab))
    ggplot(df, aes(x = To, y = From, fill = prob)) +
      geom_tile(colour = "white", linewidth = 0.5) +
      geom_text(aes(label = sprintf("%.2f", prob), colour = prob > 0.5),
                size = asb_txt(7.5, slides)) +
      scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey10"), guide = "none") +
      asb_scale_heat("Probability", limits = c(0, 1)) +
      scale_x_discrete(position = "top") +
      scale_y_discrete(limits = rev) +
      labs(title = panel_title, x = "Next month", y = "Current month") +
      theme_asb_heatmap(slides = slides) +
      theme(plot.title = element_text(face = "plain", colour = "grey10",
                                      size = if (slides) 16 else 9.5))
  }

  make_FigS5 <- function(slides = FALSE) {
    (make_markov_panel(markov_res$prob_baseline,     "Baseline",     slides) +
     make_markov_panel(markov_res$prob_intervention, "Intervention", slides)) +
      plot_layout(guides = "collect") &
      theme(legend.position = "right")
  }

  save_figure_all(make_FigS5, "FigS5_markov_transition_matrices", MS_2COL, 85)
}

# =============================================================================
# FIGURE S6 — Limit modifications during the intervention
# =============================================================================

cat("\nBuilding Fig S6 (limit modifications)...\n")

engagement_monthly <- pm |>
  filter(is_intervention == 1) |>
  group_by(year_month) |>
  summarise(pct_with_intent = mean(n_limit_intents > 0, na.rm = TRUE) * 100,
            mean_intents    = mean(n_limit_intents, na.rm = TRUE), .groups = "drop")

make_FigS6 <- function(slides = FALSE) {
  panel_theme <- theme(plot.title = element_text(face = "plain", colour = "grey10",
                                                 size = if (slides) 15 else 9))
  pa <- ggplot(engagement_monthly, aes(x = year_month, y = pct_with_intent)) +
    geom_line(colour = ASB_INTERVENTION, linewidth = if (slides) 1 else 0.6) +
    geom_point(colour = ASB_INTERVENTION, size = if (slides) 2.6 else 1.6) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, NA)) +
    labs(title = "Participants who changed a limit",
         x = NULL, y = "Participants (%)") +
    asb_scale_month("1 month") + theme_asb(slides = slides) + panel_theme
  pb <- ggplot(engagement_monthly, aes(x = year_month, y = mean_intents)) +
    geom_line(colour = ASB_INTERVENTION, linewidth = if (slides) 1 else 0.6) +
    geom_point(colour = ASB_INTERVENTION, size = if (slides) 2.6 else 1.6) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(title = "Limit changes per participant",
         x = NULL, y = "Mean number") +
    asb_scale_month("1 month") + theme_asb(slides = slides) + panel_theme
  (pa / pb) + plot_annotation(tag_levels = "A")
}

save_figure_all(make_FigS6, "FigS6_limit_modifications", 110, 130)
rm(engagement_monthly)

# =============================================================================
# FIGURE S7 — Monthly low-risk escalation and the December 2024 recalibration
# EXPLORATORY. Same construction as 09 (T9/F9), December anchor.
# =============================================================================

cat("\nBuilding Fig S7 (low-risk escalation, December 2024)...\n")

SPIKE_MONTH <- floor_date(BETBUDDY_RECALIB_DATE, "month")
stopifnot("Spike anchor must be December 2024" = SPIKE_MONTH == as.Date("2024-12-01"))

escalation_monthly <- pm |>
  arrange(participant_id, year_month) |>
  group_by(participant_id) |>
  mutate(prev_state = lag(markov_state),
         escalated  = prev_state == "L" & markov_state %in% c("M", "H")) |>
  ungroup() |>
  filter(!is.na(prev_state), prev_state == "L") |>
  group_by(year_month) |>
  summarise(n_low_prev = n(), n_escalated = sum(escalated, na.rm = TRUE),
            pct_escalated = n_escalated / n_low_prev * 100, .groups = "drop") |>
  filter(year_month >= INTERVENTION_START)

s7_expected <- mean(escalation_monthly$pct_escalated[escalation_monthly$year_month != SPIKE_MONTH],
                    na.rm = TRUE)

make_FigS7 <- function(slides = FALSE) {
  ggplot(escalation_monthly, aes(x = year_month, y = pct_escalated,
                                 fill = year_month == SPIKE_MONTH)) +
    geom_col(width = 25) +
    geom_hline(yintercept = s7_expected, linetype = "dashed",
               colour = "grey35", linewidth = 0.45) +
    scale_fill_manual(values = c(`FALSE` = "grey72", `TRUE` = "grey15"), guide = "none") +
    scale_y_continuous(labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, 0.12))) +
    annotate("text", x = SPIKE_MONTH, y = max(escalation_monthly$pct_escalated) * 1.06,
             label = "Recalibration month", size = asb_txt(8, slides),
             colour = "grey10", fontface = "bold", hjust = 0.5) +
    annotate("text", x = max(escalation_monthly$year_month) + 10, y = s7_expected,
             label = "Mean of other months", size = asb_txt(7, slides),
             colour = "grey35", hjust = 1, vjust = -0.7) +
    labs(x = NULL, y = "Escalating from low risk (%)") +
    asb_scale_month("1 month") +
    theme_asb(slides = slides) +
    asb_titles(slides, "Monthly escalation from low risk",
               "Dark bar = December 2024 recalibration month.")
}

save_figure_all(make_FigS7, "FigS7_low_risk_escalation", 110, 85)

# =============================================================================
# FIGURE S8 — Number of active intervention months per participant (FA1, FA2)
# =============================================================================

cat("\nBuilding Fig S8 (active intervention months)...\n")

active_months <- pm |>
  filter(is_intervention == 1) |>
  group_by(participant_id) |>
  summarise(n_active = sum(agd_count > 0), .groups = "drop") |>
  count(n_active) |>
  mutate(pct = n / sum(n) * 100)

print(active_months)

make_FigS8 <- function(slides = FALSE) {
  ggplot(active_months, aes(x = factor(n_active), y = pct)) +
    geom_col(width = 0.7, fill = ASB_INTERVENTION, colour = "grey30", linewidth = 0.25) +
    geom_text(aes(label = sprintf("%s\n(%.1f%%)", format(n, big.mark = ","), pct)),
              vjust = -0.3, size = asb_txt(7, slides), lineheight = 1.05) +
    scale_y_continuous(labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, 0.18))) +
    labs(x = "Active intervention months (of six)",
         y = "Participants (%)") +
    theme_asb(slides = slides) +
    asb_titles(slides, "Active months during the intervention",
               sprintf("N = %s participants", format(sum(active_months$n), big.mark = ",")))
}

save_figure_all(make_FigS8, "FigS8_active_intervention_months", 110, 80)

# =============================================================================
# FIGURE S9 — Change per active gambling day, engaged cohort (secondary)
# Same per-AGD construction as table T3 in 09.
# =============================================================================

cat("\nBuilding Fig S9 (change per AGD, engaged cohort)...\n")

per_agd <- pp_wide |>
  filter(engaged_cohort == TRUE) |>
  transmute(
    participant_id,
    total_deposits   = total_deposits_intervention   / pmax(agd_count_intervention, 1) -
                       total_deposits_baseline       / pmax(agd_count_baseline, 1),
    total_losses     = total_losses_intervention     / pmax(agd_count_intervention, 1) -
                       total_losses_baseline         / pmax(agd_count_baseline, 1),
    total_wagers     = total_wagers_intervention     / pmax(agd_count_intervention, 1) -
                       total_wagers_baseline         / pmax(agd_count_baseline, 1),
    play_hours_total = play_hours_total_intervention / pmax(agd_count_intervention, 1) -
                       play_hours_total_baseline     / pmax(agd_count_baseline, 1)) |>
  pivot_longer(all_of(outcome_vars), names_to = "outcome", values_to = "change") |>
  mutate(outcome = factor(ASB_OUTCOME_LABELS[outcome], levels = ASB_OUTCOME_ORDER))

n_eng <- n_distinct(per_agd$participant_id)
if (n_eng != 5103) warning("Fig S9: engaged cohort is ", n_eng, ", expected 5,103")

make_FigS9 <- function(slides = FALSE) {
  ggplot(per_agd, aes(x = outcome, y = change, fill = outcome)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
    geom_violin(linewidth = 0.3, trim = TRUE, colour = "grey30") +
    geom_boxplot(width = 0.12, outlier.shape = NA, linewidth = 0.3,
                 fill = "white", colour = "grey20") +
    stat_summary(fun = median, geom = "point",
                 size = if (slides) 2.2 else 1.3, colour = "grey15") +
    scale_fill_manual(values = ASB_OUTCOME_DARK, guide = "none") +
    ms_pseudolog(name = "Change per active gambling day\n(pseudo-log scale)",
                 sigma = 0.1, min_p = 1) +
    facet_wrap(~ outcome, scales = "free", nrow = 1) +
    labs(x = NULL) +
    theme_asb(slides = slides) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    asb_titles(slides, "Change per active gambling day, engaged cohort",
               sprintf("n = %s. Values below zero are reductions.", format(n_eng, big.mark = ",")))
}

save_figure_all(make_FigS9, "FigS9_change_per_agd_engaged", MS_2COL, 80)
rm(per_agd, active_months)

# =============================================================================
# FIGURE S10 — Interrupted time-series sensitivity analyses (forest plot)
# Pre-registered sensitivity specifications (prereg Section 9), frequentist
# nlme fits saved by 07_poc_its.R: primary; AR(1); recalibration-adjusted;
# recalibration-adjusted with AR(1). No model is refitted: coefficients and
# covariance matrices are read from the saved fits. 95% CIs use the t
# distribution with the fit's degrees of freedom, as in 07c.
# Shows the level change (beta_2), the slope change (beta_3) and the October
# departure (beta_2 + beta_3, SE from the covariance matrix).
# Same content as Table S3.3 (where reported) in graphical form.
# =============================================================================

cat("\nBuilding Fig S10 (ITS sensitivity forest plot)...\n")

s10_spec <- list(
  "Primary"                        = function(v) its_res$freq[[v]]$model,
  "AR(1)"                          = function(v) its_res$freq[[v]]$model_ar1,
  "Recalibration-adjusted"         = function(v) its_res$freq_recal[[v]]$model,
  "Recalibration-adjusted + AR(1)" = function(v) its_res$freq_recal[[v]]$model_ar1
)
s10_outcomes <- c(log1p_deposits = "Deposits", asinh_losses = "Net loss",
                  log1p_wagers = "Wagers", log1p_play_hours = "Play hours")

s10_rows <- function(m, spec, outcome) {
  cf <- nlme::fixed.effects(m); V <- vcov(m)
  df <- m$fixDF$terms[["time_t"]]
  q  <- qt(0.975, df)
  L  <- setNames(rep(0, length(cf)), names(cf))
  L[c("intervention_t", "time_post_t")] <- 1
  est <- c(cf[["intervention_t"]], cf[["time_post_t"]], sum(L * cf))
  se  <- c(sqrt(V["intervention_t", "intervention_t"]),
           sqrt(V["time_post_t", "time_post_t"]),
           sqrt(as.numeric(t(L) %*% V %*% L)))
  data.frame(outcome = outcome, spec = spec,
             parameter = c("Level change", "Slope change", "October departure"),
             est = est, lo = est - q * se, hi = est + q * se)
}

if (!is.null(its_res) && !is.null(its_res$freq) &&
    requireNamespace("nlme", quietly = TRUE)) {
  s10_df <- bind_rows(lapply(names(s10_outcomes), function(v)
    bind_rows(lapply(names(s10_spec), function(sp) {
      m <- tryCatch(s10_spec[[sp]](v), error = function(e) NULL)
      if (is.null(m)) { cat("  Fig S10: no saved fit for", v, "[", sp, "] - skipped\n"); return(NULL) }
      s10_rows(m, sp, s10_outcomes[[v]])
    }))))

  if (nrow(s10_df) > 0) {
    s10_df <- s10_df |>
      mutate(outcome   = factor(outcome, levels = unname(s10_outcomes)),
             spec      = factor(spec, levels = rev(names(s10_spec))),
             parameter = factor(parameter, levels = c("Level change", "Slope change",
                                                      "October departure")))
    write.csv(s10_df, "results/tables/FIG_its_sensitivity_estimates.csv", row.names = FALSE)
    cat("  Saved: FIG_its_sensitivity_estimates.csv (check against Table S3.3)\n")

    make_FigS10 <- function(slides = FALSE) {
      ggplot(s10_df, aes(x = est, y = spec, shape = parameter, colour = parameter)) +
        geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
        geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0,
                       position = position_dodge(width = 0.6), linewidth = 0.45) +
        geom_point(position = position_dodge(width = 0.6), size = if (slides) 2.6 else 1.6) +
        scale_colour_manual(values = c("Level change" = "grey10", "Slope change" = "grey50",
                                       "October departure" = ASB_INTERVENTION), name = NULL) +
        scale_shape_manual(values = c("Level change" = 16, "Slope change" = 17,
                                      "October departure" = 15), name = NULL) +
        facet_wrap(~ outcome, scales = "free_x", nrow = 2) +
        labs(x = "Estimate, transformed outcome scale (95% CI)", y = NULL) +
        theme_asb(slides = slides) +
        theme(panel.grid.major.y = element_blank()) +
        asb_titles(slides, "Interrupted time-series sensitivity analyses",
                   "Pre-registered specifications. Dashed line = 0.")
    }
    save_figure_all(make_FigS10, "FigS10_its_sensitivity_forest", MS_2COL, 120)
  }
  rm(s10_df); invisible(gc(verbose = FALSE))
} else {
  cat("  Fig S10 skipped: saved frequentist ITS fits not available\n")
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n====================================\n")
cat("Main text: Fig1 flow; Fig2 distributions + change; Fig3 H8; Fig4 ITS deposits; Fig5 risk transitions\n")
cat("SI: FigS1-FigS10\n")
cat("Verification CSVs: results/tables/FIG_monthly_medians_bootstrap_ci.csv,\n")
cat("                   results/tables/FIG_h8_change_by_baseline_risk.csv\n")
cat("====================================\n")
message("=== 12 COMPLETE ===")
