# =============================================================================
# asb_style.R
# A Safe Bet – POC Analysis
# Shared figure style: theme, semantic colour roles, scale helpers, and a
# dual-output saver (manuscript + slides).
# =============================================================================
# VISUAL RESTYLE ONLY. This module contains no data construction. Sourced by
# the figure section of 09_poc_tables_figures.R. Every figure is defined once
# as a builder function make_Fx(slides = FALSE) and rendered twice:
#
#   manuscript : PDF (vector) + TIFF (600 dpi, LZW) at journal column widths,
#                NO in-figure titles/subtitles (captions live in the
#                manuscript text, per Addiction author guidelines)
#   slides     : 16:9 PNG (300 dpi) WITH titles and enlarged type
#
# Colour roles (Okabe-Ito derived; colourblind-safe, print-safe):
#   ASB_OUTCOME_DARK/LIGHT           — one hue per outcome; SHADE encodes
#       period (light = baseline, dark = intervention). Used in F2, F2b, F3;
#       F10's deposits series inherits the deposits hue. Lightness contrast
#       survives all colour-vision deficiencies.
#   ASB_RISK                         — ordered risk levels + inactive, used
#       ONLY for risk-level fills (F4); these hues are reserved and never
#       assigned to outcomes
#   ASB_SEQ_LOW/HIGH                 — one sequential ramp shared by ALL
#       heatmaps (F5, F6)
#   F8 uses the outcome hues with shade encoding the PARAMETER within each
#       panel (dark = level change beta_2, light = slope change beta_3);
#       F9 annotations are monochrome (near-black spike among grey bars);
#       F10 annotation lines carry both colour and line pattern (solid
#       ASB_INTERVENTION orange = launch, dashed slate #4F6D8C =
#       operator-verified, dotted grey = public dates).
#   ASB_INTERVENTION                 — intervention-period engagement lines
#       (F7); ASB_BASELINE retained as its pair for period-only contexts
#   ASB_ACCENT                       — spare highlight, currently unused
#   ASB_INK / ASB_GREYBAR            — neutral ink and neutral bars
# Display terminology follows the locked manuscript terms: risk level (not
# state/tier), Moderate (not Medium), Net loss (not Losses). Underlying
# variable codes ("medium" etc.) are untouched.
# =============================================================================

library(ggplot2)
library(scales)

# Figure font. APA 7 asks for a sans-serif face at 8-14 pt inside figures;
# "sans" maps to a Helvetica/Arial-class face on the PDF device. If Arial is
# installed on the render machine, set ASB_FONT <- "Arial".
ASB_FONT <- "sans"

# --- Colour roles ------------------------------------------------------------

ASB_BASELINE     <- "#0072B2"   # Okabe-Ito blue
ASB_INTERVENTION <- "#E69F00"   # Okabe-Ito orange
ASB_RISK <- c(
  "Low"      = "#009E73",       # Okabe-Ito bluish green
  "Moderate" = "#F0E442",       # Okabe-Ito yellow
  "High"     = "#D55E00",       # Okabe-Ito vermillion
  "Inactive" = "#B0B0B0"        # neutral grey
)
ASB_SEQ_LOW  <- "#F7FBFF"       # shared heatmap ramp (single-hue blue)
ASB_SEQ_HIGH <- "#08519C"
ASB_ACCENT   <- "#CC79A7"       # Okabe-Ito reddish purple (spare; unused)
ASB_INK      <- "grey25"        # neutral data ink

# Outcome hues: one hue per outcome, shade encodes period
# (light = baseline, dark = intervention). Reserved risk hues
# (green/yellow/vermillion) are not used for outcomes.
ASB_OUTCOME_DARK <- c(
  "Deposits (GBP)" = "#0072B2",   # blue
  "Net loss (GBP)" = "#C87E00",   # amber (darkened Okabe orange)
  "Play hours"     = "#1B8A7A",   # teal
  "Wagers (GBP)"   = "#7B5AA6"    # purple
)
ASB_OUTCOME_LIGHT <- c(
  "Deposits (GBP)" = "#A8CFE8",
  "Net loss (GBP)" = "#F5CD8F",
  "Play hours"     = "#A8DBD2",
  "Wagers (GBP)"   = "#CDB9DF"
)
# Full 8-value map keyed "outcome.period" for interaction() scales
ASB_OUTCOME_PERIOD <- c(
  setNames(ASB_OUTCOME_LIGHT, paste0(names(ASB_OUTCOME_LIGHT), ".Baseline")),
  setNames(ASB_OUTCOME_DARK,  paste0(names(ASB_OUTCOME_DARK),  ".Intervention"))
)
ASB_GREYBAR  <- "grey65"        # neutral bar fill (spare; F9 uses direct greys)

# --- Locked display labels ---------------------------------------------------

ASB_OUTCOME_LABELS <- c(
  total_deposits   = "Deposits (GBP)",
  total_losses     = "Net loss (GBP)",
  total_wagers     = "Wagers (GBP)",
  play_hours_total = "Play hours"
)
ASB_OUTCOME_ORDER <- unname(ASB_OUTCOME_LABELS[
  c("total_deposits", "total_losses", "total_wagers", "play_hours_total")])

ASB_RISK_LABELS <- c(low = "Low", medium = "Moderate", high = "High")

# --- Theme -------------------------------------------------------------------

theme_asb <- function(base_size = 9, slides = FALSE) {
  if (slides) base_size <- 16
  theme_minimal(base_size = base_size, base_family = ASB_FONT) +
    theme(
      axis.line          = element_line(linewidth = 0.35, colour = "grey30"),
      axis.ticks         = element_line(linewidth = 0.25, colour = "grey30"),
      axis.ticks.length  = unit(2, "pt"),
      axis.text          = element_text(colour = "grey15"),
      axis.title         = element_text(colour = "grey5"),
      strip.text         = element_text(colour = "grey15", size = rel(0.95),
                                        margin = margin(b = 3)),
      strip.background   = element_blank(),
      legend.position    = "bottom",
      legend.title       = element_text(size = rel(0.9)),
      legend.text        = element_text(size = rel(0.9)),
      legend.key.size    = unit(if (slides) 14 else 9, "pt"),
      plot.title         = element_text(face = "bold", hjust = 0,
                                        size = rel(1.05), margin = margin(b = 2)),
      plot.subtitle      = element_text(colour = "grey40", hjust = 0,
                                        size = rel(0.9), margin = margin(b = 7)),
      plot.title.position = "plot",
      panel.grid.major.y = element_line(linewidth = 0.2, colour = "grey92"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.margin        = margin(6, 10, 6, 6)
    )
}

# Heatmap variant: no axis lines/ticks/grid
theme_asb_heatmap <- function(base_size = 9, slides = FALSE) {
  theme_asb(base_size, slides) +
    theme(panel.grid = element_blank(),
          axis.line  = element_blank(),
          axis.ticks = element_blank())
}

# Add titles only on the slides variant (manuscript captions live in text)
asb_titles <- function(slides, title, subtitle = NULL) {
  if (slides) labs(title = title, subtitle = subtitle) else NULL
}

# --- Scale helpers -----------------------------------------------------------

# Month axis: consistent "Mon YY" labels across all time figures
asb_scale_month <- function(breaks = "2 months", slides = FALSE) {
  list(
    scale_x_date(date_labels = "%b %y", date_breaks = breaks,
                 expand = expansion(mult = 0.03)),
    theme(axis.text.x = element_text(angle = 35, hjust = 1))
  )
}

# Signed log-style breaks for pseudo_log axes: 0 and +/- powers of 10
asb_symlog_breaks <- function(lims) {
  k   <- 10^(0:8)
  pos <- k[k <= max(lims, 0)]
  neg <- -k[-k >= min(lims, 0)]
  sort(unique(c(neg, 0, pos)))
}

# Shared pseudo-log y scale (handles zeros and negative net losses; the old
# log1p mapping silently dropped net losses below -1)
asb_scale_pseudolog <- function(name = "Value") {
  scale_y_continuous(
    name   = name,
    trans  = scales::pseudo_log_trans(base = 10),
    breaks = asb_symlog_breaks,
    labels = scales::label_comma()
  )
}

# Shared sequential fill for all heatmaps
asb_scale_heat <- function(name, limits, labels = waiver()) {
  scale_fill_gradient(low = ASB_SEQ_LOW, high = ASB_SEQ_HIGH,
                      name = name, limits = limits, labels = labels)
}

# --- Output ------------------------------------------------------------------

# Journal widths (Addiction / Wiley): single column 80 mm, full width 170 mm
MS_1COL <- 80
MS_2COL <- 170

DIR_MS     <- "results/figures/manuscript"
DIR_SLIDES <- "results/figures/slides"

#' Render one figure to all outputs.
#' @param make_fig function(slides = FALSE) returning a ggplot/patchwork object
#' @param name     file stem, e.g. "F2_prepost_distributions"
#' @param ms_width_mm,ms_height_mm  manuscript size in mm
#' Slides render at 16:9 (338 x 190 mm at 300 dpi).
save_figure_all <- function(make_fig, name, ms_width_mm, ms_height_mm) {
  dir.create(DIR_MS,     recursive = TRUE, showWarnings = FALSE)
  dir.create(DIR_SLIDES, recursive = TRUE, showWarnings = FALSE)
  paths <- c(
    pdf   = file.path(DIR_MS,     paste0(name, ".pdf")),
    tiff  = file.path(DIR_MS,     paste0(name, ".tiff")),
    slide = file.path(DIR_SLIDES, paste0(name, ".png"))
  )
  # Same resume logic as the registered save_figure(): skip only when ALL
  # outputs exist and OVERWRITE is FALSE
  if (exists("OVERWRITE") && !OVERWRITE && all(file.exists(paths))) {
    message(sprintf(
      "SKIPPING figure %s: outputs exist and OVERWRITE = FALSE (set TRUE or delete the files to re-render, e.g. after a data change)",
      name))
    return(invisible(NULL))
  }
  in_w <- ms_width_mm / 25.4
  in_h <- ms_height_mm / 25.4
  p_ms <- make_fig(slides = FALSE)
  ggsave(paths[["pdf"]],  p_ms, width = in_w, height = in_h, device = cairo_pdf)
  ggsave(paths[["tiff"]], p_ms, width = in_w, height = in_h, dpi = 600,
         device = "tiff", compression = "lzw", bg = "white")
  p_sl <- make_fig(slides = TRUE)
  ggsave(paths[["slide"]], p_sl, width = 338 / 25.4, height = 190 / 25.4,
         dpi = 300, bg = "white")
  cat("  Saved (pdf + tiff + slide png):", name, "\n")
  invisible(gc(verbose = FALSE))
}
