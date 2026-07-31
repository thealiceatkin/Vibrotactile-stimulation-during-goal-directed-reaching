# =============================================================================
# Mixed ANOVA: One Between-Subjects Factor × One Within-Subjects Factor
# =============================================================================
# Expected CSV format:
#   - Rows    = participants (one row per participant)
#   - Columns = participant_id, group (between-subjects factor),
#               then one column per within-subjects condition level
#
# Example header:
#   participant_id, group, time1, time2, time3
#
# The between-subjects factor is a single column (e.g. "control" / "treatment").
# The within-subjects factor levels are encoded as separate columns.
# Adjust CONFIGURATION below to match your data.
# =============================================================================


# ── 0. Install / load packages ───────────────────────────────────────────────

required_packages <- c(
  "tidyverse",  # data wrangling + ggplot2
  "afex",       # aov_ez() for mixed ANOVA
  "emmeans",    # estimated marginal means + post-hoc comparisons
  "effectsize", # eta_squared()
  "ggbeeswarm", # geom_beeswarm() for jittered individual points
  "patchwork"   # combine multiple ggplot panels
)

new_pkgs <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if (length(new_pkgs) > 0) install.packages(new_pkgs)
invisible(lapply(required_packages, library, character.only = TRUE))


# ── 1. CONFIGURATION ─────────────────────────────────────────────────────────

# Path to your CSV file
CSV_PATH <- ""

# Name of the participant ID column
ID_COL <- "Participant_ID"

# Name of the between-subjects factor column (must already exist in the CSV)
BETWEEN_FACTOR <- "Cohort"

# Desired order of between-subjects factor levels (must match values in CSV)
# Set to NULL to use alphabetical order
BETWEEN_LEVELS <- c("back", "hand")

# Name for the within-subjects factor
WITHIN_FACTOR <- "ID"

# Desired order of within-subjects factor levels on the x-axis
# (must match condition column names exactly)
# Set to NULL to use alphabetical order
WITHIN_LEVELS <- c("4.3923", "4.546",	"5.3576",	"5.5148")

# Value filtering: exclude participants with any value outside [MIN_VAL, MAX_VAL]
# Set to -Inf / Inf to skip range filtering
MIN_VAL <- -Inf
MAX_VAL <- Inf

# Multiple-comparison correction method for post-hoc tests
# Options: "holm" (recommended), "bonferroni", "BH" (Benjamini-Hochberg / FDR)
CORRECTION_METHOD <- "holm"

# Output directory for saved plots and tables
OUTPUT_DIR <- "anova_output"


# ── 2. Load & inspect data ───────────────────────────────────────────────────

dir.create(OUTPUT_DIR, showWarnings = FALSE)

raw <- read_csv(CSV_PATH, show_col_types = FALSE)
cat("Raw data dimensions:", nrow(raw), "participants x", ncol(raw), "columns\n")
cat("Columns:", paste(names(raw), collapse = ", "), "\n\n")

# Condition columns: everything except ID and the between-subjects factor
condition_cols <- setdiff(names(raw), c(ID_COL, BETWEEN_FACTOR))
cat("Within-subjects condition columns detected:\n",
    paste(condition_cols, collapse = "\n "), "\n\n")
cat("Between-subjects factor:", BETWEEN_FACTOR, "\n")
cat("Levels:", paste(unique(raw[[BETWEEN_FACTOR]]), collapse = ", "), "\n\n")


# ── 3. Filter participants ───────────────────────────────────────────────────

n_raw <- nrow(raw)

# 3a. Remove participants with ANY missing value across condition columns
data_no_na <- raw %>%
  dplyr::filter(if_all(all_of(condition_cols), ~ !is.na(.)))

n_after_na <- nrow(data_no_na)
cat(sprintf("Removed %d participant(s) with missing data. N = %d remaining.\n",
            n_raw - n_after_na, n_after_na))

# 3b. Remove participants with ANY value outside [MIN_VAL, MAX_VAL]
data_filtered <- data_no_na %>%
  dplyr::filter(if_all(all_of(condition_cols), ~ . >= MIN_VAL & . <= MAX_VAL))

n_after_range <- nrow(data_filtered)
cat(sprintf("Removed %d participant(s) with out-of-range values [%g, %g]. N = %d remaining.\n",
            n_after_na - n_after_range, MIN_VAL, MAX_VAL, n_after_range))

if (n_after_range < 5) stop("Too few participants remain after filtering. Check your MIN_VAL/MAX_VAL.")


# ── 4. Pivot to long format ──────────────────────────────────────────────────

data_long <- data_filtered %>%
  pivot_longer(
    cols      = all_of(condition_cols),
    names_to  = WITHIN_FACTOR,
    values_to = "value"
  ) %>%
  mutate(
    !!sym(ID_COL)       := factor(.data[[ID_COL]]),
    !!sym(BETWEEN_FACTOR) := factor(
      .data[[BETWEEN_FACTOR]],
      levels = if (is.null(BETWEEN_LEVELS)) sort(unique(.data[[BETWEEN_FACTOR]])) else BETWEEN_LEVELS,
      labels = c("Motors Worn on the Back", "Motors Worn on the Hand")
    ),
    !!sym(WITHIN_FACTOR) := factor(
      .data[[WITHIN_FACTOR]],
      levels = if (is.null(WITHIN_LEVELS)) sort(unique(.data[[WITHIN_FACTOR]])) else WITHIN_LEVELS
    )
  )

cat("\nLong-format preview:\n")
print(head(data_long, 10))
cat("\nBetween-subjects levels:", levels(data_long[[BETWEEN_FACTOR]]), "\n")
cat("Within-subjects levels: ", levels(data_long[[WITHIN_FACTOR]]),  "\n\n")


# ── 5. Mixed ANOVA ───────────────────────────────────────────────────────────
# aov_ez() takes both 'between' and 'within' arguments.
# Greenhouse-Geisser correction is applied to the within-subjects terms.
# Note: mixed ANOVAs require equal or near-equal group sizes for
# sphericity correction to be reliable. A warning will appear if groups
# are very unbalanced.

cat("Running mixed ANOVA...\n")

model <- aov_ez(
  id      = ID_COL,
  dv      = "value",
  data    = data_long,
  between = BETWEEN_FACTOR,
  within  = WITHIN_FACTOR,
  anova_table = list(correction = "GG", es = "pes")
)

cat("\n=== ANOVA Table ===\n")
print(model)

# Save ANOVA table
anova_tbl <- as.data.frame(model$anova_table)
anova_tbl$term <- rownames(anova_tbl)
write_csv(anova_tbl, file.path(OUTPUT_DIR, "anova_table.csv"))
cat(sprintf("\nANOVA table saved to %s/anova_table.csv\n", OUTPUT_DIR))


# ── 6. Post-hoc comparisons with multiple-comparison correction ──────────────
# Three sets of comparisons are run:
#   (a) Main effect of the within-subjects factor, collapsed across groups
#   (b) Main effect of the between-subjects factor, collapsed across time
#   (c) Simple effects: within-subjects pairwise comparisons at each group level

cat(sprintf("\n=== Post-hoc comparisons (correction: %s) ===\n", CORRECTION_METHOD))

# (a) Within-subjects main effect
emm_within <- emmeans(model,
                      specs = as.formula(paste("~", WITHIN_FACTOR)))
posthoc_within <- pairs(emm_within, adjust = CORRECTION_METHOD)
cat(sprintf("\n(a) Pairwise comparisons for %s (collapsed across %s):\n",
            WITHIN_FACTOR, BETWEEN_FACTOR))
print(posthoc_within)

# (b) Between-subjects main effect
emm_between <- emmeans(model,
                       specs = as.formula(paste("~", BETWEEN_FACTOR)))
posthoc_between <- pairs(emm_between, adjust = CORRECTION_METHOD)
cat(sprintf("\n(b) Pairwise comparisons for %s (collapsed across %s):\n",
            BETWEEN_FACTOR, WITHIN_FACTOR))
print(posthoc_between)

# (c) Simple effects: within-subjects comparisons at each level of between factor
simple_fx <- emmeans(model,
                     specs  = as.formula(paste("pairwise ~", WITHIN_FACTOR, "|", BETWEEN_FACTOR)),
                     adjust = CORRECTION_METHOD)
cat(sprintf("\n(c) Simple effects of %s at each level of %s:\n",
            WITHIN_FACTOR, BETWEEN_FACTOR))
print(simple_fx$contrasts)

# Save post-hoc tables
write_csv(as.data.frame(posthoc_within),
          file.path(OUTPUT_DIR, "posthoc_within.csv"))
write_csv(as.data.frame(posthoc_between),
          file.path(OUTPUT_DIR, "posthoc_between.csv"))
write_csv(as.data.frame(simple_fx$contrasts),
          file.path(OUTPUT_DIR, "posthoc_simple_effects.csv"))
cat(sprintf("\nPost-hoc tables saved to %s/\n", OUTPUT_DIR))


# ── 7. Effect sizes ──────────────────────────────────────────────────────────

cat("\n=== Effect Sizes ===\n")
cat("Partial η² from ANOVA table (see anova_table.csv)\n")

# Cohen's d for within-subjects post-hoc pairs
cohens_d_tbl <- as.data.frame(posthoc_within) %>%
  rowwise() %>%
  mutate(
    cohens_d = tryCatch({
      lvls   <- trimws(strsplit(as.character(contrast), " - ")[[1]])
      vals_1 <- data_long$value[data_long[[WITHIN_FACTOR]] == lvls[1]]
      vals_2 <- data_long$value[data_long[[WITHIN_FACTOR]] == lvls[2]]
      effsize::cohen.d(vals_1, vals_2)$estimate
    }, error = function(e) NA_real_)
  ) %>%
  ungroup()

cat("Cohen's d for within-subjects post-hoc pairs:\n")
print(cohens_d_tbl[, c("contrast", "estimate", "p.value", "cohens_d")])
write_csv(cohens_d_tbl, file.path(OUTPUT_DIR, "effect_sizes_within.csv"))


# ── 8. Summary statistics for plotting ──────────────────────────────────────

summary_stats <- data_long %>%
  group_by(.data[[BETWEEN_FACTOR]], .data[[WITHIN_FACTOR]]) %>%
  summarise(
    n    = n(),
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value, na.rm = TRUE),
    se   = sd / sqrt(n),
    ci95 = qt(0.975, df = n - 1) * se,
    .groups = "drop"
  )

cat("\nSummary statistics:\n")
print(summary_stats)


# ── 9. Plot ──────────────────────────────────────────────────────────────────
# Between-subjects factor mapped to colour; within-subjects factor on x-axis.
# Shows: mean ± 95% CI + individual participant points + per-participant lines.

# Colour palette — colourblind-friendly (Wong 2011)
# palette_cb <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7",
#                 "#56B4E9", "#E69F00", "#F0E442", "#000000")

palette_cb <- c("#EE6677", "#4477AA")

plot_theme <- theme_classic(base_size = 13) +
  theme(
    strip.background  = element_blank(),
    strip.text        = element_text(face = "bold", size = 13),
    axis.title        = element_text(size = 12),
    legend.position   = "bottom",
    legend.title      = element_blank(),
    plot.title        = element_text(face = "bold", size = 14),
    plot.subtitle     = element_text(size = 11, colour = "grey40")
  )

p_main <- ggplot(
  data_long,
  aes(x      = .data[[WITHIN_FACTOR]],
      y      = value,
      colour = .data[[BETWEEN_FACTOR]],
      fill   = .data[[BETWEEN_FACTOR]])
) +
  # Lines connecting individual participants across within-subjects levels
  geom_line(
    aes(group = .data[[ID_COL]]),
    alpha     = 0.2,
    linewidth = 0.4
  ) +
  # Individual participant data points
  geom_beeswarm(
    alpha       = 0.4,
    size        = 1.8,
    cex         = 2,
    dodge.width = 0.5,
    show.legend = FALSE
  ) +
  # Mean ± 95% CI
  geom_pointrange(
    data    = summary_stats,
    aes(y    = mean,
        ymin = mean - ci95,
        ymax = mean + ci95),
    position  = position_dodge(width = 0.5),
    size      = 0.9,
    linewidth = 1.2,
    fatten    = 3
  ) +
  # Lines connecting means across within-subjects levels, per group
  geom_line(
    data      = summary_stats,
    aes(y     = mean,
        group = .data[[BETWEEN_FACTOR]]),
    position  = position_dodge(width = 0.5),
    linewidth = 1
  ) +
  scale_colour_manual(values = palette_cb) +
  scale_fill_manual(values   = palette_cb) +
  labs(
    title    = "Reaching Task",
    subtitle = paste0("Mean ± 95% CI, individual participant points shown (N = ",
                      n_after_range, ")"),
    x        = WITHIN_FACTOR,
    y        = "Movement Time (s)",
    caption  = paste0("Post-hoc correction: ", toupper(CORRECTION_METHOD),
                      " | Error bars: 95% CI")
  ) +
  plot_theme

print(p_main)

ggsave(file.path(OUTPUT_DIR, "mixed_anova_plot.png"),
       p_main, width = 8, height = 5.5, dpi = 300, bg = "white")

# ── Faceted version: one panel per between-subjects group ────────────────────
# Useful when group overlap makes the combined plot hard to read

p_faceted <- p_main +
  facet_wrap(as.formula(paste("~", BETWEEN_FACTOR))) +
  theme(legend.position = "none")

print(p_faceted)

ggsave(file.path(OUTPUT_DIR, "mixed_anova_plot_faceted.png"),
       p_faceted, width = 9, height = 5, dpi = 300, bg = "white")

cat(sprintf("\nPlots saved to %s/\n", OUTPUT_DIR))
cat("\nDone. All outputs written to:", OUTPUT_DIR, "\n")
