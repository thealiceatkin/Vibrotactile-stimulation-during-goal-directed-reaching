# =============================================================================
# One-Way Repeated Measures ANOVA with Multiple Comparison Corrections
# =============================================================================
# Expected CSV format:
#   - Rows    = participants (one row per participant)
#   - Columns = participant_id, then one column per condition level
#
# Example header:
#   participant_id, time1, time2, time3
#
# Each condition column corresponds to one level of the single within-subject
# factor. Adjust CONFIGURATION below to match your data.
# =============================================================================


# ── 0. Install / load packages ───────────────────────────────────────────────

required_packages <- c(
  "tidyverse",  # data wrangling + ggplot2
  "afex",       # aov_ez() for RM ANOVA (handles sphericity automatically)
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

# Name for the single within-subject factor
CONDITION <- "Task"

# Desired order of factor levels on the x-axis (must match column names exactly)
# Set to NULL to use alphabetical order
CONDITION_LEVELS <- c("VibrotactileAlone", "AimingAlone", "DualTask")

# Value filtering: exclude participants with any value outside [MIN_VAL, MAX_VAL]
# Set to -Inf / Inf to skip range filtering
MIN_VAL <- 0
MAX_VAL <- 20

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

# Identify condition columns (everything except the ID column)
condition_cols <- setdiff(names(raw), ID_COL)
cat("Condition columns detected:\n", paste(condition_cols, collapse = "\n "), "\n\n")


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
# Each condition column name becomes a level of the single within-subject factor

data_long <- data_filtered %>%
  pivot_longer(
    cols      = all_of(condition_cols),
    names_to  = CONDITION,
    values_to = "value"
  ) %>%
  mutate(
    !!sym(ID_COL)     := factor(.data[[ID_COL]]),
    !!sym(CONDITION) := factor(
      .data[[CONDITION]],
      levels = if (is.null(CONDITION_LEVELS)) sort(unique(.data[[CONDITION]])) else CONDITION_LEVELS
    )
  )

cat("\nLong-format preview:\n")
print(head(data_long, 10))
cat("\nFactor levels:", levels(data_long[[CONDITION]]), "\n\n")

# ── 4b. Descriptive statistics ───────────────────────────────────────────────

desc_stats <- data_long %>%
  group_by(.data[[CONDITION]]) %>%
  summarise(
    n    = n(),
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value, na.rm = TRUE),
    min  = min(value, na.rm = TRUE),
    max  = max(value, na.rm = TRUE),
    range = max - min,
    .groups = "drop"
  )

cat("\n=== Descriptive Statistics ===\n")
print(desc_stats)

write_csv(desc_stats, file.path(OUTPUT_DIR, "descriptive_stats.csv"))
cat(sprintf("Descriptive statistics saved to %s/descriptive_stats.csv\n", OUTPUT_DIR))


# ── 5. One-way repeated measures ANOVA ──────────────────────────────────────
# afex::aov_ez() automatically applies the Greenhouse-Geisser sphericity
# correction when needed and reports partial eta-squared effect sizes

cat("Running one-way RM ANOVA...\n")

model <- aov_ez(
  id      = ID_COL,
  dv      = "value",
  data    = data_long,
  within  = CONDITION,
  anova_table = list(correction = "GG", es = "pes")
)

cat("\n=== ANOVA Table ===\n")
print(model)

# Save ANOVA table to CSV
anova_tbl <- as.data.frame(model$anova_table)
anova_tbl$term <- rownames(anova_tbl)
write_csv(anova_tbl, file.path(OUTPUT_DIR, "anova_table.csv"))
cat(sprintf("\nANOVA table saved to %s/anova_table.csv\n", OUTPUT_DIR))


# ── 6. Post-hoc comparisons with multiple-comparison correction ──────────────

cat(sprintf("\n=== Post-hoc comparisons (correction: %s) ===\n", CORRECTION_METHOD))

emm <- emmeans(model, specs = as.formula(paste("~", CONDITION)))

# All pairwise comparisons
posthoc <- pairs(emm, adjust = CORRECTION_METHOD)
cat("\nAll pairwise comparisons:\n")
print(posthoc)

# Save post-hoc table
write_csv(as.data.frame(posthoc),
          file.path(OUTPUT_DIR, "posthoc_all_pairs.csv"))
cat(sprintf("Post-hoc table saved to %s/posthoc_all_pairs.csv\n", OUTPUT_DIR))


# ── 7. Effect sizes ──────────────────────────────────────────────────────────

cat("\n=== Effect Sizes ===\n")
cat("Partial η² from ANOVA table (see anova_table.csv)\n")

# Cohen's d for each post-hoc pair
cohens_d_tbl <- as.data.frame(posthoc) %>%
  rowwise() %>%
  mutate(
    cohens_d = tryCatch({
      lvls   <- trimws(strsplit(as.character(contrast), " - ")[[1]])
      vals_1 <- data_long$value[data_long[[CONDITION]] == lvls[1]]
      vals_2 <- data_long$value[data_long[[CONDITION]] == lvls[2]]
      effsize::cohen.d(vals_1, vals_2)$estimate
    }, error = function(e) NA_real_)
  ) %>%
  ungroup()

cat("Cohen's d for post-hoc pairs:\n")
print(cohens_d_tbl[, c("contrast", "estimate", "p.value", "cohens_d")])
write_csv(cohens_d_tbl, file.path(OUTPUT_DIR, "effect_sizes.csv"))


# ── 8. Summary statistics for plotting ──────────────────────────────────────

summary_stats <- data_long %>%
  group_by(.data[[CONDITION]]) %>%
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
# Shows: mean ± 95% CI + individual participant points (beeswarm) +
#        lines connecting each participant across levels

# Colour palette — colourblind-friendly (Wong 2011)
palette_cb <- c("#009E73", "#CC79A7", "#0072B2", "#D55E00",
                "#F0E442", "#000000", "#56B4E9", "#E69F00")

plot_theme <- theme_classic(base_size = 16) +
  theme(
    axis.title   = element_text(size = 20),
    axis.text        = element_text(size = 16),
    legend.position = "none",          # single factor: no legend needed
    plot.title   = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 16, colour = "grey40"),
    plot.margin = margin(25, 25, 25, 25, unit = "pt")
  )

p_main <- ggplot(
  data_long,
  aes(x      = .data[[CONDITION]],
      y      = value,
      colour = .data[[CONDITION]],
      fill   = .data[[CONDITION]])
) +
  # Lines connecting individual participants across levels
  geom_line(
    aes(group = .data[[ID_COL]]),
    colour    = "grey60",
    alpha     = 0.3,
    linewidth = 0.4
  ) +
  # Individual participant data points
  # geom_beeswarm(
  #   alpha = 0.3,
  #   size  = 2,
  #   cex   = 2.5,
  #   show.legend = FALSE
  # ) +
  # Mean ± 95% CI
  geom_pointrange(
    data    = summary_stats,
    aes(y    = mean,
        ymin = mean - ci95,
        ymax = mean + ci95),
    colour    = "black",
    size      = 0.9,
    linewidth = 1.2,
    fatten    = 3
  ) +
  # Line connecting means across Factor levels
  geom_line(
    data      = summary_stats,
    aes(y     = mean,
        group = 1),
    colour    = "black",
    linewidth = 1
  ) +
  scale_colour_manual(values = palette_cb) +
  scale_fill_manual(values   = palette_cb) +
  scale_x_discrete(labels = c("Detection", "Reaching", "Reach-and-Detect"),
                   expand = expansion(add = 0.4)) +
  coord_cartesian(ylim = c(0, 16)) +
  scale_y_continuous(breaks = seq(0, 16, by = 2)) +
  labs(
    title    = "SURG-TLX - Physical Demands",
    # subtitle = paste0("Mean ± 95% CI, individual participant points shown (N = ",
    #                   n_after_range, ")"),
    x        = "\n Task",
    y        = "Score ( /20)",
    # caption  = paste0("Post-hoc correction: ", toupper(CORRECTION_METHOD),
    #                   " | Error bars: 95% CI")
  ) +
  plot_theme

print(p_main)

ggsave(file.path(OUTPUT_DIR, "rm_anova_oneway_plot.png"),
       p_main, width = 6, height = 6, dpi = 300, bg = "white")

cat(sprintf("\nPlot saved to %s/rm_anova_oneway_plot.png\n", OUTPUT_DIR))
cat("\nDone. All outputs written to:", OUTPUT_DIR, "\n")
