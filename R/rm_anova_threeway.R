# =============================================================================
# Three-Way Repeated Measures ANOVA with Multiple Comparison Corrections
# =============================================================================
# Expected CSV format:
#   - Rows    = participants (one row per participant)
#   - Columns = participant_id, then one column per combination of
#               within-subjects condition levels
#
# Example header:
#   participant_id, A_time1_low, A_time1_high, A_time2_low, A_time2_high,
#                  B_time1_low, B_time1_high, B_time2_low, B_time2_high
#
# The three within-subjects factors are encoded in column names, separated
# by "_": <level_factorA>_<level_factorB>_<level_factorC>
# Adjust CONFIGURATION below to match your actual column naming scheme.
# =============================================================================


# ── 0. Install / load packages ───────────────────────────────────────────────

required_packages <- c(
  "tidyverse",  # data wrangling + ggplot2
  "afex",       # aov_ez() for RM ANOVA
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

# Names for the three within-subjects factors (derived from column name parts)
# Column names must follow the pattern:
#   <level_factorA>_<level_factorB>_<level_factorC>
# e.g. "A_time1_low", "A_time1_high", "B_time2_low" etc.
FACTOR_A_NAME <- "Condition"
FACTOR_B_NAME <- "Target_Size"
FACTOR_C_NAME <- "Distance"

# Desired order of levels for each factor
# Set to NULL to use alphabetical order
FACTOR_A_LEVELS <- c("motor", "back", "hand")
FACTOR_B_LEVELS <- c("0.5", "1")
FACTOR_C_LEVELS <- c("20", "22.36068")

# Value filtering: exclude participants with any value outside [MIN_VAL, MAX_VAL]
# Set to -Inf / Inf to skip range filtering
MIN_VAL <- -Inf
MAX_VAL <- Inf

#Define thresholds for min values of Factor C (Distance)
# THRESHOLDS <- data.frame(
#   Distance = c("Short", "Long"),
#   threshold_start = c(19, 21.3),
#   threshold_end = c(18.5, 20.8)
# )
# THRESHOLDS$Distance <- factor(THRESHOLDS$Distance, levels = FACTOR_C_LEVELS)

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
# Column names are split into three factor columns using "_" as separator.
# If your column names use a different separator, change names_sep below.
# If the separator appears within a level name (e.g. "pre_post"), consider
# renaming columns to use a different separator (e.g. ".") before running.

data_long <- data_filtered %>%
  pivot_longer(
    cols      = all_of(condition_cols),
    names_to  = c(FACTOR_A_NAME, FACTOR_B_NAME, FACTOR_C_NAME),
    names_sep = "_",
    values_to = "value"
  ) %>%
  mutate(
    !!sym(ID_COL)        := factor(.data[[ID_COL]]),
    !!sym(FACTOR_A_NAME) := factor(
      .data[[FACTOR_A_NAME]],
      levels = if (is.null(FACTOR_A_LEVELS)) sort(unique(.data[[FACTOR_A_NAME]])) else FACTOR_A_LEVELS
    ),
    !!sym(FACTOR_B_NAME) := factor(
      .data[[FACTOR_B_NAME]],
      levels = if (is.null(FACTOR_B_LEVELS)) sort(unique(.data[[FACTOR_B_NAME]])) else FACTOR_B_LEVELS
    ),
    !!sym(FACTOR_C_NAME) := factor(
      .data[[FACTOR_C_NAME]],
      levels = if (is.null(FACTOR_C_LEVELS)) sort(unique(.data[[FACTOR_C_NAME]])) else FACTOR_C_LEVELS
    )
  )

cat("\nLong-format preview:\n")
print(head(data_long, 12))
cat("\nFactor levels:\n")
cat(FACTOR_A_NAME, ":", levels(data_long[[FACTOR_A_NAME]]), "\n")
cat(FACTOR_B_NAME, ":", levels(data_long[[FACTOR_B_NAME]]), "\n")
cat(FACTOR_C_NAME, ":", levels(data_long[[FACTOR_C_NAME]]), "\n\n")

# ── 4b. Descriptive statistics ───────────────────────────────────────────────

desc_stats <- data_long %>%
  group_by(.data[[FACTOR_A_NAME]]) %>%
    summarise(
    n    = n(),
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value, na.rm = TRUE),
    min  = min(value, na.rm = TRUE),
    max  = max(value, na.rm = TRUE),
    range = max - min,
    .groups = "drop"
  )

# cat("\n=== Descriptive Statistics ===\n")

cat("\n=== Descriptive Statistics:", FACTOR_A_NAME,
    "(collapsed across", FACTOR_A_NAME, "and", FACTOR_B_NAME, ") ===\n")

print(desc_stats)

write_csv(desc_stats, file.path(OUTPUT_DIR, "descriptive_stats_A.csv"))
cat(sprintf("Descriptive statistics saved to %s/descriptive_stats_A.csv\n", OUTPUT_DIR))


# ── 5. Three-way repeated measures ANOVA ────────────────────────────────────
# aov_ez() receives all three factors as a vector in 'within'.
# This produces a table with seven terms:
#   - Three main effects:            A, B, C
#   - Three two-way interactions:    A:B, A:C, B:C
#   - One three-way interaction:     A:B:C
# Greenhouse-Geisser sphericity correction is applied automatically.

cat("Running three-way RM ANOVA...\n")

model <- aov_ez(
  id      = ID_COL,
  dv      = "value",
  data    = data_long,
  within  = c(FACTOR_A_NAME, FACTOR_B_NAME, FACTOR_C_NAME),
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
# Post-hoc comparisons are run for the three-way interaction and each
# two-way interaction, as well as simple effects breaking down each
# interaction. Focus on whichever terms are significant in your ANOVA table.

cat(sprintf("\n=== Post-hoc comparisons (correction: %s) ===\n", CORRECTION_METHOD))

# (a) Three-way interaction: all pairwise comparisons across A × B × C
emm_abc <- emmeans(model,
                   specs = as.formula(paste("~", FACTOR_A_NAME, "*",
                                            FACTOR_B_NAME, "*", FACTOR_C_NAME)))
posthoc_abc <- pairs(emm_abc, adjust = CORRECTION_METHOD)
cat(sprintf("\n(a) All pairwise comparisons: %s × %s × %s\n",
            FACTOR_A_NAME, FACTOR_B_NAME, FACTOR_C_NAME))
print(posthoc_abc)

# (b) Simple effects of Factor C at each combination of Factor A × Factor B
simple_fx_c <- emmeans(model,
                       specs  = as.formula(paste("pairwise ~", FACTOR_C_NAME, "|",
                                                 FACTOR_A_NAME, "*", FACTOR_B_NAME)),
                       adjust = CORRECTION_METHOD)
cat(sprintf("\n(b) Simple effects of %s at each %s × %s combination:\n",
            FACTOR_C_NAME, FACTOR_A_NAME, FACTOR_B_NAME))
print(simple_fx_c$contrasts)

# (c) Simple effects of Factor B at each combination of Factor A × Factor C
simple_fx_b <- emmeans(model,
                       specs  = as.formula(paste("pairwise ~", FACTOR_B_NAME, "|",
                                                 FACTOR_A_NAME, "*", FACTOR_C_NAME)),
                       adjust = CORRECTION_METHOD)
cat(sprintf("\n(c) Simple effects of %s at each %s × %s combination:\n",
            FACTOR_B_NAME, FACTOR_A_NAME, FACTOR_C_NAME))
print(simple_fx_b$contrasts)

# (d) Simple effects of Factor A at each combination of Factor B × Factor C
simple_fx_a <- emmeans(model,
                       specs  = as.formula(paste("pairwise ~", FACTOR_A_NAME, "|",
                                                 FACTOR_B_NAME, "*", FACTOR_C_NAME)),
                       adjust = CORRECTION_METHOD)
cat(sprintf("\n(d) Simple effects of %s at each %s × %s combination:\n",
            FACTOR_A_NAME, FACTOR_B_NAME, FACTOR_C_NAME))
print(simple_fx_a$contrasts)

# (e) Two-way interaction A × B (collapsed across C)
emm_ab <- emmeans(model,
                  specs = as.formula(paste("~", FACTOR_A_NAME, "*", FACTOR_B_NAME)))
posthoc_ab <- pairs(emm_ab, adjust = CORRECTION_METHOD)
cat(sprintf("\n(e) Pairwise comparisons: %s × %s (collapsed across %s):\n",
            FACTOR_A_NAME, FACTOR_B_NAME, FACTOR_C_NAME))
print(posthoc_ab)

# (f) Two-way interaction A × C (collapsed across B)
emm_ac <- emmeans(model,
                  specs = as.formula(paste("~", FACTOR_A_NAME, "*", FACTOR_C_NAME)))
posthoc_ac <- pairs(emm_ac, adjust = CORRECTION_METHOD)
cat(sprintf("\n(f) Pairwise comparisons: %s × %s (collapsed across %s):\n",
            FACTOR_A_NAME, FACTOR_C_NAME, FACTOR_B_NAME))
print(posthoc_ac)

# (g) Two-way interaction B × C (collapsed across A)
emm_bc <- emmeans(model,
                  specs = as.formula(paste("~", FACTOR_B_NAME, "*", FACTOR_C_NAME)))
posthoc_bc <- pairs(emm_bc, adjust = CORRECTION_METHOD)
cat(sprintf("\n(g) Pairwise comparisons: %s × %s (collapsed across %s):\n",
            FACTOR_B_NAME, FACTOR_C_NAME, FACTOR_A_NAME))
print(posthoc_bc)

# Save post-hoc tables
write_csv(as.data.frame(posthoc_abc),
          file.path(OUTPUT_DIR, "posthoc_ABC.csv"))
write_csv(as.data.frame(simple_fx_c$contrasts),
          file.path(OUTPUT_DIR, "posthoc_simple_C_by_AB.csv"))
write_csv(as.data.frame(simple_fx_b$contrasts),
          file.path(OUTPUT_DIR, "posthoc_simple_B_by_AC.csv"))
write_csv(as.data.frame(simple_fx_a$contrasts),
          file.path(OUTPUT_DIR, "posthoc_simple_A_by_BC.csv"))
write_csv(as.data.frame(posthoc_ab),
          file.path(OUTPUT_DIR, "posthoc_AB.csv"))
write_csv(as.data.frame(posthoc_ac),
          file.path(OUTPUT_DIR, "posthoc_AC.csv"))
write_csv(as.data.frame(posthoc_bc),
          file.path(OUTPUT_DIR, "posthoc_BC.csv"))
cat(sprintf("\nPost-hoc tables saved to %s/\n", OUTPUT_DIR))

# ── 6b-1. Planned comparisons for a single factor ────────────────────────────
# Edit FACTOR_A_NAME to whichever factor you want to test,
# and update the contrast vectors to match its levels.

emm_planned <- emmeans(model,
                       specs = as.formula(paste("~", FACTOR_A_NAME)))

# Print to confirm level ordering before defining contrasts
cat("\nEstimated marginal means for", FACTOR_A_NAME, ":\n")
print(emm_planned)

# Define contrasts as a named list.
# Each vector must have one element per level of FACTOR_A_NAME, summing to zero.
# Examples below assume three levels: "A", "B", "C" — edit to match your data.
planned_contrasts <- list(
  "motor vs back"    = c( 1, -1,  0),   # compare first level against second
  "motor vs hand"    = c( 1,  0, -1),   # compare first level against third
  "back vs hand"    = c( 0,  1, -1),   # compare second level against third
  "motor vs back+hand"  = c( 1, -0.5, -0.5)  # compare first level against average of second and third
)

# Apply contrasts
# Set adjust = "none" if comparisons are fully independent and a priori justified,
# or keep CORRECTION_METHOD for a modest correction across the planned set
planned_results <- contrast(emm_planned,
                            method = planned_contrasts,
                            adjust = CORRECTION_METHOD)

cat(sprintf("\n=== Planned comparisons: %s (correction: %s) ===\n",
            FACTOR_A_NAME, CORRECTION_METHOD))
print(summary(planned_results))

# Save
write_csv(as.data.frame(planned_results),
          file.path(OUTPUT_DIR, "planned_comparisons.csv"))
cat(sprintf("Planned comparisons saved to %s/planned_comparisons.csv\n", OUTPUT_DIR))


# # ── 6b-2. Planned comparisons across factor combinations ─────────────────────
# # Use this when your hypotheses concern specific cells of a two-way interaction.
# # Edit the factor names and contrast vectors to match your data.
# 
# emm_planned_ab <- emmeans(model,
#                           specs = as.formula(paste("~", FACTOR_A_NAME, "*", FACTOR_B_NAME)))
# 
# # Print to confirm cell ordering before defining contrasts
# cat("\nEstimated marginal means for", FACTOR_A_NAME, "x", FACTOR_B_NAME, ":\n")
# print(emm_planned_ab)
# 
# # Define contrasts across cells.
# # Vector length = number of Factor A levels × number of Factor B levels.
# # Cells are ordered with Factor A varying fastest (i.e. all levels of Factor A
# # at the first level of Factor B, then all levels of Factor A at the second
# # level of Factor B, etc.) — verify by printing emm_planned_ab above.
# # Examples below assume Factor A has 2 levels and Factor B has 2 levels (4 cells total).
# planned_contrasts_ab <- list(
#   "A_time1 vs B_time1" = c( 1, -1,  0,  0),   # Factor A comparison at first level of Factor B
#   "A_time2 vs B_time2" = c( 0,  0,  1, -1),   # Factor A comparison at second level of Factor B
#   "A_time1 vs A_time2" = c( 1,  0, -1,  0),   # Factor B comparison at first level of Factor A
#   "B_time1 vs B_time2" = c( 0,  1,  0, -1)    # Factor B comparison at second level of Factor A
# )
# 
# # Apply contrasts
# planned_results_ab <- contrast(emm_planned_ab,
#                                method = planned_contrasts_ab,
#                                adjust = CORRECTION_METHOD)
# 
# cat(sprintf("\n=== Planned comparisons: %s x %s (correction: %s) ===\n",
#             FACTOR_A_NAME, FACTOR_B_NAME, CORRECTION_METHOD))
# print(summary(planned_results_ab))
# 
# # Save
# write_csv(as.data.frame(planned_results_ab),
#           file.path(OUTPUT_DIR, "planned_comparisons_AB.csv"))
# cat(sprintf("Planned comparisons saved to %s/planned_comparisons_AB.csv\n", OUTPUT_DIR))


# ── 7. Effect sizes ──────────────────────────────────────────────────────────

cat("\n=== Effect Sizes ===\n")
cat("Partial η² from ANOVA table (see anova_table.csv)\n")

# Cohen's d for the three-way post-hoc pairs
cohens_d_tbl <- as.data.frame(posthoc_abc) %>%
  rowwise() %>%
  mutate(
    cohens_d = tryCatch({
      lvls   <- trimws(strsplit(as.character(contrast), " - ")[[1]])
      vals_1 <- data_long$value[
        paste(data_long[[FACTOR_A_NAME]],
              data_long[[FACTOR_B_NAME]],
              data_long[[FACTOR_C_NAME]]) == lvls[1]]
      vals_2 <- data_long$value[
        paste(data_long[[FACTOR_A_NAME]],
              data_long[[FACTOR_B_NAME]],
              data_long[[FACTOR_C_NAME]]) == lvls[2]]
      effsize::cohen.d(vals_1, vals_2)$estimate
    }, error = function(e) NA_real_)
  ) %>%
  ungroup()

cat("Cohen's d for three-way post-hoc pairs:\n")
print(cohens_d_tbl[, c("contrast", "estimate", "p.value", "cohens_d")])
write_csv(cohens_d_tbl, file.path(OUTPUT_DIR, "effect_sizes_ABC.csv"))


# ── 8. Summary statistics for plotting ──────────────────────────────────────

summary_stats <- data_long %>%
  group_by(.data[[FACTOR_A_NAME]], .data[[FACTOR_B_NAME]], .data[[FACTOR_C_NAME]]) %>%
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
# Factor B on the x-axis, Factor A mapped to colour, faceted by Factor C.
# This layout makes the three-way interaction readable across panels.
# Individual participant points and connecting lines are shown within each panel.

# Colour palette
palette_cb <- c("#000000", "#EE6677", "#4477AA")

# # Alternative colour palette — colourblind-friendly (Wong 2011)
# palette_cb <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7",
#                 "#56B4E9", "#E69F00", "#F0E442", "#000000")

# Use this version for a single panel plot: Factor B on the x-axis, Factor A mapped to colour,
# Factor C mapped to linetype (solid vs dotted).
# Individual participant points are dodged by Factor A × Factor C.
# Mean lines connect across Factor B levels for each Factor A × Factor C
# combination.

# Linetype mapping: first level of Factor C = solid, second = dotted
# Add further linetypes if Factor C has more than two levels
# linetype_map <- c("solid", "dotted")
# names(linetype_map) <- levels(data_long[[FACTOR_C_NAME]])
# 
# plot_theme <- theme_classic(base_size = 16) +
#   theme(
#     axis.title        = element_text(size = 20),
#     axis.text        = element_text(size = 16),
#     legend.position   = "bottom",
#     legend.title      = element_blank(),
#     plot.title        = element_text(face = "bold", size = 18),
#     plot.subtitle     = element_text(size = 14, colour = "grey40")
#   )
# 
# p_main <- ggplot(
#   data_long,
#   aes(x        = .data[[FACTOR_B_NAME]],
#       y        = value,
#       colour   = .data[[FACTOR_A_NAME]],
#       fill     = .data[[FACTOR_A_NAME]],
#       linetype = .data[[FACTOR_C_NAME]])
# ) +
#   # Lines connecting individual participants across Factor B levels,
#   # grouped by participant × Factor A × Factor C
#   geom_line(
#     aes(group = interaction(.data[[ID_COL]],
#                             .data[[FACTOR_A_NAME]],
#                             .data[[FACTOR_C_NAME]])),
#     alpha     = 0.15,
#     linewidth = 0.4,
#     show.legend = FALSE
#   ) +
#   # Individual participant data points, dodged by Factor A × Factor C
#   # geom_beeswarm(
#   #   aes(group = interaction(.data[[FACTOR_A_NAME]], .data[[FACTOR_C_NAME]])),
#   #   alpha       = 0.3,
#   #   size        = 1.8,
#   #   cex         = 2,
#   #   dodge.width = 0.2,
#   #   show.legend = FALSE
#   # ) +
#   # Lines connecting means across Factor B levels,
#   # per combination of Factor A × Factor C
#   geom_line(
#     data = summary_stats,
#     aes(y        = mean,
#         group    = interaction(.data[[FACTOR_A_NAME]], .data[[FACTOR_C_NAME]]),
#         linetype = .data[[FACTOR_C_NAME]]),
#     # position  = position_dodge(width = 0.5),
#     linewidth = 1,
#     show.legend = FALSE
#   ) +
#   # Mean ± 95% CI, dodged by Factor A × Factor C
#   geom_pointrange(
#     data = summary_stats,
#     aes(y        = mean,
#         ymin     = mean - ci95,
#         ymax     = mean + ci95,
#         group    = interaction(.data[[FACTOR_A_NAME]], .data[[FACTOR_C_NAME]]),
#         linetype = .data[[FACTOR_C_NAME]]),
#     # position  = position_dodge(width = 0.5),
#     size      = 0.9,
#     linewidth = 1.2,
#     fatten    = 3,
#     show.legend = FALSE
#   ) +
#   scale_colour_manual(values   = palette_cb) +
#   scale_fill_manual(values     = palette_cb) +
#   scale_x_discrete(expand = expansion(add = 0.2)) +
#   scale_linetype_manual(values = linetype_map,
#     guide = guide_legend(
#         override.aes = list(
#         linetype  = linetype_map,
#         linewidth = 0.5,
#         shape     = NA
#         ),
#     key_width = grid::unit(5, "cm")   # increase for a longer line snippet
#     )
#     ) +
# 
#   labs(
#     # title    = "Precision Error",
#     # subtitle = paste0("Mean ± 95% CI, individual participant points shown (N = ",
#     #                  n_after_range, ")"),
#     x        = "Target Size",
#     y        = "Path Length (cm)",
#     # caption  = paste0("Post-hoc correction: ", toupper(CORRECTION_METHOD),
#     #                   " | Error bars: 95% CI",
#     #                   " | Line type: ", FACTOR_C_NAME,
#     #                   " | Colour: ", FACTOR_A_NAME)
#   ) +
#   plot_theme

# Use this version to facet across Factor C
# Levels of Factor C will appear on side-by-side plots

plot_theme <- theme_classic(base_size = 13) +
  theme(
    strip.background  = element_blank(),
    strip.text        = element_text(face = "bold", size = 13),
    axis.title        = element_text(size = 15),
    axis.text        = element_text(size = 12),
    legend.position   = "bottom",
    legend.title      = element_blank(),
    plot.title        = element_text(face = "bold", size = 14),
    plot.subtitle     = element_text(size = 11, colour = "grey40")
  )

p_main <- ggplot(
  data_long,
  aes(x      = .data[[FACTOR_B_NAME]],
      y      = value,
      colour = .data[[FACTOR_A_NAME]],
      fill   = .data[[FACTOR_A_NAME]])
  ) +
  facet_wrap(as.formula(paste("~", FACTOR_C_NAME)),
             labeller = as_labeller(c("20" = "Distance: Near",
                                      "22.36068"  = "Distance: Far"))
  ) +
  # # Plot horizontal thresholds
  # geom_segment(data = THRESHOLDS,
  #              inherit.aes = FALSE,
  #              aes(x    = -Inf,
  #                  xend = Inf,
  #                  y    = threshold_start,
  #                  yend = threshold_end),
  #              linetype  = "dashed",
  #              colour    = "grey40",
  #              linewidth = 0.7
  # ) +
  # Lines connecting individual participants across Factor B levels
  geom_line(
    aes(group = interaction(.data[[ID_COL]], .data[[FACTOR_A_NAME]])),
    alpha     = 0.25,
    linewidth = 0.4,
    show.legend = FALSE
  ) +
  # Individual participant data points
  # geom_beeswarm(
  #   alpha       = 0.3,
  #   size        = 1.8,
  #   cex         = 2,
  #   dodge.width = 0.2,
  #   show.legend = FALSE
  # ) +
  # Mean ± 95% CI
  # Lines connecting means across Factor B levels, per level of Factor A
  geom_line(
    data      = summary_stats,
    aes(y     = mean,
        group = .data[[FACTOR_A_NAME]]),
    # position  = position_dodge(width = 0.5),
    linewidth = 1,
    show.legend = FALSE
  ) +
  geom_pointrange(
    data    = summary_stats,
    aes(y    = mean,
        ymin = mean - ci95,
        ymax = mean + ci95),
    # position  = position_dodge(width = 0.5),
    size      = 0.9,
    linewidth = 1.2,
    fatten    = 3,
    show.legend = FALSE
  ) +
  scale_colour_manual(values = palette_cb) +
  scale_fill_manual(values   = palette_cb) +
  scale_x_discrete(labels = c("Small", "Large"),
                   expand = expansion(add = 0.2)) +
  coord_cartesian(ylim = c(0, 1.8)) +
  scale_y_continuous(breaks = seq(0, 1.8, by = 0.2)) +
  labs(
    # title    = "Precision Error",
    # subtitle = paste0("Mean ± 95% CI, individual participant points shown (N = ",
    #                   n_after_range, ")"),
    x        = "\n Target Size",
    y        = "SD of Vertical Error (cm)"
    # y        = expression("Average Acceleration (cm/s"^2*")"),
    # caption  = paste0("Post-hoc correction: ", toupper(CORRECTION_METHOD),
    #                   " | Error bars: 95% CI | Facets: ", FACTOR_C_NAME)
  ) +
  plot_theme

print(p_main)

ggsave(file.path(OUTPUT_DIR, "rm_anova_threeway_plot.png"),
       p_main + theme(legend.position = "bottom"),
       width = 6, height = 6, dpi = 300, bg = "white")

cat(sprintf("\nPlot saved to %s/rm_anova_threeway_plot.png\n", OUTPUT_DIR))
cat("\nDone. All outputs written to:", OUTPUT_DIR, "\n")
