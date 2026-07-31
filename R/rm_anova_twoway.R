# =============================================================================
# Two-Way Repeated Measures ANOVA with Multiple Comparison Corrections
# =============================================================================
# Expected CSV format:
#   - Rows    = participants (one row per participant)
#   - Columns = participant_id, then one column per condition
#
# Example header:
#   participant_id, A_time1, A_time2, B_time1, B_time2
#
# Two within-subject factors are encoded in column names, separated by "_":
#   Factor 1 = "factor_a" (e.g. "A", "B")
#   Factor 2 = "factor_b" (e.g. "time1", "time2")
# Adjust CONFIGURATION below to match your actual column naming scheme.
# =============================================================================


# ── 0. Install / load packages ───────────────────────────────────────────────

required_packages <- c(
  "tidyverse",  # data wrangling + ggplot2
  "afex",       # aov_ez() for RM ANOVA (handles sphericity automatically)
  "emmeans",    # estimated marginal means + post-hoc comparisons
  "effectsize", # eta_squared(), cohens_d()
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

# Names for the two within-subject factors (derived from column name parts)
# Column names must follow the pattern: <level_factorA>_<level_factorB>
# e.g. "A_time1", "A_time2", "B_time1", "B_time2"
FACTOR_A_NAME <- "Location"    # name you want for factor 1
FACTOR_B_NAME <- "Condition"   # name you want for factor 2

# Set the order of levels for Factors A and B (for plotting purposes)
FACTOR_A_LEVELS <- c("Motor", "Back", "Hand")
FACTOR_B_LEVELS <- c("4.3923", "4.5460", "5.3576", "5.5148") # set your desired order here

# Value filtering: exclude participants with any value outside [MIN_VAL, MAX_VAL]
# Set to -Inf / Inf to skip range filtering
MIN_VAL <- -Inf
MAX_VAL <- Inf

# Multiple-comparison correction method for post-hoc tests
# Options: "holm" (recommended), "bonferroni", "BH" (Benjamini-Hochberg / FDR)
CORRECTION_METHOD <- "holm"

# Output directory for saved plots and tables
OUTPUT_DIR <- "/Users/aatkin/anova_output"


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
# Assumes column names follow the pattern: <factorA_level>_<factorB_level>
# e.g. "A_time1" → factor_a = "A", factor_b = "time1"

data_long <- data_filtered %>%
  pivot_longer(
    cols      = all_of(condition_cols),
    names_to  = c(FACTOR_A_NAME, FACTOR_B_NAME),
    names_sep = "_",         # <── change if your separator differs
    values_to = "value"
  ) %>%
  mutate(
    !!sym(ID_COL)        := factor(.data[[ID_COL]]),
    !!sym(FACTOR_A_NAME) := factor(.data[[FACTOR_A_NAME]]),
    !!sym(FACTOR_A_NAME) := factor(.data[[FACTOR_A_NAME]], levels = FACTOR_A_LEVELS),
    !!sym(FACTOR_B_NAME) := factor(.data[[FACTOR_B_NAME]]),
    !!sym(FACTOR_B_NAME) := factor(.data[[FACTOR_B_NAME]], levels = FACTOR_B_LEVELS)
  )

cat("\nLong-format preview:\n")
print(head(data_long, 10))
cat("\nFactor levels:\n")
cat(FACTOR_A_NAME, ":", levels(data_long[[FACTOR_A_NAME]]), "\n")
cat(FACTOR_B_NAME, ":", levels(data_long[[FACTOR_B_NAME]]), "\n\n")

# ── 4b. Descriptive statistics ───────────────────────────────────────────────

desc_stats <- data_long %>%
  group_by(.data[[FACTOR_A_NAME]], .data[[FACTOR_B_NAME]]) %>%
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

# Marginal stats for Factor A only, collapsed across Factor B
desc_stats_A <- data_long %>%
  group_by(.data[[FACTOR_A_NAME]]) %>%
  summarise(
    n     = n(),
    mean  = mean(value, na.rm = TRUE),
    sd    = sd(value, na.rm = TRUE),
    min   = min(value, na.rm = TRUE),
    max   = max(value, na.rm = TRUE),
    range = max - min,
    .groups = "drop"
  )

cat("\n=== Descriptive Statistics:", FACTOR_A_NAME, "(collapsed across", FACTOR_B_NAME, ") ===\n")
print(desc_stats_A)

write_csv(desc_stats_A, file.path(OUTPUT_DIR, "descriptive_stats_A.csv"))

# Marginal stats for Factor B only, collapsed across Factor A
desc_stats_B <- data_long %>%
  group_by(.data[[FACTOR_B_NAME]]) %>%
  summarise(
    n     = n(),
    mean  = mean(value, na.rm = TRUE),
    sd    = sd(value, na.rm = TRUE),
    min   = min(value, na.rm = TRUE),
    max   = max(value, na.rm = TRUE),
    range = max - min,
    .groups = "drop"
  )

cat("\n=== Descriptive Statistics:", FACTOR_B_NAME, "(collapsed across", FACTOR_A_NAME, ") ===\n")
print(desc_stats_B)

write_csv(desc_stats_B, file.path(OUTPUT_DIR, "descriptive_stats_B.csv"))


# ── 5. Two-way repeated measures ANOVA ──────────────────────────────────────
# afex::aov_ez() automatically:
#   • applies the Greenhouse-Geisser sphericity correction when needed
#   • computes partial eta-squared effect sizes

cat("Running two-way RM ANOVA...\n")

model <- aov_ez(
  id      = ID_COL,
  dv      = "value",
  data    = data_long,
  within  = c(FACTOR_A_NAME, FACTOR_B_NAME),
  anova_table = list(correction = "GG", es = "pes")  # GG = Greenhouse-Geisser
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

emm <- emmeans(model,
               specs = as.formula(paste("~", FACTOR_A_NAME, "*", FACTOR_B_NAME)))

# All pairwise comparisons across both factors
posthoc <- pairs(emm, adjust = CORRECTION_METHOD)
cat("\nAll pairwise comparisons:\n")
print(posthoc)

# Simple effects: effect of Factor B at each level of Factor A
simple_fx <- emmeans(model,
                     specs  = as.formula(paste("pairwise ~", FACTOR_B_NAME, "|", FACTOR_A_NAME)),
                     adjust = CORRECTION_METHOD)
cat(sprintf("\nSimple effects of %s at each level of %s:\n", FACTOR_B_NAME, FACTOR_A_NAME))
print(simple_fx$contrasts)

# Save post-hoc tables
write_csv(as.data.frame(posthoc),
          file.path(OUTPUT_DIR, "posthoc_all_pairs.csv"))
write_csv(as.data.frame(simple_fx$contrasts),
          file.path(OUTPUT_DIR, "posthoc_simple_effects.csv"))
cat(sprintf("Post-hoc tables saved to %s/\n", OUTPUT_DIR))


# ── 6b-1. Planned comparisons for a single factor ────────────────────────────
# Edit FACTOR_A_NAME to whichever factor you want to test,
# and update the contrast vectors to match its levels.

# emm_planned <- emmeans(model,
#                        specs = as.formula(paste("~", FACTOR_B_NAME)))
# 
# # Print to confirm level ordering before defining contrasts
# cat("\nEstimated marginal means for", FACTOR_B_NAME, ":\n")
# print(emm_planned)
# 
# # Define contrasts as a named list.
# # Each vector must have one element per level of FACTOR_A_NAME, summing to zero.
# # Examples below assume three levels: "A", "B", "C" — edit to match your data.
# 
# planned_contrasts <- list(
#     "1 vs 2"       = c( 1, -1, 0),
#     "1 vs 3"       = c( 1, 0, -1),
#     "2 vs 3"        = c( 0, 1, -1),
#     "1 vs 2+3"  = c( 1, -0.5, -0.5)
# )
# 
# # planned_contrasts <- list(
# #   "4.3923 vs 4.5460"    = c( 1, -1, 0, 0 ),   # compare first level against second
# #   "4.3923 vs 5.3576"    = c( 1, 0, -1, 0 ),   # compare first level against third
# #   "4.3923 vs 5.5148"    = c( 0, 0, 0, -1 ),   # compare second level against third
# #   "4.5460 vs 5.3576"    = c( 0, 1, -1, 0 ),
# #   "4.5460 vs 5.5148"    = c( 0, 1, 0, -1 ),
# #   "5.3576 vs 5.5148"    = c( 0, 0, 1, -1 ),
# #   "4.3923+4.5460 vs 5.3576+5.5148"  = c( 0.5, 0.5, -0.5, -0.5)  # compare first level against average of second and third
# # )
# 
# # Apply contrasts
# # Set adjust = "none" if comparisons are fully independent and a priori justified,
# # or keep CORRECTION_METHOD for a modest correction across the planned set
# planned_results <- contrast(emm_planned,
#                             method = planned_contrasts,
#                             adjust = CORRECTION_METHOD)
# 
# cat(sprintf("\n=== Planned comparisons: %s (correction: %s) ===\n",
#             FACTOR_B_NAME, CORRECTION_METHOD))
# print(summary(planned_results))
# 
# # Save
# write_csv(as.data.frame(planned_results),
#           file.path(OUTPUT_DIR, "planned_comparisons.csv"))
# cat(sprintf("Planned comparisons saved to %s/planned_comparisons.csv\n", OUTPUT_DIR))


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

# Partial eta-squared is already in the ANOVA table (column 'pes')
cat("Partial η² from ANOVA table (see anova_table.csv)\n")

# Cohen's d for each post-hoc pair
cohens_d_tbl <- as.data.frame(posthoc) %>%
  rowwise() %>%
  mutate(
    cohens_d = tryCatch({
      lvls   <- strsplit(as.character(contrast), " - ")[[1]]
      vals_1 <- data_long$value[data_long[[FACTOR_A_NAME]] == trimws(strsplit(lvls[1], " ")[[1]][1]) &
                                  data_long[[FACTOR_B_NAME]] == trimws(strsplit(lvls[1], " ")[[1]][2])]
      vals_2 <- data_long$value[data_long[[FACTOR_A_NAME]] == trimws(strsplit(lvls[2], " ")[[1]][1]) &
                                  data_long[[FACTOR_B_NAME]] == trimws(strsplit(lvls[2], " ")[[1]][2])]
      effsize::cohen.d(vals_1, vals_2)$estimate
    }, error = function(e) NA_real_)
  ) %>%
  ungroup()

cat("Cohen's d for post-hoc pairs:\n")
print(cohens_d_tbl[, c("contrast", "estimate", "p.value", "cohens_d")])
write_csv(cohens_d_tbl, file.path(OUTPUT_DIR, "effect_sizes.csv"))


# ── 8. Summary statistics for plotting ──────────────────────────────────────

summary_stats <- data_long %>%
  group_by(.data[[FACTOR_A_NAME]], .data[[FACTOR_B_NAME]]) %>%
  summarise(
    n    = n(),
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value, na.rm = TRUE),
    se   = sd / sqrt(n),
    ci95 = qt(0.975, df = n - 1) * se,   # 95% CI half-width
    .groups = "drop"
  )

cat("\nSummary statistics:\n")
print(summary_stats)


# ── 9. Plot ──────────────────────────────────────────────────────────────────
# Shows: mean ± 95% CI (bar + error bar) + individual participant points (beeswarm)

# Colour palette
palette_cb <- c("#000000", "#EE6677", "#4477AA")

plot_theme <- theme_classic(base_size = 16) +
  theme(
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold", size = 16),
    axis.title       = element_text(size = 26),
    axis.text        = element_text(size = 20),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    legend.text      = element_text(size = 16),
    plot.title       = element_text(face = "bold", size = 18),
    plot.subtitle    = element_text(size = 14, colour = "grey40"),
    plot.margin = margin(34, 40, 34, 34, unit = "pt")
  )

# Main plot: one facet per level of Factor A
p_main <- ggplot(
  data_long,
  aes(x = .data[[FACTOR_B_NAME]],
      y = value,
      colour = .data[[FACTOR_A_NAME]],
      fill   = .data[[FACTOR_A_NAME]])
) +
  # Individual participant data points
  # geom_beeswarm(
  #   aes(group = .data[[FACTOR_A_NAME]]),
  #   alpha      = 0.3,
  #   size       = 1.8,
  #   cex        = 2,
  #   dodge.width = 0.2,
  #   show.legend = FALSE
  # ) +
  # Line showing the intercept of Y = 0
  # geom_hline(yintercept = 0,
  #            linetype   = "dashed",
  #            colour     = "grey40",
  #            linewidth  = 0.7) +
  # Lines connecting individual participants across Factor B levels
  geom_line(
    aes(group = interaction(.data[[ID_COL]], .data[[FACTOR_A_NAME]])),
    alpha      = 0.25,
    linewidth  = 0.4,
    show.legend = FALSE
  ) +
  # Mean ± 95% CI
  geom_pointrange(
    data    = summary_stats,
    aes(y    = mean,
        ymin = mean - ci95,
        ymax = mean + ci95),
    # position    = position_dodge(width = 0.25),
    size        = 0.9,
    linewidth   = 1.2,
    fatten      = 3,
    show.legend = FALSE
  ) +
  # Connecting line between means
  geom_line(
    data     = summary_stats,
    aes(y    = mean,
        group = .data[[FACTOR_A_NAME]]),
    # position  = position_dodge(width = 0.5),
    linewidth = 1,
    show.legend = FALSE
  ) +
  scale_colour_manual(values = palette_cb) +
  scale_fill_manual(values   = palette_cb) +
  scale_x_discrete(labels = c("4.39\n Target = Large\n Distance = Near", "4.54\n Target = Large\n Distance = Far", "5.35\n Target = Small\n Distance = Near", "5.51\n Target = Small\n Distance = Far"),
  # scale_x_discrete(labels = c("Pre-Onset", "At Onset", "During Movement"),
                              expand = expansion(add = 0.2)) +
  coord_cartesian(ylim = c(0.6, 1.8)) +
  scale_y_continuous(breaks = seq(0.6, 1.8, by = 0.2)) +
  labs(
    # title    = "Movement Time",
    # subtitle = paste0("Mean ± 95% CI, individual participant points shown (N = ",
                      # n_after_range, ")"),
    x        = "\n Index of Difficulty",
    y        = "Movement Time (s)"
    # y        = expression("Average Acceleration (cm/s"^2*")")
    # caption  = paste0("Post-hoc correction: ", toupper(CORRECTION_METHOD),
                      # " | Error bars: 95% CI")
  ) +
  plot_theme

print(p_main)

ggsave(file.path(OUTPUT_DIR, "rm_anova_plot.png"),
       p_main + theme(legend.position = "bottom"),
       width = 10, height = 10, dpi = 300, bg = "white")

# ── Interaction plot (Factor A × Factor B means only, cleaner overview) ──────

p_interaction <- ggplot(
  summary_stats,
  aes(x      = .data[[FACTOR_B_NAME]],
      y      = mean,
      colour = .data[[FACTOR_A_NAME]],
      group  = .data[[FACTOR_A_NAME]])
) +
  geom_line(linewidth = 1.2) +
  geom_errorbar(
    aes(ymin = mean - ci95, ymax = mean + ci95),
    width    = 0.15,
    linewidth = 0.8
  ) +
  geom_point(size = 3.5) +
  scale_colour_manual(values = palette_cb) +
  scale_x_discrete(labels = c("Alone", "Dual"),
                   expand = expansion(add = 0.4)) +
  coord_cartesian(ylim = c(0.5, 2)) +
  labs(
    # title    = "Interaction plot",
    # subtitle = "Means ± 95% CI",
    x        = "Condition",
    y        = "PSE (a.u.)"
    # y        = expression("Average Acceleration (cm/s"^2*")")
  ) +
  plot_theme

print(p_interaction)

ggsave(file.path(OUTPUT_DIR, "interaction_plot.png"),
       p_interaction, width = 6, height = 4.5, dpi = 300, bg = "white")

cat(sprintf("\nPlots saved to %s/\n", OUTPUT_DIR))
cat("\nDone. All outputs written to:", OUTPUT_DIR, "\n")
