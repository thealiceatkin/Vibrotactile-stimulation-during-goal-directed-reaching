# =============================================================================
# Multi-Panel Figure Assembly with Common Legend
# Loads individual .png panels, extracts a common legend from a source ggplot,
# and arranges everything with a custom layout using patchwork.
# =============================================================================

library(ggplot2)
library(patchwork)
library(magick)  # install.packages("magick")  |  brew install imagemagick (macOS)

# -----------------------------------------------------------------------------
# Helper: load a .png file as a ggplot-compatible raster panel
# -----------------------------------------------------------------------------
load_panel <- function(path) {
  img    <- magick::image_read(path)
  raster <- as.raster(img)
  info   <- magick::image_info(img)
  ratio  <- info$height / info$width
  ggplot() +
    annotation_raster(raster, xmin = -Inf, xmax = Inf,
                              ymin = -Inf, ymax = Inf) +
    coord_cartesian(clip = "off") +
    theme_void() +
    theme(aspect.ratio = ratio)
}

# -----------------------------------------------------------------------------
# Helper: extract just the legend from a ggplot as a standalone grob
# -----------------------------------------------------------------------------
extract_legend <- function(p) {
  g     <- ggplotGrob(p + theme(legend.position = "right"))
  which <- which(sapply(g$grobs, function(x) x$name) == "guide-box")
  if (length(which) == 0) stop("No legend found in the supplied plot.")
  g$grobs[[which]]
}

# =============================================================================
# 1.  Load panel images
#     Replace file paths with your actual .png files.
# =============================================================================

# p1 <- load_panel("/Users/aatkin/Documents/Tactile_Suppression_Study/PSE_Condition_R_Output/N=29/rm_anova_plot_nolegend.png")
# p2 <- load_panel("/Users/aatkin/Documents/Tactile_Suppression_Study/PSE_suppressionEffect_R_Output/Guess+LapseRate_Included/N=29/rm_anova_plot_nolegend.png")

# p1 <- load_panel("/Users/aatkin/Documents/Tactile_Suppression_Study/PSE_Condition_R_Output/N=29/Psychometric_Curve_back_alone_N=29.png")
# p2 <- load_panel("/Users/aatkin/Documents/Tactile_Suppression_Study/PSE_Condition_R_Output/N=29/Psychometric_Curve_hand_alone_N=29.png")
# p3 <- load_panel("/Users/aatkin/Documents/Tactile_Suppression_Study/PSE_Condition_R_Output/N=29/Psychometric_Curve_back_dualtask_N=29.png")
# p4 <- load_panel("/Users/aatkin/Documents/Tactile_Suppression_Study/PSE_Condition_R_Output/N=29/Psychometric_Curve_hand_dualtask_N=29.png")

# p1 <- load_panel("/Users/aatkin/Documents/Tactile_Suppression_Study/Movement_Time/MT_ID/N=29/rm_anova_plot_nolegend.png")
# p2 <- load_panel("/Users/aatkin/Documents/Tactile_Suppression_Study/Precision_Error/Entry_error/Signed_error/rm_anova_threeway_plot_nolegend.png")
# p3 <- load_panel("/Users/aatkin/Documents/Tactile_Suppression_Study/Path_Length/pathLength_targetSize_Distance/N=29/rm_anova_threeway_plot_nolegend.png")

p1 <- load_panel("/Users/aatkin/Documents/Tactile_Suppression_Study/SURG-TLX_MentalDemands/N=29/rm_anova_oneway_plot.png")
p2 <- load_panel("/Users/aatkin/Documents/Tactile_Suppression_Study/SURG-TLX_PhysicalDemands/N=29/rm_anova_oneway_plot.png")
p3 <- load_panel("/Users/aatkin/Documents/Tactile_Suppression_Study/SURG-TLX_Complexity/N=29/rm_anova_oneway_plot.png")

# =============================================================================
# 2.  Build a source plot to extract the common legend from
#
#     This should be a ggplot that reproduces the aesthetics shown across your
#     panels (colours, shapes, linetypes, etc.).  It is never drawn directly —
#     it exists only to supply the legend.
#
#     Adjust the data, aes(), scales, and theme to match your panels exactly.
# =============================================================================

# legend_source <- ggplot(
#     data.frame(Task = factor(c("Reaching", "Back", "Hand"))
#                # Distance = factor(c("Near", "Far", "Near"))
#                ),
#     aes(x = 1, y = 1, colour = Task,
#         # shape = Distance,
#         # linetype = Distance
#         )
#   ) +
#   geom_point(size = 2) +
#   geom_line() +                  # needed to render the linetype legend
#   scale_colour_manual(
#     name   = "Task",                          # legend title
#     breaks = c("Reaching", "Back", "Hand"),  # sets display order
#     values = c(
#                "Reaching" = "#000000",
#                "Back" = "#EE6677",
#                "Hand" = "#4477AA"
#                )
#   ) +
#   scale_fill_manual(
#     name   = "Task",
#     breaks = c("Reaching", "Back", "Hand"),  # sets display order
#     values = c(
#               "Reaching" = "#000000",
#               "Back" = "#EE6677",
#               "Hand" = "#4477AA"
#               )
#   ) +
#   scale_shape_manual(
#     name   = "Task",
#     breaks = c("Reaching", "Back", "Hand"),  # sets display order
#     values = c("Reaching" = 14, "Back" = 14, "Hand" = 14)
#   ) +
#   # scale_linetype_manual(
#   #   name   = "Distance",
#   #   breaks = c("Near", "Far"),  # sets display order
#   #   values = c("Near" = "solid",
#   #              "Far" = "dashed")
#   # ) +
# guides(
#   colour   = guide_legend(order = 1),
#   fill     = guide_legend(order = 1),
#   shape    = guide_legend(order = 1),
#   # linetype = guide_legend(order = 2)
#   ) +
#   theme_classic(base_size = 10) +
#   theme(
#     legend.position     = "right",
#     legend.title        = element_text(face = "bold"),
#     legend.key.size     = unit(0.5, "cm"),
#     legend.text         = element_text(size = 9)
#   )
# 
# legend_grob <- extract_legend(legend_source)
# 
# # Wrap the legend grob so patchwork can place it as a panel
# legend_panel <- wrap_elements(legend_grob) &
#   theme(plot.tag = element_blank())

# =============================================================================
# 3.  Define the custom layout
#
#     Each unique letter = one panel; matching letters across cells = one wider/
#     taller panel.  'L' is reserved here for the legend column on the right.
#     '#' = empty cell.
#
#     Example: A and B share the top row; C spans the full bottom row;
#              L is a narrow legend column on the right of rows 1–2.
#
#     Adjust to match your figure structure.
# =============================================================================
design <- "
ABC
ABC
"

# Fine-tune column widths (one value per column) and row heights (one per row).
# The legend column (L) is kept narrow relative to the data panels.
col_widths  <- c(2, 2, 2)   # one entry per column in the design string
row_heights <- c(1, 1)               # one entry per row

# =============================================================================
# 4.  Assemble the figure
# =============================================================================

# manual tagging

library(grid)

p1 <- p1 + annotation_custom(
  grob = textGrob("A", gp = gpar(fontsize = 10, fontface = "bold")),
  xmin = -Inf, xmax = -Inf,   # left edge
  ymin = Inf,  ymax = Inf     # top edge
) +
  coord_cartesian(clip = "off")

p2 <- p2 + annotation_custom(
  grob = textGrob("B", gp = gpar(fontsize = 10, fontface = "bold")),
  xmin = -Inf, xmax = -Inf,   # left edge
  ymin = Inf,  ymax = Inf     # top edge
) +
  coord_cartesian(clip = "off")

p3 <- p3 + annotation_custom(
  grob = textGrob("C", gp = gpar(fontsize = 10, fontface = "bold")),
  xmin = -Inf, xmax = -Inf,   # left edge
  ymin = Inf,  ymax = Inf     # top edge
) +
  coord_cartesian(clip = "off")

# p4 <- p4 + annotation_custom(
#   grob = textGrob("D", gp = gpar(fontsize = 10, fontface = "bold")),
#   xmin = -Inf, xmax = -Inf,   # left edge
#   ymin = Inf,  ymax = Inf     # top edge
# ) +
#   coord_cartesian(clip = "off")

textGrob("A", just = c("left", "top"),
         gp = gpar(fontsize = 10, fontface = "bold"))
textGrob("B", just = c("left", "top"),
         gp = gpar(fontsize = 10, fontface = "bold"))
textGrob("C", just = c("left", "top"),
         gp = gpar(fontsize = 10, fontface = "bold"))
# textGrob("D", just = c("left", "top"),
#          gp = gpar(fontsize = 10, fontface = "bold"))

figure <- (p1 + p2 + p3
           # + legend_panel
) +
  plot_layout(
    design  = design,
    widths  = col_widths,
    heights = row_heights
  ) +
  plot_annotation(tag_levels = NULL) &
  theme(plot.tag = element_blank())

# automatic tagging
# figure <- (p1 + p2
#            + p3 + legend_panel
#            ) +
#   plot_layout(
#     design  = design,
#     widths  = col_widths,
#     heights = row_heights
#   ) +
#   plot_annotation(tag_levels = "A") &
#   theme(
#     plot.tag          = element_text(size = 13, face = "bold", colour = "black"),
#     # plot.tag.position = "left"
#     plot.tag.position = c(0.05, 0.90)
#   )

# figure[[4]] <- figure[[4]] & theme(plot.tag = element_blank())

print(figure)

# =============================================================================
# 5.  Save
# =============================================================================
# ggsave(
#   filename = "multi_panel_figure.pdf",
#   plot     = figure,
#   width    = 18,      # cm — adjust to your journal's column/page width
#   height   = 12,      # cm
#   units    = "cm",
#   dpi      = 300
# )

ggsave(
  filename = "multi_panel_figure.png",
  plot     = figure,
  width    = 20,
  height   = 20,
  units    = "cm",
  dpi      = 300
)

message("Multi-panel figure saved.")

# =============================================================================
# NOTES
# -----------------------------------------------------------------------------
# Legend position alternatives:
#   Bottom strip  — add a row at the bottom of the design string and set the
#                   legend panel to span full width, e.g.:
#                     design <- "AABB\nCCCC\nLLLL"
#                     row_heights <- c(1, 1, 0.15)
#
# If extract_legend() returns an error ("No legend found"):
#   - Check that legend_source actually produces a visible legend when plotted.
#   - Some ggplot2 versions name the guide grob "guide-box-right" etc.
#     Use this variant to find it:
#       which(grepl("guide-box", sapply(g$grobs, function(x) x$name)))
#
# Tag levels:
#   "A"  → A, B, C …
#   "a"  → a, b, c …
#   "1"  → 1, 2, 3 …
#   "i"  → i, ii, iii …
#   To suppress tags on the legend panel, wrap it with:
#     legend_panel & theme(plot.tag = element_blank())
# =============================================================================
