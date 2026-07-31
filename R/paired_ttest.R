# =============================================================================
# Paired-Samples t-Test
# =============================================================================
# Reads two numeric columns from a CSV, runs a paired t-test, prints a tidy
# summary, saves results to CSV, and produces a paired-point plot matching
# the formatting of correlation_analysis.R.
# =============================================================================


# --- 1. Load Data from CSV ---------------------------------------------------

#' Read two numeric columns from a CSV file.
#'
#' @param file      Character – path to the CSV file (e.g. "data/scores.csv").
#' @param col1      Character or integer – name or index of the first column.
#' @param col2      Character or integer – name or index of the second column.
#' @param header    Logical – does the CSV have a header row? (default TRUE)
#' @param sep       Character – field separator (default ",").
#' @param na_action Character – "warn" (default) drops incomplete rows with a
#'                  message; "stop" aborts on any NA.
#' @return A list with elements $x, $y (numeric vectors) and $col_names.

load_csv <- function(file,
                     col1      = 1,
                     col2      = 2,
                     header    = TRUE,
                     sep       = ",",
                     na_action = "warn") {

  if (!file.exists(file)) stop("File not found: ", file)

  df <- read.csv(file, header = header, sep = sep, stringsAsFactors = FALSE)

  resolve_col <- function(ref) {
    if (is.numeric(ref)) {
      if (ref < 1 || ref > ncol(df))
        stop("Column index ", ref, " is out of range (1–", ncol(df), ").")
      return(ref)
    }
    idx <- match(ref, names(df))
    if (is.na(idx)) stop("Column '", ref, "' not found. Available: ",
                          paste(names(df), collapse = ", "))
    idx
  }

  i1 <- resolve_col(col1)
  i2 <- resolve_col(col2)
  col_names <- names(df)[c(i1, i2)]

  x <- suppressWarnings(as.numeric(df[[i1]]))
  y <- suppressWarnings(as.numeric(df[[i2]]))

  na_rows <- which(is.na(x) | is.na(y))
  if (length(na_rows) > 0) {
    if (na_action == "stop") {
      stop("NA values found in rows: ", paste(na_rows, collapse = ", "))
    } else {
      message(sprintf("Note: %d row(s) with NA values removed (rows: %s).",
                      length(na_rows), paste(na_rows, collapse = ", ")))
      x <- x[-na_rows]
      y <- y[-na_rows]
    }
  }

  if (!is.numeric(x) || !is.numeric(y))
    stop("Selected columns must be numeric.")

  message(sprintf("Loaded %d observations from '%s'  [%s | %s]",
                  length(x), basename(file), col_names[1], col_names[2]))

  list(x = x, y = y, col_names = col_names)
}


# -----------------------------------------------------------------------------
# SET YOUR CSV PATH AND COLUMN NAMES HERE
# -----------------------------------------------------------------------------
csv_file <- "/Users/aatkin/Documents/Tactile_Suppression_Study/Signal_Detection/dprime/dprime_results.csv"   # <-- path to your CSV file
col1     <- "dprime_back"                 # <-- first column:  name (e.g. "Pre") or index
col2     <- "dprime_hand"                 # <-- second column: name (e.g. "Post") or index
# -----------------------------------------------------------------------------

data <- load_csv(csv_file, col1 = col1, col2 = col2)
var1 <- data$x
var2 <- data$y

# Column names used as default axis labels (override in plot_paired_t() if needed)
default_x_label <- data$col_names[1]
default_y_label <- data$col_names[2]


# --- 2. Paired t-Test Function -----------------------------------------------

#' Run a paired-samples t-test and print a tidy summary.
#'
#' @param x          Numeric vector – first condition.
#' @param y          Numeric vector – second condition.
#' @param conf_level Numeric – confidence level for the CI (default 0.95).
#' @param alternative Character – "two.sided" (default), "less", or "greater".
#' @return Invisibly returns a one-row data frame of results for CSV export.

run_paired_ttest <- function(x, y,
                             conf_level  = 0.95,
                             alternative = "two.sided") {
  if (length(x) != length(y)) stop("x and y must have the same length.")
  if (length(x) < 3)          stop("At least 3 observations are required.")

  result <- t.test(x, y,
                   paired      = TRUE,
                   conf.level  = conf_level,
                   alternative = alternative)

  d       <- x - y
  cohens_d <- mean(d) / sd(d)   # Cohen's d for paired design

  cat("=============================================\n")
  cat(" Paired-Samples t-Test\n")
  cat("=============================================\n")
  cat(sprintf(" n              : %d\n",     length(x)))
  cat(sprintf(" Mean diff (x-y): %.4f\n",  mean(d)))
  cat(sprintf(" SD diff        : %.4f\n",  sd(d)))
  cat(sprintf(" t              : %.4f\n",  result$statistic))
  cat(sprintf(" df             : %.0f\n",  result$parameter))
  cat(sprintf(" %d%% CI        : [%.4f, %.4f]\n",
              round(conf_level * 100),
              result$conf.int[1],
              result$conf.int[2]))
  cat(sprintf(" p-value        : %s\n",
              ifelse(result$p.value < 0.001, "< .001",
                     sprintf("%.4f", result$p.value))))
  cat(sprintf(" Cohen's d      : %.4f\n",  cohens_d))
  cat(sprintf(" Alternative    : %s\n",    alternative))
  cat("=============================================\n\n")

  out <- data.frame(
    n            = length(x),
    mean_diff    = round(mean(d), 4),
    sd_diff      = round(sd(d), 4),
    t            = round(unname(result$statistic), 4),
    df           = unname(result$parameter),
    ci_lower     = round(result$conf.int[1], 4),
    ci_upper     = round(result$conf.int[2], 4),
    p_value      = round(result$p.value, 4),
    cohens_d     = round(cohens_d, 4),
    alternative  = alternative,
    stringsAsFactors = FALSE
  )

  invisible(out)
}


# --- 2b. Save Results to CSV -------------------------------------------------

#' Save the data frame returned by run_paired_ttest() to a CSV file.
#'
#' @param results   Data frame returned by run_paired_ttest().
#' @param save_path Character – full file path for the output CSV.

save_results <- function(results, save_path) {
  dir.create(dirname(save_path), showWarnings = FALSE, recursive = TRUE)
  write.csv(results, file = save_path, row.names = FALSE)
  message("Results saved to: ", save_path)
}


# --- 3. Plot Function --------------------------------------------------------

#' Plot individual data points connected across two conditions (paired design).
#' Subtitle shows t-statistic, df, and p-value from the paired t-test.
#'
#' @param x            Numeric vector – first condition.
#' @param y            Numeric vector – second condition.
#' @param x_label      Character – label for the first condition.
#' @param y_label      Character – label for the second condition.
#' @param colors       Character vector of length 2: point colours for each
#'                     condition. Defaults to teal/coral.
#' @param point_size   Numeric – size of individual data points (default 3).
#' @param alpha        Numeric [0,1] – transparency of connecting lines (default 0.5).
#' @param title        Character – plot title.
#' @param show_mean    Logical – overlay group means as diamonds (default TRUE).
#' @param connect_mean Logical – connect the two mean diamonds with a dashed
#'                     line (default TRUE).
#' @param save_path    Character or NULL – file path to save the plot (.png,
#'                     .pdf, or .svg); NULL displays only.
#' @param width        Numeric – saved plot width in inches (default 6).
#' @param height       Numeric – saved plot height in inches (default 5).

plot_paired_t <- function(x, y,
                          x_label      = "Condition 1",
                          y_label      = "Condition 2",
                          colors       = c("#EE6677", "#4477AA"),   # teal, coral
                          point_size   = 1.8,
                          alpha        = 0.2,
                          title        = "Paired-Samples t-Test",
                          show_mean    = TRUE,
                          connect_mean = TRUE,
                          save_path    = NULL,
                          width        = 6,
                          height       = 5) {

  if (length(x) != length(y)) stop("x and y must have the same length.")
  if (length(colors) < 2)     stop("'colors' must contain at least 2 colour values.")

  n   <- length(x)
  ids <- seq_len(n)

  # Axis range with padding
  padding  <- diff(range(c(x, y), na.rm = TRUE)) * 0.08
  y_limits <- c(min(c(x, y), na.rm = TRUE) - padding,
                max(c(x, y), na.rm = TRUE) + padding)

  # Line colour with transparency
  line_col <- adjustcolor(colors[2], alpha.f = alpha)

  # t-test subtitle
  tt    <- t.test(x, y, paired = TRUE)
  p_str <- ifelse(tt$p.value < .001, "p < .001", sprintf("p = %.3f", tt$p.value))
  subtitle <- sprintf("t(%g) = %.3f, %s", tt$parameter, tt$statistic, p_str)

  # ---- Drawing ---------------------------------------------------------------
  do_plot <- function() {
    par(mar = c(4, 4.5, 3.5, 1.5), bg = "white", family = "sans")

    plot(NULL,
         xlim     = c(0.6, 2.4),
         ylim     = y_limits,
         xaxt     = "n",
         xlab     = "",
         ylab     = "d'",
         main     = "Sensitivity",
         sub      = subtitle,
         cex.main = 1.2,
         cex.sub  = 0.9,
         col.sub  = "grey40",
         las      = 1)

    axis(1, at = 1:2, labels = c(x_label, y_label), cex.axis = 1.05)
    grid(nx = NA, ny = NULL, col = "grey90", lty = 1)

    # Individual connecting lines
    for (i in ids) {
      lines(c(1, 2), c(x[i], y[i]), col = line_col, lwd = 1.2)
    }

    # Data points
    points(rep(1, n), x, pch = 21, bg = colors[1], col = "white",
           cex = point_size, lwd = 0.8)
    points(rep(2, n), y, pch = 21, bg = colors[2], col = "white",
           cex = point_size, lwd = 0.8)

    # Optional mean diamonds
    if (show_mean) {
      mx <- mean(x, na.rm = TRUE)
      my <- mean(y, na.rm = TRUE)

      if (connect_mean) {
        lines(c(1, 2), c(mx, my), col = "grey30", lwd = 2, lty = 2)
      }

      points(1, mx, pch = 23, bg = "white", col = colors[1],
             cex = point_size * 1.6, lwd = 2)
      points(2, my, pch = 23, bg = "white", col = colors[2],
             cex = point_size * 1.6, lwd = 2)

      legend_labels <- if (connect_mean) c("Group mean", "Mean change") else "Group mean"
      legend_pch    <- if (connect_mean) c(23, NA)                       else 23
      legend_lty    <- if (connect_mean) c(NA, 2)                        else NA
      legend_col    <- if (connect_mean) c("grey40", "grey30")           else "grey40"

      legend("topleft",
             legend  = legend_labels,
             pch     = legend_pch,
             lty     = legend_lty,
             pt.bg   = "white",
             col     = legend_col,
             pt.cex  = 1.4,
             lwd     = 2,
             bty     = "n",
             cex     = 0.85)
    }
  }

  # ---- Display ---------------------------------------------------------------
  do_plot()   # always render to the active graphics device (screen)

  # ---- Save (if path given) --------------------------------------------------
  if (!is.null(save_path)) {
    ext <- tolower(tools::file_ext(save_path))
    if      (ext == "pdf") pdf(save_path, width = width, height = height)
    else if (ext == "svg") svg(save_path, width = width, height = height)
    else                   png(save_path, width = width, height = height,
                               units = "in", res = 150)
    do_plot()
    dev.off()
    message("Plot saved to: ", save_path)
  }

  invisible(NULL)
}


# --- 4. Run ------------------------------------------------------------------

# Derive output paths from the input file location
out_dir   <- dirname(normalizePath(csv_file, mustWork = FALSE))
base_name <- tools::file_path_sans_ext(basename(csv_file))

results_path <- file.path(out_dir, paste0(base_name, "_ttest.csv"))
plot_path    <- file.path(out_dir, paste0(base_name, "_plot.png"))

# Run t-test, print summary, and save CSV
ttest_results <- run_paired_ttest(var1, var2)
save_results(ttest_results, results_path)

# Produce and save plot
plot_paired_t(
  x            = var1,
  y            = var2,
  x_label      = "Back",   # auto-filled from CSV column name; override if needed
  y_label      = "Hand",   # auto-filled from CSV column name; override if needed
  colors     = c("#EE6677", "#4477AA"),   # teal, coral
  point_size   = 1.8,
  alpha        = 0.2,
  title        = paste(default_x_label, "vs", default_y_label),
  show_mean    = TRUE,
  connect_mean = TRUE,
  save_path    = plot_path
)
