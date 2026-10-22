# ============================================================
# Reporting helpers
# Purpose: Reuse common formatting and reporting tasks so the
#          analysis output is consistent and easy to read.
#
# Contents:
#   1. Console layout (headings, wrapped text, APA paragraphs)
#   2. APA number formatting
#   3. Printing/saving tables and plots
#
# APA paragraphs are *returned* (not stored in a global variable),
# so each analysis function can hand back its own write-up.
# ============================================================


## 1. Console layout ------------------------------------------------

LINE_WIDTH <- 78

# Wrap long text across multiple console lines
wrap_text <- function(text, indent = 0, exdent = 0) {
  paste(strwrap(text, width = LINE_WIDTH,
                indent = indent, exdent = exdent),
        collapse = "\n")
}

# Print a prominent heading for a major analysis section.
section <- function(title) {
  cat("\n\n", strrep("=", LINE_WIDTH), "\n", title, "\n",
      strrep("=", LINE_WIDTH), "\n", sep = "")
}

# Print a smaller heading with an underline.
subheading <- function(text) {
  cat("\n", text, "\n", strrep("-", nchar(text)), "\n", sep = "")
}

#' Print the header of a research question
#'
#' @param number Research-question number (shown as "RESEARCH QUESTION n").
#' @param question The question as a sentence.
#' @return Invisibly, the header lines for the APA text file.
rq_header <- function(number, question) {
  banner <- strrep("=", LINE_WIDTH)
  cat("\n\n", banner, "\nRESEARCH QUESTION ", number, "\n",
      question, "\n", banner, "\n", sep = "")
  invisible(c(banner,
              paste0("RESEARCH QUESTION ", number, ": ", question),
              banner, ""))
}

#' Print the plain-language background of a research question
#'
#' @param explanation What the question means, in one paragraph.
#' @param tests Character vector; one element per analysis that is run.
#' @return Invisibly, NULL (called for its printed output).
rq_background <- function(explanation, tests) {
  subheading("What this question asks")
  cat(wrap_text(explanation), "\n", sep = "")

  subheading("Analyses we will run")
  for (i in seq_along(tests)) {
    cat(wrap_text(paste0(i, ". ", tests[i]), exdent = 3), "\n", sep = "")
  }
  invisible(NULL)
}

results_header <- function() subheading("Results")
apa_header     <- function() subheading("Interpretation (APA style)")

#' Print one APA paragraph (wrapped) and return it
#'
#' @param ... Pieces of text that are pasted together without separator.
#' @return Invisibly, the wrapped paragraph followed by an empty line,
#'   ready to be appended to the APA write-up.
apa_report <- function(...) {
  text <- wrap_text(paste0(...))
  cat(text, "\n\n", sep = "")
  invisible(c(text, ""))
}


## 2. APA number formatting -----------------------------------------

# Common statistical symbols used in APA-style reporting.
apa_sym <- list(
  R2    = "R²",
  dR2   = "ΔR²",
  beta  = "β",
  eta2  = "η²",
  chi2  = "χ²",
  alpha = "α",
  times = "×"
)

# Format a number to a fixed number of decimal places.
fmt_num <- function(x, digits = 2) formatC(x, format = "f", digits = digits)

# Format numbers between -1 and 1 without a leading zero,
# e.g., 0.25 becomes ".250" and -0.25 becomes "-.250".
# Uses fmt_num() to set the number of decimal places, then
# removes the zero immediately before the decimal point.
fmt_bounded <- function(x, digits = 3) {
  sub("^(-?)0\\.", "\\1.", fmt_num(x, digits))
}

# Convert a proportion to a percentage with one decimal place.
fmt_pct <- function(x) paste0(fmt_num(100 * x, 1), "%")

# Degrees of freedom: whole numbers without decimals, Welch-type df
# with two decimals.
fmt_df <- function(x) {
  ifelse(abs(x - round(x)) < 1e-8, as.character(round(x)), fmt_num(x, 2))
}

# Format p-values in APA style, handling missing values and
# reporting p-values below .001 as "p < .001".
fmt_p <- function(p) {
  ifelse(is.na(p), "p = n/a",
         ifelse(p < 0.001, "p < .001",
                paste0("p = ", fmt_bounded(p, 3))))
}

# Format a confidence interval with optional leading-zero removal.
fmt_ci <- function(lo, hi, digits = 2, bounded = FALSE) {
  f <- if (bounded) fmt_bounded else fmt_num
  paste0("[", f(lo, digits), ", ", f(hi, digits), "]")
}

# Choose decimal places based on effect size to avoid rounding
# small values to zero, within the specified limits.
adaptive_digits <- function(x, min_digits = 2, max_digits = 5) {
  x <- abs(x)
  if (is.na(x) || x == 0) return(min_digits)
  min(max_digits, max(min_digits, ceiling(-log10(x)) + 1))
}

# Choose between two values; treat NA conditions as FALSE.
pick <- function(cond, yes, no) if (isTRUE(cond)) yes else no

# Check statistical significance at level `alpha`; NA counts as
# not significant.
is_sig <- function(p, alpha = 0.05) isTRUE(p < alpha)

# Label eta-squared effect sizes using Cohen's benchmarks.
eta_label <- function(x) {
  if (is.na(x)) return("undetermined")
  if (x < 0.01) "negligible" else if (x < 0.06) "small"
  else if (x < 0.14) "medium" else "large"
}

#' Format a regression coefficient for APA-style reporting
#'
#' @param res One-row data frame with columns `estimate`, `conf.low`,
#'   `conf.high`, `statistic` and `p.value` (e.g., a row of robust_tidy()).
#' @param df Residual degrees of freedom shown in t(df).
#' @param digits Decimal places; chosen automatically when NULL.
#' @param scale Multiplier for the coefficient and CI (e.g., 0.01 to report
#'   the effect of a 0.01-unit change). The t-statistic and p are unchanged.
#' @param unit Text appended after the coefficient (e.g., " per 0.01 nWBV").
#' @param include_t Whether to report the t-statistic.
#' @return A character string such as "b = 1.23, 95% CI [...], t(df) = ..., p = ...".
apa_b <- function(res, df, digits = NULL, scale = 1, unit = "",
                  include_t = TRUE) {
  if (is.null(digits)) digits <- adaptive_digits(res$estimate * scale)
  out <- paste0("b = ", fmt_num(res$estimate * scale, digits), unit,
                ", 95% CI ",
                fmt_ci(res$conf.low * scale, res$conf.high * scale, digits))
  if (include_t) out <- paste0(out, ", t(", df, ") = ", fmt_num(res$statistic, 2))
  paste0(out, ", ", fmt_p(res$p.value))
}

#' Format a nested-model comparison F-test for APA-style reporting
#'
#' @param test One-row output of compare_nested().
#' @return A character string such as "F(1, 230) = 4.56, p = .034".
apa_F <- function(test) {
  paste0("F(", test$`Numerator df`, ", ", fmt_df(test$`Denominator df`),
         ") = ", fmt_num(test$`F-statistic`, 2), ", ", fmt_p(test$`p-value`))
}


## 3. Tables and plots ----------------------------------------------

#' Print a table and (optionally) save a CSV copy
#'
#' @param data Data frame to print.
#' @param digits Decimal places shown in the console.
#' @param save_as File name without extension; NULL means "do not save".
#' @param output_dir Folder for the CSV; NULL means "do not save".
#' @return Invisibly, `data`.
print_table <- function(data, digits = 3, save_as = NULL, output_dir = NULL) {
  print(knitr::kable(data, digits = digits, format = "simple",
                     row.names = FALSE))
  if (!is.null(output_dir) && !is.null(save_as)) {
    utils::write.csv(data, file.path(output_dir, paste0(save_as, ".csv")),
                     row.names = FALSE, fileEncoding = "UTF-8")
  }
  invisible(data)
}

#' Show a plot and (optionally) save it as a PNG
#'
#' @param plot A ggplot object.
#' @param name File name without extension.
#' @param output_dir Folder for the PNG; NULL means "do not save".
#' @param width,height Size in inches.
#' @return Invisibly, `plot`.
save_plot <- function(plot, name, output_dir = NULL, width = 8, height = 5.5) {
  print(plot)
  if (!is.null(output_dir)) {
    ggplot2::ggsave(file.path(output_dir, paste0(name, ".png")),
                    plot = plot, width = width, height = height, dpi = 300)
  }
  invisible(plot)
}
