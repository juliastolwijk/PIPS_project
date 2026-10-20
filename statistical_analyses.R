# ============================================================
# OASIS Cross-Sectional MRI Analysis (APA-style reporting)
#
# Purpose: Examine relationships between brain volume,
#          cognitive performance, age, and dementia severity.
#
# Output structure for every research question (RQ):
#   1. The research question as a header
#   2. A short explanation of the question and the tests used
#   3. The results (tables and plots)
#   4. An APA-style interpretation of those results
# All APA paragraphs are also saved to apa_interpretations.txt.
#
# Important:
# - The data are cross-sectional: no causal claims and no
#   statements about change within a person.
# - In OASIS, MMSE, CDR, Educ and SES are only available for
#   older adults, so those conclusions apply to that subgroup.
# ============================================================


# ------------------------------------------------------------
# 0. Configuration
# Purpose: Keep the main analysis settings together in one
#          place so they can be changed easily.
# ------------------------------------------------------------

CONFIG <- list(
  file_name    = "oasis-cross-sectional-5708aa0a98d82080.xlsx",
  output_dir   = "oasis_output",
  save_outputs = TRUE,
  alpha        = 0.05,  # Significance threshold for statistical tests
  min_group_n  = 10,    # CDR groups smaller than this are merged
  nwbv_step    = 0.01,  # Brain-volume increment used to scale reported effects
  age_window   = 5      # Age range (± years) used for RQ3 plot lines
)

# Shorthand for the brain-volume increment used in effect reporting.
STEP <- CONFIG$nwbv_step

# ------------------------------------------------------------
# 1. Install and load required packages
# ------------------------------------------------------------

required_packages <- c(
  "readxl", "dplyr", "tidyr", "tibble", "ggplot2",
  "broom", "knitr", "rstatix", "sandwich", "lmtest"
)

# Identify packages that are not currently installed
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace,
          logical(1), quietly = TRUE)
]

# Install only missing packages
if (length(missing_packages) > 0) {
  message("Installing missing packages: ",
          paste(missing_packages, collapse = ", "))
  install.packages(missing_packages)
}

# Load the packages without displaying their usual startup messages
invisible(lapply(
  required_packages,
  function(pkg) suppressPackageStartupMessages(
    library(pkg, character.only = TRUE)
  )
))

# Create the output folder only when saving is enabled
if (CONFIG$save_outputs) {
  dir.create(CONFIG$output_dir, showWarnings = FALSE,
             recursive = TRUE)
}

cat("\nStarting OASIS analysis...\n")


# ------------------------------------------------------------
# 2. Helper functions
# Purpose: Reuse common formatting and reporting tasks so the
#          analysis output is consistent and easy to read.
# ------------------------------------------------------------

## 2a. Console layout -------------------------------------------

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

# Every APA paragraph printed below is also stored here so the
# complete write-up can be saved to a text file at the end.
apa_lines <- character()

# Header per research question
rq_header <- function(number, question) {
  banner <- strrep("=", LINE_WIDTH)
  cat("\n\n", banner, "\nRESEARCH QUESTION ", number, "\n",
      question, "\n", banner, "\n", sep = "")
  apa_lines <<- c(apa_lines, banner,
                  paste0("RESEARCH QUESTION ", number, ": ", question),
                  banner, "")
}

# Plain-language background: what the question means and which
# tests answer it.
rq_background <- function(explanation, tests) {
  subheading("What this question asks")
  cat(wrap_text(explanation), "\n", sep = "")
  
  subheading("Analyses we will run")
  for (i in seq_along(tests)) {
    cat(wrap_text(paste0(i, ". ", tests[i]), exdent = 3), "\n", sep = "")
  }
}

results_header <- function() subheading("Results")
apa_header     <- function() subheading("Interpretation (APA style)")

# Print one APA paragraph (wrapped) and keep a copy for the text file.
apa_report <- function(...) {
  text <- wrap_text(paste0(...))
  cat(text, "\n\n", sep = "")
  apa_lines <<- c(apa_lines, text, "")
}


## 2b. APA number formatting ------------------------------------

# Common statistical symbols used in APA-style reporting.
apa_sym <- list(
  R2   = "R²",
  dR2  = "ΔR²",
  beta = "β",
  eta2 = "η²",
  chi2 = "χ²",
  times = "×"
)

# Format a number to a fixed number of decimal places.
fmt_num     <- function(x, digits = 2) formatC(x, format = "f", digits = digits)

# Format numbers between -1 and 1 without a leading zero,
# e.g., 0.25 becomes ".250" and -0.25 becomes "-.250".
# Uses fmt_num() to set the number of decimal places, then
# removes the zero immediately before the decimal point.
fmt_bounded <- function(x, digits = 3) {
  sub("^(-?)0\\.", "\\1.", fmt_num(x, digits))
}

# Convert a proportion to a percentage with one decimal place.
fmt_pct     <- function(x) paste0(fmt_num(100 * x, 1), "%")
fmt_df      <- function(x) {
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

# Check statistical significance using the configured alpha level.
is_sig <- function(p) isTRUE(p < CONFIG$alpha)

# Label eta-squared effect sizes using Cohen's benchmarks.
eta_label <- function(x) {
  if (x < 0.01) "negligible" else if (x < 0.06) "small"
  else if (x < 0.14) "medium" else "large"
}

# Format a regression coefficient, confidence interval, and
# optional t-test results for APA-style reporting.
# `scale` rescales the coefficient and CI, not the t-statistic or p-value.
apa_b <- function(res, df, digits = NULL, scale = 1, unit = "",
                  include_t = TRUE) {
  if (is.null(digits)) digits <- adaptive_digits(res$estimate * scale)
  out <- paste0("b = ", fmt_num(res$estimate * scale, digits), unit,
                ", 95% CI ",
                fmt_ci(res$conf.low * scale, res$conf.high * scale, digits))
  if (include_t) out <- paste0(out, ", t(", df, ") = ", fmt_num(res$statistic, 2))
  paste0(out, ", ", fmt_p(res$p.value))
}

# Format a nested-model comparison F-test for APA-style reporting.
apa_F <- function(test) {
  paste0("F(", test$`Numerator df`, ", ", fmt_df(test$`Denominator df`),
         ") = ", fmt_num(test$`F-statistic`, 2), ", ", fmt_p(test$`p-value`))
}


## 2c. Labels, tables and plots ---------------------------------

# Display labels only; the original dataset column names stay unchanged.
# Direction of the OASIS scales: Educ 1 = lowest ... 5 = highest;
# SES 1 = highest ... 5 = lowest.
variable_labels <- c(
  "ID"   = "Participant ID",
  "M/F"  = "Sex",
  "Age"  = "Age (years)",
  "Educ" = "Education Level (1 = lowest, 5 = highest)",
  "SES"  = "Socioeconomic Status (1 = highest, 5 = lowest)",
  "MMSE" = "Mini-Mental State Examination Score",
  "CDR"  = "Clinical Dementia Rating",
  "eTIV" = "Estimated Total Intracranial Volume",
  "nWBV" = "Normalized Whole Brain Volume"
)

# Readable labels for model terms, including centered variables
# and interaction terms.
term_labels <- c(
  "(Intercept)"  = "Intercept",
  "nWBV"         = "Normalized Whole Brain Volume",
  "nWBV_c"       = "Brain Volume (centered)",
  "Age"          = "Age (years)",
  "Age_c"        = "Age (years, centered)",
  "Educ"         = "Education Level (1-5)",
  "SES"          = "Socioeconomic Status (1-5; lower = higher status)",
  "SexMale"      = "Sex (male vs female)",
  "nWBV:Age_c"   = "Brain Volume x Age Interaction",
  "nWBV_c:Age_c" = "Brain Volume x Age Interaction",
  "Age_c:nWBV_c" = "Brain Volume x Age Interaction"
)

# Replaces known names with labels specified above, but will keep the original
# name if there is no specified label, so names will not be empty
lookup_label <- function(x, dictionary) {
  labels <- unname(dictionary[x])
  labels[is.na(labels)] <- x[is.na(labels)]
  labels
}

# Look up labels for dataset variables and model terms
get_variable_label <- function(variable_name) lookup_label(variable_name, variable_labels)
get_term_label     <- function(term) lookup_label(term, term_labels)

# Convert CDR scores to descriptive severity labels
get_cdr_label <- function(cdr_value) {
  dplyr::case_when(
    cdr_value == 0   ~ "CDR 0 - No dementia",
    cdr_value == 0.5 ~ "CDR 0.5 - Very mild/questionable",
    cdr_value == 1   ~ "CDR 1 - Mild",
    cdr_value == 2   ~ "CDR 2 - Moderate",
    cdr_value == 3   ~ "CDR 3 - Severe",
    TRUE ~ paste("CDR", cdr_value)
  )
}

# Print a table and (optionally) save a CSV copy
print_table <- function(data, digits = 3, save_as = NULL) {
  print(knitr::kable(data, digits = digits, format = "simple",
                     row.names = FALSE))
  if (CONFIG$save_outputs && !is.null(save_as)) {
    utils::write.csv(
      data,
      file.path(CONFIG$output_dir, paste0(save_as, ".csv")),
      row.names = FALSE
    )
  }
  invisible(data)
}

# Show a plot and (optionally) save it as a PNG
save_plot <- function(plot, name, width = 8, height = 5.5) {
  print(plot)
  if (CONFIG$save_outputs) {
    ggplot2::ggsave(
      file.path(CONFIG$output_dir, paste0(name, ".png")),
      plot = plot, width = width, height = height, dpi = 300
    )
  }
  invisible(plot)
}


## 2d. Statistical helpers --------------------------------------

# Convert to numeric, but stop if a real value would silently
# become NA 
to_numeric_strict <- function(x, variable) {
  if (is.numeric(x)) return(x)
  x_chr <- trimws(as.character(x))
  x_chr[x_chr == ""] <- NA
  converted <- suppressWarnings(as.numeric(x_chr))
  if (any(!is.na(x_chr) & is.na(converted))) {
    stop("Could not convert all values in column '", variable,
         "' to numeric. Check the original data.", call. = FALSE)
  }
  converted
}

# Create a regression coefficient table with HC3 heteroscedasticity-robust standard
# errors and 95% CIs. Used for every linear model in this script.
robust_tidy <- function(model, model_name = NA_character_,
                        level = 0.95) {
  vc <- sandwich::vcovHC(model, type = "HC3")
  ct <- lmtest::coeftest(model, vcov. = vc)
  ci <- lmtest::coefci(model, vcov. = vc, level = level)
  
  tibble::tibble(
    Model     = model_name,
    term      = rownames(ct),
    estimate  = unname(ct[, 1]),
    std.error = unname(ct[, 2]),
    statistic = unname(ct[, 3]),
    p.value   = unname(ct[, 4]),
    conf.low  = unname(ci[, 1]),
    conf.high = unname(ci[, 2])
  ) %>%
    mutate(Predictor = get_term_label(term))
}

# Compare nested linear models using an HC3 robust Wald F-test.
# Also report the change in R-squared after adding predictors.
compare_nested <- function(small, large, comparison, added) {
  wt <- lmtest::waldtest(
    small, large,
    vcov = function(x) sandwich::vcovHC(x, type = "HC3"),
    test = "F"
  )
  tibble::tibble(
    Comparison = comparison,
    `Additional Predictors` = added,
    `Delta R-squared` =
      summary(large)$r.squared - summary(small)$r.squared,
    `F-statistic` = wt$F[2],
    `Numerator df` = abs(wt$Df[2]),
    `Denominator df` = wt$Res.Df[2],
    `p-value` = wt$`Pr(>F)`[2]
  )
}

# Calculate variance inflation factors (VIFs) to assess
# multicollinearity among a model's predictors.
calc_vif <- function(model) {
  design <- model.matrix(model)[, -1, drop = FALSE]
  if (ncol(design) < 2) return(setNames(1, colnames(design)))
  vapply(seq_len(ncol(design)), function(i) {
    r2 <- summary(lm(design[, i] ~ design[, -i, drop = FALSE]))$r.squared
    1 / (1 - r2)
  }, numeric(1)) %>%
    setNames(colnames(design))
}

# Merge the highest-severity CDR groups until each group has
# at least 10 participants, while retaining at least two groups.
make_cdr_groups <- function(cdr, min_n) {
  values <- sort(unique(cdr))
  sizes  <- vapply(values, function(v) sum(cdr == v), integer(1))
  groups <- as.list(values)
  
  while (length(groups) > 2 && sizes[length(sizes)] < min_n) {
    k <- length(groups)
    groups[[k - 1]] <- c(groups[[k - 1]], groups[[k]])
    groups[[k]] <- NULL
    sizes[k - 1] <- sizes[k - 1] + sizes[k]
    sizes <- sizes[-k]
  }
  
  labels <- vapply(groups, function(v) {
    if (length(v) == 1) {
      get_cdr_label(v)
    } else {
      paste0("CDR >= ", min(v), " - merged (CDR ",
             paste(v, collapse = ", "), ")")
    }
  }, character(1))
  
  index <- integer(length(cdr))
  for (i in seq_along(groups)) index[cdr %in% groups[[i]]] <- i
  
  factor(index, levels = seq_along(groups), labels = labels)
}

# Estimate the nWBV–MMSE slope at selected centered ages from
# an interaction model, using its robust covariance matrix.
# Scale slopes and confidence intervals to the requested nWBV step.
simple_slopes <- function(model, vc, ages_c, step) {
  b <- coef(model)
  main <- "nWBV_c"
  int  <- "nWBV_c:Age_c"
  slope <- b[[main]] + b[[int]] * ages_c
  variance <- vc[main, main] + ages_c^2 * vc[int, int] +
    2 * ages_c * vc[main, int]
  se <- sqrt(variance)
  df <- df.residual(model)
  crit <- qt(0.975, df)
  tibble::tibble(
    Slope = slope * step,
    `Robust SE` = se * step,
    `95% CI Lower` = (slope - crit * se) * step,
    `95% CI Upper` = (slope + crit * se) * step,
    `t-value` = slope / se,
    `p-value` = 2 * pt(-abs(slope / se), df)
  )
}


# ------------------------------------------------------------
# 3. Load and validate the dataset
# ------------------------------------------------------------

# # Find the dataset; allow file selection in interactive sessions.
# Stop with an error if the file is missing in a non-interactive run.
resolve_data_file <- function(file_name) {
  if (file.exists(file_name)) return(file_name)
  if (interactive()) {
    message("Dataset not found in the working directory. ",
            "Please select the Excel file.")
    return(file.choose())
  }
  stop("Dataset not found: ", file_name,
       ". Set CONFIG$file_name or the working directory.",
       call. = FALSE)
}

raw_data <- readxl::read_excel(resolve_data_file(CONFIG$file_name))

# Check that all required columns are present
required_columns <- c("ID", "M/F", "Age", "Educ", "SES",
                      "MMSE", "CDR", "eTIV", "nWBV")
missing_columns <- setdiff(required_columns, names(raw_data))

if (length(missing_columns) > 0) {
  stop("Missing required columns: ",
       paste(missing_columns, collapse = ", "), call. = FALSE)
}

# Convert analysis variables to numeric, stopping if conversion fails.
numeric_variables <- c("Age", "Educ", "SES", "MMSE",
                       "CDR", "eTIV", "nWBV")

oasis_data <- raw_data
for (variable in numeric_variables) {
  oasis_data[[variable]] <- to_numeric_strict(
    oasis_data[[variable]], variable
  )
}

# Set sex as a factor with Female as the reference category.
sex_clean <- toupper(trimws(as.character(oasis_data[["M/F"]])))
oasis_data$Sex <- factor(sex_clean, levels = c("F", "M"),
                         labels = c("Female", "Male"))

# Warn if any sex values are not coded as M or F.
if (any(!is.na(sex_clean) & is.na(oasis_data$Sex))) {
  warning("Some values in 'M/F' were not 'M' or 'F' and were ",
          "set to missing.", call. = FALSE)
}

# Warn if participant IDs are duplicated.
if (anyDuplicated(oasis_data$ID) > 0) {
  warning("Duplicate participant IDs found: ",
          sum(duplicated(oasis_data$ID)), " duplicate row(s).",
          call. = FALSE)
}

cat("\nDataset loaded successfully.\n")
cat("Number of participants:", nrow(oasis_data), "\n")
cat("Number of columns:", ncol(raw_data), "\n")

# Plausibility checks: values outside their documented range
# usually point to data-entry problems.
range_checks <- tibble::tibble(
  Check = c("Age < 0 or > 120", "MMSE outside 0-30",
            "CDR not in {0, 0.5, 1, 2, 3}",
            "nWBV outside 0-1", "Educ outside 1-5",
            "SES outside 1-5"),
  `Rows Flagged` = c(
    sum(oasis_data$Age < 0 | oasis_data$Age > 120, na.rm = TRUE),
    sum(oasis_data$MMSE < 0 | oasis_data$MMSE > 30, na.rm = TRUE),
    sum(!oasis_data$CDR %in% c(0, 0.5, 1, 2, 3) &
          !is.na(oasis_data$CDR)),
    sum(oasis_data$nWBV <= 0 | oasis_data$nWBV >= 1, na.rm = TRUE),
    sum(oasis_data$Educ < 1 | oasis_data$Educ > 5, na.rm = TRUE),
    sum(oasis_data$SES < 1 | oasis_data$SES > 5, na.rm = TRUE)
  )
)

if (any(range_checks$`Rows Flagged` > 0)) {
  warning("Some values fall outside their expected ranges. ",
          "See the data-quality table below.", call. = FALSE)
  print_table(range_checks)
}


# ------------------------------------------------------------
# 4. Descriptive statistics
# ------------------------------------------------------------

section("DESCRIPTIVE STATISTICS")

descriptive_variables <- c("Age", "Educ", "SES", "MMSE",
                           "CDR", "eTIV", "nWBV")

# Calculate descriptive statistics for each variable.
descriptive_stats <- oasis_data %>%
  select(all_of(descriptive_variables)) %>%
  pivot_longer(everything(), names_to = "Variable",
               values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(
    N = sum(!is.na(Value)),           
    Mean = mean(Value, na.rm = TRUE), 
    SD = sd(Value, na.rm = TRUE),     
    Median = median(Value, na.rm = TRUE),
    Min = min(Value, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Variable = factor(Variable, levels = descriptive_variables)) %>%
  arrange(Variable) %>%
  mutate(Variable = get_variable_label(as.character(Variable)))

# Display the table and save a CSV copy if enabled in the configuration above.
print_table(descriptive_stats, save_as = "01_descriptive_statistics")


# ------------------------------------------------------------
# 5. Missing-data assessment
# ------------------------------------------------------------

section("MISSING-DATA ASSESSMENT")

# Count missing values and calculate the percentage missing
# for each variable in the original dataset.
missing_data <- tibble::tibble(
  Variable = names(raw_data),
  Missing_N = vapply(raw_data, function(x) sum(is.na(x)), integer(1))
) %>%
  mutate(
    Variable = get_variable_label(Variable),
    Missing_Percent = round(100 * Missing_N / nrow(raw_data), 2)
  ) %>%
  arrange(desc(Missing_N))

print_table(missing_data, save_as = "02_missing_data")

# In OASIS, cognitive data exist only for older adults, so
# missingness is expected to be related to age. Checking this
# shows which population the cognitive analyses describe.
missingness_by_age <- oasis_data %>%
  mutate(`Cognitive data` = ifelse(
    !is.na(MMSE) & !is.na(CDR), "Available", "Missing"
  )) %>%
  group_by(`Cognitive data`) %>%
  summarise(
    Participants = n(),
    `Mean age` = mean(Age, na.rm = TRUE),
    `Min age` = min(Age, na.rm = TRUE),
    `Max age` = max(Age, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nAge of participants with and without cognitive data:\n")
print_table(missingness_by_age, save_as = "02b_missingness_by_age")

cat("\n", wrap_text(paste(
  "Note: every model below uses participants with complete data on",
  "its own variables, so sample sizes differ between analyses. If",
  "cognitive data are missing mainly for younger adults, results",
  "apply to the older subgroup only."
)), "\n", sep = "")


# ------------------------------------------------------------
# 6. Sample composition
# ------------------------------------------------------------

section("SAMPLE COMPOSITION")

# Count participants by sex.
cat("\nParticipants by sex:\n")
sex_counts <- oasis_data %>% count(Sex, name = "Participants")
print_table(sex_counts, save_as = "03_sex_counts")

# Count participants in each CDR category.
cat("\nParticipants by Clinical Dementia Rating:\n")
cdr_counts <- oasis_data %>%
  filter(!is.na(CDR)) %>%
  count(CDR, name = "Participants") %>%
  mutate(`Dementia Severity Group` = get_cdr_label(CDR)) %>%
  select(`Dementia Severity Group`, Participants)
print_table(cdr_counts, save_as = "03b_cdr_counts")

# Generate an APA-style summary of the overall sample and
# the subgroup with both MMSE and CDR scores.
subheading("Sample description (APA style)")

n_total       <- nrow(oasis_data)
n_sex_known   <- sum(!is.na(oasis_data$Sex))
share_female  <- sum(oasis_data$Sex == "Female", na.rm = TRUE) / n_sex_known
cognitive_sub <- oasis_data %>% filter(!is.na(MMSE), !is.na(CDR))

apa_report(
  "The sample comprised N = ", n_total, " adults (",
  fmt_pct(share_female), " female) aged ",
  min(oasis_data$Age, na.rm = TRUE), " to ",
  max(oasis_data$Age, na.rm = TRUE), " years (M = ",
  fmt_num(mean(oasis_data$Age, na.rm = TRUE), 1), ", SD = ",
  fmt_num(sd(oasis_data$Age, na.rm = TRUE), 1),
  "). Mean normalized whole brain volume (nWBV) was M = ",
  fmt_bounded(mean(oasis_data$nWBV, na.rm = TRUE), 3), " (SD = ",
  fmt_bounded(sd(oasis_data$nWBV, na.rm = TRUE), 3),
  "). Cognitive and clinical measures (MMSE and CDR) were available for n = ",
  nrow(cognitive_sub), " participants (age range ",
  min(cognitive_sub$Age, na.rm = TRUE), " to ",
  max(cognitive_sub$Age, na.rm = TRUE), " years; M = ",
  fmt_num(mean(cognitive_sub$Age, na.rm = TRUE), 1), ", SD = ",
  fmt_num(sd(cognitive_sub$Age, na.rm = TRUE), 1),
  "); analyses involving these measures therefore describe this subgroup."
)

# Store p-values for the three research questions for later
# correction for multiple testing using the Holm method.
p_rq1 <- NA_real_
p_rq2 <- NA_real_
p_rq3 <- NA_real_


# ============================================================
# RESEARCH QUESTION 1
# ============================================================

rq_header(1, "Does brain volume predict cognitive performance?")

rq_background(
  explanation = paste(
    "This question asks whether people with a larger normalized whole",
    "brain volume (nWBV: brain volume relative to intracranial volume, so a lower",
    "value indicates more atrophy) tend to score higher on the",
    "Mini-Mental State Examination (MMSE: 0-30, higher = better",
    "cognition). Age, education, socioeconomic status (SES) and sex are",
    "related to both brain volume and cognition, so we also test whether",
    "any association remains once they are taken into account."
  ),
  tests = c(
    paste("Four nested linear regression models predicting MMSE, adding",
          "predictors step by step: (1) nWBV only, (2) + age,",
          "(3) + education and SES, (4) + sex. All models use the same",
          "complete cases so they can be compared directly."),
    paste("Robust (HC3) Wald F-tests comparing each model with the",
          "previous one (change in R-squared). Robust standard errors are",
          "used because MMSE has a ceiling at 30 and uneven variance."),
    paste("Coefficients with 95% confidence intervals, plus standardized",
          "betas for the final model, to judge the unique contribution",
          "of nWBV."),
    paste("Assumption checks for the final model: multicollinearity (VIF),",
          "heteroscedasticity (Breusch-Pagan), influential cases (Cook's",
          "distance) and residual plots."),
    "A scatter plot of the unadjusted relationship."
  )
)

# Keep participants with complete data for all four models, stopping if sample is too small
regression_data <- oasis_data %>%
  select(MMSE, nWBV, Age, Educ, SES, Sex) %>%
  drop_na()

if (nrow(regression_data) < 30) {
  stop("Too few complete observations for the regression analyses.",
       call. = FALSE)
}

# Fit four nested models, adding potential confounders in stages.
model_names <- c(
  "Model 1: Brain volume only",
  "Model 2: + Age",
  "Model 3: + Education and socioeconomic status",
  "Model 4: + Sex"
)

models <- list(
  lm(MMSE ~ nWBV, data = regression_data),
  lm(MMSE ~ nWBV + Age, data = regression_data),
  lm(MMSE ~ nWBV + Age + Educ + SES, data = regression_data),
  lm(MMSE ~ nWBV + Age + Educ + SES + Sex, data = regression_data)
)
final_model <- models[[4]]

results_header()

# Report the number of participants included in the models.
cat("\nComplete cases used:", nrow(regression_data),
    "of", nrow(oasis_data), "\n")

# ---- Coefficients ----------------------------------------------

# Extract coefficients and HC3 robust confidence intervals
# from each model.
regression_coefficients <- bind_rows(lapply(
  seq_along(models),
  function(i) robust_tidy(models[[i]], model_names[i])
)) %>%
  transmute(
    Model, Predictor,
    Estimate = estimate,
    `Robust SE` = std.error,
    `95% CI Lower` = conf.low,
    `95% CI Upper` = conf.high,
    `p-value` = p.value
  )

cat("\nRegression coefficients (HC3 robust SEs; ",
    "unstandardized, per 1.0 unit of nWBV):\n", sep = "")
print_table(regression_coefficients,
            save_as = "04_regression_coefficients")

# Standardize numeric variables to compare coefficient sizes
# on a common scale.
standardized_data <- regression_data %>%
  mutate(across(c(MMSE, nWBV, Age, Educ, SES),
                ~ as.numeric(scale(.x))))

standardized_model <- lm(
  MMSE ~ nWBV + Age + Educ + SES + Sex,
  data = standardized_data
)

# Report standardized coefficients for the final model.
standardized_coefficients <- robust_tidy(standardized_model) %>%
  filter(term != "(Intercept)") %>%
  transmute(
    Predictor,
    `Standardized Beta` = estimate,
    `95% CI Lower` = conf.low,
    `95% CI Upper` = conf.high,
    `p-value` = p.value
  )

cat("\nStandardized coefficients, Model 4 ",
    "(outcome and numeric predictors in SD units):\n", sep = "")
print_table(standardized_coefficients,
            save_as = "04b_standardized_coefficients")

# ---- Model fit and nested comparisons --------------------------

# Summarize model fit using R-squared, adjusted R-squared,
# AIC, and BIC.
model_fit <- bind_rows(lapply(
  seq_along(models),
  function(i) {
    broom::glance(models[[i]]) %>% mutate(Model = model_names[i])
  }
)) %>%
  transmute(
    Model,
    Participants = nobs,
    R_squared = r.squared,
    Adjusted_R_squared = adj.r.squared,
    AIC, BIC
  )

cat("\nModel fit statistics:\n")
print_table(model_fit, save_as = "05_model_fit")

# Test whether each added set of predictors improves model fit.
model_comparisons <- bind_rows(
  compare_nested(models[[1]], models[[2]],
                 "Model 1 vs Model 2", "Age"),
  compare_nested(models[[2]], models[[3]],
                 "Model 2 vs Model 3",
                 "Education and socioeconomic status"),
  compare_nested(models[[3]], models[[4]],
                 "Model 3 vs Model 4", "Sex")
)

cat("\nNested-model comparisons (robust Wald F-tests):\n")
print_table(model_comparisons, save_as = "05b_model_comparisons")

# ---- Assumption checks (final model) ---------------------------

# Check multicollinearity among the predictors using VIF.
vif_values <- calc_vif(final_model)

cat("\nCollinearity (variance inflation factors, Model 4):\n")
print_table(
  tibble::tibble(
    Predictor = get_term_label(names(vif_values)),
    VIF = unname(vif_values)
  ),
  save_as = "06_vif"
)

# Test for heteroscedasticity
bp_test <- lmtest::bptest(final_model)
cat("\nBreusch-Pagan test for heteroscedasticity: statistic = ",
    round(unname(bp_test$statistic), 2), ", df = ",
    unname(bp_test$parameter), ", p ",
    ifelse(bp_test$p.value < 0.001, "< .001",
           paste0("= ", sprintf("%.3f", bp_test$p.value))),
    "\n", sep = "")

# Flag potentially influential observations using Cook's distance.
augmented <- broom::augment(final_model)
cooks_threshold <- 4 / nrow(augmented)

influence_table <- tibble::tibble(
  `Max Cook's distance` = max(augmented$.cooksd),
  `Threshold (4/n)` = cooks_threshold,
  `Observations above threshold` = sum(augmented$.cooksd > cooks_threshold)
)
cat("\nInfluential observations:\n")
print_table(influence_table, save_as = "06b_influence")

# ---- Plots ----------------------------------------------------
# Inspect residual patterns for signs of nonlinearity or
# unequal variance.
residual_plot <- ggplot(augmented, aes(.fitted, .std.resid)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE) +
  labs(title = "Residuals vs Fitted Values (Model 4)",
       x = "Fitted MMSE", y = "Standardized residual") +
  theme_minimal(base_size = 12)
save_plot(residual_plot, "06c_residuals_vs_fitted")

# Assess whether residuals approximately follow a normal distribution.
qq_plot <- ggplot(augmented, aes(sample = .std.resid)) +
  stat_qq(alpha = 0.4) +
  stat_qq_line() +
  labs(title = "Normal Q-Q Plot of Residuals (Model 4)",
       x = "Theoretical quantiles", y = "Standardized residual") +
  theme_minimal(base_size = 12)
save_plot(qq_plot, "06d_qq_plot")

# Plot the unadjusted association between nWBV and MMSE.
# Vertical jitter helps separate overlapping integer MMSE scores.
brain_cognition_plot <- ggplot(regression_data,
                               aes(x = nWBV, y = MMSE)) +
  geom_jitter(alpha = 0.45, size = 2, width = 0, height = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +
  labs(
    title = "Brain Volume and Cognitive Performance",
    subtitle = "Unadjusted linear trend with 95% confidence band",
    x = get_variable_label("nWBV"),
    y = get_variable_label("MMSE"),
    caption = paste("Each point is one participant (jittered",
                    "vertically). The trend is unadjusted.")
  ) +
  theme_minimal(base_size = 12)
save_plot(brain_cognition_plot, "07_brain_volume_vs_mmse")

# ---- APA interpretation ----------------------------------------
# Generate written results from the fitted models and tests.
apa_header()

null_model   <- lm(MMSE ~ 1, data = regression_data)
model1_test  <- compare_nested(null_model, models[[1]],
                               "Null vs Model 1", "Brain volume")
overall_test <- compare_nested(null_model, final_model,
                               "Null vs Model 4", "All predictors")

df_resid  <- df.residual(final_model)
r2_model1 <- summary(models[[1]])$r.squared
r2_final  <- summary(final_model)$r.squared
adj_final <- summary(final_model)$adj.r.squared

final_coefs <- robust_tidy(final_model)
nwbv_row    <- final_coefs %>% filter(term == "nWBV")
nwbv_std    <- robust_tidy(standardized_model) %>% filter(term == "nWBV")
nwbv_sig    <- is_sig(nwbv_row$p.value)
nwbv_pos    <- nwbv_row$estimate > 0
p_rq1       <- nwbv_row$p.value

# Describe whether each step significantly improved model fit.
step_sentences <- vapply(seq_len(nrow(model_comparisons)), function(i) {
  row <- model_comparisons[i, ]
  paste0(
    "Adding ", tolower(row$`Additional Predictors`), " ",
    pick(is_sig(row$`p-value`), "significantly improved",
         "did not significantly improve"),
    " model fit (", apa_sym$dR2, " = ",
    fmt_bounded(row$`Delta R-squared`, 3), ", ", apa_F(row), ")."
  )
}, character(1))

# Report the overall model fit and the contribution of each
# block of added predictors.
apa_report(
  "A hierarchical multiple regression with heteroscedasticity-robust (HC3) ",
  "standard errors tested whether normalized whole brain volume (nWBV) ",
  "predicted cognitive performance (MMSE) in N = ", nrow(regression_data),
  " participants with complete data. nWBV alone explained ",
  fmt_pct(r2_model1), " of the variance in MMSE, ", apa_F(model1_test), ". ",
  paste(step_sentences, collapse = " "), " The final model explained ",
  fmt_pct(r2_final), " of the variance in MMSE (", apa_sym$R2, " = ",
  fmt_bounded(r2_final, 3), ", adjusted ", apa_sym$R2, " = ",
  fmt_bounded(adj_final, 3), "), ", apa_F(overall_test), "."
)

# Report the adjusted nWBV coefficient and its standardized beta.
apa_report(
  "In the final model, nWBV was ",
  pick(nwbv_sig,
       paste0("a significant ", pick(nwbv_pos, "positive", "negative"),
              " predictor of MMSE"),
       "not a significant predictor of MMSE"),
  " (", apa_b(nwbv_row, df_resid, scale = STEP,
              unit = paste0(" per ", STEP, "-unit increase in nWBV")),
  "; ", apa_sym$beta, " = ", fmt_bounded(nwbv_std$estimate, 2), "). ",
  pick(nwbv_sig,
       paste0("Holding age, education, SES, and sex constant, a ", STEP,
              "-unit higher nWBV was associated with a predicted MMSE ",
              "score that was ", fmt_num(abs(nwbv_row$estimate * STEP), 2),
              " points ", pick(nwbv_pos, "higher", "lower"), "."),
       paste0("There was insufficient evidence that brain volume was ",
              "associated with MMSE after accounting for the covariates."))
)

# Summarize the adjusted associations of age, education, SES,
# and sex with MMSE.
covariate_labels <- c(Age = "age", Educ = "education", SES = "SES",
                      SexMale = "sex (male vs. female)")

covariate_sentences <- vapply(names(covariate_labels), function(term_name) {
  row <- final_coefs[final_coefs$term == term_name, ]
  if (nrow(row) == 0) return(NA_character_)
  paste0(covariate_labels[[term_name]], " was ",
         pick(is_sig(row$p.value), "a significant", "not a significant"),
         " predictor (", apa_b(row, df_resid, include_t = FALSE), ")")
}, character(1))

apa_report(
  "Among the covariates, ",
  paste(covariate_sentences[!is.na(covariate_sentences)], collapse = "; "),
  "."
)

# Summarize multicollinearity, heteroscedasticity, and influential cases.
apa_report(
  "Assumption checks for the final model: variance inflation factors ",
  "ranged from ", fmt_num(min(vif_values), 2), " to ",
  fmt_num(max(vif_values), 2),
  pick(max(vif_values) < 5,
       ", indicating no problematic multicollinearity. ",
       ", suggesting possible multicollinearity. "),
  "The Breusch–Pagan test ",
  pick(bp_test$p.value < CONFIG$alpha,
       "indicated heteroscedasticity",
       "did not indicate heteroscedasticity"),
  ", ", apa_sym$chi2, "(", unname(bp_test$parameter), ") = ",
  fmt_num(unname(bp_test$statistic), 2), ", ", fmt_p(bp_test$p.value),
  "; robust standard errors were used in either case. ",
  influence_table$`Observations above threshold`, " of ", nrow(augmented),
  " observations (",
  fmt_pct(influence_table$`Observations above threshold` / nrow(augmented)),
  ") exceeded the Cook's distance threshold of 4/n (maximum = ",
  fmt_num(influence_table$`Max Cook's distance`, 3), ")."
)

# Clarify that cross-sectional associations do not establish causality.
apa_report(
  "These results are cross-sectional and describe differences between ",
  "people, not changes within a person or a causal effect of brain volume ",
  "on cognition. Because MMSE is capped at 30, associations may be ",
  "attenuated among cognitively healthy participants."
)

  # ============================================================
  # RESEARCH QUESTION 2
  # ============================================================
  
  rq_header(2, "Does brain volume differ across levels of dementia severity?")
  
  rq_background(
    explanation = paste(
      "This question asks whether average brain volume (nWBV) differs",
      "between people at different stages of dementia, as rated by the",
      "Clinical Dementia Rating (CDR: 0 = no dementia, 0.5 = very mild or",
      "questionable, 1 = mild, 2 = moderate). Age is strongly related to",
      "both brain volume and CDR, so we also test whether group differences",
      "persist after accounting for age and sex. CDR groups with fewer than",
      CONFIG$min_group_n, "participants are merged with the adjacent group",
      "to keep the comparisons stable."
    ),
    tests = c(
      "Descriptive statistics (mean and SD of nWBV, and mean age) per CDR group.",
      paste("Levene's test for equal variances (a diagnostic only; Welch's",
            "test is used either way)."),
      paste("Welch's one-way ANOVA as the primary test: it does not assume",
            "equal variances or equal group sizes. Eta-squared gives the",
            "effect size."),
      paste("If Welch's test is significant: Games-Howell post-hoc comparisons",
            "to see which pairs of groups differ."),
      paste("Sensitivity checks: a rank-based Kruskal-Wallis test, and a",
            "model that adjusts for age and sex (robust Wald F-test and",
            "adjusted group differences)."),
      "A violin/box plot of nWBV by CDR group."
    )
  )
  
  # Use participants with complete data for all variables in this analysis.
  rq2_data <- oasis_data %>%
    select(nWBV, CDR, Age, Sex) %>%
    drop_na()
  
  # Merge small CDR groups with adjacent groups to avoid unstable comparisons.
  rq2_data$CDR_group <- make_cdr_groups(rq2_data$CDR,
                                        CONFIG$min_group_n)
  
  results_header()
  
  # Report whether any original CDR groups were merged.
  if (nlevels(rq2_data$CDR_group) < length(unique(rq2_data$CDR))) {
    cat("\nNote: CDR groups with fewer than ", CONFIG$min_group_n,
        " participants were merged with the adjacent group.\n", sep = "")
  }
  
  # Count participants in each final group.
  group_sizes <- rq2_data %>%
    count(`Dementia Severity Group` = CDR_group, name = "Participants")
  
  cat("\nParticipants in each dementia severity group:\n")
  print_table(group_sizes, save_as = "08_group_sizes")
  
  # ANOVA requires at least two groups. If fewer remain, skip this
  # research question's analysis and continue with the next script section.
  if (nlevels(rq2_data$CDR_group) < 2) {
    
    cat("\nANOVA was not performed because fewer than two ",
        "dementia severity groups have usable observations.\n", sep = "")
    
  } else {
    
    # Summarise brain volume and age within each group.
    volume_by_cdr <- rq2_data %>%
      group_by(`Dementia Severity Group` = CDR_group) %>%
      summarise(
        Participants = n(),
        `Mean Age` = mean(Age),
        `Mean nWBV` = mean(nWBV),
        `SD nWBV` = sd(nWBV),
        `Median nWBV` = median(nWBV),
        .groups = "drop"
      )
    
    cat("\nBrain volume (and age) by dementia severity group:\n")
    print_table(volume_by_cdr, save_as = "09_volume_by_cdr")
    
    # Check whether group variances differ; Welch's ANOVA is used regardless.
    levene_result <- rstatix::levene_test(rq2_data, nWBV ~ CDR_group)
    cat("\nVariance diagnostic (Levene's test):\n")
    print_table(levene_result, save_as = "10_levene")
    
    # Test whether mean nWBV differs across groups without assuming equal variances.
    welch_result <- rstatix::welch_anova_test(rq2_data, nWBV ~ CDR_group)
    cat("\nWelch's ANOVA:\n")
    print_table(welch_result, save_as = "10b_welch_anova")
    
    # Estimate the proportion of nWBV variance associated with CDR group.
    # This uses a standard one-way ANOVA, not Welch's adjusted calculation.
    one_way <- anova(lm(nWBV ~ CDR_group, data = rq2_data))
    eta_squared <- one_way$`Sum Sq`[1] / sum(one_way$`Sum Sq`)
    cat("\nEffect size (eta-squared): ", round(eta_squared, 3), "\n", sep = "")
    
    p_rq2 <- welch_result$p[1]
    
    # Run pairwise comparisons only if the overall Welch test is significant.
    posthoc_result <- NULL
    
    if (is_sig(p_rq2)) {
      
      # Games-Howell compares group pairs while allowing unequal variances.
      posthoc_result <- rstatix::games_howell_test(
        rq2_data, nWBV ~ CDR_group
      ) %>%
        transmute(
          `First Dementia Severity Group` = as.character(group1),
          `Second Dementia Severity Group` = as.character(group2),
          `Mean Difference` = estimate,
          `95% CI Lower` = conf.low,
          `95% CI Upper` = conf.high,
          `Adjusted p-value` = p.adj
        )
      
      cat("\nGames-Howell post-hoc comparisons:\n")
      print_table(posthoc_result, save_as = "11_games_howell")
    }
    
    # Sensitivity check: test for group differences using ranks rather than means.
    kruskal <- kruskal.test(nWBV ~ CDR_group, data = rq2_data)
    cat("\nSensitivity check, Kruskal-Wallis test: chi-squared = ",
        round(unname(kruskal$statistic), 2), ", df = ",
        unname(kruskal$parameter), ", p ",
        ifelse(kruskal$p.value < 0.001, "< .001",
               paste0("= ", sprintf("%.3f", kruskal$p.value))),
        "\n", sep = "")
    
    # Compare models to test whether CDR groups explain additional variation
    # in nWBV after accounting for age and sex.
    adjusted_null <- lm(nWBV ~ Age + Sex, data = rq2_data)
    adjusted_full <- lm(nWBV ~ Age + Sex + CDR_group, data = rq2_data)
    
    adjusted_test <- compare_nested(
      adjusted_null, adjusted_full,
      "Age + sex vs Age + sex + CDR group",
      "Dementia severity group"
    )
    
    # Estimate the proportion of remaining variation explained by adding CDR.
    partial_eta2 <- (sum(resid(adjusted_null)^2) -
                       sum(resid(adjusted_full)^2)) /
      sum(resid(adjusted_null)^2)
    adjusted_test$`Partial eta-squared` <- partial_eta2
    
    cat("\nAge- and sex-adjusted test of dementia severity group ",
        "(robust Wald F-test):\n", sep = "")
    print_table(adjusted_test, save_as = "12_adjusted_group_test")
    
    # Use the first factor level as the reference group for adjusted comparisons.
    reference_group <- levels(rq2_data$CDR_group)[1]
    
    # Extract adjusted differences and robust confidence intervals for other groups.
    adjusted_coefs <- robust_tidy(adjusted_full) %>%
      filter(grepl("^CDR_group", term)) %>%
      mutate(Group = sub("^CDR_group", "", term))
    
    cat("\nAdjusted differences in nWBV relative to ", reference_group,
        " (holding age and sex constant):\n", sep = "")
    print_table(
      adjusted_coefs %>%
        transmute(
          Group,
          `Adjusted Mean Difference` = estimate,
          `95% CI Lower` = conf.low,
          `95% CI Upper` = conf.high,
          `p-value` = p.value
        ),
      save_as = "12b_adjusted_contrasts"
    )
    
    # ---- Plot ----------------------------------------------------
    
    # Show individual observations and the distribution of nWBV in each group.
    brain_volume_cdr_plot <- ggplot(rq2_data,
                                    aes(x = CDR_group, y = nWBV)) +
      geom_violin(trim = FALSE, alpha = 0.35) +
      geom_boxplot(width = 0.14, outlier.shape = NA, alpha = 0.65) +
      geom_jitter(width = 0.08, alpha = 0.25, size = 1.5) +
      labs(
        title = "Brain Volume Across Dementia Severity Groups",
        x = "Clinical Dementia Rating Group",
        y = get_variable_label("nWBV"),
        caption = paste("Points are participants; boxes show medians",
                        "and interquartile ranges. Unadjusted.")
      ) +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 25, hjust = 1))
    save_plot(brain_volume_cdr_plot, "13_brain_volume_by_cdr")
    
    # ---- APA interpretation ----------------------------------------
    
    # Set flags for the significance decisions used in the written report.
    apa_header()
    
    welch_sig <- is_sig(p_rq2)
    adj_sig   <- is_sig(adjusted_test$`p-value`)
    kw_sig    <- is_sig(kruskal$p.value)
    
    # Format each group's sample size, mean, and standard deviation for reporting.
    group_descriptions <- paste(
      sprintf("%s (n = %s, M = %s, SD = %s)",
              as.character(volume_by_cdr$`Dementia Severity Group`),
              volume_by_cdr$Participants,
              fmt_bounded(volume_by_cdr$`Mean nWBV`, 3),
              fmt_bounded(volume_by_cdr$`SD nWBV`, 3)),
      collapse = "; "
    )
    
    # Report the overall test, variance check, and effect size.
    apa_report(
      "A Welch's one-way ANOVA compared nWBV across ",
      nlevels(rq2_data$CDR_group), " dementia severity (CDR) groups (N = ",
      nrow(rq2_data), "). Levene's test indicated that variances were ",
      pick(is_sig(levene_result$p), "unequal", "comparable"),
      " across groups, F(", fmt_df(levene_result$df1), ", ",
      fmt_df(levene_result$df2), ") = ", fmt_num(levene_result$statistic, 2),
      ", ", fmt_p(levene_result$p), "; Welch's correction was applied in ",
      "either case. Mean nWBV ",
      pick(welch_sig, "differed significantly", "did not differ significantly"),
      " across CDR groups, Welch's F(", fmt_df(welch_result$DFn), ", ",
      fmt_df(welch_result$DFd), ") = ", fmt_num(welch_result$statistic, 2),
      ", ", fmt_p(p_rq2), ", ", apa_sym$eta2, " = ",
      fmt_bounded(eta_squared, 2), " (", eta_label(eta_squared), " effect)."
    )
    
    apa_report("Group means were: ", group_descriptions, ".")
    
    if (welch_sig && !is.null(posthoc_result)) {
      
      # Use observed group means to describe which group has higher nWBV.
      group_means <- setNames(volume_by_cdr$`Mean nWBV`,
                              as.character(volume_by_cdr$`Dementia Severity Group`))
      
      # Reverse the estimate and confidence interval if needed so the
      # reported difference follows the higher-volume minus lower-volume direction.
      describe_pair <- function(row) {
        g1 <- row$`First Dementia Severity Group`
        g2 <- row$`Second Dementia Severity Group`
        high_g <- if (group_means[[g1]] >= group_means[[g2]]) g1 else g2
        low_g  <- if (identical(high_g, g1)) g2 else g1
        est <- row$`Mean Difference`
        lo  <- row$`95% CI Lower`
        hi  <- row$`95% CI Upper`
        if (sign(est) != sign(group_means[[high_g]] - group_means[[low_g]])) {
          flipped_lo <- -hi
          hi  <- -lo
          lo  <- flipped_lo
          est <- -est
        }
        paste0("participants in the ", high_g, " group had higher nWBV than ",
               "those in the ", low_g, " group (mean difference = ",
               fmt_bounded(est, 3), ", 95% CI ",
               fmt_ci(lo, hi, 3, bounded = TRUE), ", ",
               sub("^p ", "adjusted p ", fmt_p(row$`Adjusted p-value`)), ")")
      }
      
      # Keep only significant pairwise comparisons for the detailed report.
      significant_rows <- posthoc_result %>%
        filter(!is.na(`Adjusted p-value`),
               `Adjusted p-value` < CONFIG$alpha)
      n_other_pairs <- nrow(posthoc_result) - nrow(significant_rows)
      
      pair_sentences <- vapply(seq_len(nrow(significant_rows)),
                               function(i) describe_pair(significant_rows[i, ]),
                               character(1))
      
      # Report significant pairs and note any remaining non-significant comparisons.
      apa_report(
        "Games–Howell post hoc comparisons showed that ",
        if (length(pair_sentences) > 0) {
          paste(pair_sentences, collapse = "; ")
        } else {
          "no individual pair of groups differed significantly"
        },
        ". ",
        if (n_other_pairs > 0 && length(pair_sentences) > 0) {
          paste0("The remaining ", n_other_pairs, " pairwise comparison",
                 if (n_other_pairs > 1) "s were" else " was",
                 " not statistically significant.")
        } else {
          ""
        }
      )
    }
    
    # Prepare adjusted comparisons against the reference group for reporting.
    adjusted_sentences <- vapply(seq_len(nrow(adjusted_coefs)), function(i) {
      row <- adjusted_coefs[i, ]
      paste0("the ", row$Group, " group had ",
             pick(row$estimate < 0, "lower", "higher"),
             " adjusted mean nWBV than the ", reference_group, " group (",
             apa_b(row, df.residual(adjusted_full), digits = 3,
                   include_t = FALSE), ")")
    }, character(1))
    
    # Summarise whether the sensitivity tests agree and whether adjusted
    # group differences remain statistically significant.
    apa_report(
      "A Kruskal–Wallis test ",
      pick(welch_sig == kw_sig, "led to the same conclusion",
           "led to a different conclusion"),
      ", H(", unname(kruskal$parameter), ") = ",
      fmt_num(unname(kruskal$statistic), 2), ", ", fmt_p(kruskal$p.value),
      ". After adjusting for age and sex, CDR group ",
      pick(adj_sig, "remained a significant predictor of nWBV",
           "was no longer a significant predictor of nWBV"),
      ", robust ", apa_F(adjusted_test), ", partial ", apa_sym$eta2, " = ",
      fmt_bounded(partial_eta2, 2), ". ",
      if (adj_sig) {
        paste0("Specifically, ", paste(adjusted_sentences, collapse = "; "), ".")
      } else {
        "This suggests that the unadjusted differences largely reflect age."
      }
    )
    
    # Note that cross-sectional results cannot establish causality or change over time.
    apa_report(
      "These group differences are cross-sectional and observational; they ",
      "do not show that dementia severity causes lower brain volume, or how ",
      "brain volume changes within a person over time. Group sizes were ",
      "unequal, and small groups at higher CDR levels limit the precision of ",
      "those estimates."
    )
  }
  # ============================================================
  # RESEARCH QUESTION 3
  # ============================================================
  
  rq_header(3, paste("Does the relationship between brain volume and",
                     "cognitive performance depend on age?"))
  
  rq_background(
    explanation = paste(
      "This question asks whether the link between brain volume and MMSE",
      "is stronger or weaker at different ages (moderation). For example,",
      "a given amount of atrophy might be more closely tied to cognitive",
      "performance in older participants. Moderation is tested with an",
      "interaction term (nWBV x age): if it is non-zero, the nWBV-MMSE",
      "slope changes with age."
    ),
    tests = c(
      paste("A linear regression predicting MMSE from nWBV, age and their",
            "interaction, controlling for education, SES and sex. nWBV and",
            "age are mean-centered, so each main effect refers to the",
            "average participant; HC3 robust standard errors are used."),
      paste("The interaction coefficient with its 95% CI is the moderation",
            "test. A model without covariates is shown as a sensitivity",
            "check."),
      paste("Simple slopes: the nWBV-MMSE slope at the 25th, 50th and 75th",
            "percentile of age, to show what the interaction means in",
            "practice."),
      "A plot of the model-predicted nWBV-MMSE lines at those ages."
    )
  )
  
  # Use participants with complete data for all variables in this model.
  interaction_data <- oasis_data %>%
    select(MMSE, nWBV, Age, Educ, SES, Sex) %>%
    drop_na()
  
  results_header()
  
  # Skip this analysis if fewer than 30 complete cases are available.
  # The script continues with the next section after this if/else block.
  if (nrow(interaction_data) < 30) {
    
    cat("\nThe moderation analysis was not performed because ",
        "there are too few complete observations.\n", sep = "")
    
    # An interaction cannot be estimated if age or brain volume has no variation.
  } else if (length(unique(interaction_data$Age)) < 2 ||
             length(unique(interaction_data$nWBV)) < 2) {
    
    cat("\nThe moderation analysis was not performed because ",
        "age or brain volume has insufficient variation.\n", sep = "")
    
  } else {
    
    # Center age and brain volume around their sample means.
    # This makes the main effects refer to average age and average nWBV.
    mean_age  <- mean(interaction_data$Age)
    mean_nwbv <- mean(interaction_data$nWBV)
    
    interaction_data <- interaction_data %>%
      mutate(Age_c = Age - mean_age, nWBV_c = nWBV - mean_nwbv)
    
    # Test whether the nWBV-MMSE association changes with age,
    # while controlling for education, SES and sex.
    interaction_model <- lm(
      MMSE ~ nWBV_c * Age_c + Educ + SES + Sex,
      data = interaction_data
    )
    
    # Calculate HC3 robust covariance estimates for inference.
    interaction_vcov <- sandwich::vcovHC(interaction_model, type = "HC3")
    
    # Report model coefficients with robust standard errors and confidence intervals.
    interaction_coefficients <- robust_tidy(interaction_model) %>%
      transmute(
        Predictor,
        Estimate = estimate,
        `Robust SE` = std.error,
        `95% CI Lower` = conf.low,
        `95% CI Upper` = conf.high,
        `p-value` = p.value
      )
    
    cat("\nModeration model coefficients (HC3 robust SEs; ",
        "brain volume and age centered):\n", sep = "")
    print_table(interaction_coefficients,
                save_as = "14_moderation_coefficients")
    
    # Summarise overall model fit, including the variance explained.
    interaction_fit <- broom::glance(interaction_model) %>%
      transmute(
        Participants = nobs,
        R_squared = r.squared,
        Adjusted_R_squared = adj.r.squared,
        AIC, BIC
      )
    cat("\nModeration model fit:\n")
    print_table(interaction_fit, save_as = "14b_moderation_fit")
    
    # Extract the interaction term: this is the main test of moderation.
    interaction_result <- robust_tidy(interaction_model) %>%
      filter(term == "nWBV_c:Age_c")
    p_rq3 <- interaction_result$p.value[1]
    
    cat("\nInteraction test (per 1.0 nWBV x 1 year of age):\n")
    print_table(
      interaction_result %>%
        transmute(
          Predictor,
          Estimate = estimate,
          `95% CI Lower` = conf.low,
          `95% CI Upper` = conf.high,
          `p-value` = p.value
        ),
      save_as = "14c_interaction_test"
    )
    
    # Sensitivity check: repeat the interaction test without covariates.
    # This checks whether the conclusion changes when education, SES and sex
    # are excluded from the model.
    unadjusted_model <- lm(MMSE ~ nWBV_c * Age_c, data = interaction_data)
    unadjusted_p <- robust_tidy(unadjusted_model) %>%
      filter(term == "nWBV_c:Age_c") %>%
      pull(p.value)
    cat("\nSensitivity check: the unadjusted interaction model gives p ",
        ifelse(unadjusted_p < 0.001, "< .001",
               paste0("= ", sprintf("%.3f", unadjusted_p))), "\n", sep = "")
    
    # ---- Simple slopes at representative ages -------------------
    
    # Choose younger, middle and older reference ages using sample percentiles.
    age_quantiles <- quantile(interaction_data$Age,
                              probs = c(0.25, 0.50, 0.75))
    
    age_reference <- tibble::tibble(
      Percentile = c("25th percentile", "Median", "75th percentile"),
      Phrase = c("at the 25th percentile of age",
                 "at the median age",
                 "at the 75th percentile of age"),
      Age = as.numeric(age_quantiles)
    ) %>%
      mutate(
        Age_c = Age - mean_age,
        Age_Group = sprintf("%s: %.0f years", Percentile, Age)
      )
    
    # Estimate the nWBV-MMSE slope at each reference age.
    slopes <- bind_cols(
      tibble::tibble(`Illustrative Age` = age_reference$Age_Group),
      simple_slopes(interaction_model, interaction_vcov,
                    age_reference$Age_c, step = STEP)
    )
    
    cat("\nSimple slopes: change in MMSE per ", STEP,
        " higher nWBV at each reference age (robust CIs):\n", sep = "")
    print_table(slopes, save_as = "15_simple_slopes")
    
    # ---- Plot ----------------------------------------------------
    
    # Restrict each prediction line to the observed nWBV range near its age.
    # If fewer than five participants fall within the age window, use the
    # full sample range instead.
    modal_sex <- names(which.max(table(interaction_data$Sex)))
    
    prediction_grid <- bind_rows(lapply(
      seq_len(nrow(age_reference)),
      function(i) {
        nearby <- interaction_data %>%
          filter(abs(Age - age_reference$Age[i]) <= CONFIG$age_window)
        if (nrow(nearby) < 5) nearby <- interaction_data
        tibble::tibble(
          nWBV = seq(min(nearby$nWBV), max(nearby$nWBV), length.out = 100),
          Age_Group = age_reference$Age_Group[i],
          Age_c = age_reference$Age_c[i]
        )
      }
    )) %>%
      # Hold other predictors constant to compare the age-specific lines.
      mutate(
        nWBV_c = nWBV - mean_nwbv,
        Educ = mean(interaction_data$Educ),
        SES = mean(interaction_data$SES),
        Sex = factor(modal_sex, levels = levels(interaction_data$Sex)),
        Age_Group = factor(Age_Group, levels = age_reference$Age_Group)
      )
    
    # Calculate predicted MMSE and robust confidence intervals using the
    # model's HC3 covariance estimates.
    design_matrix <- model.matrix(
      delete.response(terms(interaction_model)),
      data = prediction_grid
    )
    fitted_values <- drop(design_matrix %*% coef(interaction_model))
    fitted_se <- sqrt(rowSums((design_matrix %*% interaction_vcov) *
                                design_matrix))
    crit <- qt(0.975, df.residual(interaction_model))
    
    prediction_grid <- prediction_grid %>%
      mutate(
        Predicted_MMSE = fitted_values,
        Lower_CI = fitted_values - crit * fitted_se,
        Upper_CI = fitted_values + crit * fitted_se
      )
    
    # Plot observed participants alongside the fitted lines and their CIs.
    interaction_plot <- ggplot(interaction_data,
                               aes(x = nWBV, y = MMSE)) +
      geom_jitter(alpha = 0.18, size = 1.5, width = 0, height = 0.2) +
      geom_ribbon(
        data = prediction_grid,
        aes(x = nWBV, ymin = Lower_CI, ymax = Upper_CI,
            fill = Age_Group, group = Age_Group),
        inherit.aes = FALSE, alpha = 0.12
      ) +
      geom_line(
        data = prediction_grid,
        aes(x = nWBV, y = Predicted_MMSE,
            color = Age_Group, group = Age_Group),
        inherit.aes = FALSE, linewidth = 1
      ) +
      labs(
        title = "Does Age Change the Brain Volume-Cognition Relationship?",
        subtitle = paste("Predictions from an adjusted regression model",
                         "with a brain volume x age interaction"),
        x = get_variable_label("nWBV"),
        y = get_variable_label("MMSE"),
        color = "Illustrative age",
        fill = "Illustrative age",
        caption = paste(
          "Shaded bands: 95% robust CIs for the fitted mean.",
          "Education and SES held at their means; sex at its most",
          "common category.\nLines span observed brain volumes within",
          "+/-", CONFIG$age_window, "years of each reference age."
        )
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
    save_plot(interaction_plot, "16_age_moderation_plot",
              width = 9, height = 6)
    
    # ---- APA interpretation ----------------------------------------
    
    # Prepare model statistics and significance decisions for the written report.
    apa_header()
    
    df_int       <- df.residual(interaction_model)
    null_int     <- lm(MMSE ~ 1, data = interaction_data)
    overall_int  <- compare_nested(null_int, interaction_model,
                                   "Null vs moderation model",
                                   "All predictors")
    r2_int       <- summary(interaction_model)$r.squared
    adj_int      <- summary(interaction_model)$adj.r.squared
    int_sig      <- is_sig(p_rq3)
    int_pos      <- interaction_result$estimate > 0
    
    # Report overall fit and whether the interaction provides evidence of moderation.
    apa_report(
      "A moderated multiple regression tested whether age moderated the ",
      "association between nWBV and MMSE (N = ", nrow(interaction_data),
      "). nWBV and age were mean-centered, education, SES, and sex were ",
      "included as covariates, and heteroscedasticity-robust (HC3) standard ",
      "errors were used. The model explained ", fmt_pct(r2_int),
      " of the variance in MMSE (", apa_sym$R2, " = ", fmt_bounded(r2_int, 3),
      ", adjusted ", apa_sym$R2, " = ", fmt_bounded(adj_int, 3), "), ",
      apa_F(overall_int), ". The nWBV ", apa_sym$times, " age interaction was ",
      pick(int_sig, "significant", "not significant"), " (",
      apa_b(interaction_result, df_int, scale = STEP,
            unit = paste0(" per ", STEP, "-unit nWBV per year of age")),
      "). ",
      pick(int_sig,
           paste0("The ", pick(int_pos, "positive", "negative"),
                  " coefficient indicates that the association between nWBV ",
                  "and MMSE became more ", pick(int_pos, "positive", "negative"),
                  " with increasing age."),
           paste0("There was insufficient evidence that the association ",
                  "between brain volume and cognitive performance varied with ",
                  "age; this does not establish that it is identical at all ",
                  "ages."))
    )
    
    # Format the estimated nWBV-MMSE slope and confidence interval at each age.
    slope_sentences <- vapply(seq_len(nrow(slopes)), function(i) {
      res <- tibble::tibble(
        estimate  = slopes$Slope[i],
        conf.low  = slopes$`95% CI Lower`[i],
        conf.high = slopes$`95% CI Upper`[i],
        statistic = slopes$`t-value`[i],
        p.value   = slopes$`p-value`[i]
      )
      paste0(age_reference$Phrase[i], " (", fmt_num(age_reference$Age[i], 0),
             " years), ", apa_b(res, df_int,
                                unit = paste0(" per ", STEP, "-unit nWBV")))
    }, character(1))
    
    apa_report(
      "Simple slopes analyses estimated the association between nWBV and ",
      "MMSE at three ages: ", paste(slope_sentences, collapse = "; "), "."
    )
    
    # Compare the adjusted result with the unadjusted sensitivity analysis.
    apa_report(
      "A model without covariates gave an interaction ",
      pick(is_sig(unadjusted_p) == int_sig, "consistent with", "different from"),
      " the adjusted model (", fmt_p(unadjusted_p), "). As in the previous ",
      "analyses, these cross-sectional estimates describe differences between ",
      "people of different ages and cannot show how brain volume or cognition ",
      "change within a person."
    )
  }
  # ------------------------------------------------------------
  # 7. Multiplicity across the three primary tests
  # ------------------------------------------------------------
  
  section("MULTIPLICITY ADJUSTMENT FOR PRIMARY TESTS")
  
  # Use one main p-value from each research question.
  # Holm's correction accounts for testing three hypotheses and
  # controls the overall risk of at least one false-positive result.
  # Follow-up and sensitivity tests are treated as supporting analyses.
  primary_tests <- tibble::tibble(
    Hypothesis = c(
      "RQ1: Brain volume predicts MMSE (fully adjusted model)",
      "RQ2: Brain volume differs across CDR groups (Welch ANOVA)",
      "RQ3: Age moderates brain volume-MMSE association"
    ),
    `p-value` = c(p_rq1, p_rq2, p_rq3)
  ) %>%
    # Calculate corrected p-values for the three primary tests.
    # Adjusted p-values can be larger than the original p-values.
    mutate(`Holm-adjusted p-value` = p.adjust(`p-value`, method = "holm"))
  
  # Display and optionally save the original and adjusted p-values.
  print_table(primary_tests, save_as = "17_primary_tests_holm")
  
  apa_header()
  
  # Extract the corrected p-values to summarise the overall results.
  holm_p <- primary_tests$`Holm-adjusted p-value`
  
  # Report how many research questions remain significant after correction.
  apa_report(
    "Holm's correction was applied across the three primary tests (RQ1 to ",
    "RQ3). After correction, ", sum(holm_p < CONFIG$alpha, na.rm = TRUE),
    " of 3 tests remained significant at ", "α = ",
    fmt_bounded(CONFIG$alpha, 2), " (RQ1: ", fmt_p(holm_p[1]), "; RQ2: ",
    fmt_p(holm_p[2]), "; RQ3: ", fmt_p(holm_p[3]), ")."
  )
  
  
  # ------------------------------------------------------------
  # 8. Final analytical notes
  # ------------------------------------------------------------
  
  section("ANALYSIS COMPLETE")
  
  # Print reminders about interpretation, assumptions and study limitations.
  cat(
    "\nInterpretation reminders:\n",
    "1. Statistical significance does not by itself indicate that ",
    "an association is practically important; use the effect sizes ",
    "and confidence intervals.\n",
    "2. Linear-model inference uses HC3 robust standard errors ",
    "because MMSE has a ceiling at 30 and non-constant variance. ",
    "Residual diagnostics are saved for inspection.\n",
    "3. Education and socioeconomic status are treated as numeric ",
    "1-5 scores, which assumes equal spacing between levels. Note ",
    "that SES is coded 1 = highest, 5 = lowest.\n",
    "4. Age, brain volume, and dementia severity are strongly ",
    "intertwined; adjusted and unadjusted results can differ.\n",
    "5. The data are cross-sectional and observational: they cannot ",
    "establish causality or within-person change over time.\n",
    "6. Cognitive and clinical variables are available only for ",
    "older adults in OASIS, so conclusions apply to that subgroup.\n",
    "7. Merging small CDR groups improves stability but reduces ",
    "resolution for the highest dementia severities.\n",
    sep = ""
  )
  
  # Save the written results and R session details if output saving is enabled.
  if (CONFIG$save_outputs) {
    # Save the APA write-up as UTF-8 to preserve symbols such as Greek letters.
    apa_file <- file(file.path(CONFIG$output_dir, "apa_interpretations.txt"),
                     encoding = "UTF-8")
    writeLines(apa_lines, apa_file)
    close(apa_file)
    
    # Record R and package versions to help reproduce the analysis later.
    writeLines(
      capture.output(sessionInfo()),
      file.path(CONFIG$output_dir, "session_info.txt")
    )
    
    # Confirm where the output files were saved.
    cat("\nTables, plots, APA interpretations and session info were saved to: ",
        normalizePath(CONFIG$output_dir), "\n", sep = "")
  }
  
  # Confirm that the script has reached the end.
  cat("\nAnalysis script finished.\n")