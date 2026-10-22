# ============================================================
# OASIS Cross-Sectional MRI Analysis
# Data: OASIS-1 cross-sectional release (Marcus et al., 2007).
# See README.md for the project structure, variables and how to run it.
# ============================================================


# ------------------------------------------------------------
# 0. Configuration
# Purpose: Keep every setting that a user may want to change in one place.
#          The rest of the script reads its settings from CONFIG, so
#          changing a value here changes it everywhere; no other line of
#          code needs to be edited.
# ------------------------------------------------------------

# Sys.getenv("NAME", default) uses the environment variable NAME if it is
# set and the default otherwise. This lets another user point the script
# at a different data file or output folder without editing it.
CONFIG <- list(
  # Location of the dataset (.xlsx, .xls or .csv).
  data_file    = Sys.getenv("OASIS_DATA_FILE",
                            "oasis_cross-sectional-5708aa0a98d82080.xlsx"),
  # Folder where tables (CSV), plots (PNG) and the APA text are written.
  output_dir   = Sys.getenv("OASIS_OUTPUT_DIR", "oasis_output"),
  # FALSE prints all results to the console without writing any files.
  save_outputs = TRUE,
  # Significance threshold used for every test and for the wording of the
  # APA interpretations ("significant" vs "not significant").
  alpha        = 0.05,
  # OASIS contains a few participants who were scanned twice. TRUE keeps
  # only their first scan, so every person is counted once.
  first_scan_only = TRUE,
  # The regression models of RQ1 and RQ3 are skipped (with a message) when
  # fewer participants than this have complete data.
  min_complete_n = 30,
  # In RQ2, the highest-severity CDR group is merged with the next-lower
  # group while it has fewer participants than this, to keep group
  # comparisons stable.
  min_group_n  = 10,
  # nWBV is a proportion (about 0.6-0.9), so an effect "per 1 unit" is
  # unrealistically large. Effects are reported per 0.01 unit instead.
  nwbv_step    = 0.01,
  # Ages (as sample percentiles) at which RQ3 reports the brain
  # volume-MMSE slope: here the 25th percentile, the median and the 75th.
  age_probs    = c(0.25, 0.50, 0.75),
  # Each predicted line in the RQ3 plot only covers the brain volumes seen
  # in participants within +/- this many years of that line's age.
  age_window   = 5
)


# ------------------------------------------------------------
# 1. Packages and project functions
# Purpose: Make the functions in the R/ folder available, then install
#          (if needed) and load the packages they use.
# ------------------------------------------------------------

# Each file in R/ only defines functions; nothing is run yet.
source(file.path("R", "packages.R"))
source(file.path("R", "helpers_reporting.R"))
source(file.path("R", "helpers_data.R"))
source(file.path("R", "descriptives.R"))
source(file.path("R", "helpers_stats.R"))
source(file.path("R", "rq1_brain_cognition.R"))
source(file.path("R", "rq2_dementia_severity.R"))
source(file.path("R", "rq3_age_moderation.R"))

required_packages <- c(
  "readxl", "dplyr", "tidyr", "tibble", "ggplot2",
  "broom", "knitr", "rstatix", "sandwich", "lmtest"
)
load_packages(required_packages)

# Decide once whether results are saved. The helpers receive output_dir
# and save a file only when it is not NULL, so NULL switches saving off.
output_dir <- if (CONFIG$save_outputs) CONFIG$output_dir else NULL
if (!is.null(output_dir)) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
}

cat("\nStarting OASIS analysis...\n")


# ------------------------------------------------------------
# 2. Load and validate the dataset
# Purpose: Read the data and check it before any analysis is run.
# ------------------------------------------------------------

# prepare_oasis_data() stops if required columns are missing or a value
# cannot be converted to a number, codes sex as a factor (Female =
# reference) and, if CONFIG$first_scan_only is TRUE, drops repeat scans.

raw_data   <- read_oasis_file(resolve_data_file(CONFIG$data_file))
oasis_data <- prepare_oasis_data(raw_data,
                                 keep_first_scan_only = CONFIG$first_scan_only)

cat("\nDataset loaded successfully.\n")
cat("Rows in file:", nrow(raw_data), "\n")
cat("Repeat scans removed:", nrow(raw_data) - nrow(oasis_data), "\n")
cat("Number of participants:", nrow(oasis_data), "\n")
cat("Number of columns:", ncol(raw_data), "\n")

# Values outside their documented range usually point to data-entry problems.
range_checks <- check_value_ranges(oasis_data)
if (any(range_checks$`Rows Flagged` > 0)) {
  warning("Some values fall outside their expected ranges. ",
          "See the data-quality table below.", call. = FALSE)
  print_table(range_checks)
}


# ------------------------------------------------------------
# 3. Descriptive statistics
# Purpose: Summarise every numeric variable (N, mean, SD, median, range).
# ------------------------------------------------------------

section("DESCRIPTIVE STATISTICS")

print_table(describe_variables(oasis_data, NUMERIC_VARIABLES),
            save_as = "01_descriptive_statistics", output_dir = output_dir)


# ------------------------------------------------------------
# 4. Missing-data assessment
# Purpose: Show how much data is missing per variable and whether the
#          participants without cognitive data differ in age, which tells
#          us which population the cognitive analyses describe.
# ------------------------------------------------------------

section("MISSING-DATA ASSESSMENT")

# Only the original columns are checked, not the ones added during cleaning.
print_table(summarise_missing(oasis_data[names(raw_data)]),
            save_as = "02_missing_data", output_dir = output_dir)

cat("\nAge of participants with and without cognitive data:\n")
print_table(summarise_missing_by_age(oasis_data),
            save_as = "02b_missingness_by_age", output_dir = output_dir)

cat("\n", wrap_text(paste(
  "Note: every model below uses participants with complete data on",
  "its own variables, so sample sizes differ between analyses. If",
  "cognitive data are missing mainly for younger adults, results",
  "apply to the older subgroup only."
)), "\n", sep = "")


# ------------------------------------------------------------
# 5. Sample composition
# Purpose: Count participants by sex and by dementia rating, and describe
#          the sample in an APA-style paragraph.
# ------------------------------------------------------------

section("SAMPLE COMPOSITION")

cat("\nParticipants by sex:\n")
print_table(oasis_data %>% count(Sex, name = "Participants"),
            save_as = "03_sex_counts", output_dir = output_dir)

cat("\nParticipants by Clinical Dementia Rating:\n")
print_table(count_by_cdr(oasis_data), save_as = "03b_cdr_counts",
            output_dir = output_dir)

# apa_lines collects every APA paragraph for the text file saved at the
# end. It starts with the sample description; each research question
# below adds its own paragraphs.
subheading("Sample description (APA style)")
apa_lines <- apa_report(describe_sample_apa(oasis_data))


# ------------------------------------------------------------
# 6. Research question 1: Does brain volume predict cognitive performance?
# Purpose: Hierarchical regression of MMSE on nWBV, adding age, then
#          education and SES, then sex (see R/rq1_brain_cognition.R).
# ------------------------------------------------------------

# Each run_rq*() function prints its own tables, plots and interpretation,
# and returns a list. Its p-value is used in Section 9, its APA text is
# added to apa_lines.

rq1 <- run_rq1(oasis_data,
               alpha = CONFIG$alpha,
               step = CONFIG$nwbv_step,
               min_complete_n = CONFIG$min_complete_n,
               output_dir = output_dir)
apa_lines <- c(apa_lines, rq1$apa)


# ------------------------------------------------------------
# 7. Research question 2: Does brain volume differ across dementia severity?
# Purpose: Welch's ANOVA of nWBV across CDR groups, with post hoc tests
#          and an age- and sex-adjusted check (see R/rq2_dementia_severity.R).
# ------------------------------------------------------------

rq2 <- run_rq2(oasis_data,
               alpha = CONFIG$alpha,
               min_group_n = CONFIG$min_group_n,
               output_dir = output_dir)
apa_lines <- c(apa_lines, rq2$apa)


# ------------------------------------------------------------
# 8. Research question 3: Does the brain volume-cognition link depend on age?
# Purpose: Regression with an nWBV x age interaction and simple slopes at
#          the ages set in CONFIG$age_probs (see R/rq3_age_moderation.R).
# ------------------------------------------------------------

rq3 <- run_rq3(oasis_data,
               alpha = CONFIG$alpha,
               step = CONFIG$nwbv_step,
               min_complete_n = CONFIG$min_complete_n,
               age_probs = CONFIG$age_probs,
               age_window = CONFIG$age_window,
               output_dir = output_dir)
apa_lines <- c(apa_lines, rq3$apa)


# ------------------------------------------------------------
# 9. Multiplicity across the three primary tests
# Purpose: Testing three hypotheses raises the chance of a false positive,
#          so the three main p-values are corrected together.
# ------------------------------------------------------------

section("MULTIPLICITY ADJUSTMENT FOR PRIMARY TESTS")

# One main p-value per research question, corrected with Holm's method to
# control the risk of at least one false positive. Follow-up and
# sensitivity tests are supporting analyses and are not included.
primary_tests <- holm_table(
  hypotheses = c(
    "RQ1: Brain volume predicts MMSE (fully adjusted model)",
    "RQ2: Brain volume differs across CDR groups (Welch ANOVA)",
    "RQ3: Age moderates brain volume-MMSE association"
  ),
  p_values = c(rq1$p_value, rq2$p_value, rq3$p_value)
)
print_table(primary_tests, save_as = "17_primary_tests_holm",
            output_dir = output_dir)

apa_header()
holm_p <- primary_tests$`Holm-adjusted p-value`
apa_lines <- c(apa_lines, apa_report(
  "Holm's correction was applied across the three primary tests (RQ1 to ",
  "RQ3). After correction, ", sum(holm_p < CONFIG$alpha, na.rm = TRUE),
  " of ", sum(!is.na(holm_p)), " tests remained significant at ",
  apa_sym$alpha, " = ", fmt_bounded(CONFIG$alpha, 2), " (RQ1: ",
  fmt_p(holm_p[1]), "; RQ2: ", fmt_p(holm_p[2]), "; RQ3: ",
  fmt_p(holm_p[3]), ")."
))


# ------------------------------------------------------------
# 10. Final analytical notes and saved files
# Purpose: Print interpretation reminders and save the APA write-up and
#          session details.
# ------------------------------------------------------------

section("ANALYSIS COMPLETE")

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

if (!is.null(output_dir)) {
  # Save the APA write-up as UTF-8 to preserve symbols such as Greek letters.
  apa_file <- file(file.path(output_dir, "apa_interpretations.txt"),
                   encoding = "UTF-8")
  writeLines(apa_lines, apa_file)
  close(apa_file)

  # Record R and package versions to help reproduce the analysis later.
  save_session_info(output_dir)

  cat("\nTables, plots, APA interpretations and session info were saved to: ",
      normalizePath(output_dir), "\n", sep = "")
}

cat("\nAnalysis script finished.\n")


# ------------------------------------------------------------
# Reference
# ------------------------------------------------------------
# Marcus, D. S., Wang, T. H., Parker, J., Csernansky, J. G., Morris, J. C.,
#   & Buckner, R. L. (2007). Open Access Series of Imaging Studies (OASIS):
#   Cross-sectional MRI data in young, middle aged, nondemented, and
#   demented older adults. Journal of Cognitive Neuroscience, 19(9),
#   1498-1507. https://doi.org/10.1162/jocn.2007.19.9.1498
