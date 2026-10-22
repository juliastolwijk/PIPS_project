# ============================================================
# Data helpers
# Purpose: Locate, read, validate and clean the OASIS dataset,
#          and translate column names / model terms into
#          readable labels.
# ============================================================


## 1. Dataset schema ------------------------------------------------

# Columns the analysis needs, and those that must be numeric.
REQUIRED_COLUMNS  <- c("ID", "M/F", "Age", "Educ", "SES",
                       "MMSE", "CDR", "eTIV", "nWBV")
NUMERIC_VARIABLES <- c("Age", "Educ", "SES", "MMSE",
                       "CDR", "eTIV", "nWBV")


## 2. Labels --------------------------------------------------------

# Display labels only; the original dataset column names stay unchanged.
# Direction of the OASIS scales: Educ 1 = lowest ... 5 = highest;
# SES 1 = highest ... 5 = lowest (Marcus et al., 2007).
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
# and the interaction term.
term_labels <- c(
  "(Intercept)"  = "Intercept",
  "nWBV"         = "Normalized Whole Brain Volume",
  "nWBV_c"       = "Brain Volume (centered)",
  "Age"          = "Age (years)",
  "Age_c"        = "Age (years, centered)",
  "Educ"         = "Education Level (1-5)",
  "SES"          = "Socioeconomic Status (1-5; lower = higher status)",
  "SexMale"      = "Sex (male vs female)",
  "nWBV_c:Age_c" = "Brain Volume x Age Interaction"
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


## 3. Reading the data ----------------------------------------------

#' Find the dataset file
#'
#' Looks for `file`; if it is not there, also tries the bare file name in
#' the working directory. In interactive sessions the user may then pick
#' the file; in scripts the function stops with an informative error.
#'
#' @param file Path to the dataset (.xlsx, .xls or .csv).
#' @return The path of an existing file.
resolve_data_file <- function(file) {
  candidates <- unique(c(file, basename(file)))
  found <- candidates[file.exists(candidates)]
  if (length(found) > 0) return(found[1])
  if (interactive()) {
    message("Dataset not found at '", file, "'. Please select the file.")
    return(file.choose())
  }
  stop("Dataset not found: ", file,
       ". Check the file name or change CONFIG$data_file.",
       call. = FALSE)
}

#' Read the OASIS cross-sectional file (Excel or CSV)
#'
#' @param file Path to an .xlsx, .xls or .csv file.
#' @return A tibble with the original column names.
read_oasis_file <- function(file) {
  extension <- tolower(tools::file_ext(file))
  if (extension %in% c("xlsx", "xls")) {
    readxl::read_excel(file)
  } else if (extension == "csv") {
    tibble::as_tibble(utils::read.csv(file, check.names = FALSE,
                                      na.strings = c("", "NA", "N/A")))
  } else {
    stop("Unsupported file type '.", extension,
         "'. Use .xlsx, .xls or .csv.", call. = FALSE)
  }
}


## 4. Validation and cleaning ---------------------------------------

# Convert to numeric, but stop if a real value would silently
# become NA
to_numeric_strict <- function(x, variable) {
  if (is.numeric(x)) return(x)
  x_chr <- trimws(as.character(x))
  x_chr[x_chr == "" | toupper(x_chr) %in% c("NA", "N/A")] <- NA
  converted <- suppressWarnings(as.numeric(x_chr))
  if (any(!is.na(x_chr) & is.na(converted))) {
    stop("Could not convert all values in column '", variable,
         "' to numeric. Check the original data.", call. = FALSE)
  }
  converted
}

#' Validate and clean the raw OASIS data
#'
#' Checks that all required columns exist, converts analysis variables to
#' numeric, creates a `Sex` factor (Female = reference) and a subject-level
#' `Subject` ID. OASIS IDs have the form "OAS1_0001_MR1", where the suffix
#' numbers the scanning session. The cross-sectional release contains a
#' small reliability subsample scanned a second time (Marcus et al., 2007);
#' keeping both scans would count those people twice and violate the
#' independence assumption of every test, so by default only the first
#' scan per subject is kept.
#'
#' @param raw_data Data frame as read by read_oasis_file().
#' @param keep_first_scan_only Drop repeat scans of the same subject?
#' @return The cleaned data frame (original columns plus `Sex`, `Subject`).
prepare_oasis_data <- function(raw_data, keep_first_scan_only = TRUE) {
  # Check that all required columns are present
  missing_columns <- setdiff(REQUIRED_COLUMNS, names(raw_data))
  if (length(missing_columns) > 0) {
    stop("Missing required columns: ",
         paste(missing_columns, collapse = ", "), call. = FALSE)
  }
  if (nrow(raw_data) == 0) stop("The dataset has no rows.", call. = FALSE)

  # Convert analysis variables to numeric, stopping if conversion fails.
  data <- raw_data
  for (variable in NUMERIC_VARIABLES) {
    data[[variable]] <- to_numeric_strict(data[[variable]], variable)
  }

  # Set sex as a factor with Female as the reference category.
  sex_clean <- toupper(trimws(as.character(data[["M/F"]])))
  data$Sex <- factor(sex_clean, levels = c("F", "M"),
                     labels = c("Female", "Male"))

  # Warn if any sex values are not coded as M or F.
  if (any(!is.na(sex_clean) & is.na(data$Sex))) {
    warning("Some values in 'M/F' were not 'M' or 'F' and were ",
            "set to missing.", call. = FALSE)
  }

  # Warn if participant IDs are duplicated.
  if (anyDuplicated(data$ID) > 0) {
    warning("Duplicate participant IDs found: ",
            sum(duplicated(data$ID)), " duplicate row(s).", call. = FALSE)
  }

  # Subject ID without the session suffix; session number for ordering.
  id <- as.character(data$ID)
  data$Subject <- sub("_MR[0-9]+$", "", id)
  session <- suppressWarnings(as.integer(sub("^.*_MR", "", id)))

  if (keep_first_scan_only) {
    # Keep the lowest session number per subject, preserving row order.
    ordering <- order(data$Subject, session, na.last = TRUE)
    keep <- logical(nrow(data))
    keep[ordering] <- !duplicated(data$Subject[ordering])
    data <- data[keep, ]
  }

  data
}

#' Count values outside their documented ranges
#'
#' Values outside these ranges usually point to data-entry problems.
#'
#' @param data Cleaned data from prepare_oasis_data().
#' @return A tibble with one row per check and the number of flagged rows.
check_value_ranges <- function(data) {
  tibble::tibble(
    Check = c("Age < 0 or > 120", "MMSE outside 0-30",
              "CDR not in {0, 0.5, 1, 2, 3}",
              "nWBV outside 0-1", "Educ outside 1-5",
              "SES outside 1-5"),
    `Rows Flagged` = c(
      sum(data$Age < 0 | data$Age > 120, na.rm = TRUE),
      sum(data$MMSE < 0 | data$MMSE > 30, na.rm = TRUE),
      sum(!data$CDR %in% c(0, 0.5, 1, 2, 3) & !is.na(data$CDR)),
      sum(data$nWBV <= 0 | data$nWBV >= 1, na.rm = TRUE),
      sum(data$Educ < 1 | data$Educ > 5, na.rm = TRUE),
      sum(data$SES < 1 | data$SES > 5, na.rm = TRUE)
    )
  )
}

#' Stop with an informative error if columns are missing
#'
#' @param data Data frame to check.
#' @param columns Column names that must be present.
#' @param context Short description used in the error message.
#' @return Invisibly, TRUE.
require_columns <- function(data, columns, context = "this analysis") {
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0) {
    stop("Columns needed for ", context, " are missing: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}
