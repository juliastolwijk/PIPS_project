# src/r/data_prep.R
# Purpose: Load, validate, and clean the OASIS cross-sectional MRI dataset.

ensure_packages <- function(pkgs) {
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, repos = "https://cloud.r-project.org")
    }
  }
}

#' Load and Clean OASIS Cross-Sectional Data
load_oasis_data <- function(file_path = NULL, exclude_repeat_scans = TRUE) {
  ensure_packages(c("readxl", "dplyr"))

  if (is.null(file_path) || !file.exists(file_path)) {
    candidates <- c(
      list.files("data", pattern = "^oasis.*cross-sectional.*\\.xlsx$", full.names = TRUE),
      list.files(".", pattern = "^oasis.*cross-sectional.*\\.xlsx$", full.names = TRUE)
    )
    if (length(candidates) == 0) {
      stop("OASIS Excel file not found. Ensure the .xlsx file is in the 'data/' folder.")
    }
    file_path <- candidates[1]
  }

  df <- readxl::read_excel(file_path)

  # Filter out short-delay reliability scans (OAS1_xxxx_MR2) to ensure independent observations
  if (exclude_repeat_scans) {
    df <- df |> dplyr::filter(!grepl("_MR2$", ID))
  }

  # Clean categorical variables
  df <- df |>
    dplyr::mutate(
      Educ = ifelse(is.na(Educ), "Missing", as.character(Educ)),
      SES  = ifelse(is.na(SES),  "Missing", as.character(SES)),
      CDR  = ifelse(is.na(CDR),  "Missing", as.character(CDR))
    )

  return(df)
}
