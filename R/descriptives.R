# ============================================================
# Descriptive statistics, missing data and sample composition
# ============================================================

#' Descriptive statistics (N, mean, SD, median, min, max) per variable
#'
#' @param data Data frame.
#' @param variables Numeric columns to describe, in display order.
#' @return A tibble with one row per variable (labelled).
describe_variables <- function(data, variables) {
  require_columns(data, variables, "the descriptive statistics")
  data %>%
    select(all_of(variables)) %>%
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
    mutate(Variable = factor(Variable, levels = variables)) %>%
    arrange(Variable) %>%
    mutate(Variable = get_variable_label(as.character(Variable)))
}

#' Number and percentage of missing values per column
#'
#' @param data Data frame.
#' @return A tibble sorted from most to least missing.
summarise_missing <- function(data) {
  tibble::tibble(
    Variable = names(data),
    Missing_N = vapply(data, function(x) sum(is.na(x)), integer(1))
  ) %>%
    mutate(
      Variable = get_variable_label(Variable),
      Missing_Percent = round(100 * Missing_N / nrow(data), 2)
    ) %>%
    arrange(desc(Missing_N))
}

#' Compare the age of participants with and without cognitive data
#'
#' In OASIS, cognitive data exist only for older adults, so missingness
#' is expected to be related to age. Checking this shows which population
#' the cognitive analyses describe.
#'
#' @param data Data frame with Age, MMSE and CDR.
#' @return A tibble with one row per availability group.
summarise_missing_by_age <- function(data) {
  data %>%
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
}

#' Number of participants in each CDR category (labelled)
#'
#' @param data Data frame with a CDR column.
#' @return A tibble with the severity label and count.
count_by_cdr <- function(data) {
  data %>%
    filter(!is.na(CDR)) %>%
    count(CDR, name = "Participants") %>%
    mutate(`Dementia Severity Group` = get_cdr_label(CDR)) %>%
    select(`Dementia Severity Group`, Participants)
}

#' APA-style description of the full sample and the cognitive subgroup
#'
#' @param data Cleaned data frame.
#' @return A single character string (one paragraph).
describe_sample_apa <- function(data) {
  n_sex_known   <- sum(!is.na(data$Sex))
  share_female  <- sum(data$Sex == "Female", na.rm = TRUE) / n_sex_known
  cognitive_sub <- data %>% filter(!is.na(MMSE), !is.na(CDR))

  paste0(
    "The sample comprised N = ", nrow(data), " adults (",
    fmt_pct(share_female), " female) aged ",
    min(data$Age, na.rm = TRUE), " to ",
    max(data$Age, na.rm = TRUE), " years (M = ",
    fmt_num(mean(data$Age, na.rm = TRUE), 1), ", SD = ",
    fmt_num(sd(data$Age, na.rm = TRUE), 1),
    "). Mean normalized whole brain volume (nWBV) was M = ",
    fmt_bounded(mean(data$nWBV, na.rm = TRUE), 3), " (SD = ",
    fmt_bounded(sd(data$nWBV, na.rm = TRUE), 3),
    "). Cognitive and clinical measures (MMSE and CDR) were available for n = ",
    nrow(cognitive_sub), " participants (age range ",
    min(cognitive_sub$Age, na.rm = TRUE), " to ",
    max(cognitive_sub$Age, na.rm = TRUE), " years; M = ",
    fmt_num(mean(cognitive_sub$Age, na.rm = TRUE), 1), ", SD = ",
    fmt_num(sd(cognitive_sub$Age, na.rm = TRUE), 1),
    "); analyses involving these measures therefore describe this subgroup."
  )
}
