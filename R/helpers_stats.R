# ============================================================
# Statistical helpers
# Purpose: Model summaries with robust standard errors, nested
#          model tests, diagnostics, CDR grouping, simple slopes
#          and the Holm correction across research questions.
# ============================================================

#' Coefficient table with HC3 heteroscedasticity-robust inference
#'
#' HC3 standard errors are used for every linear model in this project
#' because MMSE has a ceiling at 30 and unequal residual variance.
#'
#' @param model A fitted `lm` object.
#' @param model_name Optional label stored in the `Model` column.
#' @param level Confidence level of the intervals.
#' @return A tibble with term, estimate, std.error, statistic, p.value,
#'   conf.low, conf.high and a readable `Predictor` label.
robust_tidy <- function(model, model_name = NA_character_, level = 0.95) {
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

#' Compare nested linear models with an HC3 robust Wald F-test
#'
#' Also reports the change in R-squared after adding predictors.
#'
#' @param small,large Nested `lm` fits on the same observations.
#' @param comparison Label for the comparison (e.g., "Model 1 vs Model 2").
#' @param added Description of the predictors added in `large`.
#' @return A one-row tibble (see column names) for use with apa_F().
compare_nested <- function(small, large, comparison, added) {
  if (nobs(small) != nobs(large)) {
    stop("Nested models must be fitted to the same observations.",
         call. = FALSE)
  }
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

#' Variance inflation factors (VIFs) for a model's predictors
#'
#' Each column of the design matrix is regressed on all other columns;
#' VIF = 1 / (1 - R^2). Values above about 5 suggest multicollinearity.
#'
#' @param model A fitted `lm` object.
#' @return A named numeric vector with one VIF per design-matrix column.
calc_vif <- function(model) {
  design <- model.matrix(model)[, -1, drop = FALSE]
  if (ncol(design) < 2) return(setNames(1, colnames(design)))
  vapply(seq_len(ncol(design)), function(i) {
    r2 <- summary(lm(design[, i] ~ design[, -i, drop = FALSE]))$r.squared
    1 / (1 - r2)
  }, numeric(1)) %>%
    setNames(colnames(design))
}

#' Group CDR scores, merging small high-severity groups
#'
#' Merges the highest-severity CDR groups until each group has at least
#' `min_n` participants, while retaining at least two groups.
#'
#' @param cdr Numeric vector of CDR scores without missing values.
#' @param min_n Minimum group size.
#' @return An ordered-by-severity factor with readable group labels.
make_cdr_groups <- function(cdr, min_n) {
  if (anyNA(cdr)) stop("make_cdr_groups() needs CDR scores without NA.",
                       call. = FALSE)
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

#' Simple slopes of a focal predictor at chosen moderator values
#'
#' Estimates the focal slope (b_focal + b_interaction * moderator) at each
#' value in `moderator_values`, with standard errors from the supplied
#' (robust) covariance matrix. Slopes and CIs are
#' multiplied by `step`, so they describe a `step`-unit change.
#'
#' @param model An `lm` with the terms `focal` and `interaction`.
#' @param vc Covariance matrix of the coefficients (e.g., HC3).
#' @param moderator_values Values of the (centered) moderator.
#' @param step Size of the focal-predictor change the slopes refer to.
#' @param focal,interaction Names of the focal and interaction terms.
#' @param level Confidence level.
#' @return A tibble with one row per moderator value.
simple_slopes <- function(model, vc, moderator_values, step = 1,
                          focal = "nWBV_c", interaction = "nWBV_c:Age_c",
                          level = 0.95) {
  b <- coef(model)
  if (!all(c(focal, interaction) %in% names(b))) {
    stop("The model does not contain the terms '", focal, "' and '",
         interaction, "'.", call. = FALSE)
  }
  slope <- b[[focal]] + b[[interaction]] * moderator_values
  variance <- vc[focal, focal] + moderator_values^2 * vc[interaction, interaction] +
    2 * moderator_values * vc[focal, interaction]
  se <- sqrt(variance)
  df <- df.residual(model)
  crit <- qt(1 - (1 - level) / 2, df)
  tibble::tibble(
    Slope = slope * step,
    `Robust SE` = se * step,
    `95% CI Lower` = (slope - crit * se) * step,
    `95% CI Upper` = (slope + crit * se) * step,
    `t-value` = slope / se,
    `p-value` = 2 * pt(-abs(slope / se), df)
  )
}

#' Model predictions with confidence intervals from a given covariance
#'
#' @param model A fitted `lm`.
#' @param vc Covariance matrix of the coefficients (e.g., HC3).
#' @param newdata Data frame containing every predictor of `model`.
#' @param level Confidence level.
#' @return `newdata` with the columns Predicted, Lower_CI and Upper_CI.
predict_with_ci <- function(model, vc, newdata, level = 0.95) {
  design_matrix <- model.matrix(delete.response(terms(model)),
                                data = newdata,
                                xlev = model$xlevels)
  fitted_values <- drop(design_matrix %*% coef(model))
  fitted_se <- sqrt(rowSums((design_matrix %*% vc) * design_matrix))
  crit <- qt(1 - (1 - level) / 2, df.residual(model))

  newdata %>%
    mutate(
      Predicted = fitted_values,
      Lower_CI  = fitted_values - crit * fitted_se,
      Upper_CI  = fitted_values + crit * fitted_se
    )
}

#' Holm-adjusted p-values for the primary test of each research question
#'
#' Holm's procedure controls the family-wise error rate.
#' Missing p-values (a skipped analysis) are left as NA and do not count
#' towards the number of tests.
#'
#' @param hypotheses Character vector describing each primary test.
#' @param p_values Numeric vector of unadjusted p-values (same length).
#' @return A tibble with the hypothesis, raw and Holm-adjusted p-values.
holm_table <- function(hypotheses, p_values) {
  if (length(hypotheses) != length(p_values)) {
    stop("'hypotheses' and 'p_values' must have the same length.",
         call. = FALSE)
  }
  tibble::tibble(Hypothesis = hypotheses, `p-value` = p_values) %>%
    mutate(`Holm-adjusted p-value` = p.adjust(`p-value`, method = "holm"))
}

#' Ordinal label for a percentile (e.g., 0.25 -> "25th percentile")
#'
#' @param prob Probability between 0 and 1.
#' @return "Median" for 0.5, otherwise "<n>th percentile".
percentile_label <- function(prob) {
  n <- round(100 * prob)
  suffix <- ifelse(n %% 100 %in% 11:13, "th",
                   c("th", "st", "nd", "rd", rep("th", 6))[n %% 10 + 1])
  ifelse(prob == 0.5, "Median", paste0(n, suffix, " percentile"))
}
