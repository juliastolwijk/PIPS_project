# ============================================================
# Research question 1
# Does brain volume predict cognitive performance?
# ============================================================

#' Run research question 1: hierarchical regression of MMSE on nWBV
#'
#' Fits four nested models (nWBV; + age; + education and SES; + sex) on the
#' same complete cases, prints tables, diagnostics, plots and an APA
#' interpretation, and returns the key results.
#'
#' @param data Cleaned OASIS data (needs MMSE, nWBV, Age, Educ, SES, Sex).
#' @param alpha Significance level.
#' @param step nWBV increment used to express effects (e.g., 0.01).
#' @param min_complete_n Minimum number of complete cases to run the models.
#' @param output_dir Folder for tables/plots; NULL means "do not save".
#' @return A list with `p_value` (nWBV in the final model; NA if skipped),
#'   `apa` (character vector of the APA write-up), `models`,
#'   `coefficients` and `model_fit`.
run_rq1 <- function(data, alpha = 0.05, step = 0.01, min_complete_n = 30,
                    output_dir = NULL) {
  require_columns(data, c("MMSE", "nWBV", "Age", "Educ", "SES", "Sex"),
                  "research question 1")

  apa <- rq_header(1, "Does brain volume predict cognitive performance?")

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

  # Keep participants with complete data for all four models
  regression_data <- data %>%
    select(MMSE, nWBV, Age, Educ, SES, Sex) %>%
    drop_na()

  results_header()

  # Skip this research question (instead of stopping the whole script)
  # if the sample is too small for stable estimates.
  if (nrow(regression_data) < min_complete_n) {
    cat("\nThe regression analyses were not performed because there are ",
        "too few complete observations (", nrow(regression_data), " < ",
        min_complete_n, ").\n", sep = "")
    return(list(p_value = NA_real_, apa = apa))
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

  # Report the number of participants included in the models.
  cat("\nComplete cases used:", nrow(regression_data),
      "of", nrow(data), "\n")

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
              save_as = "04_regression_coefficients", output_dir = output_dir)

  # Standardize numeric variables to compare coefficient sizes
  # on a common scale.
  standardized_data <- regression_data %>%
    mutate(across(c(MMSE, nWBV, Age, Educ, SES),
                  ~ as.numeric(scale(.x))))

  standardized_model <- lm(
    MMSE ~ nWBV + Age + Educ + SES + Sex,
    data = standardized_data
  )
  standardized_tidy <- robust_tidy(standardized_model)

  # Report standardized coefficients for the final model.
  standardized_coefficients <- standardized_tidy %>%
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
              save_as = "04b_standardized_coefficients", output_dir = output_dir)

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
  print_table(model_fit, save_as = "05_model_fit", output_dir = output_dir)

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
  print_table(model_comparisons, save_as = "05b_model_comparisons",
              output_dir = output_dir)

  # ---- Assumption checks (final model) ---------------------------

  # Check multicollinearity among the predictors using VIF.
  vif_values <- calc_vif(final_model)

  cat("\nCollinearity (variance inflation factors, Model 4):\n")
  print_table(
    tibble::tibble(
      Predictor = get_term_label(names(vif_values)),
      VIF = unname(vif_values)
    ),
    save_as = "06_vif", output_dir = output_dir
  )

  # Test for heteroscedasticity (studentized Breusch-Pagan test)
  bp_test <- lmtest::bptest(final_model)
  cat("\nBreusch-Pagan test for heteroscedasticity: statistic = ",
      fmt_num(unname(bp_test$statistic), 2), ", df = ",
      unname(bp_test$parameter), ", ", fmt_p(bp_test$p.value),
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
  print_table(influence_table, save_as = "06b_influence", output_dir = output_dir)

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
  save_plot(residual_plot, "06c_residuals_vs_fitted", output_dir = output_dir)

  # Assess whether residuals approximately follow a normal distribution.
  qq_plot <- ggplot(augmented, aes(sample = .std.resid)) +
    stat_qq(alpha = 0.4) +
    stat_qq_line() +
    labs(title = "Normal Q-Q Plot of Residuals (Model 4)",
         x = "Theoretical quantiles", y = "Standardized residual") +
    theme_minimal(base_size = 12)
  save_plot(qq_plot, "06d_qq_plot", output_dir = output_dir)

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
  save_plot(brain_cognition_plot, "07_brain_volume_vs_mmse",
            output_dir = output_dir)

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
  nwbv_std    <- standardized_tidy %>% filter(term == "nWBV")
  nwbv_sig    <- is_sig(nwbv_row$p.value, alpha)
  nwbv_pos    <- nwbv_row$estimate > 0

  # Describe whether each step significantly improved model fit.
  step_sentences <- vapply(seq_len(nrow(model_comparisons)), function(i) {
    row <- model_comparisons[i, ]
    paste0(
      "Adding ", tolower(row$`Additional Predictors`), " ",
      pick(is_sig(row$`p-value`, alpha), "significantly improved",
           "did not significantly improve"),
      " model fit (", apa_sym$dR2, " = ",
      fmt_bounded(row$`Delta R-squared`, 3), ", ", apa_F(row), ")."
    )
  }, character(1))

  # Report the overall model fit and the contribution of each
  # block of added predictors.
  apa <- c(apa, apa_report(
    "A hierarchical multiple regression with heteroscedasticity-robust (HC3) ",
    "standard errors tested whether normalized whole brain volume (nWBV) ",
    "predicted cognitive performance (MMSE) in N = ", nrow(regression_data),
    " participants with complete data. nWBV alone explained ",
    fmt_pct(r2_model1), " of the variance in MMSE, ", apa_F(model1_test), ". ",
    paste(step_sentences, collapse = " "), " The final model explained ",
    fmt_pct(r2_final), " of the variance in MMSE (", apa_sym$R2, " = ",
    fmt_bounded(r2_final, 3), ", adjusted ", apa_sym$R2, " = ",
    fmt_bounded(adj_final, 3), "), ", apa_F(overall_test), "."
  ))

  # Report the adjusted nWBV coefficient and its standardized beta.
  apa <- c(apa, apa_report(
    "In the final model, nWBV was ",
    pick(nwbv_sig,
         paste0("a significant ", pick(nwbv_pos, "positive", "negative"),
                " predictor of MMSE"),
         "not a significant predictor of MMSE"),
    " (", apa_b(nwbv_row, df_resid, scale = step,
                unit = paste0(" per ", step, "-unit increase in nWBV")),
    "; ", apa_sym$beta, " = ", fmt_bounded(nwbv_std$estimate, 2), "). ",
    pick(nwbv_sig,
         paste0("Holding age, education, SES, and sex constant, a ", step,
                "-unit higher nWBV was associated with a predicted MMSE ",
                "score that was ", fmt_num(abs(nwbv_row$estimate * step), 2),
                " points ", pick(nwbv_pos, "higher", "lower"), "."),
         paste0("There was insufficient evidence that brain volume was ",
                "associated with MMSE after accounting for the covariates."))
  ))

  # Summarize the adjusted associations of age, education, SES,
  # and sex with MMSE.
  covariate_labels <- c(Age = "age", Educ = "education", SES = "SES",
                        SexMale = "sex (male vs. female)")

  covariate_sentences <- vapply(names(covariate_labels), function(term_name) {
    row <- final_coefs[final_coefs$term == term_name, ]
    if (nrow(row) == 0) return(NA_character_)
    paste0(covariate_labels[[term_name]], " was ",
           pick(is_sig(row$p.value, alpha), "a significant", "not a significant"),
           " predictor (", apa_b(row, df_resid, include_t = FALSE), ")")
  }, character(1))

  apa <- c(apa, apa_report(
    "Among the covariates, ",
    paste(covariate_sentences[!is.na(covariate_sentences)], collapse = "; "),
    "."
  ))

  # Summarize multicollinearity, heteroscedasticity, and influential cases.
  n_influential <- influence_table$`Observations above threshold`
  apa <- c(apa, apa_report(
    "Assumption checks for the final model: variance inflation factors ",
    "ranged from ", fmt_num(min(vif_values), 2), " to ",
    fmt_num(max(vif_values), 2),
    pick(max(vif_values) < 5,
         ", indicating no problematic multicollinearity. ",
         ", suggesting possible multicollinearity. "),
    "The Breusch–Pagan test ",
    pick(is_sig(bp_test$p.value, alpha),
         "indicated heteroscedasticity",
         "did not indicate heteroscedasticity"),
    ", ", apa_sym$chi2, "(", unname(bp_test$parameter), ") = ",
    fmt_num(unname(bp_test$statistic), 2), ", ", fmt_p(bp_test$p.value),
    "; robust standard errors were used in either case. ",
    n_influential, " of ", nrow(augmented), " observations (",
    fmt_pct(n_influential / nrow(augmented)),
    ") exceeded the Cook's distance threshold of 4/n (maximum = ",
    fmt_num(influence_table$`Max Cook's distance`, 3), ")."
  ))

  # Clarify that cross-sectional associations do not establish causality.
  apa <- c(apa, apa_report(
    "These results are cross-sectional and describe differences between ",
    "people, not changes within a person or a causal effect of brain volume ",
    "on cognition. Because MMSE is capped at 30, associations may be ",
    "attenuated among cognitively healthy participants."
  ))

  list(
    p_value      = nwbv_row$p.value,
    apa          = apa,
    models       = models,
    coefficients = regression_coefficients,
    model_fit    = model_fit
  )
}
