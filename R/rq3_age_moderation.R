# ============================================================
# Research question 3
# Does the relationship between brain volume and cognitive
# performance depend on age?
# ============================================================

#' Run research question 3: age as a moderator of the nWBV-MMSE link
#'
#' Fits MMSE ~ nWBV_c * Age_c + Educ + SES + Sex with HC3 robust inference,
#' reports the interaction, an unadjusted sensitivity model, simple slopes at
#' chosen age percentiles, a prediction plot and an APA write-up.
#'
#' @param data Cleaned OASIS data (needs MMSE, nWBV, Age, Educ, SES, Sex).
#' @param alpha Significance level.
#' @param step nWBV increment used to express effects (e.g., 0.01).
#' @param min_complete_n Minimum number of complete cases to run the model.
#' @param age_probs Age percentiles at which simple slopes are reported.
#' @param age_window Prediction lines span the nWBV range of participants
#'   within +/- this many years of each reference age.
#' @param output_dir Folder for tables/plots; NULL means "do not save".
#' @return A list with `p_value` (interaction; NA if skipped), `apa`,
#'   `model` and `simple_slopes`.
run_rq3 <- function(data, alpha = 0.05, step = 0.01, min_complete_n = 30,
                    age_probs = c(0.25, 0.50, 0.75), age_window = 5,
                    output_dir = NULL) {
  require_columns(data, c("MMSE", "nWBV", "Age", "Educ", "SES", "Sex"),
                  "research question 3")

  apa <- rq_header(3, paste("Does the relationship between brain volume and",
                            "cognitive performance depend on age?"))

  age_percentile_text <- paste(percentile_label(age_probs), collapse = ", ")

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
      paste0("Simple slopes: the nWBV-MMSE slope at the following ages (",
             age_percentile_text, "), to show what the interaction ",
             "means in practice."),
      "A plot of the model-predicted nWBV-MMSE lines at those ages."
    )
  )

  # Use participants with complete data for all variables in this model.
  interaction_data <- data %>%
    select(MMSE, nWBV, Age, Educ, SES, Sex) %>%
    drop_na()

  results_header()

  # Skip this analysis if there are too few complete cases, or if age or
  # brain volume has no variation (an interaction cannot be estimated).
  if (nrow(interaction_data) < min_complete_n) {
    cat("\nThe moderation analysis was not performed because ",
        "there are too few complete observations (", nrow(interaction_data),
        " < ", min_complete_n, ").\n", sep = "")
    return(list(p_value = NA_real_, apa = apa))
  }
  if (length(unique(interaction_data$Age)) < 2 ||
      length(unique(interaction_data$nWBV)) < 2) {
    cat("\nThe moderation analysis was not performed because ",
        "age or brain volume has insufficient variation.\n", sep = "")
    return(list(p_value = NA_real_, apa = apa))
  }

  # Center age and brain volume around their sample means, so the main
  # effects refer to average age and average nWBV.
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
  interaction_tidy <- robust_tidy(interaction_model)

  # Report model coefficients with robust standard errors and confidence intervals.
  interaction_coefficients <- interaction_tidy %>%
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
              save_as = "14_moderation_coefficients", output_dir = output_dir)

  # Summarise overall model fit, including the variance explained.
  interaction_fit <- broom::glance(interaction_model) %>%
    transmute(
      Participants = nobs,
      R_squared = r.squared,
      Adjusted_R_squared = adj.r.squared,
      AIC, BIC
    )
  cat("\nModeration model fit:\n")
  print_table(interaction_fit, save_as = "14b_moderation_fit",
              output_dir = output_dir)

  # Extract the interaction term: this is the main test of moderation.
  interaction_result <- interaction_tidy %>%
    filter(term == "nWBV_c:Age_c")
  p_interaction <- interaction_result$p.value[1]

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
    save_as = "14c_interaction_test", output_dir = output_dir
  )

  # Sensitivity check: repeat the interaction test without covariates.
  # This checks whether the conclusion changes when education, SES and sex
  # are excluded from the model.
  unadjusted_model <- lm(MMSE ~ nWBV_c * Age_c, data = interaction_data)
  unadjusted_p <- robust_tidy(unadjusted_model) %>%
    filter(term == "nWBV_c:Age_c") %>%
    pull(p.value)
  cat("\nSensitivity check: the unadjusted interaction model gives ",
      fmt_p(unadjusted_p), "\n", sep = "")

  # ---- Simple slopes at representative ages -------------------

  # Choose reference ages using sample percentiles (default: younger,
  # middle and older participants).
  age_reference <- tibble::tibble(
    Percentile = percentile_label(age_probs),
    Age = as.numeric(quantile(interaction_data$Age, probs = age_probs))
  ) %>%
    mutate(
      Phrase = ifelse(Percentile == "Median", "at the median age",
                      paste0("at the ", Percentile, " of age")),
      Age_c = Age - mean_age,
      Age_Group = sprintf("%s: %.0f years", Percentile, Age)
    )

  # Estimate the nWBV-MMSE slope at each reference age.
  slopes <- bind_cols(
    tibble::tibble(`Illustrative Age` = age_reference$Age_Group),
    simple_slopes(interaction_model, interaction_vcov,
                  age_reference$Age_c, step = step)
  )

  cat("\nSimple slopes: change in MMSE per ", step,
      " higher nWBV at each reference age (robust CIs):\n", sep = "")
  print_table(slopes, save_as = "15_simple_slopes", output_dir = output_dir)

  # ---- Plot ----------------------------------------------------

  # Restrict each prediction line to the observed nWBV range near its age.
  # If fewer than five participants fall within the age window, use the
  # full sample range instead.
  modal_sex <- names(which.max(table(interaction_data$Sex)))

  prediction_grid <- bind_rows(lapply(
    seq_len(nrow(age_reference)),
    function(i) {
      nearby <- interaction_data %>%
        filter(abs(Age - age_reference$Age[i]) <= age_window)
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
      Age_Group = factor(Age_Group, levels = unique(age_reference$Age_Group))
    )

  # Calculate predicted MMSE and robust confidence intervals using the
  # model's HC3 covariance estimates.
  prediction_grid <- predict_with_ci(interaction_model, interaction_vcov,
                                     prediction_grid)

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
      aes(x = nWBV, y = Predicted,
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
        "+/-", age_window, "years of each reference age."
      )
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  save_plot(interaction_plot, "16_age_moderation_plot", output_dir = output_dir,
            width = 9, height = 6)

  # ---- APA interpretation ----------------------------------------

  # Prepare model statistics and significance decisions for the written report.
  apa_header()

  df_int      <- df.residual(interaction_model)
  null_int    <- lm(MMSE ~ 1, data = interaction_data)
  overall_int <- compare_nested(null_int, interaction_model,
                                "Null vs moderation model",
                                "All predictors")
  r2_int      <- summary(interaction_model)$r.squared
  adj_int     <- summary(interaction_model)$adj.r.squared
  int_sig     <- is_sig(p_interaction, alpha)
  int_pos     <- interaction_result$estimate > 0

  # Report overall fit and whether the interaction provides evidence of moderation.
  apa <- c(apa, apa_report(
    "A moderated multiple regression tested whether age moderated the ",
    "association between nWBV and MMSE (N = ", nrow(interaction_data),
    "). nWBV and age were mean-centered, education, SES, and sex were ",
    "included as covariates, and heteroscedasticity-robust (HC3) standard ",
    "errors were used. The model explained ", fmt_pct(r2_int),
    " of the variance in MMSE (", apa_sym$R2, " = ", fmt_bounded(r2_int, 3),
    ", adjusted ", apa_sym$R2, " = ", fmt_bounded(adj_int, 3), "), ",
    apa_F(overall_int), ". The nWBV ", apa_sym$times, " age interaction was ",
    pick(int_sig, "significant", "not significant"), " (",
    apa_b(interaction_result, df_int, scale = step,
          unit = paste0(" per ", step, "-unit nWBV per year of age")),
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
  ))

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
                              unit = paste0(" per ", step, "-unit nWBV")))
  }, character(1))

  apa <- c(apa, apa_report(
    "Simple slopes analyses estimated the association between nWBV and ",
    "MMSE at ", length(slope_sentences), " ages: ",
    paste(slope_sentences, collapse = "; "), "."
  ))

  # Compare the adjusted result with the unadjusted sensitivity analysis.
  apa <- c(apa, apa_report(
    "A model without covariates gave an interaction ",
    pick(is_sig(unadjusted_p, alpha) == int_sig, "consistent with", "different from"),
    " the adjusted model (", fmt_p(unadjusted_p), "). As in the previous ",
    "analyses, these cross-sectional estimates describe differences between ",
    "people of different ages and cannot show how brain volume or cognition ",
    "change within a person."
  ))

  list(
    p_value       = p_interaction,
    apa           = apa,
    model         = interaction_model,
    simple_slopes = slopes
  )
}
