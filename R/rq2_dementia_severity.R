# ============================================================
# Research question 2
# Does brain volume differ across levels of dementia severity?
# ============================================================

#' Run research question 2: nWBV across CDR groups
#'
#' Welch's ANOVA (primary test), Levene/Brown-Forsythe variance diagnostic,
#' Games-Howell post hoc tests, a Kruskal-Wallis sensitivity test and an
#' age- and sex-adjusted model, followed by a plot and an APA write-up.
#'
#' @param data Cleaned OASIS data (needs nWBV, CDR, Age, Sex).
#' @param alpha Significance level.
#' @param min_group_n CDR groups smaller than this are merged.
#' @param output_dir Folder for tables/plots; NULL means "do not save".
#' @return A list with `p_value` (Welch's ANOVA; NA if skipped), `apa`,
#'   `group_summary`, `welch` and `posthoc` (NULL if not run).
run_rq2 <- function(data, alpha = 0.05, min_group_n = 10, output_dir = NULL) {
  require_columns(data, c("nWBV", "CDR", "Age", "Sex"), "research question 2")

  apa <- rq_header(2, "Does brain volume differ across levels of dementia severity?")

  rq_background(
    explanation = paste(
      "This question asks whether average brain volume (nWBV) differs",
      "between people at different stages of dementia, as rated by the",
      "Clinical Dementia Rating (CDR: 0 = no dementia, 0.5 = very mild or",
      "questionable, 1 = mild, 2 = moderate). Age is strongly related to",
      "both brain volume and CDR, so we also test whether group differences",
      "persist after accounting for age and sex. CDR groups with fewer than",
      min_group_n, "participants are merged with the adjacent group",
      "to keep the comparisons stable."
    ),
    tests = c(
      "Descriptive statistics (mean and SD of nWBV, and mean age) per CDR group.",
      paste("Levene's test for equal variances (median-centred; a diagnostic",
            "only, Welch's test is used either way)."),
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
  rq2_data <- data %>%
    select(nWBV, CDR, Age, Sex) %>%
    drop_na()

  results_header()

  # Not enough distinct CDR values (or no data) for a group comparison.
  if (length(unique(rq2_data$CDR)) < 2) {
    cat("\nANOVA was not performed because fewer than two ",
        "dementia severity groups have usable observations.\n", sep = "")
    return(list(p_value = NA_real_, apa = apa, posthoc = NULL))
  }

  # Merge small CDR groups with adjacent groups to avoid unstable comparisons.
  rq2_data$CDR_group <- make_cdr_groups(rq2_data$CDR, min_group_n)

  # Report whether any original CDR groups were merged.
  if (nlevels(rq2_data$CDR_group) < length(unique(rq2_data$CDR))) {
    cat("\nNote: CDR groups with fewer than ", min_group_n,
        " participants were merged with the adjacent group.\n", sep = "")
  }

  # Count participants in each final group.
  group_sizes <- rq2_data %>%
    count(`Dementia Severity Group` = CDR_group, name = "Participants")

  cat("\nParticipants in each dementia severity group:\n")
  print_table(group_sizes, save_as = "08_group_sizes", output_dir = output_dir)

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
  print_table(volume_by_cdr, save_as = "09_volume_by_cdr", output_dir = output_dir)

  # Check whether group variances differ; Welch's ANOVA is used regardless.
  # rstatix centres on the median by default (Brown-Forsythe variant),
  # which is robust to skew.
  levene_result <- rstatix::levene_test(rq2_data, nWBV ~ CDR_group)
  cat("\nVariance diagnostic (Levene's test, median-centred):\n")
  print_table(levene_result, save_as = "10_levene", output_dir = output_dir)

  # Test whether mean nWBV differs across groups without assuming
  # equal variances.
  welch_result <- rstatix::welch_anova_test(rq2_data, nWBV ~ CDR_group)
  cat("\nWelch's ANOVA:\n")
  print_table(welch_result, save_as = "10b_welch_anova", output_dir = output_dir)

  # Estimate the proportion of nWBV variance associated with CDR group.
  # This uses a standard one-way ANOVA, not Welch's adjusted calculation.
  one_way <- anova(lm(nWBV ~ CDR_group, data = rq2_data))
  eta_squared <- one_way$`Sum Sq`[1] / sum(one_way$`Sum Sq`)
  cat("\nEffect size (eta-squared): ", fmt_num(eta_squared, 3), "\n", sep = "")

  p_welch <- welch_result$p[1]

  # Run pairwise comparisons only if the overall Welch test is significant.
  posthoc_result <- NULL

  if (is_sig(p_welch, alpha)) {

    # Games-Howell compares group pairs while
    # allowing unequal variances and group sizes.
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
    print_table(posthoc_result, save_as = "11_games_howell", output_dir = output_dir)
  }

  # Sensitivity check: test for group differences using ranks rather
  # than means.
  kruskal <- kruskal.test(nWBV ~ CDR_group, data = rq2_data)
  cat("\nSensitivity check, Kruskal-Wallis test: chi-squared = ",
      fmt_num(unname(kruskal$statistic), 2), ", df = ",
      unname(kruskal$parameter), ", ", fmt_p(kruskal$p.value),
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
  print_table(adjusted_test, save_as = "12_adjusted_group_test",
              output_dir = output_dir)

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
    save_as = "12b_adjusted_contrasts", output_dir = output_dir
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
  save_plot(brain_volume_cdr_plot, "13_brain_volume_by_cdr",
            output_dir = output_dir)

  # ---- APA interpretation ----------------------------------------

  # Set flags for the significance decisions used in the written report.
  apa_header()

  welch_sig <- is_sig(p_welch, alpha)
  adj_sig   <- is_sig(adjusted_test$`p-value`, alpha)
  kw_sig    <- is_sig(kruskal$p.value, alpha)

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
  apa <- c(apa, apa_report(
    "A Welch's one-way ANOVA compared nWBV across ",
    nlevels(rq2_data$CDR_group), " dementia severity (CDR) groups (N = ",
    nrow(rq2_data), "). A median-centred Levene's test indicated that ",
    "variances were ",
    pick(is_sig(levene_result$p, alpha), "unequal", "comparable"),
    " across groups, F(", fmt_df(levene_result$df1), ", ",
    fmt_df(levene_result$df2), ") = ", fmt_num(levene_result$statistic, 2),
    ", ", fmt_p(levene_result$p), "; Welch's correction was applied in ",
    "either case. Mean nWBV ",
    pick(welch_sig, "differed significantly", "did not differ significantly"),
    " across CDR groups, Welch's F(", fmt_df(welch_result$DFn), ", ",
    fmt_df(welch_result$DFd), ") = ", fmt_num(welch_result$statistic, 2),
    ", ", fmt_p(p_welch), ", ", apa_sym$eta2, " = ",
    fmt_bounded(eta_squared, 2), " (", eta_label(eta_squared), " effect)."
  ))

  apa <- c(apa, apa_report("Group means were: ", group_descriptions, "."))

  if (welch_sig && !is.null(posthoc_result)) {

    # Use observed group means to describe which group has higher nWBV.
    group_means <- setNames(volume_by_cdr$`Mean nWBV`,
                            as.character(volume_by_cdr$`Dementia Severity Group`))

    # Keep only significant pairwise comparisons for the detailed report.
    significant_rows <- posthoc_result %>%
      filter(!is.na(`Adjusted p-value`), `Adjusted p-value` < alpha)
    n_other_pairs <- nrow(posthoc_result) - nrow(significant_rows)

    pair_sentences <- vapply(
      seq_len(nrow(significant_rows)),
      function(i) describe_posthoc_pair(significant_rows[i, ], group_means),
      character(1)
    )

    # Report significant pairs and note any remaining non-significant comparisons.
    apa <- c(apa, apa_report(
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
    ))
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
  apa <- c(apa, apa_report(
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
  ))

  # Note that cross-sectional results cannot establish causality or change over time.
  apa <- c(apa, apa_report(
    "These group differences are cross-sectional and observational; they ",
    "do not show that dementia severity causes lower brain volume, or how ",
    "brain volume changes within a person over time. Group sizes were ",
    "unequal, and small groups at higher CDR levels limit the precision of ",
    "those estimates."
  ))

  list(
    p_value       = p_welch,
    apa           = apa,
    group_summary = volume_by_cdr,
    welch         = welch_result,
    posthoc       = posthoc_result
  )
}

#' Describe one significant Games-Howell comparison in APA style
#'
#' The reported difference always follows the higher-volume minus
#' lower-volume direction; the estimate and CI are flipped if needed.
#'
#' @param row One row of the post hoc table built in run_rq2().
#' @param group_means Named vector of observed mean nWBV per group.
#' @return A character string (one clause).
describe_posthoc_pair <- function(row, group_means) {
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
