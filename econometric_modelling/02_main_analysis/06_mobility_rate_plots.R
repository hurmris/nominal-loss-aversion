# R Script Title: Mobility rate plots
# Author: Risto Hurmeranta
# Description:  Plots outcome variable means by 2% expected return bins, conditional
#               on spell length, LTV and year fixed effects. Used as visual inspection
#               of the relationship between expected returns and outcomes before
#               the main regression estimation.
#               Produces two sets of plots:
#               1. Main plots: all outcome variables over full sample
#               2. Heterogeneity plots: sale, move, landlord, no-sale and rental
#                  outcomes split by LTV (above/below 50%) and spell length (above/below 5 years)
#
# Input:  Sourced via 01_read_data.R:
#           W:/Nominal_loss_aversion/Hurmeranta/data/final_data/panel/analysis_panel_hki_tku_tre_20062018.rds
# Output: W:/Nominal_loss_aversion/Hurmeranta/results/main_results/plots/mobility_rate_plots/
#           bin_avg_[y_var].png                              (one per outcome, full sample)
#           bin_avg_[y_var]_[subsample_name].png             (one per outcome x subsample)
# Note:   Model uses no-intercept specification (~ -1) so bin coefficients represent
#         outcome means within each return bin conditional on controls.
#         Dotted vertical lines mark the donut hole boundaries (+-5%) used in main analysis.

# Required packages ----
library(dplyr)
library(ggplot2)
library(broom)
library(estimatr)


# Define path to folders where results saved ----
mobility_plot_path <- "W:/Nominal_loss_aversion/Hurmeranta/results/main_results/plots/mobility_rate_plots/"

# Read data ----
source("W:/Nominal_loss_aversion/Hurmeranta/code/econometric_modelling/02_main_analysis/01_read_data.R")
dependent_vars <- dependent_vars %>% filter(!(y_var %in% c("y_becomes_landlord_t2_muutto",
                                                         "y_becomes_landlord_t3_muutto",
                                                         "y_ei_myynti_t2_muutto","y_ei_myynti_t3_muutto")))

# Main plots ----
# lm_robust with HC1 standard errors used to get conditional bin means
# with correct uncertainty -- robust to heteroskedasticity
# Vertical lines:
#   solid at [0, 0.02): cutoff between loss and gain domain
#   dotted at [-0.06,-0.04) and [0.04,0.06): donut hole edges

for (i in 1:nrow(dependent_vars)) {
  y <- dependent_vars$y_var[i]
  y_label <- dependent_vars$y_label[i]

  # Main plot
  plot <- lm_robust(
    formula = as.formula(paste0(y, "~ - 1 +  expected_return_bin_2pct + spell_length_c + spell_length_c_sq + ltv_custom_bin + vuosi")),
    data = panel,
    se_type = "HC1"
  ) %>%
    tidy() %>%
    filter(startsWith(term, "expected_return_bin_2pct")) %>%
    mutate(
      term = str_remove(term, pattern = "expected_return_bin_2pct"),
      term = factor(term, levels = term)
    ) %>%
    ggplot(data = ., aes(x = term, y = estimate)) +
    geom_vline(xintercept = "[0,0.02)", color = "gray80", linetype = "solid") +
    geom_vline(xintercept = "[-0.06,-0.04)", color = "gray80", linetype = "dotted", linewidth = 0.8 / overleaf_scale) +
    geom_vline(xintercept = "[0.04,0.06)", color = "gray80", linetype = "dotted", linewidth = 0.8 / overleaf_scale) +
    geom_point(
      size = 2 / overleaf_scale,
      color = "black"
    ) +
    xlab("Expected returns (2% bins)") +
    ylab("Estimate") +
    geom_errorbar(
      aes(
        ymin = conf.low,
        ymax = conf.high
      ),
      linewidth = 0.7 / overleaf_scale,
      width = .2 / overleaf_scale,
      color = "black"
    ) +
    theme(axis.text.x = element_text(angle = 55, hjust = 1))

  print(plot)

  ggsave(paste0(mobility_plot_path, "bin_avg_", y, ".png"), plot = plot, width = 8, height = 6, units = "in")
  rm(plot)
}


# Heterogeneity plots ----
# Four subsamples from crossing LTV and spell length dummies:
#   low_ltv  = ltv_gt50 == 0  (LTV <= 50%)
#   high_ltv = ltv_gt50 == 1  (LTV > 50%)
#   short_spell = spell_length_gt5 == 0  (spell <= 5 years)
#   long_spell  = spell_length_gt5 == 1  (spell > 5 years)
# Restricted to 5 key outcomes to keep number of plots manageable

low_ltv_short_spell <- panel %>% filter(ltv_gt50 == 0, spell_length_gt5 == 0)
low_ltv_long_spell <- panel %>% filter(ltv_gt50 == 0, spell_length_gt5 == 1)
high_ltv_short_spell <- panel %>% filter(ltv_gt50 == 1, spell_length_gt5 == 0)
high_ltv_long_spell <- panel %>% filter(ltv_gt50 == 1, spell_length_gt5 == 1)


sample_list <- list(
  "low_ltv_short_spell" = low_ltv_short_spell,
  "low_ltv_long_spell" = low_ltv_long_spell,
  "high_ltv_short_spell" = high_ltv_short_spell,
  "high_ltv_long_spell" = high_ltv_long_spell)

rm(low_ltv_short_spell, low_ltv_long_spell, high_ltv_short_spell, high_ltv_long_spell)



dependent_vars <- dependent_vars %>% filter(y_var %in% c("y_myynti", "y_muutto","y_becomes_landlord_t1_muutto","y_ei_myynti_t1_muutto","y_vuokra_muutto"))

for (i in 1:nrow(dependent_vars)) {
  y <- dependent_vars$y_var[i]
  y_label <- dependent_vars$y_label[i]


  # Loop model over sub-samples
  for (j in c(1:length(sample_list))) {
    fit_sample <- sample_list[[j]]
    subsample_name <- names(sample_list)[j]

    # Fit subsample model
    het_plot <- lm_robust(
      formula = as.formula(paste0(y, "~ - 1 +  expected_return_bin_2pct + spell_length_c + spell_length_c_sq + ltv_custom_bin + vuosi")),
      data = fit_sample, se_type = "HC1"
    ) %>%
      tidy() %>%
      filter(startsWith(term, "expected_return_bin_2pct")) %>%
      mutate(
        term = str_remove(term, pattern = "expected_return_bin_2pct"),
        term = factor(term, levels = term)
      ) %>%
      ggplot(data = ., aes(x = term, y = estimate)) +
      geom_vline(xintercept = "[0,0.02)", color = "gray80", linetype = "solid") +
      geom_vline(xintercept = "[-0.06,-0.04)", color = "gray80", linetype = "dotted", linewidth = 0.8 / overleaf_scale) +
      geom_vline(xintercept = "[0.04,0.06)", color = "gray80", linetype = "dotted", linewidth = 0.8 / overleaf_scale) +
      geom_point(
        size = 2 / overleaf_scale,
        color = "black"
      ) +
      xlab("Expected returns (2% bins)") +
      ylab("Estimate") +
      geom_errorbar(
        aes(
          ymin = conf.low,
          ymax = conf.high
        ),
        linewidth = 0.7 / overleaf_scale,
        width = .2 / overleaf_scale,
        color = "black"
      ) +
      theme(axis.text.x = element_text(angle = 55, hjust = 1))


    print(het_plot)

    ggsave(paste0(mobility_plot_path, "bin_avg_", y, "_", subsample_name, ".png"), plot = het_plot, width = 8, height = 6, units = "in")
  }
}


rm(list = ls())
gc()
