 R Script Title: RDrobust models
# Author: Risto Hurmeranta
# Description:  Fits rdrobust local polynomial RDD models as robustness check
#               for the main linear specification. Loops over all combinations of:
#               - Donut sizes: none, 2.5%, 5%
#               - Bandwidths: MSE-optimal (rd), MSE-optimal (two), 15%, 20%
#               - Outcomes: all selected_deps outcomes
#               Uses uniform kernel, nearest-neighbour variance estimation,
#               and bias-corrected robust inference for optimal bandwidth models.
#
# Input:  Sourced via 01_read_data.R:
#           W:/Nominal_loss_aversion/Hurmeranta/data/final_data/panel/analysis_panel_hki_tku_tre_20062018.rds
# Output: W:/Nominal_loss_aversion/Hurmeranta/results/main_results/regression_tables/rdrobust/
#           main_rdrobust_results.rds          (all results in single file)
#           rdd_robustness_[y_var].tex         (one table per outcome)
#         W:/Nominal_loss_aversion/Hurmeranta/results/main_results/plots/rdrobust_plots/
#           rdd_coef_[y_var].png               (one plot per outcome)
# Note:   Robust bias-corrected estimates used for MSE-optimal bandwidths.
#         Conventional estimates used for manual bandwidths.



# Required packages ----
library(dplyr)
library(ggplot2)
library(kableExtra)
library(tidyr)
library(stringr)
library(rdrobust)
library(data.table)

# Define path to folders where results saved ----
reg_tables_path <- "W:/Nominal_loss_aversion/Hurmeranta/results/main_results/regression_tables/rdrobust/"
plots_path <- "W:/Nominal_loss_aversion/Hurmeranta/results/main_results/plots/rdrobust_plots/"

# Read data ----
source("W:/Nominal_loss_aversion/Hurmeranta/code/econometric_modelling/02_main_analysis/01_read_data.R")

panel <- panel %>%
  filter(omistusasunto == 1) %>%
  drop_na(ltv_custom_bin) %>%
  select(
    selected_deps$y_var,
    expected_return_pct, spell_length_c, spell_length_c_sq, ltv_custom_bin, vuosi, mean_owner_ika_c, korkeakoulutus, kturaha_c, lapsi
  ) %>%
  fastDummies::dummy_cols(., select_columns = c("ltv_custom_bin", "vuosi"), remove_first_dummy = TRUE, remove_selected_columns = TRUE) %>%
  as.data.table()

gc()

# Define settings for rdrobust models  ----

outcomes <- selected_deps$y_var
donut_sizes <- c(NA, 0.025, 0.05)
bandwidths <- c("mserd", "msetwo", 0.15, 0.20)
covariate_names <- panel %>%
  select(spell_length_c, spell_length_c_sq, mean_owner_ika_c, korkeakoulutus, kturaha_c, lapsi, starts_with("vuosi"), starts_with("ltv_")) %>%
  names()
results <- tibble()


# Custom function for results collection ----

custom_result_store <- function(est, donut) { # This takes rdrobust outcome as argument and extracts the specified information
  tibble(
    y_var = outcome,
    loss_estimate = est$coef[1], # "Conventional" without bias-correction
    ci_l = est$ci[1, 1],
    ci_u = est$ci[1, 2],
    se = est$Estimate[3],
    loss_estimate_robust = est$coef[3], # "Robust" with bias-correction
    ci_l_robust = est$ci[3, 1],
    ci_u_robust = est$ci[3, 2],
    se_robust = est$Estimate[4],
    beta_Y_p_l = est$beta_Y_p_l, # Left of the cutoff estimate for outcome variable
    beta_Y_p_r = est$beta_Y_p_r, # Right of the cut off estimate  outcome variable
    bw_type = est$bwselect,
    bw_l = est$bws[1, 1],
    bw_h = est$bws[1, 2],
    N_h_l = est$N_h[1],
    N_h_r = est$N_h[2],
    d = donut,
    p = est$p,
    kernel = est$kernel
  )
}


# Bandwidth / donut loop structure ----
# Outer loop: donut sizes (controls exclusion zone around cutoff)
# Middle loop: bandwidths (controls how far from cutoff to use data)
# Inner loop: outcomes

# Loop over donut sizes
for (donut in donut_sizes) {
  panel_donut <- if (is.na(donut)) {
    panel # No donut
  } else {
    panel[abs(expected_return_pct) >= donut] # Apply donut trimming
  }

  # Loop over bandwidths
  for (bw in bandwidths) {
    # Trim for manual bandwidths (to make manual bw:s as close as results from main_model)
    panel_trimmed <- if (bw %in% c("mserd", "msetwo")) {
      panel_donut # No trimming
    } else {
      panel_trimmed <- panel_donut[abs(expected_return_pct) <= as.numeric(bw)]
    }


    # Loop over outcomes
    for (outcome in outcomes) {
      covs <- panel_trimmed %>% select(all_of(covariate_names))
      y_var <- panel_trimmed[[outcome]]
      x_var <- -panel_trimmed$expected_return_pct # Obs! Convert here to negative so that the treatment is LOSS rather than GAIN!

      # Run rdrobust
      est <- if (bw %in% c("mserd", "msetwo")) {
        rdrobust(
          y = y_var,
          x = x_var,
          covs = covs,
          kernel = "uniform",
          p = 1,
          vce = "nn",
          masspoints = "off",
          bwselect = bw
        ) # rdbwselect() bandwidths to bw_select!
      } else {
        rdrobust(
          y = y_var,
          x = x_var,
          covs = covs,
          kernel = "uniform",
          p = 1,
          vce = "nn",
          masspoints = "off",
          h = as.numeric(bw)
        ) # Manual bandwidhts to h!
      }

      # Store results
      results <- bind_rows(results, custom_result_store(est, donut))

      # Clean memory
      rm(est)
      gc()
    }
    # Free memory after all outcomes are looped for given bandwidth
    rm(covs, x_var, y_var, panel_trimmed)
    gc()
  }
  # Free memory after all bandwidths are looped for given donut size
  rm(panel_donut)
  gc()
}

saveRDS(results, paste0(reg_tables_path,"main_rdrobust_results.rds"))
gc()


# Main results: Plot ----

# Custom function for plots
custom_coef_plot <- function(i) {
  print(
    results %>%
      filter(y_var == i) %>%
      mutate(
        loss_estimate = if_else(bw_type != "Manual", loss_estimate_robust, loss_estimate), # Plot robust stuff for optimal bw models!
        ci_l = if_else(bw_type != "Manual", ci_l_robust, ci_l),
        ci_u = if_else(bw_type != "Manual", ci_u_robust, ci_u),
        bw_type = if_else(bw_type == "Manual", paste0(bw_type, ": ", as.character(bw_l)), bw_type),
        donut_type = if_else(is.na(d), "-", as.character(d))
      ) %>%
      ggplot(aes(x = bw_type, y = loss_estimate, group = donut_type)) +
      geom_point(aes(color = donut_type, shape = donut_type), position = position_dodge(width = 0.5), size = 2 / overleaf_scale) +
      geom_errorbar(aes(ymin = ci_l, ymax = ci_u, color = donut_type),
        position = position_dodge(width = 0.5), width = 0.2, linewidth = 0.7 / overleaf_scale
      ) +
      scale_color_manual(values = c("#1F78B4", "#33A02C", "#E31A1C")) + # These are from brewer.pal(n = 8,name = "Paired"),  display.brewer.pal(n = 8,name = "Paired")
      labs(x = " ", y = "Estimate", color = "Donut size", shape = "Donut size")
  )
}

# Loop plots for all outcomes
for (outcome in outcomes) {
  plot <- custom_coef_plot(outcome)

  ggsave(paste0(plots_path,"rdd_coef_", outcome, ".png"), plot = plot, width = 8, height = 6, units = "in")
}



# Main results: table ----

# Custom function for tables
custom_rdd_table <- function(i) {
  results %>%
    filter(y_var == i) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
    mutate(
      loss_estimate = if_else(bw_type != "Manual", loss_estimate_robust, loss_estimate), # Table robust stuff for optimal bw models!
      ci_l = if_else(bw_type != "Manual", ci_l_robust, ci_l),
      ci_u = if_else(bw_type != "Manual", ci_u_robust, ci_u),
      loss_estimate = paste(loss_estimate, " [", ci_l, " , ", ci_u, "]"),
      bw = paste0("[-", bw_l, " , ", bw_h, "]"),
      n = paste0("[-", N_h_l, " , ", N_h_r, "]")
    ) %>%
    mutate(bw_type = case_when(
      bw_type == "msetwo" ~ "MSE-optimal: (two)",
      bw_type == "mserd" ~ "MSE-optimal: (rd)",
      .default = paste0("Manual: ", bw_l)
    )) %>%
    left_join(dependent_vars, by = "y_var") %>%
    select(bw_type, d, loss_estimate, bw, n, p, kernel) %>%
    arrange(bw_type)
}


# Loop .tex tables for outcomes
for (outcome in outcomes) {
  custom_rdd_table(i = outcome) %>%
    kbl(
      format = "latex",
      booktabs = TRUE,
      caption = paste0("RDD robustness checks", outcome),
      label = paste0("rdd_robustness_", outcome),
      linesep = ""
    ) %>%
    kable_styling(latex_options = c("basic", "hold_position")) %>%
    writeLines(paste0(reg_tables_path,"rdd_robustness_", outcome, ".tex"))
}

rm(list = ls())
gc()
