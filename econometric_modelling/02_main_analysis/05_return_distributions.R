# R Script Title: Return distributions and density tests
# Author: Risto Hurmeranta
# Description:  Compares distributions of realized and predicted returns for test-set sales.
#               Plots realized vs predicted return distributions, missing mass around zero,
#               and running variable density with discontinuity test results annotated.
#               Reports loss/gain domain shares and density discontinuity tests
#               (Cattaneo-Jansson-Ma via rddensity, McCrary via rdd).
#
# Input:  Sourced via 01_read_data.R:
#           W:/Nominal_loss_aversion/Hurmeranta/data/final_data/panel/analysis_panel_hki_tku_tre_20062018.rds
#         W:/Nominal_loss_aversion/Hurmeranta/data/final_data/predictions/ensemble/
#           [prefix]_ensemble_test_set_predictions.rds  (to identify test set spells)
# Output: W:/Nominal_loss_aversion/Hurmeranta/results/main_results/plots/
#           real_vs_est_returns.png
#           missing_return_mass.png
#           expected_return_density_owner_occupied.png
#         W:/Nominal_loss_aversion/Hurmeranta/results/main_results/descriptives/
#           domain_shares_unrestricted.tex
#           mcrary_density_test.csv
# Note:   Return distribution plots restricted to test-set spells to ensure
#         predicted returns are genuine out-of-sample predictions.
#         Domain shares table uses full sample (not test-set restricted) for reference.
#         Density discontinuity tests reported for owner-occupied sample within +-20% bandwidth.

# Required packages ----
library(dplyr)
library(ggplot2)
library(kableExtra)
library(tidyr)
library(rdd)
library(rddensity)

# Define path to folders where results saved ----
results_path <- "W:/Nominal_loss_aversion/Hurmeranta/results/main_results/descriptives/"
plots_path <- "W:/Nominal_loss_aversion/Hurmeranta/results/main_results/plots/"

# Set up data ----
source("W:/Nominal_loss_aversion/Hurmeranta/code/econometric_modelling/02_main_analysis/01_read_data.R")


# Test set identification ----
# Test set flag lives on purchase rows in ensemble_test_set_predictions.rds
# Join via (vuosi, srnro, huonnro) rather than spell_id since sale rows

test_set_dummies <- data.frame()

for (i in c("tku_tk", "tre_tk", "hki_tk")) {
  area_test_set_dummies <- readRDS(paste0("W:/Nominal_loss_aversion/Hurmeranta/data/final_data/predictions/ensemble/", i, "_ensemble_test_set_predictions.rds")) %>%
    select(vuosi, srnro, huonnro, ospvm, set)
  
  test_set_dummies <- bind_rows(test_set_dummies, area_test_set_dummies)
  rm(area_test_set_dummies)
}

# Keep only sales transactions and convert to long format
test_set <- test_set_dummies %>%  select(vuosi,srnro,huonnro) %>% mutate(apt_year = paste0(vuosi,srnro,huonnro)) %>% pull(apt_year)

sales <- panel %>%
  filter(myynti == 1,
         paste0(vuosi,srnro,huonnro) %in% test_set) %>% # Return is calculated only for sales -rows
  mutate(
    .pred_tuotto = .pred_ensemble_eur - velaton_hinta,
    .pred_suht_tuotto = .pred_tuotto / velaton_hinta
  ) %>%
  select(suht_tuotto, .pred_suht_tuotto) %>%
  rename(
    "Realized" = suht_tuotto,
    "Predicted" = .pred_suht_tuotto
  ) %>%
  pivot_longer(cols = c(Realized, Predicted), names_to = "model", values_to = "suht_tuotto") %>%
  mutate(model = as.factor(model))


sales$model <- factor(sales$model, levels = rev(levels(sales$model))) # Reorder factor level so looks nice in pictures

# Plot: Realized vs. Predicted returns ----

total_sales_count <- panel %>%
  filter(myynti == 1,
         paste0(vuosi,srnro,huonnro) %in% test_set
         ) %>%
  nrow()

(real_vs_est_returns <- ggplot(
  data = sales,
  aes(suht_tuotto,
    color = model,
    linetype = model,
    shape = model
  )
) +
  geom_vline(xintercept = 0, color = "gray80", linetype = "solid", linewidth = 0.3) +
  # Add line
  stat_bin(aes(y = after_stat(count / total_sales_count * 100), group = model),
    binwidth = 0.01,
    boundary = 0,
    closed = "left",
    position = "identity",
    geom = "step",
    linewidth = 0.7 / overleaf_scale
  ) +
  # Add point
  stat_bin(
    aes(
      y = after_stat(count / total_sales_count * 100),
      group = model
    ),
    binwidth = 0.01,
    boundary = 0,
    closed = "left",
    position = "identity",
    geom = "point",
    size = 2 / overleaf_scale
  ) +
  labs(
    y = "Frequency (%)", x = "Return (1% bins)",
    shape = NULL, color = NULL, linetype = NULL
  ) +
  scale_color_manual(values = c(
    "Realized" = "#33A02C",
    "Predicted" = "#1F78B4"
  )) +
  scale_shape_manual(values = c(
    "Realized" = 16,
    "Predicted" = 17
  )) +
  scale_linetype_manual(values = c(
    "Realized" = "dotted",
    "Predicted" = "dashed"
  )) +
  coord_cartesian(xlim = c(-0.25, 0.50)) # Zoom picture to this area
)


ggsave(path = plots_path,filename =  "real_vs_est_returns.png", plot = real_vs_est_returns, width = 8, height = 6, units = "in")

# Missing mass plot ----
# Density estimated separately for realized and predicted distributions
# Difference = density_realized - density_estimated, scaled by 100
# Positive values mean more realized sales than predicted at that return bin

# Create data
missing_mass_data <- sales %>%
  mutate(bin_center = floor(suht_tuotto / 0.01) * 0.01 + 0.005) %>% # Calculate the center of each bin
  group_by(model, bin_center) %>%
  summarize(
    count = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = model,
    values_from = count,
    values_fill = 0
  ) %>%
  mutate(
    total_estimated = sum(Predicted),
    total_realized = sum(Realized),
    density_estimated = Predicted / total_estimated / 0.01, # Convert to density by dividing 0.01
    density_realized = Realized / total_realized / 0.01,
    difference = (density_realized - density_estimated) * 100
  )


(missing_mass <- ggplot(missing_mass_data, aes(x = bin_center, y = difference)) +
  geom_vline(xintercept = 0, color = "gray80", linetype = "solid", linewidth = 0.7 / overleaf_scale) +
  geom_line(color = "black", linetype = "dashed") +
  geom_point(color = "black", shape = 16, size = 2 / overleaf_scale) +
  labs(
    y = "Frequency difference (%)", x = "Return (1% bins)",
    shape = NULL, color = NULL, linetype = NULL
  ) +
  coord_cartesian(xlim = c(-0.25, 0.50)) # Zoom picture to this area
)


ggsave(path = plots_path,filename =  "missing_return_mass.png", plot = missing_mass, width = 8, height = 6, units = "in")


# Table: Domain shares ----

panel %>%
  filter(myynti==1) %>% 
  mutate(
    .pred_tuotto = .pred_ensemble_eur - velaton_hinta,
    .pred_suht_tuotto = .pred_tuotto / velaton_hinta
  ) %>%
  select(suht_tuotto, .pred_suht_tuotto) %>%
  rename(
    "Realized" = suht_tuotto,
    "Predicted" = .pred_suht_tuotto
  ) %>%
  pivot_longer(cols = c(Realized, Predicted), names_to = "model", values_to = "suht_tuotto") %>%
  mutate(model = as.factor(model)) %>% 
  mutate(
    loss = if_else(suht_tuotto < 0, 1, 0),
    gain = if_else(suht_tuotto >= 0, 1, 0)
  ) %>%
  group_by(model) %>%
  summarize(
    "Loss" = round(sum(loss) / n(), 3),
    "Gain" = round(sum(gain) / n(), 3),
    "Sales" = n()
  ) %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    caption = "Loss and Gain domain shares, 2006-2018",
    label = "domain_shares"
  ) %>%
  kable_styling(latex_options = c("basic", "hold_position")) %>%
  writeLines(paste0(results_path, "domain_shares_unrestricted.tex"))


# Running variable density ----


running_var <- panel %>%
  filter(omistusasunto == 1, abs(expected_return_pct) <= 0.20) %>%
  pull(expected_return_pct)

# Density discontinuity test
## rddensity
density <- rddensity(running_var, c = 0)
t_jk <- round(density$test$t_jk, 3) # Store t-test statistic
p_jk <- round(density$test$p_jk, 3) # Store related p-value
l_bw <- round(density$h$left, 3)
r_bw <- round(density$h$right, 3)
total_obs <- panel %>%
  filter(omistusasunto == 1) %>%
  nrow()


## Mccrary
rdd_dat <- rddtools::rdd_data(
  y = rep(0, length(running_var)),  # y is required by rdd_data but irrelevant for density test
  x = running_var,
  cutpoint = 0
)

mcrary_test <- rddtools::dens_test(rdd_dat, plot = FALSE)

mcrary <- tibble(
  theta    = mcrary_test$test.output$theta,
  se       = mcrary_test$test.output$se,
  z        = mcrary_test$test.output$z,
  p        = mcrary_test$test.output$p,
  binsize  = mcrary_test$test.output$binsize,
  bw       = mcrary_test$test.output$bw,
  cutpoint = mcrary_test$test.output$cutpoint
)

write.csv(mcrary, file = paste0(results_path, "mcrary_density_test.csv"))




(density_plot <- panel %>%
  filter(omistusasunto == 1) %>%
  ggplot(data = ., aes(expected_return_pct)) +
  geom_vline(xintercept = 0, color = "gray80", linetype = "solid") +
  # Add line
  stat_bin(aes(y = after_stat(count / total_obs * 100)),
    binwidth = 0.01,
    boundary = 0,
    closed = "left",
    position = "identity",
    color = "black",
    geom = "step",
    linewidth = 0.7 / overleaf_scale,
    linetype = "dotted"
  ) +
  # Add point
  stat_bin(aes(y = after_stat(count / total_obs * 100)),
    binwidth = 0.01,
    boundary = 0,
    closed = "left",
    color = "black",
    position = "identity",
    geom = "point",
    size = 1 / overleaf_scale
  ) +
  labs(y = "Frequency (%)", x = "Predicted returns (1% bins)") +
  annotate("text",
    label = paste0("Test stat = ", t_jk, "\np: ", p_jk, "\nBW(L/R): ", l_bw, " / ", r_bw),
    x = 0.1,
    y = 0.30,
    size = 4,
    hjust = 0
  ) +
  coord_cartesian(xlim = c(-0.25, 0.50))) # Zoom picture to this area


# Save ----
ggsave(path = plots_path, filename = "expected_return_density_owner_occupied.png", plot = density_plot, units = "in", width = 8, height = 6)


# Free memory ----
rm(list = ls())
gc()
