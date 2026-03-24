# R Script Title: Read data
# Author: Risto Hurmeranta
# Description:  Reads the analysis panel and defines shared objects used across all
#               analysis scripts (02-09). Sets ggplot theme scaled for Overleaf output.
#               This script is sourced at the start of each analysis script rather than run directly.
#
# Input:  W:/Nominal_loss_aversion/Hurmeranta/data/final_data/panel/analysis_panel_hki_tku_tre_20062018.rds
# Output: Objects assigned to environment on source():
#           panel           -- the analysis panel
#           dependent_vars  -- lookup table of all y_ variable names and display labels
#           selected_deps   -- subset of dependent_vars used in main results tables
#           overleaf_scale  -- scaling factor for plot text sizes
# Note:  Sourced by analysis scripts 02-09 via source("01_read_data.R").

# Required packages ----
library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)

# Read data ----
panel <- readRDS("W:/Nominal_loss_aversion/Hurmeranta/data/final_data/panel/analysis_panel_hki_tku_tre_20062018.rds")

# Define outcome labels ----
# dependent_vars maps internal y_ variable names to display labels used in
# tables and plots. .default = y_var ensures unlabelled variables show their
# raw name rather than NA.

dependent_vars <- panel %>%
  select(starts_with("y_")) %>%
  names() %>%
  as_tibble() %>%
  rename("y_var" = value) %>%
  mutate(y_label = case_when(
    y_var == "y_myynti" ~ "Sale",
    y_var == "y_muutto" ~ "Move",
    y_var == "y_myynti_muutto" ~ "Sale conditional on move",
    y_var == "y_ei_myynti_muutto" ~ "No sale conditional on move",
    y_var == "y_myynti_t1_muutto" ~ "Sale conditional on move (t+1)",
    y_var == "y_ei_myynti_t1_muutto" ~ "No sale conditional on move (t+1)",
    y_var == "y_ei_myynti_t2_muutto" ~ "No sale conditional on move (t+2)",
    y_var == "y_ei_myynti_t3_muutto" ~ "No sale conditional on move (t+3)",
    y_var == "y_omistus_muutto" ~ "Owner-occupied conditional on move",
    y_var == "y_vuokra_muutto" ~ "Rental conditional on move",
    y_var == "y_muu_omistus_muutto" ~ "Other ownership conditional on move",
    y_var == "y_becomes_landlord_t1_muutto" ~ "Became landlord conditional on move",
    y_var == "y_becomes_landlord_t2_muutto" ~ "Post-move landlord at t+2",
    y_var == "y_becomes_landlord_t3_muutto" ~ "Post-move landlord at t+3",
    y_var == "y_muutto_tk_alue_sama" ~ "Move within TWA",
    y_var == "y_muutto_tk_alue_eri" ~ "Move between TWAs",
    y_var == "y_muutto_tk_alue_muu" ~ "Move to undefined TWA",
    y_var == "y_muutto_kunta_sama" ~ "Move within municipality",
    y_var == "y_muutto_kunta_eri" ~ "Move between municipalities",
    y_var == "y_muutto_kunta_muu" ~ "Move to undefined municipality",
    y_var == "y_muutto_yli_50km" ~ "Move distance over 50 km",
    y_var == "y_muutto_alle_50km" ~ "Move distance less than 50 km",
    y_var == "y_new_employer" ~ "Change of employer",
    y_var == "y_muutto_new_employer" ~ "Move and change employer",
    y_var == "y_muutto_yli50km_new_employer" ~ "Move and change employer, over 50km",
    y_var == "y_got_job" ~ "End of unemployment",
    y_var == "y_muutto_got_job" ~ "Move and end of unemployment",
    y_var == "y_muutto_yli50km_got_job" ~ "Move and end of unemployment, over 50km",
    y_var == "y_bal_mean_owner_ika" ~ "Avg. owner age",
    y_var == "y_bal_lapsi" ~ "Children",
    y_var == "y_bal_korkeakoulutus" ~ "Higher education",
    y_var == "y_bal_2_owners" ~ "2 owners",
    y_var == "y_bal_tulot" ~ "Household disposable income",
    .default = y_var
  ))


# Define selected (main) dependent variables ----
# selected_deps is the subset shown in main results tables.
# Full dependent_vars including balance test variables (y_bal_*) and
# t+2/t+3 outcomes used in robustness and persistence tables.

selected_deps <- dependent_vars %>%
  filter(y_var %in% c(
    "y_myynti", "y_muutto",
    "y_myynti_t1_muutto", "y_ei_myynti_t1_muutto",
    "y_omistus_muutto", "y_vuokra_muutto",
    "y_becomes_landlord_t1_muutto",
    "y_got_job", "y_new_employer",
    "y_muutto_new_employer","y_muutto_yli50km_new_employer",
    "y_muutto_got_job","y_muutto_yli50km_got_job",
    "y_muutto_tk_alue_sama", "y_muutto_tk_alue_eri",
    "y_muutto_yli_50km", "y_muutto_alle_50km"
  ))


# Set ggplot theme ----
overleaf_scale <- 0.75
theme_set(
  theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.spacing.x = unit(0.8, "lines"),
      legend.position = "bottom",
      legend.text = element_text(size = 11 / overleaf_scale),
      legend.title = element_text(size = 12 / overleaf_scale),
      axis.text = element_text(size = 11 / overleaf_scale),
      axis.title = element_text(size = 12 / overleaf_scale),
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
      axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
      strip.text = element_text(size = 11 / overleaf_scale)
    )
)
