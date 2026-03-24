# R Script Title: Summary statistics
# Author: Risto Hurmeranta
# Description:  Creates descriptive statistics tables and plots used in the paper.
#               Produces: main summary statistics table, apartment characteristics
#               by travel-to-work area, sale rates by mobility decision, and
#               stacked bar chart of predicted loss/gain share by year and area.
#
# Input:  Sourced via 01_read_data.R:
#           W:/Nominal_loss_aversion/Hurmeranta/data/final_data/panel/analysis_panel_hki_tku_tre_20062018.rds
# Output: W:/Nominal_loss_aversion/Hurmeranta/results/main_results/descriptives/
#           summary_statistics.csv / .tex
#           secondary_summary_statistics.csv / .tex
#           mobility_by_sales_p.csv / .tex
#         W:/Nominal_loss_aversion/Hurmeranta/results/main_results/plots/
#           loss_by_year.png

options(scipen = 999)

# Required packages ----
library(dplyr)
library(kableExtra)
library(stringr)
library(ggplot2)

# Read data ----
source("W:/Nominal_loss_aversion/Hurmeranta/code/econometric_modelling/02_main_analysis/01_read_data.R")

# Define path to folders where results saved ----
results_path <- "W:/Nominal_loss_aversion/Hurmeranta/results/main_results/descriptives/"
plots_path <- "W:/Nominal_loss_aversion/Hurmeranta/results/main_results/plots/"

# Main summary statistics for outcomes ----

# Set up summary table
summary <- panel %>%
  summarize(across(
    .cols = c(
      expected_return_pct,
      spell_length_yrs,
      ltv,
      loss,
      gain,
      starts_with("y_")
    ),
    .fns = list(
      mean = ~ mean(.x, na.rm = TRUE),
      std = ~ sd(.x, na.rm = TRUE),
      p01 = ~ quantile(.x, 0.01, na.rm = TRUE),
      p99 = ~ quantile(.x, 0.99, na.rm = TRUE)
      ,n = ~ sum(if_else(!is.na(.x),1,0),na.rm=TRUE)
    ),
    .names = "{col}__{fn}"
  )) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("var", "statistic"),
    names_pattern = "(.*)__(.*)"
  ) %>%
  pivot_wider(
    names_from = "statistic",
    values_from = "value"
  ) %>%
  mutate(across(where(is.numeric), ~ (round(.x, digits = 3)))) %>%
  filter(!str_detect(var, "y_bal"),
         !(var %in% c("y_becomes_landlord_t2_muutto",
                      "y_becomes_landlord_t3_muutto",
                      "y_ei_myynti_t2_muutto","y_ei_myynti_t3_muutto"
                      ))) %>% # Drop balance covariates from the table
  left_join(dependent_vars, by = c("var" = "y_var"), keep = FALSE) %>% # Convert var names to nice looking
  mutate(
    var = if_else(str_detect(var, "y_"), y_label, var),
    var = case_when(var == "expected_return_pct" ~ "Expected return",
      var == "spell_length_yrs" ~ "Spell length",
      var == "ltv" ~ "LTV",
      var == "loss" ~ "Loss",
      var == "gain" ~ "Gain",
      .default = var
    )
  ) %>%
  select(-y_label) %>%
  rename(" " = var)


# Save as .csv
summary %>%
  write.csv(file = paste0(results_path, "summary_statistics.csv"))

# Save as .tex
summary %>%
  select(-n) %>% 
  kbl(
    format = "latex",
    booktabs = TRUE,
    caption = "Summary statistics",
    label = "summary_statistics",
    linesep = ""
  ) %>%
  kable_styling(latex_options = c("basic", "hold_position")) %>%
  pack_rows("Key explanatory variables", 1, 5) %>%
  pack_rows("Outcome", 6, 29) %>%
  writeLines(paste0(results_path, "summary_statistics.tex"))


# Secondary summary statistics by TWA ----

secondary_summary <- panel %>%
  group_by(travel_to_work_area) %>%
  summarize(
    "Sales" = sum(myynti),
    "Apartments" = n_distinct(srnro, huonnro),
    "Debt-free sales price (2005 EUR)" = paste0(round(mean(omhinta, na.rm = TRUE), 0), " (", round(sd(omhinta, na.rm = TRUE), 0), ")"),
    "Floor-area" = paste0(round(mean(pala, na.rm = TRUE), 0), " (", round(sd(pala, na.rm = TRUE), 0), ")"),
    "Room count" = paste0(round(mean(as.numeric(as.character(hulu)), na.rm = TRUE), 0), " (", round(sd(as.numeric(as.character(hulu)), na.rm = TRUE), 0), ")"),
    "Sauna" = round(mean(as.numeric(as.character(husa)), na.rm = TRUE), 2),
    "Balcony" = round(mean(as.numeric(as.character(pave)), na.rm = TRUE), 2),
    "Separate kitchen" = round(mean(as.numeric(as.character(keittio)), na.rm = TRUE), 2),
    "Floor count" = paste0(round(mean(as.numeric(as.character(kelu)), na.rm = TRUE), 0), " (", round(sd(as.numeric(as.character(kelu)), na.rm = TRUE), 0), ")"),
    "Year of construction" = paste0(round(mean(as.numeric(as.character(vavv)), na.rm = TRUE), 0), " (", round(sd(as.numeric(as.character(vavv)), na.rm = TRUE), 0), ")"),
    "Own lot" = round(mean(as.numeric(as.character(oma_tontti)), na.rm = TRUE), 2),
    "Elevator" = round(mean(as.numeric(as.character(hissi)), na.rm = TRUE), 2),
    "Row-house" = round(mean(as.numeric(as.character(rivitalo)), na.rm = TRUE), 2),
    "Distance to CBD" = round(mean(dist2_center_km, na.rm = TRUE), 2), .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ as.character(.))) %>%
  pivot_longer(
    cols = -travel_to_work_area,
    names_to = " ",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = "travel_to_work_area",
    values_from = "value"
  )


# Save as .csv
secondary_summary %>%
  write.csv(file = paste0(results_path, "secondary_summary_statistics.csv"))

# Save as .tex
secondary_summary %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    caption = "Summary statistics",
    label = "secondary_summary_statistics",
    linesep = "",
    format.args = list(big.mark = ",")
  ) %>%
  kable_styling(latex_options = c("basic", "hold_position")) %>%
  writeLines(paste0(results_path, "secondary_summary_statistics.tex"))


# Mobility by sales decision table ----

shares <- panel %>%
  group_by(muutto) %>%
  summarize(
    "Total" = n(),
    "No sale" = sum(if_else(myynti == 0, 1, 0), na.rm = TRUE) / n(),
    "Sale" = sum(myynti, na.rm = TRUE) / n(),
    "No sale, (t+1)" = sum(
      case_when(muutto == 0 & myynti == 0 ~ 1, # For non-movers look at only the current EOY sale!
        muutto == 1 & myynti == 0 & myynti_lead == 0 ~ 1, # For movers check also the next year EOY sale
        .default = 0
      )
    ) / n(),
    "Sale, (t+1)" = sum(
      case_when(
        muutto == 0 & myynti == 1 ~ 1,
        muutto == 1 & (myynti == 1 | myynti_lead == 1) ~ 1,
        .default = 0
      )
    ) / n()
  ) %>%
  mutate(across(3:6, ~ round(.x, 3))) %>%
  rename("Move" = muutto) %>%
  mutate(
    `No sale, (t+1)` = if_else(Move == 0, "", as.character(`No sale, (t+1)`)), # Fix t+1 outcomes empty for non-movers
    `Sale, (t+1)` = if_else(Move == 0, "", as.character(`Sale, (t+1)`))
  )

# Save as .csv
shares %>%
  write.csv(file = paste0(results_path, "mobility_by_sales_p.csv"))

# Save as .tex
shares %>% # Save latex
  kbl(
    format = "latex",
    booktabs = TRUE,
    caption = "Homeowners sale rates by moving decision",
    label = "mobility_by_sales"
  ) %>%
  kable_styling(latex_options = c("basic", "hold_position")) %>%
  writeLines(., paste0(results_path, "mobility_by_sales_p.tex"))


# Loss share by year and area plot ----

(loss_by_year <- panel %>%
  group_by(travel_to_work_area, vuosi) %>%
  summarize(
    n = n(),
    loss = sum(loss),
    gain = n - loss,
    loss_share = loss / n,
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(loss, gain),
    names_to = "status",
    values_to = "count"
  ) %>%
  mutate(status = if_else(status == "loss", "Predicted loss", "Predicted gain")) %>%
  ggplot(aes(
    x = vuosi,
    y = count,
    fill = status
  )) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(~travel_to_work_area) +
  scale_fill_manual(name = NULL, values = c(
    "Predicted loss" = "#A6CEE3",
    "Predicted gain" = "#1F78B4"
  )) +
  scale_x_discrete(breaks = c("2007", "2012", "2017")) +
  labs(x = NULL, y = "Owner-occupied apartments"))


ggsave(paste0(plots_path, "loss_by_year.png"), plot = loss_by_year, width = 8, height = 6, units = "in")


# Free memory
rm(list = ls())
gc()
