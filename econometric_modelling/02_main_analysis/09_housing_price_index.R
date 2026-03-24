# R Script Title: Housing price index
# Author: Risto Hurmeranta
# Description:  Estimates a hedonic housing price index for Helsinki, Tampere and Turku
#               travel-to-work areas, 2006-2018. Fits a log-linear hedonic model with
#               year x TWA interactions using feols(). Extracts year and TWA coefficients
#               and exponentiates to construct a price level index with Tampere 2006 = 1.
#
# Input:  Sourced via 01_read_data.R (for overleaf_scale theme):
#           W:/Nominal_loss_aversion/Hurmeranta/data/final_data/panel/analysis_panel_hki_tku_tre_20062018.rds
#         W:/Nominal_loss_aversion/Hurmeranta/data/final_data/sales/sales_20062018.rds
# Output: W:/Nominal_loss_aversion/Hurmeranta/results/main_results/plots/
#           housing_price_index.png
# Note:   Index uses nominal prices (log_velaton_hinta not deflated) since the purpose
#         is to show nominal price trends across areas.
#         Base: Tampere TWA, 2006 = 1.


# Required packages ----
library(dplyr)
library(ggplot2)
library(tidyr)
library(fixest)
library(forcats)


# Define path to folders where results saved ----
plots_path <- "W:/Nominal_loss_aversion/Hurmeranta/results/main_results/plots/"

# Read data ----
source("W:/Nominal_loss_aversion/Hurmeranta/code/econometric_modelling/02_main_analysis/01_read_data.R")

full_sales <- readRDS("W:/Nominal_loss_aversion/Hurmeranta/data/final_data/sales/sales_20062018.rds") %>%
  sf::st_drop_geometry() %>%
  filter(tyossakayntial_name_fi %in% c("Helsingin tk-alue", "Tampereen tk-alue", "Turun tk-alue")) %>% # Filter area
  mutate(across(
    .cols = where(is.factor), # Remove redundant factor levels
    .fns = ~ droplevels(.)
  )) %>%
  mutate(
    log_velaton_hinta = log(velaton_hinta),
    pala_sq = pala^2,
    ika_sq = ika^2,
    tyossakayntial_name_fi = fct_relevel(tyossakayntial_name_fi, "Tampereen tk-alue")
  )

# Fit model ----
fit <- feols(
  fml = log_velaton_hinta ~ pala + pala_sq + ika + ika_sq + hulu + husa + pave + hissi + kelu + keittio + dist2_center_km + rivitalo + vuosi * tyossakayntial_name_fi,
  vcov = "hetero",
  data = full_sales
)



# Index construction ----
# Model: log(price) = beta_year + beta_twa + beta_year:twa + controls
# Index for Helsinki/Turku: exp(beta_year + beta_twa + beta_year:twa)
# Index for Tampere (base TWA): exp(beta_year)  [beta_twa = beta_interact = 0 by construction]
# 2006 base year: beta_year = NA for 2006 (dropped as reference) -> set to 0
# Index value = price relative to Tampere 2006

# Extract coefficients
coefs <- coef(fit)

# Create base index = 1 for Tampere
index_data <- full_sales %>%
  distinct(vuosi, tyossakayntial_name_fi) %>%
  mutate(twa_hpi = NA_real_)

# Loop over years and TWAs
for (i in c("Helsingin tk-alue", "Turun tk-alue")) {
  for (y in 2006:2018) {
    beta_year <- coefs[paste0("vuosi", y)]
    beta_twa <- coefs[paste0("tyossakayntial_name_fi", i)]
    beta_interact <- coefs[paste0("vuosi", y, ":tyossakayntial_name_fi", i)]

    # Calculate index value (Take care of missing year and interaction terms for Helsinki and Turku for 2006 base year)
    idx_value <- exp(if_else(is.na(beta_year), 0, beta_year) + beta_twa + if_else(is.na(beta_interact), 0, beta_interact))

    index_data <- index_data %>%
      mutate(twa_hpi = if_else(tyossakayntial_name_fi == i & vuosi == y,
        idx_value,
        twa_hpi
      ))
  }
}




# Tampere: only year effects
for (y in 2007:2018) {
  beta_year <- coefs[paste0("vuosi", y)]
  idx_value <- exp(beta_year)

  index_data <- index_data %>%
    mutate(twa_hpi = case_when(tyossakayntial_name_fi == "Tampereen tk-alue" & vuosi == y ~ idx_value,
      tyossakayntial_name_fi == "Tampereen tk-alue" & vuosi == 2006 ~ 1,
      .default = twa_hpi
    ))
}


# Keep one observation per TWA
index_data <- index_data %>%
  filter(twa_hpi >= 0) %>% # 2006 years for Helsinki and Turku missing
  mutate(tyossakayntial_name_fi = fct_recode(tyossakayntial_name_fi,
    "Tampere TWA" = "Tampereen tk-alue",
    "Helsinki TWA" = "Helsingin tk-alue",
    "Turku TWA" = "Turun tk-alue"
  ))

# Plot
plot <- ggplot(index_data, aes(
  x = as.numeric(as.character(vuosi)), y = twa_hpi,
  color = tyossakayntial_name_fi,
  group = tyossakayntial_name_fi,
  linetype = tyossakayntial_name_fi,
  shape = tyossakayntial_name_fi
)) +
  geom_line(aes(linetype = tyossakayntial_name_fi), linewidth = 0.7 / overleaf_scale) +
  geom_point(aes(shape = tyossakayntial_name_fi), size = 2 / overleaf_scale) +
  scale_color_manual(values = c("#33A02C", "#1F78B4", "#E31A1C")) +
  scale_linetype_manual(values = c("solid", "longdash", "dashed")) +
  labs(x = "", y = "HPI", linetype = NULL, shape = NULL, shape = NULL, color = NULL, linetype = NULL) +
  scale_x_continuous(breaks = c(seq(2005, 2020, 5))) +
  coord_cartesian(xlim = c(2005, 2020)) +
  scale_y_continuous(breaks = c(1, 1.25, 1.5))

ggsave(paste0(plots_path, "housing_price_index.png"), plot = plot, width = 8, height = 6, units = "in")
rm(list=ls())
gc()
