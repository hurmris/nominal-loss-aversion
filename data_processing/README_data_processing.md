# Data Processing Pipeline

This folder contains scripts that build the analysis panel from raw register data. The pipeline reads transfer tax records, cleans and augments them with household, property and spatial characteristics, applies sample selection filters, and produces the final panel and supplementary datasets used in the price model and main analysis.

## Overview

Raw transfer tax data is cleaned and merged with Statistics Finland register data (FOLK, building and apartment registers, spatial data) to construct an apartment-level panel of ownership spells for Helsinki, Tampere and Turku travel-to-work areas, 2006–2018. 

Scripts must be run in order. Each script reads outputs from the previous step.

**Prerequisites:**
- Replication requires data access and permissions from Statistics Finland Research Services. The authors are happy to provide guidance on the data application process.
- All scripts use hardcoded absolute paths (e.g., W:/Nominal_loss_aversion/Hurmeranta/data/, D:/d67/custom-made/) that reflect the directory structure of Statistics Finland's FIONA remote access environment where the analysis was conducted. Replicating the analysis requires updating these input and output paths in each script to match your own directory structure

## Scripts

| Script | Description |
|--------|-------------|
| `01_read_transfer_tax.R` | Reads raw transfer tax `.dta` files and saves as `.rds`. Run once. |
| `02_cleaning_transfer_tax_20062017.R` | Cleans transfer tax data for 2006–2017. Handles variable encoding, date parsing, ownership share formats. |
| `03_cleaning_transfer_tax_2018.R` | Cleans transfer tax data for 2018. Differs from 2006–2017 in `omos` and `haos` encoding. |
| `04_create_clean_panel_20062018.R` | Binds yearly cleaned files into a single panel. One row per owner per transaction. |
| `05_flag_weirds.R` | Flags problematic apartments (missing values, company owners, flips, inconsistent prices etc.). Does not drop anything — flagging only. |
| `06_initial_panel_sample_selection.R` | Drops flagged apartments. Filters applied at apartment level: if any year of an apartment fails a filter, all years are dropped. |
| `07_create_apartment_level_panel.R` | Aggregates from owner level to apartment level. Stores up to two owners as `shnro1`/`shnro2`. Creates sale and purchase dummies. |
| `08_property_apartment_characteristics.R` | Joins property and apartment register characteristics and grid coordinates to the panel, by year. |
| `09_household_characteristics.R` | Joins FOLK household characteristics, mortgage data and end-of-year residence data for each owner. Aggregates back to apartment level. |
| `10_area_classification.R` | Creates area classification: harmonises municipality codes to 2018 boundaries, assigns postcodes, StatFi price area classification and housing allowance groups. One row per apartment. |
| `11_distance_to_city_center.R` | Calculates Euclidean distance from each apartment to its own and the closest municipality center using ETRS-TM35FIN grid coordinates. |
| `12_merge_characteristics_to_panel.R` | Merges all characteristics (property, household, area, distance, housing price index) to the apartment panel. |
| `13_matched_panel_sample_selection.R` | Applies final sample selection filters: area restriction, apartment characteristics, price/profit outlier trimming, ownership consistency and spell-level filters. Produces the main analysis panel. |
| `14_moves.R` | Defines owner-occupancy status, spell lengths and move type variables (permanent, temporary, partial, divorce). |
| `15_next_home_characteristics.R` | For each mover, finds characteristics of the next apartment (tenure type, size change, house type, region, municipality, move distance). |
| `16_employment.R` | Creates household-level employment change variables: end of unemployment and change of employer. |
| `17_rent_income.R` | Creates household-level rental income variables at t−1, t, t+1, t+2 and t+3. |
| `18_sales_data_sample_selection.R` | Applies sample selection filters to produce the sales dataset used for price model training. |
|`record_function.R`| Tracks observation counts after each filter step and is sourced by several scripts|

