# Main Analysis

This folder contains scripts that run the price model, construct the analysis panel and produce all results reported in the paper.

## Overview

The analysis proceeds in two stages. First, an ensemble of hedonic price models is estimated on transaction data to generate counterfactual price predictions for each apartment-spell-year. Second, these predictions are used to construct expected returns which serve as the running variable in a regression discontinuity **type** design estimating the effect of nominal loss aversion on homeowner mobility and labour market outcomes.

## Folder structure

```
01_price_modelling/     Price model scripts (00–06)
02_main_analysis/       Analysis scripts (00–09)
```

---

## Price model (`01_price_modelling/`)

| Script | Description |
|--------|-------------|
| `00_run_all.R` | Master script. Defines all parameters and runs the full pipeline. **Start here.** |
| `01_modeling_data.R` | Data preparation functions: train/test split, CV folds, common transformations. |
| `02_ols.R` | OLS hedonic price model. Selects best formula by cross-validated RMSE. |
| `03_bagging.R` | Bagging model. Tunes number of trees via Bayesian optimisation on OOB error. |
| `04_random_forest.R` | Random forest. Tunes mtry, trees, node size and sampling via Bayesian optimisation. |
| `05_xgboost.R` | XGBoost. Tunes learning rate, depth, regularisation via Bayesian optimisation with early stopping. |
| `06_ensemble.R` | Combines base model predictions using non-negative least squares (NNLS) weights estimated on out-of-fold CV predictions. |

**How to run:**

Source `01_price_modelling/00_run_all.R`. Takes time -- expect several hours with full panel.

---

## Main analysis (`02_main_analysis/`)

| Script | Description | Tables* | Figures*|
|--------|-------------|--------| -------|
| `00_create_analysis_data.R` | Assembles final analysis panel. Joins price predictions, move variables, employment and rental income. Creates all outcome variables (`y_*`), expected return, LTV and control variables. Restricts to owner-occupied spells. |||
| `01_read_data.R` | Shared setup script sourced by all analysis scripts. Reads the panel, defines outcome variable labels and sets ggplot theme.|
| `02_price_model_rmse_weights.R` | Creates RMSE and ensemble weight tables for the price model appendix. |A4, A5, A6, A7 & A8||
| `03_price_model_test_set_residuals.R` | Evaluates price model accuracy by share of observations with absolute relative residual below 5/10/15/20% thresholds. | 3 & A3 ||
| `04_summary_statistics.R` | Descriptive statistics tables and loss/gain share plot by year and area. |1, 2 & A2|A1|
| `05_return_distributions.R` | Compares realized and predicted return distributions (test-set restricted). Missing mass plot. Density discontinuity tests (CJM and McCrary). | |2, 3 & A2|
| `06_mobility_rate_plots.R` | Plots outcome variable means by 2% expected return bins. Full sample and heterogeneity subsamples by LTV and spell length. ||4, 5, 6, 7, A3, A4, A5|
| `07_main_linear_model.R` | Main linear RDD specification and all robustness checks. Runs for both ensemble and OLS price estimates. |4, 5, 6, A9, A10||
| `08_rdrobust.R` | rdrobust local polynomial RDD models. Loops over donut sizes (0, 2.5%, 5%) and bandwidths (MSE-optimal, 15%, 20%). ||8, A6, A7, A8|
| `09_housing_price_index.R` | Estimates and plots hedonic housing price index for each travel-to-work area, 2006–2018. ||1|
* Refer to tables and figures in the VATT Working papers series https://www.doria.fi/handle/10024/193400

**How to run:**

Scripts 02–09 each source `01_read_data.R` at the top. Run them independently in any order after `00_create_analysis_data.R` has been run.

## Key outputs

All outputs saved to `results/main_results/`:

| Folder | Contents |
|--------|----------|
| `price_model/` | RMSE tables, ensemble weight tables, residual tables |
| `descriptives/` | Summary statistics, domain shares, density test results |
| `plots/` | All figures |
| `regression_tables/main_linear_model/` | Main results, robustness checks, heterogeneity, covariate balance |
| `regression_tables/rdrobust/` | rdrobust results across bandwidths and donut sizes |






