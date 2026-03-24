# Nominal Loss Aversion in the Housing Market and Household Mobility

Replication code for the VATT Working Paper No. 178:

> **Hurmeranta, R. & Lyytikäinen, T.** (2025). *Nominal Loss Aversion in the Housing Market and Household Mobility*. VATT Working Papers 178. VATT Institute for Economic Research.
>
> Available at: [https://www.doria.fi/handle/10024/193400](https://www.doria.fi/handle/10024/193400)

## Abstract
Households are averse to realizing nominal housing market losses. Reduced household mobility, in an attempt to avoid selling at a loss, implies misallocation of housing and can affect the functioning of the labor market. However, direct evidence on mobility responses is scarce. This paper studies the effect of expected losses on homeowners' propensity to move using administrative data on housing transactions matched with detailed data on household characteristics. We use an ensemble machine learning method to estimate expected prices for the universe of apartments in the three largest travel-to-work areas in Finland in 2006–2018. We find that homeowners below the zero-return cutoff are 51% less likely to sell than those above the cutoff. The effect of loss aversion on mobility is somewhat smaller than the effect on sales. Homeowners with an expected loss are more likely to move without selling their previous home. Renting out their previous apartment seems to enable homeowners to move without realizing nominal losses. Expected losses also reduce inter-regional mobility, which suggests that loss aversion can lead to misallocation of the labor force.

## Requirements

- **R** ≥ 4.3.1. Run `install_packages.R` once to install all dependencies.
- **Data access:** The analysis uses confidential Finnish administrative register data accessible through Statistics Finland's FIONA remote access system. Researchers can apply for permission to access the data sources used in this study through [Statistics Finland Research Services](https://stat.fi/en/services/services-for-researchers). The authors are happy to provide guidance on the data application process — see contact details below.
- **File paths:** All scripts use hardcoded absolute paths (e.g., `W:/Nominal_loss_aversion/Hurmeranta/data/`, `D:/d67/custom-made/`) that reflect the FIONA directory structure. Users replicating the analysis will need to update these paths in each script.

## Methodology and Replication

The analysis proceeds in three stages. Scripts must be run in order — each stage depends on outputs from the previous one. See the sub-folder READMEs for detailed script-level documentation.

**1. Data processing** (`data_processing/`, scripts 01–18). Transfer tax records are cleaned and merged with Statistics Finland register data (FOLK, building and apartment registers, spatial data) to construct an apartment-level panel of ownership spells. Run scripts sequentially.

**2. Price modelling** (`econometric_modelling/01_price_modelling/`). An ensemble of hedonic price models — OLS, bagging, random forest and XGBoost — is trained on transaction data. Base model predictions are combined via non-negative least squares (NNLS) weights estimated on out-of-fold cross-validation predictions. Run `00_run_all.R`.

**3. Main analysis** (`econometric_modelling/02_main_analysis/`). Expected returns (predicted price / purchase price − 1) serve as the running variable in a regression discontinuity type design at the zero-return cutoff. The main specification is linear in expected return with a slope change at the cutoff, estimated within a ±20% bandwidth with a ±5% donut hole. Run `00_create_analysis_data.R` first, then any analysis script independently. See `README_econometric_modelling.md` for a mapping of scripts to tables and figures in the paper.


## Citation

```bibtex
@techreport{hurmeranta2025nominal,
  title     = {Nominal Loss Aversion in the Housing Market and Household Mobility},
  author    = {Hurmeranta, Risto and Lyytik{\"a}inen, Teemu},
  year      = {2025},
  institution = {VATT Institute for Economic Research},
  type      = {VATT Working Papers},
  number    = {178},
  url       = {https://www.doria.fi/handle/10024/193400}
}
```

## Authors

- **Risto Hurmeranta** — University of Helsinki, Finnish Centre of Excellence in Tax Systems Research (FIT) ([risto.hurmeranta@helsinki.fi](mailto:risto.hurmeranta@helsinki.fi))
- **Teemu Lyytikäinen** — VATT Institute for Economic Research ([teemu.lyytikainen@vatt.fi](mailto:teemu.lyytikainen@vatt.fi))

## Acknowledgements

Lyytikäinen acknowledges funding from the Strategic Research Council for the SustAgeable consortium (grant no. 345384) and from the Research Council of Finland (grant no. 370060). Hurmeranta acknowledges funding from the Yrjö Jahnsson Foundation (grant nos. 20237631 and 20237657), the Kone Foundation (grant no. 202308821), and Research Council of Finland (grant no. 359536).

## License

This project is distributed under [MIT Licence](https://github.com/hurmris/nominal-loss-aversion/blob/main/LICENSE). The underlying data are confidential and subject to Statistics Finland's access and use restrictions.
