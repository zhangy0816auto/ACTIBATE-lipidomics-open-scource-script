# ACTIBATE Lipidomics Analysis

This repository provides modular, open-source R scripts for analyzing lipidomics data from the ACTIBATE study, focusing on exercise effects, sex differences, and responder classification.

## 📁 Project Structure

```
ACTIBATE-lipidomics/
├── data/                      # Input data files (mock examples)
├── results/
│   ├── figures/               # Output plots
│   └── tables/                # Output statistics
├── src/                       # Modular R analysis scripts
└── README.md
```

## 🔧 Requirements

R packages needed:
- `openxlsx`
- `ggplot2`
- `dplyr`
- `tidyr`
- `Hmisc`
- `ggrepel`
- `mixOmics`
- `broom`
- `viridis`

Install all packages using:
```r
install.packages(c("openxlsx", "ggplot2", "dplyr", "tidyr", "Hmisc", "ggrepel", "broom", "viridis"))
if (!require("mixOmics")) BiocManager::install("mixOmics")
```

## ▶️ How to Run

Each script is standalone and located in `src/`:

- `01_zscore_correlation.R`: z-score normalization + correlation with VO2peak
- `02_anova.R`: ANOVA by exercise group
- `03_volcano_plot.R`: Paired t-test and volcano plot
- `04_plsda.R`: PLS-DA by group
- `05_bubble_plot.R`: Structure-based lipid bubble plot
- `06_gender_foldchange_plot.R`: Fold-change: Female vs Male
- `07_sensitivity_analysis.R`: PLS-DA of Responder vs Non-responder

Open and run scripts in order depending on your analysis needs.

## 📄 License

MIT License