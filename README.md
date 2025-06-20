# ACTIBATE Lipidomics Analysis

This repository contains R scripts for reproducing the lipidomics analysis from the ACTIBATE study. The code calculates paired statistics and generates volcano plots comparing lipid levels before and after a moderate intensity exercise intervention.

## Requirements

- R (\>= 4.0)
- Packages: `tidyverse`, `ggrepel`, `readxl` (for Excel input)

Install packages in R using:

```R
install.packages(c("tidyverse", "ggrepel", "readxl"))
```

## Usage

1. Place your data file in CSV or Excel format. It should contain a `Sample` column, a `Group` column (e.g. `Moderate_PRE` and `Moderate_POS`) and one column per lipid measurement.
2. Run the analysis script:

```bash
Rscript scripts/volcano_analysis.R <path_to_your_data_file>
```

Results (statistics table and volcano plot) are written to the `results/` directory.

An example dataset is provided in [`example/sample_data.csv`](example/sample_data.csv). You can test the script with:

```bash
Rscript scripts/volcano_analysis.R example/sample_data.csv
```

## Repository Structure

- `scripts/` – R scripts used for the analysis
- `example/` – example dataset
- `data/` – place your own data files here (not included)
- `results/` – created by the script to store output plots and tables

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.
