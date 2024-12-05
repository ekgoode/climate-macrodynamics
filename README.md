# READ ME: Projecting the Impact of Rising Temperatures: The Role of Macroeconomic Dynamics.

This repository contains the replication package for the study "Projecting the Impact of Rising Temperatures: The Role of Macroeconomic Dynamics" by Gregory Casey, Stephie Fried, and Ethan Goode, published in the *IMF Economic Review* in 2023. The study examines how temperature variations influence economic growth, focusing on the distinction between temporary and permanent effects on output per capita.

**For greater detail on replicating the results in this repository, please reference the .pdf readme file in the main directory.**

## Overview

The provided materials include all necessary data and code to reproduce the results presented in the paper. The structure of the repository is as follows:

- **Data**: Located in the `/data/` directory, with subfolders:
  - `/raw/`: Contains original datasets.
  - `/climProj/`: Includes climate model projections.
- **Programs**: Found in the `/programs/` directory, comprising scripts written in R, Stata, and Matlab for data processing, analysis, and visualization.
- **Results**: Stored in the `/results/` directory, with subfolders:
  - `/tables/`: Generated tables.
  - `/figures/`: Produced figures.

The analysis workflow involves data cleaning, regression analysis, simulations, and the creation of tables and figures, as detailed in the "Instructions for Replicators" section below.

## Data Availability and Provenance

All data utilized in this study are publicly accessible. The primary sources include:

- **Climate Model Projections**: Downloaded from the World Meteorological Organization’s KNMI Climate Change Atlas.
- **Historical Weather Data**: Obtained from the replication data for "Global non-linear effect of temperature on economic production" by Burke, Hsiang, and Miguel (2016).
- **Economic Data**: Sourced from the Penn World Table version 10.0.
- **Agricultural Value-Added Data**: Collected from the World Bank and supplemented with data from the Economic Transformation Database for Vietnam.
- **Regional Classifications**: Derived from the appendix of "Temperature Shocks and Economic Growth: Evidence from the Last Half Century" by Dell, Jones, and Olken (2012).

Detailed information on each dataset, including download links and specific usage, is provided in the "Dataset List" section below.

## Computational Requirements

To replicate the study's results, the following software and packages are required:

- **Stata** (version 15 or later) with the `ivreg2` and `estout` packages.
- **R** (version 4.2.0 or later) with packages: `tidyverse`, `readxl`, `here`, `parallel`, `RColorBrewer`, `stargazer`, `viridis`, `scales`, `ggmap`, `foreign`, `maps`, and `renv`.
- **Matlab** (version R2020a or later).

Approximate runtimes on a standard 2019 laptop are:

- Data processing: ~9 seconds.
- Bootstrap sample processing: Time to be determined.
- Remaining analyses: ~30 minutes.

## Description of Programs

The `/programs/` directory contains scripts categorized as follows:

- **Data Cleaning**: `create_data.R` processes raw data into a unified dataset for analysis.
- **Analysis**: Various scripts (`regression_tables_baseline.do`, `regression_tables_het.do`, etc.) perform regression analyses and simulations.
- **Figures and Tables**: Scripts like `create_figures.R` and `create_maps.R` generate the visualizations and tables presented in the paper.
- **Setup**: `initialize_env.R` loads required libraries and custom functions.

Each script's specific function and sequence are detailed in the "Instructions for Replicators" section below.

## Instructions for Replicators

To reproduce the study's results:

1. **Initialize Environment**: Run `/programs/initialize_env.R` to load necessary R libraries and functions.
2. **Data Cleaning**: Execute `/programs/create_data.R` to process and clean the data.
3. **Regression Analysis**: Run the relevant Stata scripts (`regression_tables_baseline.do`, `regression_tables_het.do`, etc.) to perform regression analyses and generate result tables.
4. **Simulations**: Use `run_analysis_tfp.R` and `run_analysis_gdppc.R` to simulate economic trajectories based on the model's estimates.
5. **Bootstrap Analysis**: Execute `run_bootstrap.do` to perform bootstrap sampling, followed by `run_bootstrap_tfp.R` for simulations based on bootstrap estimates.
6. **Generate Figures and Tables**: Run `create_figures.R`, `create_maps.R`, and `create_tables.R` to produce all visualizations and tables included in the paper.

Intermediate datasets are saved in `/data/output/` during this process. For detailed instructions and sequence, refer to the "Instructions for Replicators" section in the provided documentation.

## Citation

If you utilize this replication package or reference the study, please cite:

Casey, G., Fried, S., & Goode, E. (2023). Projecting the Impact of Rising Temperatures: The Role of Macroeconomic Dynamics. *IMF Economic Review*, 71(3), 688–718. https://doi.org/10.1057/s41308-023-00203-0

For any questions or further information, please contact the authors. 
