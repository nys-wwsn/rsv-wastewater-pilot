# rsv-wastewater-pilot

## Description
This repository houses the code and data for the paper "RSV concentration in wastewater and trends among incident hospitalizations in children and older adults" by Hill et al. The project linked to this paper investigated respiratory synctial virus (RSV) in wastewater in four New York counties during two respiratory seasons. 

## Citation
Hill DT, Laplante J, Byun S, Alazawi M, Gowie D, Biswas S, Hill L, Zhu Y, Foote MR, Kappus-Kron H, Blatz MN, Bradley I, Ye Y, St. George K, Larsen DA. 2026. RSV concentration in wastewater and trends among incident hospitalizations in children and older adults. 

## Repo Overview

### `Main` folder

**RSV - Data prep.R**: This R script loads and preprocesses the data for analysis.

**RSV - functions.R**: This R script contains functions used across R scripts in the analysis.

**tidycensus - population numbers.R**: This R script downloads age-adjusted population totals for New York State counties. 

### `data` folder

**County_Region.csv**: This data file contains county and regional population numbers for New York.

**ny_pop_numbers.csv**: This is the output of the `tidycensus - population numbers.R` script.

**rsv_combined_data.Rdata**: This is an R data filed that contains the dataset used for the analysis and creating the final figures and tables.

**rsv-cases-county.csv**: This data file is the county level RSV case counts for the counties included in the study. Original data are from the National Respiratory and Enteric Virus Surveillance Sysem (NREVSS).

**rsv-hospitalizations-county.csv**: This file contains county level RSV hospitalizations. ICD 10 codes that were used for curating these data include the following: 

| ICD 10 code type | Coding System | Codes* | Description |
|---|---|---|---|
| All diagnoses | ICD-10 | J12.1 | RSV Pneumonia |
| All diagnoses | ICD-10 | J21.0 | RSV Acute Bronchiolitis |
| All diagnoses | ICD-10 | J20.5 | RSV Bronchitis |
| All diagnoses | ICD-10 | B97.4 | RSV as cause of other diseases |

**rsv-wastewater-county.csv**: This dataset contains county level wastewater results for RSV concentration. Results are measured in gene copies/mL of wastewater. County level estimates are obtained from population-weighed data grouping by week.

### `figures and tables` folder

This folder contains the R scripts for making each figure or table, including supplementary figures and tables. Note that only county level figures and tables can be made from these scripts. Sewershed level data are not available in this repository.