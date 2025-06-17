# Agroforestry Systems Maintain Higher Reproductive Phenology and Functional Diversity than Ecological Restoration Areas in a Fragmented Tropical Forest Landscape

This repository contains the R scripts and data used for the analyses presented in the research project by Laíla Arnauth.

## Repository Structure

### 1. Phenological Potential - CWM-like Index

**Script:** `Script_fp_CWM.R`

- Calculates a phenological index (Fp) for flowering and fruiting based on monthly abundance and binary indicators.
- Performs Wilcoxon tests per month to compare Agroforestry Systems (AFS) and Restoration (RES) areas.
- Outputs line plots with error bars and significance asterisks.

**Input data:**
- `data_fp_fruiting_cwmlike.csv`
- `data_fp_flowering_cwmlike.csv`

**Output figures:**
**Output figures:**
- `cwm_fruiting_potential.jpeg`
- `cwm_flowering_potential.jpeg`
- `cwm_fruit_length.jpeg`
- `cwm_fruit_length_only_natives.jpeg`
- `cwm_flower_length.jpeg`
- `cwm_flower_length_only_natives.jpeg`
- `cwm_seed_length.jpeg`
- `cwm_seed_length_only_natives.jpeg`

### 2. Functional Diversity Analyses – Reproductive Traits

**Script:** `Script_functional.R`

- Calculates Functional Dispersion (FDis) and Community Weighted Mean (CWM) for floral, fruit and seed traits.
- Compares AFS and RES systems, including separate analyses for native species only.

**Input data examples:**
- `community.csv`, `functional_fl.csv`, `functional_fr.seed.csv`
- Versions filtered for only native species.

## Required R Packages

```r
library(dplyr)
library(ggplot2)
library(readxl)
library(writexl)
library(FD)
library(vegan)
library(tidyverse)  # (only for some native-only scripts)
```

## Citation
If you use any part of this repository, please cite the project appropriately.

## Contact
Laíla Arnauth – M.Sc. in Ecology – UFRJ
www.linkedin.com/in/laíla-iglesias-ufrj

---
_Last updated: 2025-06-17_
