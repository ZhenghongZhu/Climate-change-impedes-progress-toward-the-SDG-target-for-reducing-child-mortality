# Climate-change-impedes-progress-toward-the-SDG-target-for-reducing-child-mortality
## Overview

This repository contains the code and data used for the analysis of temperature-related under-5 and neonatal mortality across multiple regions. 

---

## Model Construction

The first-stage model uses a **monthly aggregation function** adapted from the method proposed by:

Basagaña, X. & Ballester, J. (2024). *Unbiased temperature-related mortality estimates using weekly and monthly health data: a new method for environmental epidemiology and climate impact studies*. The Lancet Planetary Health, 8, e766–e777.

* The implementation of the monthly aggregation function is based on publicly available code.
* The original code can be accessed via GitHub (see referenced publication for repository details).

---

## Code and Data Structure

* Each analysis script (e.g., `Fig1`) corresponds to a specific figure in the manuscript.
* For each script:

  * The **full model objects and processed datasets** are stored in corresponding `.RData` files.
  * These files ensure reproducibility of results without re-running the entire pipeline.

---

## Data Availability

All datasets used in this study are publicly available from the following sources:

### Historical Data

* **Meteorological Data**
  https://cds.climate.copernicus.eu/datasets/derived-near-surface-meteorological-variables?tab=overview

* **Mortality Data (INDEPTH Network)**
  https://www.indepth-ishare.org/index.php/catalog/central

* **Population Data**
  https://zenodo.org/records/10088105

* **Birth Rate Data (World Bank)**
  https://data.worldbank.org/indicator/SP.DYN.CBRT.IN

---

### Future Projections

* **Climate Model Data (ISIMIP)**
  https://data.isimip.org/search/

* **Population Projections**
  https://figshare.com/articles/dataset/Projecting_1_km-grid_population_distributions_from_2020_to_2100_globally_under_shared_socioeconomic_pathways/19608594/2

* **Birth Rate Projections (IIASA-WIC)**
  https://dataexplorer.wittgensteincentre.org

---

## Reproducibility

* All analyses can be reproduced using the provided scripts and `.RData` files.
* External data sources are openly accessible and can be re-downloaded if needed.


---

## Notes

* Ensure all required R packages are installed (e.g., `mixmeta`, `dlnm`, `dplyr`, `ggplot2`).
* File paths in scripts may need to be adjusted to match local directory structures.

---
