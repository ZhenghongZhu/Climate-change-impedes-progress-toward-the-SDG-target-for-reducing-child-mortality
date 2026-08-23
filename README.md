# Tanzania temperature–child mortality projection demo

This folder contains a fully synthetic demonstration of the temperature–under-five mortality workflow used in the manuscript. It contains no real individual mortality records and no restricted INDEPTH data.

## Requirements

- R 4.4 or later
- R packages: `dlnm` and `ggplot2`

Install the packages once if needed:

```r
install.packages(c("dlnm", "ggplot2"))
```

## Running the demo

1. Unzip the demo.
2. In RStudio, set the working directory to the extracted `tanzania_temperature_mortality_demo` folder.
3. Run the five scripts in numerical order:

```r
source("01_mortality_data_preparation.R")
source("02_exposure_data_preparation.R")
source("03_exposure_response_model.R")
source("04_future_climate_population_preparation.R")
source("05_future_mortality_projection.R")
```

Each script uses `project_path <- "."`; no computer-specific absolute path is required.

## Workflow

1. `01_mortality_data_preparation.R` creates synthetic 1990–2018 death records with artificial mid-month date heaping, monthly mortality counts, historical national U5MR, births and population inputs.
2. `02_exposure_data_preparation.R` creates historical daily temperature and relative-humidity exposures.
3. `03_exposure_response_model.R` fits the monthly aggregate DLNM and estimates a significant U-shaped cumulative temperature–mortality curve.
4. `04_future_climate_population_preparation.R` creates synthetic daily temperatures, population and births for four SSP-style scenarios from 2020 to 2100.
5. `05_future_mortality_projection.R` follows the manuscript sequence:
   - estimates historical heat- and cold-related mortality from daily attributable fractions;
   - obtains historical temperature-unrelated U5MR as the residual;
   - extends the 1990–2018 annual reduction in temperature-unrelated U5MR through 2100;
   - calculates future daily attributable deaths as `(1 - 1 / RR) × daily deaths`;
   - combines temperature-unrelated, heat-related and cold-related mortality.

The future projection follows the daily attributable-number framework illustrated in the EDE18-0469 tutorial by Vicedo-Cabrera, Sera and Gasparrini, while retaining the mortality-component sequence described in the manuscript.

## Main outputs

Generated data are written to `data_raw/` and `data_processed/`. Results and `ggplot2` figures are written to `outputs/`.

- `outputs/model_diagnostics.csv`: MMT, Wald test and heat/cold RR diagnostics.
- `outputs/tanzania_historical_mortality_decomposition.csv`: historical heat, cold and temperature-unrelated U5MR.
- `outputs/temperature_unrelated_arr_summary.csv`: 1990–2018 annual reduction estimate.
- `outputs/tanzania_future_u5mr_projection.csv`: annual projections by SSP and adaptation level.
- `outputs/tanzania_projection_summary_2090.csv`: 2090 summary.
- `outputs/future_u5mr_projections.png`: projected U5MR pathways.

## Scope

All temperatures, deaths, populations, births and mortality rates are simulated for code demonstration. The demo provides point estimates and does not reproduce the manuscript's full multi-country, multi-GCM or Monte Carlo uncertainty analysis.
