# Climate change and child mortality

Code, processed data and a synthetic-data demonstration accompanying the manuscript:

**Rising heat under climate change undermines Sustainable Development Goal 3.2 to end preventable child deaths**

This repository supports analyses of temperature-related under-five mortality (U5MR) and neonatal mortality (NMR), future mortality projections, and progress towards SDG 3.2. It contains figure scripts, saved R objects, source-data workbooks and a simulated Tanzania example. Individual-level mortality records are not included.

## Repository contents

| File | Contents |
| --- | --- |
| `Fig*.R` | Scripts or analysis examples for the corresponding figures |
| `Fig*.RData` | Processed figure data and saved analysis objects, where available |
| `SupplementaryTables.RData` | Data for Supplementary Tables S1-S16 |
| `Source Data.zip` | Source-data workbooks for the main figures, supplementary figures and supplementary tables |
| `demo.zip` | A five-script workflow using simulated Tanzania data |
| `MANIFEST_SHA256.txt` | File checksums |

After extracting `Source Data.zip`, open `SourceData_MainFigures.xlsx` or `SourceData_Supplementary.xlsx`. Worksheet names identify the corresponding figure or table. Read the column headings and notes for units and definitions.

## RData file mapping

| RData file | Figure or table |
| --- | --- |
| `Fig1_U5MR.RData`, `Fig1_NMR.RData` | Figure 1 |
| `Fig2_FigS6.RData` | Figure 2 and Supplementary Figure S6 |
| `Fig3.RData` | Figure 3 |
| `Fig4.RData` | Figure 4 |
| `Fig5.RData` | Figure 5 |
| `FigS1.RData` | Supplementary Figure S1 |
| `FigS2.RData` | Supplementary Figure S2 |
| `FigS3_S4_S5.RData` | Supplementary Figures S3-S5 |
| `FigS7.RData` | Supplementary Figure S7 |
| `FigS8.RData` | Supplementary Figure S8 |
| `FigS9.RData` | Supplementary Figure S9 |
| `FigS10.RData` | Supplementary Figure S10 |
| `FigS11.RData` | Supplementary Figure S11 |
| `FigS12.RData` | Supplementary Figure S12 |
| `FigS13.RData` | Supplementary Figure S13 |
| `SupplementaryTables.RData` | Supplementary Tables S1-S16 |

Supplementary Figure S14 is a schematic and has no numerical RData file. Separate plotting scripts are not supplied for every RData file.

## Reading the data

Use R or RStudio and set the working directory to the repository folder. The RData files can be loaded with base R. Each includes a `figure_source_data` list and a `source_data_provenance` record.

```r
data_env <- new.env()
load("FigS8.RData", envir = data_env)

# Inspect the available objects and data blocks.
ls(data_env)
names(data_env$figure_source_data)
names(data_env$figure_source_data$FigS8)

# Read the data underlying Figure S8.
data <- data_env$figure_source_data$FigS8$block1
head(data)
```

Use keys `Fig1`-`Fig5` for the main figures, `FigS1`-`FigS13` for supplementary figures, and `TableS1`-`TableS16` for supplementary tables. Some tables contain embedded header rows; inspect their structure before converting columns to numeric values.

In Figure S8, the country code `NA` denotes Namibia. If exporting these data to CSV, do not treat this code as a missing value.

## Plotting scripts

Keep each script and its input RData file in the same folder. The supplied files were checked with R 4.4.2. Package requirements vary by script; see its `library()` and `package::function()` calls.

The scripts use `ggplot2`, `dplyr`, `dlnm`, `mixmeta`, `fixest`, `foreach`, `doParallel`, `gridExtra`, `sf`, `ragg`, `jsonlite` and `scales`, as applicable. `splines` and `grid` are included with R.

For example, to reproduce Figure S8:

```r
install.packages(c("ggplot2", "ragg"))  # Run once if needed.
source("FigS8.R")
```

This script writes `FigS8.pdf`, `FigS8.png` and a preview to the working directory. Times New Roman and Cairo PDF support are required to preserve the supplied figure style. Other scripts may contain examples for selected panels rather than exporting a complete assembled figure.

## Synthetic Tanzania demonstration

Extract `demo.zip` and set the R working directory to the extracted `tanzania_temperature_mortality_demo` folder. Use R 4.4 or later with `dlnm` and `ggplot2`. Run the scripts in this order:

```r
source("01_mortality_data_preparation.R")
source("02_exposure_data_preparation.R")
source("03_exposure_response_model.R")
source("04_future_climate_population_preparation.R")
source("05_future_mortality_projection.R")
```

The workflow covers mortality-data preparation, temperature-data preparation, exposure-response modelling, future climate and population inputs, and mortality projections. The projection step estimates historical temperature-unrelated mortality, projects this component forward, and then calculates future temperature-related mortality.

All demonstration inputs are simulated, including deaths, temperatures, births, population and mortality rates. The simulated deaths include clustering around the middle of each month. Outputs are illustrative and must not be interpreted as empirical estimates for Tanzania.

Generated input and processed data are stored in `data_raw/` and `data_processed/`; model results and figures are stored in `outputs/`.

## Data scope

The package provides processed source data and code examples, not a complete refitting pipeline for every manuscript result. The synthetic demonstration produces single-country point estimates and does not reproduce the full multi-country, multi-climate-model uncertainty analysis. Refer to the manuscript's Methods and Data Availability sections for the original data sources and access conditions.

For Figure S9, the saved regional summaries are provided, but the original country-membership and grouping code are not included.
