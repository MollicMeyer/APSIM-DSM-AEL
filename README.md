# DSM and SSURGO soil inputs in APSIM

R scripts for the soil-input comparison accompanying the Agricultural & Environmental Letters manuscript. The workflow starts with soil maps and archived survey tables, builds soil profiles, runs a continuous maize-soybean rotation, and returns simulated yields to the field map.

## Run the analysis

Open this folder as your R working directory. Install the packages in [SETUP.md](SETUP.md), including **apsimx 2.8.0**, and obtain APSIM Next Generation **2025.3.7681.0** separately. Set its executable path in `settings.R` or set the `APSIM_EXE` environment variable.

Run the numbered scripts in order:

1. `01_DSM_maps_to_profiles.R`: aggregate the original 3-m TIFFs to 12 m using the median of each 4-by-4 block; calculate the original soil hydraulic estimates and create an `aqp::SoilProfileCollection`.
2. `02_SSURGO_tables_to_profiles.R`: use `apsimx::ssurgo2sp()` with the archived survey tables; retain the dominant component of each of six map units and create its soil profile collection.
3. `03_profiles_to_APSIM.R`: convert both collections with `apsimx_soil_profile()` and replace the soil in the common rotation template.
4. `04_run_simulations.R`: run all 2,036 profiles sequentially with `apsimx()`. Completed runs are checked before being reused; failed or unverified outputs are not silently overwritten.
5. `05_analyze_yields_and_spatial_patterns.R`: extract 2017, 2019 and 2021 maize yields, reconstruct native-grid pairs, summarize hillslope patterns, calculate raw-soil associations, and fit semivariograms with `gstat`.
6. `06_figures_and_maps.R`: draw the yield, soil-profile, association and spatial figures with `ggplot2`, `sf`, `terra` and `aqp`.
7. `07_climate_context.R`: summarize the archived PRISM departures from 1991-2020 normals and plot climate context.

For example, in R: `source("01_DSM_maps_to_profiles.R")`. Run each subsequent script after its predecessor finishes. To test just two already prepared profiles, use `Rscript 04_run_simulations.R Maize_1 Maize_2018_Canisteo_2922007_d` after step 03. Step 05 requires the complete field experiment.

All generated files go into the ignored `results/` directory. Start a new reproduction in a fresh project copy without `results/`; preparation refuses to overwrite an existing profile collection or model directory. Step 04 can resume completed simulations after checking input, weather, executable and database hashes. Computing time depends on the machine and is not estimated from the two-profile test.

## What is compared

The experiment uses 2,030 DSM profiles and six dominant SSURGO profiles over 2013-2021, with common weather, rotation and management. Initial mineral N is 63.9519167 kg/ha in 2013 only, initial profile water is 609 mm, and soil C:N is 12. Mineral N is not inferred from measured total N. Common AirDry fractions are 0.5 of LL15 at 0-15 cm, 0.8 at 15-30 cm, and LL15 below; fractions are overlap-weighted for survey layers. Crop lower limits are retained from the archived setup.

Field summaries use 1,943 paired 12-m cells, approximately 28 ha. The six survey-component outputs are assigned through the original categorical map-unit raster, not treated as thousands of independent soil observations. Map boundaries come from the source polygons. Cubic resampling is for figure display only; numerical summaries use native cells. Soil-yield regressions are descriptive associations, not independent estimates of causal sensitivity or validation against measured yields.

## Source data

The soil maps, survey tables, weather and rotation template are in `data/`. See [data/SOURCES.md](data/SOURCES.md) for their origins and terms of use. Software references and tested versions are listed in [REFERENCES.md](REFERENCES.md) and [package_versions.csv](package_versions.csv).
