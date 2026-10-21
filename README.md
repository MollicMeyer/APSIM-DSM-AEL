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

## Scope and data

The included source files reproduce the simulation experiment from the mapped soil inputs onward. They do **not** retrain the original DSM models or reproduce soil sampling and laboratory work. No observed-yield validation data, manuscript drafts, reviewer responses, engine binaries, package libraries or revision-history utilities are included. APSIM must be installed separately under its own terms. See [data/SOURCES.md](data/SOURCES.md) for source files, attribution and redistribution limits.

The complete workflow has been run from the included source maps and survey tables through all 2,036 simulations, native yield maps, summaries and figures. Independent checks verified normal termination, continuous daily outputs, initial stocks, 6,108 primary-year profile yields and nine native-map round trips. Field means and soil-association results retain the manuscript's reported precision. Rebuilding profiles from source TIFFs introduces very small storage-precision differences from the archived profiles: the largest yield difference across all simulated years was 0.070 kg/ha. Semivariograms use `gstat::variogram()` and `fit.variogram()` with several starting values; standard package fitting gives small parameter differences from the earlier implementation, without changing the selected model families.
