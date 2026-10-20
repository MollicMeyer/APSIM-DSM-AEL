# Reproduce the V14 comparison

This repository is code-only. First obtain the separately authorized input/context archive described in README.md; it is not included or automatically downloaded. Extract that archive into a new Windows folder and place this code at its root. Install a licensed APSIM Next Generation 2025.3.7681.0 separately. Use the R packages and versions recorded in README.md; complete sessionInfo records accompany the local review archive, not this repository. The package does not install software or publish anything.

Run from the extracted package root. Replace `C:/APSIM/bin/Models.exe` with the actual engine location. If packages are in a separate existing library, set `APSIM_R_LIB` to that library before running Rscript. Output directories must be new. The full run uses one capped model worker and may wait for memory.

```text
Rscript R/import_source_bundle.R --bundle data/source_bundle --out outputs/source
Rscript R/prepare.R --source-manifest outputs/source/manifest.csv --out outputs/experiment
Rscript R/run.R --experiment outputs/experiment --engine C:/APSIM/bin/Models.exe
Rscript R/build_cell_index.R --dsm-xy data/context/spatial/dsm_xy.csv --native-grid data/context/spatial/rasters/Maize_yield_2018_kgm2.tif --reference-grid data/context/spatial/rasters/Kitchen_DHP_12m.tif --paired-cells data/context/spatial/paired_grid.csv --out outputs/index
Rscript R/postprocess.R --experiment outputs/experiment --cell-index outputs/index/cell_index.csv --polygons data/context/spatial/ssurgo_polygons.geojson --climate-source data/context/climate --out outputs/analysis
```

For a two-profile smoke test, add `--test-pair yes` to preparation and use separate output folders. Extraction and independent verification of such a test require `--test-mode yes`; a test is never a substitute for the full-field comparison. `tests/test_relocated_end_to_end.R` provides the tested DSM/SSURGO workflow and compares it with verified production reference results.

The postprocessor creates frozen copies of its R scripts and stage-specific logs. If a stage stops, preserve its output and diagnose the log. Never rerun into existing folders or remove a live lock. A memory-check stop before map rendering can be continued manually with the same verified inputs and a new map output folder after RAM recovers. The following figure commands also work separately:

```text
Rscript R/plot_maps_lowmem.R --paired outputs/analysis/paired --polygons data/context/spatial/ssurgo_polygons.geojson --out outputs/maps
Rscript R/plot_summary_figures.R --paired outputs/analysis/paired --soil-inputs outputs/analysis/soil_inputs --associations outputs/analysis/associations --variograms outputs/analysis/variograms --out outputs/summary_figures
Rscript R/plot_climate.R --climate outputs/analysis/climate --out outputs/climate_figure
```

All yield comparisons use 1,943 native cells and six dominant SSURGO profiles. Display smoothing does not change numerical summaries. The published DSM models and historical SSURGO/profile-construction code are upstream provenance; model retraining is outside this package. Original Supplemental Figure S1 belongs to the separate local review archive as static artwork, with its source identity. It is not included in this repository, and its cartographic composition is not recreated by these scripts.

For a memory-constrained Windows desktop, use tools/render_maps_capped.ps1 with -Rscript, -Paired, -Polygons, -Output and a new -ControlDirectory. This checks1,536MB before launching R and imposes the same1,024MB committed-job limit and384MB emergency floor used by the resource helper. Default uncapped R rendering still checks1,536MB after loading libraries; do not pass --resource-job yes manually outside the capped helper. The lower-memory renderer reads the original GeoJSON rings without sf, retains holes and source vertices, and uses terra for display-only cubic resampling. No geometry simplification or native-value filtering is performed.

Rscript R/review_landscape_groups.R --paired outputs/analysis/paired --out outputs/landscape_groups computes area-weighted upper/middle/lower descriptive summaries while retaining five-class figure detail. These summaries and regressions do not supply independent replication for interaction tests.

Reference results, figures, and verification evidence belong to the separate local review archive, not this repository. Historical results used only in the AirDry change audit are explicitly identified there and are not V14 results. This repository contains no observed yields, APSIM executable, engine DLLs, credentials, or cloud publication settings. A code license and data redistribution permissions must be settled before public release.
