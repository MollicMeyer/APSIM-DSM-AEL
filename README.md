# DSM and SSURGO APSIM comparison

## Code-only repository and required external data

This private repository contains simulation and analysis code, tests, Windows resource-limit helpers, and setup instructions. It does not contain the study data. A clone alone cannot reproduce the full experiment or run the data-dependent tests.

The following must be supplied separately, subject to the study authors' redistribution permissions:

- The source bundle and checksum manifests for 2,030 DSM and six dominant SSURGO model inputs, including their weather forcing.
- The spatial context bundle: DSM coordinates, paired-cell lookup tables, native/reference grid rasters, and source SSURGO polygons.
- Archived PRISM climate values, normals, and retrieval manifests used by the climate-context calculations.
- Reference results and assets required for production-comparison tests and the retained site-context artwork.

These materials remain in a separate local review archive, not in this repository. No data-download identifier is provided here. Obtain an authorized archive from the study authors before following the data-dependent commands in REPRODUCE.md. A separately licensed APSIM Next Generation 2025.3.7681.0 installation and the R dependencies listed below are also required. No APSIM engine binary, raw/private data, manuscript, reviewer response, or internal review evidence is uploaded. Upstream DSM model training is outside this code package's reproduction scope.

Reproducibility code for the author-review development, retaining the verified V14 simulation results. The author approved MollicMeyer/APSIM-DSM-AEL as a private review repository. No collaborators or code license have been authorized. Public release, data redistribution, and a public release identifier remain pending. See PUBLICATION_CHECKLIST.md.

The git source tree is code-only. The complete local V14 review archive supplies the 2,036 portable inputs and spatial/climate assets; a clone without that archive cannot run the data-dependent commands. V15 changes presentation and descriptive grouping, not the simulations. The final public release must identify the authorized data archive explicitly.

The coordinated local V15 archive retains those verified inputs and results with the current code and updated figures. Its PACKAGE_MANIFEST.csv records SHA256 identities. New components include descriptive upper/middle/lower summaries, a conceptual hillslope profile, precipitation and temperature departures, clearer labels, and an optional hard-capped lower-memory map renderer. The Windows capped-render helper passed an end-to-end repeat with byte-identical map PNGs; native limits and statistics matched the earlier renderer within1e-10. These are rendering and reporting changes, not new simulation outcomes.

## Current experiment

The AirDry-consistent experiment uses 2,030 DSM profiles and six dominant SSURGO profiles in the same 2013–2021 maize–soybean rotation. Primary maize years are 2017, 2019, and 2021. Initial mineral N is 63.9519167 kg/ha in 2013 only, C:N is 12, and initial water storage is 609 mm. The engine is APSIM Next Generation 2025.3.7681.0. Weather, management, crop lower limits, layer structures, and non-AirDry soil inputs are preserved.

AirDry is estimated as 0.5 × LL15 at 0–15 cm, 0.8 × LL15 at 15–30 cm, and LL15 below 30 cm, with exact overlap weighting for each existing model layer. These are documented assumptions, not field measurements. The differing DSM/SSURGO layer structures remain unchanged.

All 2,036 full-field simulations and independent SQL checks passed. The V14 analyses and figures use these outputs. Earlier single-year experiments and the October 10 package are historical and are not substitutes for this package. See REPRODUCE.md for the portable command sequence.

## Execution

All scientific preparation, orchestration, extraction, statistics, and mapping use R. The PowerShell/C# helper only enforces Windows process-resource limits. Use an appropriately licensed APSIM installation; no engine binary is distributed or automatically downloaded.

```text
Rscript R/prepare.R --source-manifest /path/to/source/manifest.csv --out /new/experiment
Rscript R/run.R --experiment /new/experiment --engine C:/APSIM/bin/Models.exe
Rscript R/extract.R --experiment /new/experiment --out /new/extracted_results
Rscript R/verify_extraction.R --experiment /new/experiment --results /new/extracted_results --out /new/independent_verification
```

The source manifest identifies archived simulation inputs and their MD5 hashes. Preparation creates new files, verifies that only AirDry, passive reporting, and weather-path relocation changed, and records the resulting identities. `--test-pair yes` selects one DSM and one SSURGO profile for a smoke test; the default prepares all 2,036. No dataset has been publicly distributed.

The source-bundle commands preserve every non-weather field and retain both upstream and relocated hashes. Export is a maintainer operation; import provides a new source manifest without any dependency on the original drive paths. Output folders must not already exist. The importer never changes the bundle.

```text
Rscript R/export_source_bundle.R --source-manifest /archive/manifest.csv --out /new/source_bundle
Rscript R/import_source_bundle.R --bundle /path/source_bundle --out /new/relocated_source
Rscript R/prepare.R --source-manifest /new/relocated_source/manifest.csv --out /new/experiment
```

`R/export_context_bundle.R --spatial-source /archive/data --climate-source /archive/climate --cell-index /verified/cell_index.csv --out /new/context_bundle` collects grid geometry, source polygons and archived PRISM retrieval evidence. Its paired-grid table contains coordinates, DHP, true MUKEY and cell area. The legacy native-grid raster supplies geometry only: the index builder replaces all its values. The rebuilt index is byte-identical to the verified production index. Climate arithmetic also reproduces from the relocated context bundle.

Extraction verifies per-run receipts and database hashes, independently checks weather and C/N accounting, and writes small per-profile checkpoints. Use `--resume yes` only on an interrupted extraction with unchanged inputs/code; existing differing products are preserved and cause a stop. Annual tables include all nine years; primary-year interfaces are 2017, 2019 and 2021. Full-profile soil means in the compatibility table are not the manuscript's 0–30/30–100-cm association predictors.

After all 2,036 simulations are verified, `R/postprocess.R --experiment /experiment --cell-index /verified/cell_index.csv --polygons /context/spatial/ssurgo_polygons.geojson --climate-source /context/climate --out /new/analysis` runs extraction, independent verification, soil-input extraction, paired summaries, associations, variograms and figures in sequence. It freezes R sources, records hashes, waits for at least 1,536 MB free memory before each stage, and stops with outputs preserved on failure. It rejects an incomplete simulation batch. In production, the map-stage memory check stopped after library loading; its failed logs were preserved, and maps were subsequently rendered into a new folder once memory recovered. Remaining figure stages passed separately. A stopped pipeline must be reviewed rather than blindly restarted against existing output folders.

`APSIM_R_LIB` can identify an existing R library directory. Scripts never install packages. Direct dependencies used here are jsonlite 1.7.3, DBI 1.1.2, RSQLite 2.3.1, digest 0.6.29, raster 3.5-15, sp 1.4-6, sf 1.0-6, terra 1.5-21, ggplot2 3.3.5, scales 1.1.1, aqp 1.41 and lattice 0.20-45, with R 4.1.2 and base grid. Stage-specific sessionInfo records include transitive dependencies. The capped simulation runner is Windows-specific; other operating systems have not been tested.

## Memory and recovery

One APSIM process runs at a time, at below-normal priority. A Windows Job Object enforces a 1,024-MB aggregate committed-memory limit on the simulation process tree. The R coordinator and operating system are outside that job, so this is not a 1-GB limit on all computer activity. The coordinator waits until at least 1,536 MB is available before starting a model. The helper stops its own job if available physical memory stays below 384 MB for two seconds. Every run records its peak committed memory and lowest observed system headroom. These safeguards cannot guarantee protection against memory pressure caused by unrelated applications.

The first full-field attempt stopped after 21 verified models because free system memory crossed the emergency floor. The interrupted output was archived with checksums, all prior results and inputs were verified, and a separately frozen recovery runtime raised the launch threshold from 1,024 to 1,536 MB. No cap was relaxed and no scientific input changed. Operational failure preservation and context migration are recorded outside the public source tree; they are not silent retries.

The helper assigns a suspended process to the capped job before allowing APSIM to run. The job closes with its helper and kills any remaining child processes. Existing databases, logs, and failed attempts are never silently overwritten. A failed run stops the batch for review. A live lock must be investigated before any resume; never start a second coordinator against a live batch.

## Verification boundary

The complete batch passed initialization, daily continuity, annual carryover, rotation, termination and hash checks. Independent SQL verification passed for all 2,036 profiles. A DSM/SSURGO pair was regenerated from the relocated full source bundle in a path containing spaces, simulated with the same cap, extracted and independently verified. Its yields reproduced production values within 1e-7 Mg/ha and its soil predictors matched. The earlier 36-run AirDry pilot is an operational diagnostic, outside the manuscript results.

`tests/test_extract.R` checks annual grain yields against a separate SQL aggregation, forces an extraction interruption, resumes from the checkpoint, compares CSV hashes with an uninterrupted extraction, and checks idempotent resume. `tests/test_bundle.R` verifies JSON/weather round-trip relocation. `tests/test_relocated_end_to_end.R` tests both soil sources through preparation, capped simulations, extraction, SQL verification and predictor checks. `tests/audit_engine.R` checks the archived engine-tree manifest. Curated evidence accompanies the local review package.

`R/build_cell_index.R` rebuilds the original nearest-neighbor mapping between DSM identifiers and the common DHP grid; it does not interpolate yield for statistics. `tests/test_cell_index.R` verifies all 1,943 paired cells against the archived maps. `R/summarize_paired.R` computes field/DHP means, spatial SDs, paired differences and percentages relative to SSURGO. Its `--historical yes` mode explicitly labels earlier outputs. `R/build_review_tables.R` independently rejoins native cells to extracted profile yields and recomputes all 18 field/DHP summaries before generating manuscript numbers.

`R/extract_soil_inputs.R` saves exact horizons and overlap-weighted OC, pH, sand, silt and clay for 0–30 and 30–100 cm. `R/associate_soils.R` binds those predictors to the verified yields and computes descriptive regressions and raw-input correlations. Hydraulic estimates and bulk density are absent from these regressions. Both modules passed production integration and the relocated pair test. `R/check_environment.R` records package versions and parses repository R scripts without installing anything.

`R/fit_variograms.R` computes omnidirectional empirical semivariograms on the native paired cells with twelve 24-m lags through 288 m. The portable fitter retains the approved spherical/exponential/Gaussian candidate algorithm; `tests/test_variogram_fit.R` reproduces all 27 archived fits and all nine selected models within CSV precision. Unidentified ranges remain missing, not extrapolated as confident estimates.

`R/plot_maps.R` and `R/plot_summary_figures.R` preserve the approved palettes and author's depth-function colors. They refuse to render alongside a Models process and require 1,536 MB free RAM after loading their libraries. All main and analysis-dependent supplemental figures were regenerated. Original Supplemental Figure S1 is retained as static site-context artwork with source provenance; its original cartographic composition is not recreated by the analysis scripts.

`R/verify_extraction.R` independently queries each SQLite database for annual grain maxima, initial mineral N and layerwise year-boundary continuity; all 2,036 production profiles passed. `R/associate_oc_differences.R` describes paired OC-input and yield differences by year and hillslope position at both depths; it does not attribute differences causally to OC loss.

`R/reproduce_climate.R --source-data /archive/climate --weather /experiment/weather/forcing.met --out /new/climate` reproduces climate arithmetic from checksum-verified archived PRISM downloads, without retrieving new data or repeating the source geometry audit. `tests/test_climate.R` reproduces all six archived tables within 1e-10 tolerance. Known annual precipitation and dataset-label caveats remain recorded. `R/plot_climate.R --climate /new/climate --out /new/climate_figure` regenerated the approved blue positive and vermillion negative precipitation departures. PRISM context is separate from actual simulation forcing and does not isolate weather-only effects.

This repository's target is reproduction from archived soil/model inputs through simulation and manuscript analysis. Retraining the published DSM models and rebuilding historical SSURGO downloads are separate upstream provenance tasks, not currently supported commands. No observed-yield validation, significance testing, or soil equilibrium claim is made.

## Release checklist

The approved upload scope is code, tests, resource helpers, and documentation only, in a private repository with no collaborators. No engine binary, observed-yield dataset, soil inputs, spatial assets, manuscript, or review evidence is included in the git source tree. Before public release, review script ownership and data redistribution rights, select a code license, and publish an authorized tagged release with the actual input-archive identifier. A private code repository is not yet a public reproducibility deposit.
