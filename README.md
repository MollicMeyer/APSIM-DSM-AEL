# APSIM comparison using DSM and SSURGO soil inputs

Code for the final soil-input comparison in the Agricultural & Environmental Letters paper. It prepares 2,030 DSM and six dominant SSURGO profiles, simulates a common 2013–2021 maize–soybean rotation, and compares maize yields in 2017, 2019 and 2021.

## Run the study

1. Open R in this repository's folder and edit the paths in **settings.R**.
2. Put the separately supplied study inputs under **data/**, as described below.
3. Run these scripts in order:

```r
source('01_prepare_profiles.R')
source('02_run_simulations.R')
source('03_analyze_and_plot.R')
```

The first script prepares the soil profiles. The second calls APSIM directly from R. The third extracts yields, summarizes differences and soil–yield associations, fits semivariograms, and draws the final graphs and maps. Outputs go to **results/**. Supporting functions are grouped by scientific purpose under **R/**; they do not need to be run individually.

## Requirements

- A separately licensed APSIM Next Generation **2025.3.7681.0** installation.
- R **4.1.2** and the packages listed with their study versions in **settings.R**. Install packages in your R library before running the scripts; nothing is installed automatically.
- The study's soil/model inputs, weather, spatial context and climate files. These are **not included** in this private code repository:

```text
data/
  source_bundle/       # APSIM profiles, shared weather and their input manifest
  context/
    spatial/           # DSM coordinates, grid rasters, paired cells and SSURGO polygons
    climate/           # Archived PRISM observations, normals and retrieval metadata
```

These directories must come from an authorized study input archive. A code-only clone cannot run the data-dependent workflow without them. Raw/private data, model binaries and static site-context artwork are not uploaded.

## Scientific scope

The workflow begins with archived mapped soil profiles; it does not retrain DSM maps or reconstruct historical SSURGO downloads. Both soil input maps use common management and initial conditions. Comparisons use native 12 m cells; cubic convolution is for map display only. Soil–yield regressions use raw inputs at 0–30 and 30–100 cm and are descriptive, not isolated causal effects.

Existing outputs are not silently overwritten. Use a new results directory for another run.
