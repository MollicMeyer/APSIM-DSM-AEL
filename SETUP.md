# R and APSIM setup

Use R 4.1.2 for the tested environment and APSIM Next Generation 2025.3.7681.0. Obtain APSIM from the [APSIM Initiative](https://registration.apsim.info/) under the applicable licence; it is not included here.

The scientific scripts use `apsimx`, `aqp`, `soilDB`, `terra`, `sf`, `dplyr`, `tidyr`, `gstat`, `ggplot2`, `gridExtra`, `lattice`, `scales`, `jsonlite`, `DBI` and `RSQLite`, plus R's base and recommended packages. Their tested versions are listed in `package_versions.csv`. Each analytical stage also saves `sessionInfo()` with its results.

Install dependencies into a project-specific library if your normal R library uses different versions. The scripts do not install packages or change your system libraries. In particular, install the archived **apsimx 2.8.0**, available from the [official CRAN archive](https://cran.r-project.org/src/contrib/Archive/apsimx/), after its dependencies. In R, the installation command is:

```r
install.packages(
  "https://cran.r-project.org/src/contrib/Archive/apsimx/apsimx_2.8.0.tar.gz",
  repos = NULL, type = "source"
)
```

On Windows, source packages containing compiled code require the Rtools version matching your R installation. APSIM and geospatial package system dependencies must also be installed appropriately for your operating system. The full reproduction is tested on Windows; portability to other operating systems is not established.

Do not automatically upgrade `apsimx` for this experiment: more recent `ssurgo2sp()` defaults change survey hydraulic properties, bulk density and organic-carbon conversion. Version 2.8.0 reproduces the archived survey profiles. The DSM aggregation deliberately supplies a generic `stats::median()` function; the `terra` 1.5.21 built-in median shortcut does not reproduce the archived values.

Run from the project root. Set `APSIM_EXE` to the full path of `Models.exe`, or edit only its path in `settings.R`. Keep the supplied weather, rotation, mineral-N stocks and soil conversion settings unchanged when reproducing this experiment. Use a fresh output directory for any scientifically different experiment.
