# Edit these paths, then run the three numbered scripts from this folder.
project_directory <- normalizePath(getwd(), winslash = '/', mustWork = TRUE)
data_directory <- file.path(project_directory, 'data')
results_directory <- file.path(project_directory, 'results')
apsim_executable <- 'C:/path/to/APSIM2025.03.7681/bin/Models.exe'

# Use an existing R library here if packages are not in your default library.
# Sys.setenv(APSIM_R_LIB = 'C:/path/to/R/library')
if (nzchar(Sys.getenv('APSIM_R_LIB'))) {
  .libPaths(c(Sys.getenv('APSIM_R_LIB'), .libPaths()))
}

# Versions used for the final analysis. This code does not install packages.
package_versions <- c(
  jsonlite = '1.7.3', DBI = '1.1.2', RSQLite = '2.3.1', digest = '0.6.29',
  raster = '3.5.15', terra = '1.5.21', ggplot2 = '3.3.5', scales = '1.1.1',
  aqp = '1.41', lattice = '0.20.45'
)
missing_packages <- names(package_versions)[!vapply(
  names(package_versions), requireNamespace, logical(1), quietly = TRUE
)]
if (length(missing_packages)) {
  stop('Install these R packages first: ', paste(missing_packages, collapse = ', '))
}
.workflow_directory <- file.path(project_directory, 'R')
