# Set the path to your separately licensed APSIM installation before step 04.
# Alternatively define APSIM_EXE in your R session or operating environment.
apsim_executable <- Sys.getenv('APSIM_EXE','C:/path/to/APSIM2025.03.7681/bin/Models.exe')
simulation_directory <- 'results/simulations'
analysis_directory <- 'results/analysis'

# These scripts reproduce the archived survey conversion with this package version.
if(as.character(utils::packageVersion('apsimx'))!='2.8.0') {
  stop('Use apsimx 2.8.0; newer survey-conversion defaults change the soil inputs.')
}
