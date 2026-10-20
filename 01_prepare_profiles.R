# Prepare the final DSM and SSURGO APSIM inputs from the portable soil bundle.
# Run from the repository root: source('01_prepare_profiles.R')
source('settings.R')
source(file.path(.workflow_directory, 'common.R'), encoding = 'UTF-8')
source(file.path(.workflow_directory, 'profile_preparation.R'), encoding = 'UTF-8')
dir.create(results_directory, recursive = TRUE, showWarnings = FALSE)

# Relocate weather paths without altering the supplied soil-profile values.
import_soil_inputs(list(
  bundle = file.path(data_directory, 'source_bundle'),
  out = file.path(results_directory, 'source_profiles')
))

# Use one common AirDry rule, initial mineral N, initial water and C:N.
# All other profile, crop and management inputs are checked for preservation.
prepare_soil_profiles(list(
  `source-manifest` = file.path(results_directory, 'source_profiles', 'manifest.csv'),
  out = file.path(results_directory, 'experiment')
))
cat('Profiles prepared. Next run 02_run_simulations.R.\n')
