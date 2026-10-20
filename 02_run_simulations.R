# Run the common 2013-2021 rotation for each prepared soil profile.
# Run from the repository root: source('02_run_simulations.R')
source('settings.R')
source(file.path(.workflow_directory, 'common.R'), encoding = 'UTF-8')
source(file.path(.workflow_directory, 'simulate_profiles.R'), encoding = 'UTF-8')

run_simulations(
  experiment = file.path(results_directory, 'experiment'),
  engine = apsim_executable
)
cat('Simulations complete. Next run 03_analyze_and_plot.R.\n')
