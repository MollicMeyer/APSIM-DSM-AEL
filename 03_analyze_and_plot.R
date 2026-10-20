# Summarize the final simulations and produce the paper's graphs and maps.
# Run from the repository root: source('03_analyze_and_plot.R')
source('settings.R')
for (file in c('common.R', 'variogram_functions.R', 'yield_analysis.R', 'figures.R')) {
  source(file.path(.workflow_directory, file), encoding = 'UTF-8')
}
experiment <- file.path(results_directory, 'experiment')
analysis <- file.path(results_directory, 'analysis')
spatial <- file.path(data_directory, 'context', 'spatial')
dir.create(analysis, recursive = TRUE, showWarnings = FALSE)

# 1. Read annual yields and check them independently against the APSIM databases.
extract_simulated_yields(list(experiment = experiment, out = file.path(analysis, 'yields')))
verify_simulated_yields(list(experiment = experiment, results = file.path(analysis, 'yields'),
  out = file.path(analysis, 'yield_checks')))
gc()

# 2. Match DSM profiles and SSURGO map units on the original 12 m grid.
# The legacy-named raster supplies geometry only, not historical yield values.
align_field_cells(list(
  `dsm-xy` = file.path(spatial, 'dsm_xy.csv'),
  `native-grid` = file.path(spatial, 'rasters', 'Maize_yield_2018_kgm2.tif'),
  `reference-grid` = file.path(spatial, 'rasters', 'Kitchen_DHP_12m.tif'),
  `paired-cells` = file.path(spatial, 'paired_grid.csv'),
  out = file.path(analysis, 'cell_index')
))
summarize_yield_differences(list(results = file.path(analysis, 'yields'),
  `cell-index` = file.path(analysis, 'cell_index', 'cell_index.csv'),
  out = file.path(analysis, 'paired_yields')))

# 3. Describe soil-yield associations at 0-30 and 30-100 cm, and spatial dependence.
summarize_soil_depths(list(experiment = experiment, out = file.path(analysis, 'soil_depths')))
relate_soils_to_yield(list(results = file.path(analysis, 'yields'),
  `soil-inputs` = file.path(analysis, 'soil_depths'), out = file.path(analysis, 'soil_yield')))
relate_carbon_and_yield_differences(list(paired = file.path(analysis, 'paired_yields'),
  `soil-inputs` = file.path(analysis, 'soil_depths'), out = file.path(analysis, 'carbon_yield')))
describe_spatial_dependence(list(paired = file.path(analysis, 'paired_yields'),
  out = file.path(analysis, 'variograms')))
summarize_hillslope_positions(list(paired = file.path(analysis, 'paired_yields'),
  out = file.path(analysis, 'hillslope_positions')))
gc()

# 4. Calculate climate departures for interpretation, without changing model weather.
summarize_seasonal_climate(list(`source-data` = file.path(data_directory, 'context', 'climate'),
  weather = file.path(experiment, 'weather', 'forcing.met'), out = file.path(analysis, 'climate')))

# 5. Draw the final figures. Smoothing changes map display only, not the statistics.
plot_yield_maps(list(paired = file.path(analysis, 'paired_yields'),
  polygons = file.path(spatial, 'ssurgo_polygons.geojson'), out = file.path(results_directory, 'maps')))
gc()
plot_scientific_figures(list(paired = file.path(analysis, 'paired_yields'),
  `soil-inputs` = file.path(analysis, 'soil_depths'), associations = file.path(analysis, 'soil_yield'),
  variograms = file.path(analysis, 'variograms'), out = file.path(results_directory, 'figures')))
plot_seasonal_climate(list(climate = file.path(analysis, 'climate'),
  out = file.path(results_directory, 'climate_figure')))
cat('Final summaries, graphs and maps are in results/.\n')
