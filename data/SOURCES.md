# Scientific input sources

These files are the inputs to the mapped-soil simulation comparison, not a replacement for the original DSM training dataset. Coordinates identify the study field; location disclosure is authorized by the author. No measured-yield records or laboratory-personal records are included.

| Files | Source and role |
| --- | --- |
| `DSM/3m/*.tif` | Author's original clipped 3-m maps: organic C, pH, sand, silt and carbon-stock-derived bulk density at seven depths from 0 to 200 cm. Step 01 reproduces the original factor-four median aggregation and the author's hydraulic calculations. |
| `DSM/cells.csv` | Author's native DSM profile identifiers and projected coordinates; retained to link profiles to maps. |
| `SSURGO/mapunits.csv`, `components.csv`, `chorizons.csv` | Archived USDA-NRCS survey tables used by the original study. The component table is the original raw survey table, not a later hand-entered summary. Archiving avoids changes from newer live Soil Data Access queries. |
| `SSURGO/query_polygons.geojson` | Archived survey geometries supplied to `ssurgo2sp()`. |
| `SSURGO/delineations.geojson` | Source survey polygon boundaries used to draw maps. |
| `SSURGO/mapunits_12m.tif` and sidecars | Author's original categorical map-unit grid, including its MUSYM categories. Nearest-neighbor alignment to the DHP grid preserves the original pairing. This does not increase survey positional accuracy. |
| `landscape/DHP_12m.tif` | Author's digital hillslope-position grid. Codes 1-5 denote summit, shoulder, backslope, footslope and toeslope. |
| `rotation_template.apsimx` | Expert maize-first continuous 2013-2021 rotation template retained for the approved experiment, including crop and management settings. The soil is replaced in step 03. This is model configuration, not APSIM engine software. |
| `initial_mineral_N.csv`, `initial_water.csv` | Initial stocks and water concentrations exported from the common maize template, conservatively allocated to each profile's layers. |
| `weather.met` | Author's archived `SABR_2021.met`, identical by SHA-256 to the original APSIM3 weather file and the maize-first rotation copy. The original R script obtains 2013-01-01 through 2021-12-31 with `apsimx::get_iem_apsim_met(state="IA", station="IA0200")` and writes it with `write_apsim_met()`. Site header: IA-IA0200, latitude 42.03, longitude -93.8. This is not PRISM forcing. |
| `SSURGO/label_positions.csv` | Approved V16 cartographic label positions, retained to reproduce the figure layout. These coordinates are not additional soil or yield observations. |
| `climate/*.csv` | PRISM monthly and yearly point data and 1991-2020 monthly normals. Used for climate context only, not APSIM forcing. Site is the source-polygon centroid. |

Spatial coordinates are UTM zone 15N (EPSG:32615) unless a file explicitly specifies otherwise. Soil depths in `aqp` are centimetres; APSIM layer thicknesses are millimetres. Soil carbon and texture are percent; volumetric water limits are mm/mm, bulk density is g/cm3, and saturated conductivity is mm/day. Starting nitrate and ammonium are kg N/ha, not total soil N or ppm.

SSURGO source: USDA-NRCS, [Soil Survey Geographic Database](https://www.nrcs.usda.gov/resources/data-and-reports/soil-survey-geographic-database-ssurgo). The saved tables reflect the study's archived extraction, not the present online survey vintage.

PRISM attribution: PRISM Group, Oregon State University, https://prism.oregonstate.edu, accessed 9 October 2026. Download headers retain the point location, 4-km resolution, dataset labels and retrieval date. PRISM's [terms](https://prism.oregonstate.edu/terms/) permit reproduction and distribution of data with attribution; ownership remains with PRISM. These files must not be described as the actual station forcing used by APSIM.

Weather attribution: Iowa Environmental Mesonet, Iowa State University, https://mesonet.agron.iastate.edu. Its [usage statement](https://mesonet.agron.iastate.edu/disclaimer.php) identifies website materials as public domain and permits lawful use. The archived file is supplied unchanged rather than re-downloaded from a potentially revised record; its original retrieval date is not established. Weather-file SHA-256: `4cc46248e3375400a78073005e39ba2ee9afbeff0c5882a456b6a387c83df42c`. Header parameters, including AMP, are retained from the author's original file rather than re-estimated during this rebuild.

The author's original contributions are available under [CC BY 4.0](../LICENSE.md). Third-party material retains its original rights and terms rather than being relicensed under CC BY 4.0. The author has confirmed permission to publish `rotation_template.apsimx`, including its embedded crop definitions and management code. APSIM and R packages retain their separate licences and are obtained from their original distributors; the repository does not include the APSIM engine or grant an APSIM software licence.
