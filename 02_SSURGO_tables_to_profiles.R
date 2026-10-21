# Convert archived SSURGO tables to the six dominant-component soil profiles.
# apsimx 2.8.0 is required to reproduce the original survey interpolation.
library(apsimx)
library(soilDB)
library(sf)
library(aqp)
library(dplyr)
source('R/SaxtonRawls2006.R')
stopifnot(as.character(packageVersion('apsimx'))=='2.8.0')
if(file.exists('results/SSURGO/profiles.rds')) stop('SSURGO profiles already exist; use a fresh results folder.')
dir.create('results/SSURGO',recursive=TRUE,showWarnings=FALSE)
mapunits <- read.csv('data/SSURGO/mapunits.csv',stringsAsFactors=FALSE)
components <- read.csv('data/SSURGO/components.csv',stringsAsFactors=FALSE)
chorizons <- read.csv('data/SSURGO/chorizons.csv',stringsAsFactors=FALSE)
query_polygons <- sf::st_read('data/SSURGO/query_polygons.geojson',quiet=TRUE)

# Archived soilDB survey tables keep the study independent of later SDA updates.
# ssurgo2sp ranks components by percentage within each map unit. Retain the first.
survey_profiles <- apsimx::ssurgo2sp(mapunit=mapunits,component=components,
  chorizon=chorizons,mapunit.shp=query_polygons,nmapunit=6,nsoil=2,
  method='linear',soil.bottom=200,nlayers=10)
dominant <- seq(1,12,2)
layers <- vector('list',length(dominant))
for(i in seq_along(dominant)) {
  raw <- survey_profiles[[dominant[i]]]
  id <- paste0('Maize_2018_',gsub(':','_',names(survey_profiles)[dominant[i]]),'_d')
  h <- data.frame(profile_id=id,top=seq(0,180,20),bottom=seq(20,200,20),
    sand=raw$ParticleSizeSand,silt=raw$ParticleSizeSilt,clay=raw$ParticleSizeClay,
    Carbon=raw$Carbon,PH=raw$PH,BD_survey=raw$BD,AirDry_survey=raw$AirDry)
  # These are the original author's post-conversion hydraulic calculations.
  h$om <- h$Carbon*1.72
  h$BD_rawls <- SaxtonRawlsBD(h$sand,h$clay,h$om)
  h$BD <- pmin(h$BD_survey,h$BD_rawls)
  h$LL15 <- SaxtonRawlsLL15(h$sand,h$clay,h$om)
  h$DUL <- SaxtonRawlsDUL(h$sand,h$clay,h$om)
  h$SAT <- SaxtonRawlsSAT(h$sand,h$clay,h$om)
  h$KS <- SaxtonRawlsKSAT(h$sand,h$clay,h$om)
  # Retain the archived crop lower limits, not new crop calibration parameters.
  h$crop_LL <- pmax(.15*exp(-.2*(seq_len(nrow(h))-1)),h$AirDry_survey,h$LL15)
  shallow <- pmax(0,pmin(h$bottom,15)-h$top)
  middle <- pmax(0,pmin(h$bottom,30)-pmax(h$top,15))
  deep <- pmax(0,h$bottom-pmax(h$top,30))
  h$AirDry <- h$LL15*(.5*shallow+.8*middle+deep)/(h$bottom-h$top)
  h$SoilCNRatio <- 12
  layers[[i]] <- h
}
horizons <- as.data.frame(bind_rows(layers))
stopifnot(nrow(horizons)==60,all(is.finite(as.matrix(horizons[-1]))))
write.csv(horizons,'results/SSURGO/horizons.csv',row.names=FALSE)
aqp::depths(horizons) <- profile_id ~ top + bottom
saveRDS(horizons,'results/SSURGO/profiles.rds')
writeLines(capture.output(sessionInfo()),'results/SSURGO/sessionInfo.txt')
cat('Prepared six dominant SSURGO soil profiles from archived survey tables.\n')
