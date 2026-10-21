# Convert the original DSM maps to a SoilProfileCollection.
# Run from the project folder. Source TIFFs are at 3 m; APSIM profiles use 12 m.
library(terra)
library(aqp)
library(dplyr)
source('R/SaxtonRawls2006.R')

dir.create('results/DSM',recursive=TRUE,showWarnings=FALSE)
if (file.exists('results/DSM/profiles.rds')) stop('DSM profiles already exist; use a fresh results folder.')
cells <- read.csv('data/DSM/cells.csv')
stopifnot(nrow(cells)==2030,!anyDuplicated(cells$site))
depths <- data.frame(label=c('0_5','5_15','15_30','30_60','60_100','100_150','150_200'),
  top=c(0,5,15,30,60,100,150),bottom=c(5,15,30,60,100,150,200))

# Use the ordinary R median for every 4 x 4 block of source cells.
# It reproduces the archived ArcGIS aggregation in the tested package versions.
layers <- vector('list',nrow(depths))
for (i in seq_len(nrow(depths))) {
  layer <- data.frame(profile_id=paste0('Maize_',cells$site),top=depths$top[i],bottom=depths$bottom[i])
  for (property in c('TOC','pH','sand','silt','BD')) {
    r <- terra::rast(file.path('data/DSM/3m',paste0(property,'_',depths$label[i],'.tif')))
    stopifnot(all(abs(terra::res(r)-3)<1e-8))
    aggregated <- terra::aggregate(r,fact=4,
      fun=function(values,...)stats::median(values,na.rm=TRUE),na.rm=TRUE)
    terra::writeRaster(aggregated,file.path('results/DSM',paste0(property,'_',depths$label[i],'_12m.tif')),
      overwrite=FALSE)
    layer[[property]] <- terra::extract(aggregated,as.matrix(cells[c('x','y')]))[,1]
  }
  layers[[i]] <- layer
}

# The original profiles retained median sand and silt, with clay closed to 100%.
# BD comes from the mapped carbon-stock derivation, constrained as in the original code.
horizons <- bind_rows(layers) %>%
  mutate(clay=100-sand-silt,Carbon=TOC,PH=pH,om=Carbon*1.724,
    BD_mapped=BD,BD_rawls=SaxtonRawlsBD(sand,clay,om),BD=pmin(BD_mapped,BD_rawls),
    LL15=SaxtonRawlsLL15(sand,clay,om),DUL=SaxtonRawlsDUL(sand,clay,om),
    SAT=SaxtonRawlsSAT(sand,clay,om),KS=SaxtonRawlsKSAT(sand,clay,om),
    AirDry=LL15*ifelse(bottom<=15,.5,ifelse(bottom<=30,.8,1)),SoilCNRatio=12) %>%
  arrange(profile_id,top) %>% as.data.frame()
stopifnot(nrow(horizons)==14210,all(is.finite(as.matrix(horizons[-1]))))
write.csv(horizons,'results/DSM/horizons.csv',row.names=FALSE)

# Keep profile IDs and coordinates attached for the return from simulations to maps.
aqp::depths(horizons) <- profile_id ~ top + bottom
sites <- data.frame(profile_id=paste0('Maize_',cells$site),x=cells$x,y=cells$y)
aqp::site(horizons) <- sites
saveRDS(horizons,'results/DSM/profiles.rds')
writeLines(capture.output(sessionInfo()),'results/DSM/sessionInfo.txt')
cat('Prepared',length(horizons),'DSM soil profiles from the source TIFFs.\n')
