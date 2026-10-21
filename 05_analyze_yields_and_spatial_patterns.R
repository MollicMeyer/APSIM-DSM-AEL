# Read APSIM yields, return them to the native field grid, and summarize patterns.
library(apsimx)
library(aqp)
library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(gstat)
source('settings.R')
dir.create(analysis_directory,recursive=TRUE,showWarnings=FALSE)
manifest <- read.csv(file.path(simulation_directory,'manifest.csv'))
stopifnot(nrow(manifest)==2036)
stopifnot(all(file.exists(paste0(manifest$input,'.complete.json'))))
if(file.exists(file.path(analysis_directory,'paired_cells.csv')))stop('Analysis already exists; do not overwrite reviewed results.')
years <- c(2017,2019,2021)

# Maize.Grain.Wt is reported as g/m2; multiply by 0.01 for Mg/ha.
annual <- vector('list',nrow(manifest))
for(i in seq_len(nrow(manifest))) {
  db <- sub('[.]apsimx$','.db',manifest$input[i])
  daily <- apsimx::read_apsimx(file=basename(db),src.dir=dirname(db),value='WarmupDaily')
  daily$year <- as.integer(substr(daily$Date,1,4))
  annual[[i]] <- daily %>% filter(year %in% years) %>% group_by(year) %>%
    summarise(yield_Mg_ha=max(MaizeGrain)*.01,.groups='drop') %>%
    mutate(profile_id=manifest$profile_id[i],product=manifest$product[i])
  if(i%%200==0)cat('Read yields for',i,'profiles\n')
}
yields <- bind_rows(annual)
write.csv(yields,file.path(analysis_directory,'profile_yields.csv'),row.names=FALSE)

# Recreate the 12m profile-ID grid and align it to the field's DHP grid.
cells <- read.csv('data/DSM/cells.csv')
profile_grid <- terra::rast(xmin=min(cells$x)-6,xmax=max(cells$x)+6,
  ymin=min(cells$y)-6,ymax=max(cells$y)+6,resolution=12,crs='EPSG:32615')
profile_grid[terra::cellFromXY(profile_grid,as.matrix(cells[c('x','y')]))] <- cells$site
dhp <- terra::rast('data/landscape/DHP_12m.tif')
aligned <- terra::resample(profile_grid,dhp,method='near')
names(aligned) <- 'site';names(dhp) <- 'DHP_code'
survey_grid <- terra::rast('data/SSURGO/mapunits_12m.tif')
survey_grid <- terra::resample(survey_grid,dhp,method='near')
names(survey_grid) <- 'MUSYM'
index <- as.data.frame(c(aligned,dhp,survey_grid),xy=TRUE,na.rm=TRUE)
lookup <- read.csv('data/SSURGO/mapunits.csv') %>% transmute(MUSYM=as.character(musym),mukey=as.character(mukey))
index$MUSYM <- as.character(index$MUSYM)
index <- left_join(index,lookup,by='MUSYM')
index$profile_id <- paste0('Maize_',index$site)
index$mukey <- as.character(index$mukey)
index$DHP <- c('SU','SH','BS','FS','TS')[index$DHP_code]
index$area_ha <- 12*12/10000
stopifnot(nrow(index)==1943,!anyNA(index$DHP),!anyNA(index$mukey),all(index$profile_id %in% paste0('Maize_',cells$site)))
write.csv(index,file.path(analysis_directory,'field_cell_index.csv'),row.names=FALSE)

dominant <- yields %>% filter(product=='SSURGO') %>%
  mutate(mukey=sub('.*_([0-9]+)_d$','\\1',profile_id)) %>%
  select(year,mukey,dominant=yield_Mg_ha)
paired <- index %>% inner_join(yields %>% filter(product=='DSM') %>% select(year,profile_id,dsm=yield_Mg_ha),by='profile_id',multiple='all') %>%
  inner_join(dominant,by=c('year','mukey')) %>% mutate(delta=dsm-dominant)
stopifnot(nrow(paired)==3*nrow(index))
write.csv(paired,file.path(analysis_directory,'paired_cells.csv'),row.names=FALSE)

# Return every paired value to its actual 12m cell, without display smoothing.
map_directory <- file.path(analysis_directory,'native_maps')
dir.create(map_directory,showWarnings=FALSE)
for(year in years)for(map in c('dsm','dominant','delta')) {
  z <- paired[paired$year==year,]
  r <- terra::rast(dhp);terra::values(r) <- NA_real_
  r[terra::cellFromXY(r,as.matrix(z[c('x','y')]))] <- z[[map]]
  names(r) <- paste0(map,'_Mg_ha')
  terra::writeRaster(r,file.path(map_directory,paste0(map,'_',year,'.tif')),
    datatype='FLT8S',overwrite=FALSE)
}
by_position <- paired %>% group_by(year,DHP) %>% summarise(cells=n(),area_ha=sum(area_ha),
  mean_DSM=mean(dsm),mean_SSURGO=mean(dominant),mean_difference=mean(delta),
  median_difference=median(delta),SD_difference=sd(delta),.groups='drop')
field <- paired %>% group_by(year) %>% summarise(cells=n(),area_ha=sum(area_ha),
  mean_DSM=mean(dsm),mean_SSURGO=mean(dominant),mean_difference=mean(delta),
  percent_difference=100*mean(delta)/mean(dominant),SD_DSM=sd(dsm),SD_SSURGO=sd(dominant),.groups='drop')
write.csv(by_position,file.path(analysis_directory,'hillslope_summary.csv'),row.names=FALSE)
write.csv(field,file.path(analysis_directory,'field_summary.csv'),row.names=FALSE)

# Raw soil inputs, not pedotransfer outputs: separate 0-30 and 30-100cm associations.
predictors <- list()
for(product in c('DSM','SSURGO')) {
  h <- aqp::horizons(readRDS(file.path('results',product,'profiles.rds')))
  for(interval in c('0-30 cm','30-100 cm')) {
    limits <- if(interval=='0-30 cm')c(0,30)else c(30,100)
    z <- h %>% mutate(overlap=pmax(0,pmin(bottom,limits[2])-pmax(top,limits[1]))) %>%
      filter(overlap>0) %>% group_by(profile_id) %>%
      summarise(OC=weighted.mean(Carbon,overlap),pH=weighted.mean(PH,overlap),
        sand=weighted.mean(sand,overlap),silt=weighted.mean(silt,overlap),clay=weighted.mean(clay,overlap),
        .groups='drop') %>% mutate(product=product,interval=interval)
    predictors[[paste(product,interval)]] <- z
  }
}
predictors <- bind_rows(predictors)
write.csv(predictors,file.path(analysis_directory,'soil_depth_predictors.csv'),row.names=FALSE)
associations <- inner_join(yields,predictors,by=c('profile_id','product'),multiple='all') %>%
  pivot_longer(c(OC,pH,sand,silt,clay),names_to='property',values_to='soil_value')
r2 <- associations %>% group_by(year,product,interval,property) %>%
  group_modify(~{
    fit <- lm(yield_Mg_ha~soil_value,data=.x)
    data.frame(n=nrow(.x),R2=summary(fit)$r.squared,slope=unname(coef(fit)[2]))
  }) %>% ungroup()
write.csv(r2,file.path(analysis_directory,'soil_yield_associations.csv'),row.names=FALSE)

# Relate paired topsoil OC contrasts to the paired yield contrasts in the paper.
topsoil <- predictors %>% filter(interval=='0-30 cm')
survey_topsoil <- topsoil %>% filter(product=='SSURGO') %>%
  mutate(mukey=sub('.*_([0-9]+)_d$','\\1',profile_id)) %>%
  select(mukey,OC_SSURGO=OC)
oc_pairs <- paired %>%
  left_join(topsoil %>% filter(product=='DSM') %>% select(profile_id,OC_DSM=OC),by='profile_id') %>%
  left_join(survey_topsoil,by='mukey') %>% mutate(OC_difference=OC_DSM-OC_SSURGO)
stopifnot(!anyNA(oc_pairs$OC_difference))
oc_associations <- oc_pairs %>% group_by(year) %>% group_modify(~{
  fit <- lm(delta~OC_difference,data=.x)
  data.frame(cells=nrow(.x),mean_OC_DSM=mean(.x$OC_DSM),mean_OC_SSURGO=mean(.x$OC_SSURGO),
    R2=summary(fit)$r.squared,slope=unname(coef(fit)[2]))
}) %>% ungroup()
write.csv(oc_associations,file.path(analysis_directory,'paired_OC_yield_associations.csv'),row.names=FALSE)
soil_covariation <- topsoil %>% group_by(product) %>% summarise(profiles=n(),
  OC_clay_r=cor(OC,clay),.groups='drop')
write.csv(soil_covariation,file.path(analysis_directory,'topsoil_OC_clay_correlation.csv'),row.names=FALSE)

# Empirical semivariograms and standard package-based model fitting.
empirical <- list();candidates <- list();curves <- list()
for(year in years) for(map in c('dsm','dominant','delta')) {
  z <- paired[paired$year==year,]
  z$value <- z[[map]]
  points <- sf::st_as_sf(z,coords=c('x','y'),crs=32615)
  # Match the approved V16 lag support: twelve 24m lags, ending at 288m.
  v <- gstat::variogram(value~1,points,cutoff=288,boundaries=seq(24,288,24))
  empirical[[paste(year,map)]] <- transform(v,year=year,map=map)
  for(model in c('Sph','Exp','Gau')) {
    # Several ordinary starting values reduce local-fit sensitivity in gstat.
    starts <- expand.grid(range=c(12,24,36,48,72,96,144,180,240,300,600),
      nugget_fraction=c(0,.1,.2,.4,.6))
    fits <- lapply(seq_len(nrow(starts)),function(i) {
      range <- starts$range[i];fraction <- starts$nugget_fraction[i]
      try(gstat::fit.variogram(v,gstat::vgm(psill=max(v$gamma)*(1-fraction),model=model,range=range,
        nugget=max(v$gamma)*fraction),fit.method=7,debug.level=0),silent=TRUE)
    })
    fits <- Filter(function(f)!inherits(f,'try-error')&&is.finite(attr(f,'SSErr'))&&
      !isTRUE(attr(f,'singular')),fits)
    if(!length(fits))next
    fit <- fits[[which.min(vapply(fits,function(f)attr(f,'SSErr'),numeric(1)))]]
    nugget <- fit$psill[fit$model=='Nug'];partial <- fit$psill[fit$model!='Nug']
    scale <- fit$range[fit$model!='Nug']
    range_curve <- gstat::variogramLine(fit,dist_vector=seq(0,4*scale,length.out=4001))
    practical <- if(partial>0)stats::approx(range_curve$gamma,range_curve$dist,
      xout=nugget+.95*partial,ties='ordered')$y else NA_real_
    key <- paste(year,map,model)
    candidates[[key]] <- data.frame(year=year,map=map,model=model,nugget=nugget,
      partial_sill=partial,total_sill=nugget+partial,range_parameter_m=scale,
      finite_sill_range_m=if(model=='Sph')scale else NA_real_,
      practical_range95_m=practical,nugget_to_sill=nugget/(nugget+partial),
      sill_to_nugget=if(nugget>0)(nugget+partial)/nugget else NA_real_,
      weighted_SSE=attr(fit,'SSErr'),singular=isTRUE(attr(fit,'singular')))
    curves[[key]] <- transform(gstat::variogramLine(fit,maxdist=300,n=301),year=year,map=map,model=model)
  }
}
empirical <- bind_rows(empirical);candidates <- bind_rows(candidates)
selected <- candidates %>% group_by(year,map) %>% slice_min(weighted_SSE,n=1,with_ties=FALSE) %>% ungroup()
fitted <- inner_join(bind_rows(curves),selected %>% select(year,map,model),by=c('year','map','model'))
write.csv(empirical,file.path(analysis_directory,'empirical_variograms.csv'),row.names=FALSE)
write.csv(candidates,file.path(analysis_directory,'variogram_candidates.csv'),row.names=FALSE)
write.csv(selected,file.path(analysis_directory,'variogram_parameters.csv'),row.names=FALSE)
write.csv(fitted,file.path(analysis_directory,'fitted_variograms.csv'),row.names=FALSE)
writeLines(capture.output(sessionInfo()),file.path(analysis_directory,'sessionInfo.txt'))
cat('Descriptive field, hillslope, soil-input and geostatistical summaries complete.\n')
