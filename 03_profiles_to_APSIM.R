# Insert the mapped soil profiles into the common maize-soybean rotation template.
# Inputs/outputs use explicit R package objects, not a collection of prebuilt models.
library(apsimx)
library(aqp)
source('settings.R')
if(dir.exists(simulation_directory)) stop('Simulation folder exists; do not overwrite model inputs.')
dir.create(simulation_directory,recursive=TRUE)
initial_N <- read.csv('data/initial_mineral_N.csv')
initial_water <- read.csv('data/initial_water.csv')
weather <- normalizePath('data/weather.met',winslash='/',mustWork=TRUE)
manifest <- list()

# Conservative overlap allocation retains the original template's depth distribution.
# Stocks are kg N/ha; water is a volumetric concentration (mm/mm).
stock_in_layer <- function(top,bottom,table,column) {
  overlap <- pmax(0,pmin(bottom,table$bottom_mm)-pmax(top,table$top_mm))
  sum(table[[column]]*overlap/(table$bottom_mm-table$top_mm))
}
water_in_layer <- function(top,bottom) {
  overlap <- pmax(0,pmin(bottom,initial_water$bottom_mm)-pmax(top,initial_water$top_mm))
  sum(initial_water$SW_mm_mm*overlap)/(bottom-top)
}

for(product in c('DSM','SSURGO')) {
  spc <- readRDS(file.path('results',product,'profiles.rds'))
  horizons <- aqp::horizons(spc)
  folder <- file.path(simulation_directory,product)
  dir.create(folder)
  for(id in aqp::profile_id(spc)) {
    h <- horizons[horizons$profile_id==id,]
    h <- h[order(h$top),]
    thickness <- (h$bottom-h$top)*10 # aqp cm -> APSIM mm
    top <- h$top*10;bottom <- h$bottom*10
    no3 <- mapply(stock_in_layer,top,bottom,MoreArgs=list(table=initial_N,column='NO3_kg_ha'))
    nh4 <- mapply(stock_in_layer,top,bottom,MoreArgs=list(table=initial_N,column='NH4_kg_ha'))
    water <- mapply(water_in_layer,top,bottom)
    stopifnot(sum(thickness)==2000,abs(sum(no3+nh4)-63.9519166666667)<1e-8,
      abs(sum(water*thickness)-609)<1e-8)
    layer_index <- seq_len(nrow(h))-1
    # Original crop lower limits remain unchanged by the common-AirDry revision.
    crop_LL <- if(product=='DSM') {
      pmax(.15*exp(-.2*layer_index),h$LL15*ifelse(h$top==0,.5,1))
    } else h$crop_LL
    water_parameters <- apsimx::initialwater_parms(Thickness=thickness,InitialValues=water)
    drainage_parameters <- apsimx::soilwat_parms(Thickness=thickness,SWCON=rep(.3,nrow(h)))
    profile <- apsimx::apsimx_soil_profile(nlayers=nrow(h),Thickness=thickness,
      BD=h$BD,AirDry=h$AirDry,LL15=h$LL15,DUL=h$DUL,SAT=h$SAT,KS=h$KS,
      Carbon=h$Carbon,PH=h$PH,SoilCNRatio=rep(12,nrow(h)),
      ParticleSizeSand=h$sand,ParticleSizeSilt=h$silt,ParticleSizeClay=h$clay,
      crop.LL=crop_LL,crop.KL=.06*exp(-.2*layer_index),crop.XF=rep(1,nrow(h)),
      FOM=150*exp(-.2*layer_index),FBiom=.04*exp(-.2*layer_index),FInert=.8*exp(.01*layer_index),
      # Legacy constructor argument names; the modern template uses kg/ha units=1.
      NO3N=no3,NH4N=nh4,soil.bottom=200,
      initialwater=water_parameters,soilwat=drainage_parameters)
    file <- paste0(id,'.apsimx')
    stopifnot(file.copy('data/rotation_template.apsimx',file.path(folder,file),overwrite=FALSE))
    apsimx::edit_apsimx_replace_soil_profile(file=file,src.dir=folder,wrt.dir=folder,
      soil.profile=profile,overwrite=TRUE,verbose=FALSE)
    apsimx::edit_apsimx(file=file,src.dir=folder,node='Weather',parm='FileName',
      value=weather,overwrite=TRUE,verbose=FALSE)
    manifest[[length(manifest)+1]] <- data.frame(product=product,profile_id=id,
      file=file,input=file.path(folder,file),md5=unname(tools::md5sum(file.path(folder,file))))
    if(length(manifest)%%100==0)cat('Prepared',length(manifest),'APSIM profiles\n')
  }
}
manifest <- do.call(rbind,manifest)
stopifnot(nrow(manifest)==2036,sum(manifest$product=='DSM')==2030,sum(manifest$product=='SSURGO')==6)
write.csv(manifest,file.path(simulation_directory,'manifest.csv'),row.names=FALSE)
writeLines(capture.output(sessionInfo()),file.path(simulation_directory,'sessionInfo.txt'))
cat('All 2,036 mapped soil profiles are ready for APSIM.\n')
