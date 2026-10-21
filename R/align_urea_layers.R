# apsimx 2.8.0 replaces NO3 and NH4 layers but has no Urea constructor argument.
# Align the zero-Urea pool with the ten SSURGO layers before first execution.
align_urea_layers <- function(manifest) {
  for(i in which(manifest$product=='SSURGO')) {
    path <- manifest$input[i]
    if(file.exists(sub('[.]apsimx$','.db',path))) next
    for(parameter in c('Thickness','InitialValues')) {
      apsimx::edit_apsimx(file=basename(path),src.dir=dirname(path),node='Other',
        parm.path='.Simulations.Simulation.Field.Soil.Urea',parm=parameter,
        value=as.list(if(parameter=='Thickness')rep(200,10)else rep(0,10)),
        overwrite=TRUE,verbose=FALSE)
    }
  }
  manifest$md5 <- unname(tools::md5sum(manifest$input))
  manifest
}
