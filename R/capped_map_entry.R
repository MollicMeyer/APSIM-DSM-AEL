# Entry point for tools/render_maps_capped.ps1; no scientific calculations here.
script<-Sys.getenv('APSIM_RENDER_SCRIPT');stopifnot(file.exists(script))
arguments<-c('--paired',Sys.getenv('APSIM_RENDER_PAIRED'),'--polygons',Sys.getenv('APSIM_RENDER_POLYGONS'),'--out',Sys.getenv('APSIM_RENDER_OUTPUT'),'--resource-job','yes')
stopifnot(all(nzchar(arguments)))
commandArgs<-function(trailingOnly=FALSE)if(trailingOnly)arguments else c(paste0('--file=',script),arguments)
source(script,encoding='UTF-8')
