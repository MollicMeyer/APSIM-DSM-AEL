# Sequential production analysis after the complete model batch; no model launches.
here<-dirname(normalizePath(sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE)),winslash='/'));source(file.path(here,'common.R'))
a<-args_named();experiment<-normal(need(a,'experiment'));index<-normal(need(a,'cell-index'));polygons<-normal(need(a,'polygons'));climate<-normal(need(a,'climate-source'));out<-normal(need(a,'out'),FALSE)
p<-fromJSON(file.path(experiment,'progress.json'));stopifnot(p$status=='ALL_MODELS_COMPLETE_VALIDATED_EXTRACTION_PENDING',p$completed==2036,p$total==2036,!dir.exists(out))
stopifnot(as.integer(tail(ps('@(Get-Process Models -ErrorAction SilentlyContinue).Count'),1))==0)
dir.create(out,recursive=TRUE);runtime<-file.path(out,'runtime/R');dir.create(runtime,recursive=TRUE)
files<-list.files(here,pattern='[.]R$',recursive=TRUE,full.names=TRUE)
for(f in files){rel<-substring(f,nchar(here)+2);dest<-file.path(runtime,rel);dir.create(dirname(dest),recursive=TRUE,showWarnings=FALSE);stopifnot(file.copy(f,dest,overwrite=FALSE),md5(f)==md5(dest))}
frozen<-list.files(runtime,pattern='[.]R$',recursive=TRUE,full.names=TRUE);hashes<-unname(tools::md5sum(frozen));write.csv(data.frame(relative_path=substring(frozen,nchar(out)+2),md5=hashes),file.path(out,'runtime_hashes.csv'),row.names=FALSE)
ctx<-c(file.path(experiment,c('manifest.csv','preparation.json')),index,polygons,file.path(climate,'status.json'));ctxhash<-unname(tools::md5sum(ctx))
writej(list(paths=ctx,md5=ctxhash),file.path(out,'context.json'))
status<-function(s,...)writej(c(list(status=s,updated=as.character(Sys.time()),pid=Sys.getpid()),list(...)),file.path(out,'status.json'))
steps<-list(
 list('extract','extract.R',c('--experiment',experiment,'--out',file.path(out,'results'))),
 list('verify','verify_extraction.R',c('--experiment',experiment,'--results',file.path(out,'results'),'--out',file.path(out,'independent_verification'))),
 list('soil','extract_soil_inputs.R',c('--experiment',experiment,'--out',file.path(out,'soil_inputs'))),
 list('paired','summarize_paired.R',c('--results',file.path(out,'results'),'--cell-index',index,'--out',file.path(out,'paired'))),
 list('associations','associate_soils.R',c('--results',file.path(out,'results'),'--soil-inputs',file.path(out,'soil_inputs'),'--out',file.path(out,'associations'))),
 list('OC_differences','associate_oc_differences.R',c('--paired',file.path(out,'paired'),'--soil-inputs',file.path(out,'soil_inputs'),'--out',file.path(out,'OC_differences'))),
 list('variograms','fit_variograms.R',c('--paired',file.path(out,'paired'),'--out',file.path(out,'variograms'))),
 list('climate','reproduce_climate.R',c('--source-data',climate,'--weather',file.path(experiment,'weather/forcing.met'),'--out',file.path(out,'climate'))),
 list('maps','plot_maps.R',c('--paired',file.path(out,'paired'),'--polygons',polygons,'--out',file.path(out,'maps'))),
 list('summary_figures','plot_summary_figures.R',c('--paired',file.path(out,'paired'),'--soil-inputs',file.path(out,'soil_inputs'),'--associations',file.path(out,'associations'),'--variograms',file.path(out,'variograms'),'--out',file.path(out,'summary_figures'))),
 list('climate_figure','plot_climate.R',c('--climate',file.path(out,'climate'),'--out',file.path(out,'climate_figure')))
)
completed<-character();current<-'initialization'
tryCatch({
 for(step in steps){
  current<-step[[1]];stopifnot(all(unname(tools::md5sum(frozen))==hashes),all(unname(tools::md5sum(ctx))==ctxhash))
  repeat{free<-as.numeric(tail(ps('(Get-CimInstance Win32_OperatingSystem).FreePhysicalMemory'),1))/1024;if(free>=1536)break;status('WAITING_RESOURCES',next_stage=current,completed=completed,free_RAM_MB=free);Sys.sleep(30)}
  stopifnot(as.integer(tail(ps('@(Get-Process Models -ErrorAction SilentlyContinue).Count'),1))==0)
  status('ANALYSIS_RUNNING',stage=current,completed=completed)
  code<-system2(file.path(R.home('bin'),'Rscript.exe'),c(shQuote(file.path(runtime,step[[2]])),vapply(step[[3]],shQuote,character(1))),stdout=file.path(out,paste0(current,'.stdout.txt')),stderr=file.path(out,paste0(current,'.stderr.txt')))
  if(code!=0)stop('Stage ',current,' exited ',code,'; outputs preserved')
  if(current=='verify'){v<-fromJSON(file.path(out,'independent_verification/status.json'));stopifnot(v$status=='INDEPENDENT_VERIFICATION_PASS',v$profiles==2036)}
  completed<-c(completed,current)
 }
 status('ANALYSIS_RENDERED_NUMERIC_AND_VISUAL_QA_PENDING',completed=completed,manuscript_released=FALSE)
},error=function(e){status('STOPPED_REVIEW_REQUIRED',stage=current,completed=completed,error=conditionMessage(e),outputs_preserved=TRUE);stop(e)})
