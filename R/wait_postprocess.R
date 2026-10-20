# Dispatch analysis only after the existing simulation coordinator completes.
here<-dirname(normalizePath(sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE)),winslash='/'));source(file.path(here,'common.R'))
a<-args_named();experiment<-normal(need(a,'experiment'));index<-normal(need(a,'cell-index'));polygons<-normal(need(a,'polygons'));climate<-normal(need(a,'climate-source'));out<-normal(need(a,'out'),FALSE);dispatch<-normal(need(a,'dispatch'))
stopifnot(!dir.exists(out),dir.create(file.path(dispatch,'WATCHER.lock'),showWarnings=FALSE))
writej(list(pid=Sys.getpid(),started=as.character(Sys.time())),file.path(dispatch,'WATCHER.lock/owner.json'))
status<-function(s,...)writej(c(list(status=s,pid=Sys.getpid(),updated=as.character(Sys.time())),list(...)),file.path(dispatch,'status.json'))
tryCatch({
 repeat{
  p<-tryCatch(fromJSON(file.path(experiment,'progress.json')),error=function(e)NULL)
  if(!is.null(p)){
   if(p$status=='STOPPED_REVIEW_REQUIRED')stop('Simulation coordinator stopped; review required before analysis')
   if(p$status=='ALL_MODELS_COMPLETE_VALIDATED_EXTRACTION_PENDING'){
    stopifnot(p$completed==2036,p$total==2036)
    if(as.integer(tail(ps('@(Get-Process Models -ErrorAction SilentlyContinue).Count'),1))==0)break
   }
   status('WAITING_FOR_EXISTING_SIMULATIONS',completed=p$completed,total=p$total)
  }
  Sys.sleep(30)
 }
 hashes<-read.csv(file.path(dispatch,'runtime_hashes.csv'));stopifnot(all(unname(tools::md5sum(file.path(dispatch,hashes$relative_path)))==hashes$md5),!dir.exists(out))
 status('POSTPROCESS_STARTING',analysis_output=out)
 code<-system2(file.path(R.home('bin'),'Rscript.exe'),c(shQuote(file.path(here,'postprocess.R')),'--experiment',shQuote(experiment),'--cell-index',shQuote(index),'--polygons',shQuote(polygons),'--climate-source',shQuote(climate),'--out',shQuote(out)),stdout=file.path(dispatch,'postprocess.stdout.txt'),stderr=file.path(dispatch,'postprocess.stderr.txt'))
 if(code!=0)stop('Postprocess exited ',code,'; outputs preserved for review')
 stopifnot(fromJSON(file.path(out,'status.json'))$status=='ANALYSIS_RENDERED_NUMERIC_AND_VISUAL_QA_PENDING')
 status('ANALYSIS_RENDERED_REVIEW_AND_V14_PENDING',analysis_output=out)
},error=function(e){status('STOPPED_REVIEW_REQUIRED',error=conditionMessage(e),outputs_preserved=TRUE);stop(e)})
