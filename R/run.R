here<-dirname(normalizePath(sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE)),winslash='/'));source(file.path(here,'common.R'));library(DBI);library(RSQLite)
a<-args_named();root<-normal(need(a,'experiment'));engine<-normal(need(a,'engine'));repo<-dirname(here)
m<-read.csv(file.path(root,'manifest.csv'));prep<-fromJSON(file.path(root,'preparation.json'));stopifnot(nrow(m)==prep$profiles)
lock<-file.path(root,'RUNNING.lock');if(!dir.create(lock,showWarnings=FALSE))stop('Run lock exists; inspect live processes before any recovery')
writej(list(pid=Sys.getpid(),started=as.character(Sys.time()),script=normal(file.path(here,'run.R'))),file.path(lock,'owner.json'))
progress<-function(z){z$updated<-as.character(Sys.time());tmp<-file.path(root,'progress.pending.json');writej(z,tmp);dest<-file.path(root,'progress.json');if(file.exists(dest)){backup<-file.path(root,'progress.previous.json');ps(paste0("[IO.File]::Replace('",normal(tmp),"','",normal(dest),"','",normal(backup,FALSE),"')"))}else stopifnot(file.rename(tmp,dest))}
finish_lock<-function(){# Only remove our small owned lock, never an experiment directory.
 p<-file.path(lock,'owner.json');if(file.exists(p)&&fromJSON(p)$pid==Sys.getpid()){
  stopifnot(identical(normal(lock),paste0(normal(root),'/RUNNING.lock')),identical(list.files(lock,all.files=TRUE,no..=TRUE),'owner.json'))
  stopifnot(file.remove(p));stopifnot(length(list.files(lock,all.files=TRUE,no..=TRUE))==0);unlink(lock,recursive=TRUE)
 }}
dates<-seq(as.Date('2013-01-01'),as.Date('2021-12-31'),by='day')
validate<-function(i){
 con<-dbConnect(SQLite(),m$expected_db[i],flags=SQLITE_RO);on.exit(dbDisconnect(con))
 z<-normcols(dbReadTable(con,'WarmupDaily'));s<-normcols(dbReadTable(con,'WarmupStart'));msg<-dbReadTable(con,'_Messages');ic<-dbReadTable(con,'_InitialConditions')
 stopifnot(nrow(z)==3287,nrow(s)==3287,identical(as.Date(substr(z$Date,1,10)),dates),identical(as.Date(substr(s$Date,1,10)),dates),any(grepl('Simulation terminated normally',msg$Message,fixed=TRUE)))
 stopifnot(all(is.finite(z$MaizeGrain)),all(z$MaizeGrain>=0),all(is.finite(z$AD_Es)),all(z$AD_Es>=0))
 x<-fromJSON(m$input_file[i],simplifyVector=FALSE);p<-node(x,'Models.Soils.Physical');th<-vec(p$Thickness)
 iv<-function(n,path){v<-ic$Value[ic$Name==n&grepl(path,ic$ModelPath)];stopifnot(length(v)==1);as.numeric(strsplit(v,',',fixed=TRUE)[[1]])}
 eq(iv('AirDry','[.]Physical$'),airdry(p));eq(iv('SoilCNRatio','[.]Organic$'),rep(12,length(th)));eq(iv('InitialValues','[.]Water$'),vec(node(x,'Models.Soils.Water')$InitialValues));eq(sum(iv('InitialValues','[.]Water$')*th),609)
 for(sp in c('NO3','NH4')){q<-Filter(function(z)type_is(z,'Models.Soils.Solute')&&z$Name==sp,walk(x))[[1]];eq(as.numeric(s[1,cols(s,sp)]),vec(q$InitialValues))}
 eq(sum(as.numeric(s[1,c(cols(s,'NO3'),cols(s,'NH4'))])),63.9519166666667)
 jan<-which(format(dates,'%m-%d')=='01-01')[-1]
 for(n in c('TotalC','OrganicN','MineralN','AD_SWmm'))eq(as.numeric(as.matrix(s[jan,cols(s,n)])),as.numeric(as.matrix(z[jan-1,cols(z,n)])))
 sow<-msg[grepl('A crop of (Maize|Soybean)',msg$Message),];stopifnot(nrow(sow)==9,identical(sort(as.integer(substr(sow$Date[grepl('A crop of Maize',sow$Message)],1,4))),seq(2013L,2021L,2L)))
 list(status='PASS',normal_termination=TRUE,days=3287,initialization='PASS',continuous_rotation='PASS',warnings=sum(msg$MessageType==1))
}
tryCatch({
 stopifnot(as.integer(tail(ps('@(Get-Process Models -ErrorAction SilentlyContinue).Count'),1))==0)
 stopifnot(trimws(tail(ps(paste0("(Get-Item -LiteralPath '",engine,"').VersionInfo.FileVersion")),1))=='2025.3.7681.0')
 bind<-c(file.path(here,c('common.R','run.R')),file.path(repo,'tools',c('run_capped.ps1','CappedProcess.cs')),file.path(root,c('manifest.csv','preparation.json')),engine,prep$weather)
 hashes<-unname(tools::md5sum(bind));stopifnot(!anyNA(hashes),md5(prep$weather)==prep$weather_md5)
 context<-list(engine_version='2025.3.7681.0',paths=bind,md5=hashes,model_job_commit_limit_MB=1024,minimum_free_before_launch_MB=1536,emergency_free_floor_MB=384,workers=1)
 contextfile<-file.path(root,'run_context.json');if(file.exists(contextfile))stopifnot(identical(fromJSON(contextfile,simplifyVector=FALSE),fromJSON(toJSON(context,auto_unbox=TRUE,digits=NA),simplifyVector=FALSE)))else writej(context,contextfile)
 completed<-0;durations<-numeric();memory<-numeric();began<-Sys.time()
 for(i in seq_len(nrow(m))){
  stopifnot(identical(unname(tools::md5sum(bind)),hashes),md5(m$original_input[i])==m$original_md5[i],md5(m$input_file[i])==m$input_md5[i])
  verified<-paste0(m$input_file[i],'.verified.json');receipt<-paste0(m$input_file[i],'.resource.json')
  if(file.exists(verified)){r<-fromJSON(verified);stopifnot(r$status=='PASS',r$input_md5==m$input_md5[i],r$engine_md5==md5(engine),md5(m$expected_db[i])==r$database_md5);completed<-completed+1;next}
  if(file.exists(m$expected_db[i])||file.exists(receipt)||file.exists(paste0(m$input_file[i],'.engine.log')))stop('Partial output exists; preserved for explicit review: ',m$run_id[i])
  repeat{
   free<-as.numeric(tail(ps('(Get-CimInstance Win32_OperatingSystem).FreePhysicalMemory'),1))/1024;disk<-as.numeric(tail(ps('(Get-PSDrive E).Free'),1))/1024^3
   if(free>=1536&&disk>=20)break
   progress(list(status='WAITING_RESOURCES',completed=completed,total=nrow(m),free_RAM_MB=free,free_disk_GB=disk,next_run=m$run_id[i]));Sys.sleep(30)
  }
  stopifnot(as.integer(tail(ps('@(Get-Process Models -ErrorAction SilentlyContinue).Count'),1))==0)
  progress(list(status='RUNNING',completed=completed,total=nrow(m),current=m$run_id[i],model_job_commit_limit_MB=1024,free_RAM_MB=free,mean_seconds=if(length(durations))mean(durations)else NA))
  start<-Sys.time();code<-system2('powershell.exe',c('-NoProfile','-NonInteractive','-ExecutionPolicy','Bypass','-File',shQuote(file.path(repo,'tools/run_capped.ps1')),'-Engine',shQuote(engine),'-InputFile',shQuote(m$input_file[i]),'-Receipt',shQuote(receipt),'-MemoryLimitMB','1024','-EmergencyFreeMB','384','-TimeoutSeconds','180'),stdout=paste0(m$input_file[i],'.wrapper.stdout.txt'),stderr=paste0(m$input_file[i],'.wrapper.stderr.txt'))
  stopifnot(file.exists(receipt));rec<-fromJSON(receipt);stopifnot(code==0,rec$ExitCode==0,isTRUE(rec$JobLimitEnforced),rec$MemoryLimitMB==1024,md5(m$input_file[i])==m$input_md5[i])
  checks<-validate(i);writej(c(checks,list(input_md5=m$input_md5[i],original_md5=m$original_md5[i],database_md5=md5(m$expected_db[i]),engine_md5=md5(engine),resource_receipt_md5=md5(receipt))),verified)
  completed<-completed+1;durations<-c(durations,as.numeric(difftime(Sys.time(),start,units='secs')));memory<-c(memory,rec$PeakJobCommitMB)
  progress(list(status='RUNNING',completed=completed,total=nrow(m),last=m$run_id[i],mean_seconds=mean(durations),estimated_remaining_hours=(nrow(m)-completed)*mean(durations)/3600,peak_model_job_commit_MB=max(memory),minimum_system_free_MB_last=rec$MinimumSystemFreeMB))
  cat(completed,'/',nrow(m),m$run_id[i],'peak',round(rec$PeakJobCommitMB),'MB\n');flush.console();gc(FALSE)
 }
 progress(list(status='ALL_MODELS_COMPLETE_VALIDATED_EXTRACTION_PENDING',completed=completed,total=nrow(m),elapsed_hours=as.numeric(difftime(Sys.time(),began,units='hours')),peak_model_job_commit_MB=if(length(memory))max(memory)else NA));finish_lock()
},error=function(e){progress(list(status='STOPPED_REVIEW_REQUIRED',error=conditionMessage(e),completed=if(exists('completed'))completed else 0,total=nrow(m),outputs_preserved=TRUE));finish_lock();stop(e)})
