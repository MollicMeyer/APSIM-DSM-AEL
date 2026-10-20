# Versioned R-only extraction; immutable checkpoints and explicit recovery.
here<-dirname(normalizePath(sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE)),winslash='/'));source(file.path(here,'common.R'));library(DBI);library(RSQLite)
stopifnot(requireNamespace('digest',quietly=TRUE))
a<-args_named();root<-normal(need(a,'experiment'))
test_mode<-identical(a[['test-mode']],'yes');resume<-identical(a[['resume']],'yes')
test_stop<-if(is.null(a[['test-stop-after']]))Inf else as.integer(a[['test-stop-after']])
stopifnot(is.finite(test_stop)||is.infinite(test_stop),test_mode||is.infinite(test_stop))
out<-normal(need(a,'out'),FALSE)
root_abs<-normal(root)
stopifnot(out!=root_abs)
manifest_path<-normal(file.path(root,'manifest.csv'))
m<-read.csv(manifest_path);prep_path<-file.path(root,'preparation.json');prep<-fromJSON(prep_path)
progress<-fromJSON(file.path(root,'progress.json'))
stopifnot(progress$status=='ALL_MODELS_COMPLETE_VALIDATED_EXTRACTION_PENDING',progress$completed==nrow(m),prep$profiles==nrow(m))
if(test_mode)stopifnot(nrow(m)>=1,nrow(m)<=6)else stopifnot(nrow(m)==2036,sum(m$source=='DSM')==2030,sum(m$source=='SSURGO')==6)
stopifnot(all(grepl('_[0-9]+_d$',m$profile_id[m$source=='SSURGO'])))
stopifnot(!anyDuplicated(m$run_id),!anyDuplicated(m$profile_id),
 all(unname(tools::md5sum(m$input_file))==m$input_md5),
 all(unname(tools::md5sum(m$original_input))==m$original_md5),
 md5(prep$weather)==prep$weather_md5)
self<-normal(sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE)))
stopifnot(length(self)==1)
context<-list(mode=if(test_mode)'TEST_ONLY'else'PRODUCTION',profiles=nrow(m),
 manifest_md5=md5(manifest_path),preparation_md5=md5(prep_path),
 weather_md5=prep$weather_md5,extractor_md5=md5(self),
 common_md5=md5(file.path(here,'common.R')))
context_key<-as.character(toJSON(context,auto_unbox=TRUE,digits=NA))
atomic<-function(path,writer){
 temp<-tempfile(pattern=paste0(basename(path),'.partial_'),tmpdir=dirname(path))
 writer(temp);stopifnot(file.exists(temp))
 if(file.exists(path)){
  if(md5(temp)!=md5(path))stop('Existing output differs; preserved old and candidate: ',path,' / ',temp)
  stopifnot(file.remove(temp))
 }else stopifnot(file.rename(temp,path))
 invisible(path)
}
context_path<-file.path(out,'context.json')
if(dir.exists(out)){
 stopifnot(resume,file.exists(context_path))
 existing<-fromJSON(context_path,simplifyVector=FALSE)
 stopifnot(identical(existing$key,context_key))
}else{
 stopifnot(!resume);dir.create(out,recursive=TRUE)
 atomic(context_path,function(p)writej(list(key=context_key,context=context),p))
}
dir.create(file.path(out,'checkpoints'),showWarnings=FALSE)
dir.create(file.path(out,'year_profiles'),showWarnings=FALSE)
expected_years<-2013:2021
payload_hash<-function(cp)digest::digest(cp[c('annual','stress','warnings','properties','checks')],algo='sha256',serialize=TRUE)
validate_checkpoint<-function(cp,i,key){
 stopifnot(is.list(cp),identical(cp$key,key),identical(cp$payload_sha256,payload_hash(cp)),nrow(cp$annual)==9,nrow(cp$stress)==5,
 nrow(cp$properties)==1,nrow(cp$checks)==1,
 identical(cp$annual$year,expected_years),
 identical(cp$stress$year,seq(2013,2021,2)),
 all(cp$annual$run_id==m$run_id[i]),all(cp$stress$run_id==m$run_id[i]),
 cp$checks$input_md5==m$input_md5[i])
 invisible(TRUE)
}
cols<-function(z,n){v<-grep(paste0('^',n,'[.][0-9]+[.]$'),names(z),value=TRUE);stopifnot(length(v)>0);v[order(as.integer(sub(paste0('^',n,'[.]([0-9]+)[.]$'),'\\1',v)))]}
sums<-function(z,n)rowSums(as.matrix(z[,cols(z,n),drop=FALSE]))
eq<-function(x,y,tol=1e-7)stopifnot(length(x)==length(y),all(is.finite(x)),all(is.finite(y)),max(abs(x-y))<tol)
dates<-seq(as.Date('2013-01-01'),as.Date('2021-12-31'),by='day');years<-as.integer(format(dates,'%Y'))
metlines<-readLines(prep$weather);h<-grep('^year day ',metlines);stopifnot(length(h)==1)
met<-read.table(prep$weather,skip=h+1,col.names=c('year','day','Radn','MaxT','MinT','Rain'))
met$Date<-as.Date(paste0(met$year,'-01-01'))+met$day-1;stopifnot(identical(met$Date,dates))
f32<-function(x)readBin(writeBin(as.double(x),raw(),size=4),numeric(),n=length(x),size=4)
annual<-stress<-warnings<-checks<-properties<-list()
for(i in seq_len(nrow(m))){
 recordfile<-paste0(m$input_file[i],'.verified.json')
 rec<-fromJSON(recordfile)
 stopifnot(rec$status=='PASS',rec$input_md5==m$input_md5[i],rec$original_md5==m$original_md5[i],
  md5(m$expected_db[i])==rec$database_md5)
 resource<-paste0(m$input_file[i],'.resource.json')
 stopifnot(md5(resource)==rec$resource_receipt_md5)
 cap<-fromJSON(resource);stopifnot(cap$ExitCode==0,isTRUE(cap$JobLimitEnforced),cap$MemoryLimitMB==1024)
 checkpoint<-file.path(out,'checkpoints',paste0(m$run_id[i],'.rds'))
 key<-paste(context_key,m$run_id[i],rec$database_md5,md5(recordfile),sep='|')
 if(file.exists(checkpoint)){
  cp<-tryCatch(readRDS(checkpoint),error=function(e)stop('Unreadable checkpoint preserved: ',checkpoint,'; ',conditionMessage(e)))
  validate_checkpoint(cp,i,key)
  annual[[i]]<-cp$annual;stress[[i]]<-cp$stress;warnings[[i]]<-cp$warnings
  properties[[i]]<-cp$properties;checks[[i]]<-cp$checks
  next
 }
 con<-dbConnect(SQLite(),m$expected_db[i],flags=SQLITE_RO)
 z<-normcols(dbReadTable(con,'WarmupDaily'));s<-normcols(dbReadTable(con,'WarmupStart'));r<-normcols(dbReadTable(con,'Report'));msg<-dbReadTable(con,'_Messages');ic<-dbReadTable(con,'_InitialConditions');dbDisconnect(con)
 z<-z[order(z$Date),];s<-s[order(s$Date),]
 stopifnot(nrow(z)==3287,nrow(s)==3287,identical(as.Date(substr(z$Date,1,10)),dates),identical(as.Date(substr(s$Date,1,10)),dates))
 stopifnot(any(grepl('Simulation terminated normally',msg$Message,fixed=TRUE)),
  all(is.finite(z$MaizeGrain)),all(z$MaizeGrain>=0),all(is.finite(z$SoybeanGrain)),all(z$SoybeanGrain>=0),
  all(is.finite(z$MaizeLAI)),all(z$MaizeLAI>=0),all(z$MaizeAlive%in%c('True','False')))
 sow<-msg[grepl('A crop of (Maize|Soybean)',msg$Message),,drop=FALSE]
 stopifnot(nrow(sow)==9,identical(sort(as.integer(substr(sow$Date[grepl('A crop of Maize',sow$Message)],1,4))),seq(2013L,2021L,2L)),
  identical(sort(as.integer(substr(sow$Date[grepl('A crop of Soybean',sow$Message)],1,4))),seq(2014L,2020L,2L)))
 x<-fromJSON(m$input_file[i],simplifyVector=FALSE);p<-node(x,'Models.Soils.Physical');o<-node(x,'Models.Soils.Organic');c<-node(x,'Models.Soils.Chemical');th<-vec(p$Thickness)
 iv<-function(n,path){v<-ic$Value[ic$Name==n & grepl(path,ic$ModelPath)];stopifnot(length(v)==1);as.numeric(strsplit(v,',',fixed=TRUE)[[1]])}
 eq(iv('InitialValues','[.]Water$'),vec(node(x,'Models.Soils.Water')$InitialValues))
 eq(iv('SoilCNRatio','[.]Organic$'),rep(12,length(th)))
 eq(iv('AirDry','[.]Physical$'),airdry(p));eq(sum(iv('InitialValues','[.]Water$')*th),609)
 sols<-Filter(function(v)type_is(v,'Models.Soils.Solute'),walk(x))
 for(species in c('NO3','NH4','Urea')){q<-Filter(function(v)v$Name==species,sols);stopifnot(length(q)==1,q[[1]]$InitialValuesUnits==1);eq(as.numeric(s[1,cols(s,species)]),vec(q[[1]]$InitialValues))}
 carbon<-vec(o$Carbon)*100*vec(p$BD)*th;inert<-carbon*vec(o$FInert)
 microbial<-(carbon-inert)*vec(o$FBiom)/(1+vec(o$FBiom));humic<-carbon-inert-microbial
 eq(as.numeric(s[1,cols(s,'OrganicN')]),(inert+humic)/12+microbial/8+.4*vec(o$FOM)/vec(o$FOMCNRatio))
 for(n in c('Rain','Radn','MaxT','MinT'))eq(z[[n]],f32(met[[n]]),1e-12)
 eq(sums(z,'TotalN'),sums(z,'OrganicN')+sums(z,'MineralN'))
 eq(sums(z,'OrganicN'),sums(z,'InertN')+sums(z,'MicrobialN')+sums(z,'HumicN')+sums(z,'FOMN'))
 jan<-which(format(dates,'%m-%d')=='01-01')[-1]
 for(n in c('TotalC','OrganicN','MineralN','AD_SWmm'))eq(as.numeric(as.matrix(s[jan,cols(s,n)])),as.numeric(as.matrix(z[jan-1,cols(z,n)])))
 eq(sum(as.numeric(s[1,c(cols(s,'NO3'),cols(s,'NH4'))])),63.9519166666667)
 role<-if(m$source[i]=='DSM')'DSM_grid_cell'else'SSURGO_dominant_component'
 id<-data.frame(run_id=m$run_id[i],source=m$source[i],profile_id=m$profile_id[i],role=role)
 annual[[i]]<-do.call(rbind,lapply(2013:2021,function(y){q<-which(years==y);j<-q[1];k<-tail(q,1)
  data.frame(id,year=y,crop=if(y%%2==1)'Maize'else'Soybean',
   primary_year=y%in%c(2017,2019,2021),C_Jan1_start_kg_ha=sums(s[j,,drop=FALSE],'TotalC'),C_Dec31_end_kg_ha=sums(z[k,,drop=FALSE],'TotalC'),
   OrganicN_Jan1_start_kg_ha=sums(s[j,,drop=FALSE],'OrganicN'),OrganicN_Dec31_end_kg_ha=sums(z[k,,drop=FALSE],'OrganicN'),
   MineralN_Jan1_start_kg_ha=sums(s[j,,drop=FALSE],'MineralN'),MineralN_Dec31_end_kg_ha=sums(z[k,,drop=FALSE],'MineralN'),
   annual_net_mineralised_N_kg_ha=sum(sums(z[q,,drop=FALSE],'NetMineralisedN')),
   Maize_peak_grain_Mg_ha=max(z$MaizeGrain[q])*.01,Soybean_peak_grain_Mg_ha=max(z$SoybeanGrain[q])*.01,
   annual_soil_evaporation_mm=sum(z$AD_Es[q]),annual_drainage_mm=sum(z$AD_Drainage[q]),annual_runoff_mm=sum(z$AD_Runoff[q]))}))
 stress[[i]]<-do.call(rbind,lapply(seq(2013,2021,2),function(y){rr<-r[substr(r$Clock.Today,1,4)==as.character(y),]
  first<-as.Date(substr(rr$Clock.Today[which(rr$Maize.Phenology.CurrentStageName=='Emergence')],1,10))
  last<-as.Date(substr(rr$Clock.Today[which(rr$Maize.Phenology.CurrentStageName=='HarvestRipe')],1,10))
  # Preserve missing/failed crop phases explicitly rather than inventing windows.
  if(length(first)!=1||length(last)!=1||last<first)return(data.frame(id,year=y,emergence=NA_character_,harvest_ripe=NA_character_,stage_window_status='MISSING_OR_AMBIGUOUS',active_green_days=0L,N_factor_min=NA_real_,N_factor_mean=NA_real_,N_factor_below1_days=NA_integer_,water_factor_min=NA_real_,water_factor_mean=NA_real_,water_factor_below1_days=NA_integer_,zero_canopy_zero_N_days=NA_integer_))
  q<-z[dates>=first & dates<=last,];active<-tolower(as.character(q$MaizeAlive))=='true' & q$MaizeLAI>.01;g<-q[active,]
  if(nrow(g)==0)return(data.frame(id,year=y,emergence=as.character(first),harvest_ripe=as.character(last),stage_window_status='NO_ACTIVE_GREEN_CANOPY',active_green_days=0L,N_factor_min=NA_real_,N_factor_mean=NA_real_,N_factor_below1_days=NA_integer_,water_factor_min=NA_real_,water_factor_mean=NA_real_,water_factor_below1_days=NA_integer_,zero_canopy_zero_N_days=sum(q$MaizeLAI==0 & q$MaizeNFactor==0)))
  stopifnot(all(is.finite(g$MaizeNFactor)),all(is.finite(g$MaizeWaterFactor)))
  data.frame(id,year=y,emergence=as.character(first),harvest_ripe=as.character(last),stage_window_status='PASS',active_green_days=nrow(g),
   N_factor_min=min(g$MaizeNFactor),N_factor_mean=mean(g$MaizeNFactor),N_factor_below1_days=sum(g$MaizeNFactor<1-1e-8),
   water_factor_min=min(g$MaizeWaterFactor),water_factor_mean=mean(g$MaizeWaterFactor),water_factor_below1_days=sum(g$MaizeWaterFactor<1-1e-8),
   zero_canopy_zero_N_days=sum(q$MaizeLAI==0 & q$MaizeNFactor==0))}))
 w<-msg[msg$MessageType==1,,drop=FALSE];w$run_id<-rep(m$run_id[i],nrow(w));warnings[[i]]<-w
 x<-fromJSON(m$input_file[i],simplifyVector=FALSE);p<-node(x,'Models.Soils.Physical');o<-node(x,'Models.Soils.Organic');c<-node(x,'Models.Soils.Chemical');th<-vec(p$Thickness)
 avg<-function(v)sum(vec(v)*th)/sum(th)
 properties[[i]]<-data.frame(id,BD=avg(p$BD),Clay=avg(p$ParticleSizeClay),Sand=avg(p$ParticleSizeSand),Silt=avg(p$ParticleSizeSilt),OC=avg(o$Carbon),pH=avg(c$PH))
 checks[[i]]<-data.frame(id,input_md5=m$input_md5[i],database_md5=rec$database_md5,engine_md5=rec$engine_md5,dates=3287,weather_exact=TRUE,pool_identity=TRUE,year_boundary_continuity=TRUE)
 # Small independent per-profile checkpoint supports recovery without any large daily dump.
 cp<-list(key=key,annual=annual[[i]],stress=stress[[i]],warnings=w,properties=properties[[i]],checks=checks[[i]])
 cp$payload_sha256<-payload_hash(cp)
 validate_checkpoint(cp,i,key)
 atomic(checkpoint,function(p)saveRDS(cp,p))
 if(test_mode && i>=test_stop)stop('TEST_ONLY intentional interruption after checkpoint ',i)
 rm(z,s,r,msg,x);if(i%%50==0){gc(FALSE);cat('Extracted',i,'of',nrow(m),'\n');flush.console()}
}
aa<-do.call(rbind,annual);st<-do.call(rbind,stress);ww<-do.call(rbind,warnings);pp<-do.call(rbind,properties)
stopifnot(nrow(aa)==nrow(m)*9,nrow(st)==nrow(m)*5,nrow(pp)==nrow(m),
 !anyDuplicated(paste(aa$run_id,aa$year)),!anyDuplicated(paste(st$run_id,st$year)),
 !anyDuplicated(pp$run_id))
csv<-function(z,path)atomic(path,function(p)write.csv(z,p,row.names=FALSE))
csv(aa,file.path(out,'annual_stocks_and_yields.csv'))
csv(st,file.path(out,'active_green_canopy_stress.csv'))
csv(ww,file.path(out,'warnings.csv'))
csv(pp,file.path(out,'depth_weighted_soil_properties.csv'))
csv(do.call(rbind,checks),file.path(out,'verification_and_provenance.csv'))
for(y in c(2017,2019,2021)){
 q<-aa[aa$year==y,];q$grain_Mg_ha<-q$Maize_peak_grain_Mg_ha
 q$grain_bu_ac_15_5<-q$grain_Mg_ha*1000*.40468564224/(56*.45359237)/(1-.155)
 q<-merge(q,pp,by=c('run_id','source','profile_id','role'),sort=FALSE)
 ss<-st[st$year==y,c('run_id','year','stage_window_status','active_green_days')]
 q<-merge(q,ss,by=c('run_id','year'),sort=FALSE)
 q$crop_year_QA<-ifelse(q$stage_window_status=='PASS','PASS','REVIEW_REQUIRED')
 stopifnot(nrow(q)==nrow(m),!anyDuplicated(q$run_id),!anyNA(q$crop_year_QA))
 csv(q,file.path(out,'year_profiles',paste0('profile_results_',y,'.csv')))
}
weather<-do.call(rbind,lapply(2013:2021,function(y){q<-met[met$year==y&format(met$Date,'%m-%d')>='05-01'&format(met$Date,'%m-%d')<='09-30',];data.frame(year=y,period='May1_Sep30',days=nrow(q),rain_mm=sum(q$Rain),radiation_MJ_m2=sum(q$Radn),mean_maxT_C=mean(q$MaxT),mean_minT_C=mean(q$MinT))}))
csv(weather,file.path(out,'common_weather_summary.csv'))
# Final status is written last; incomplete folders/checkpoints are never completion evidence.
artifacts<-list.files(out,pattern='[.]csv$',recursive=TRUE,full.names=TRUE)
artifact_hashes<-data.frame(path=substring(artifacts,nchar(out)+2),md5=unname(tools::md5sum(artifacts)))
status<-list(status=if(test_mode)'TEST_ONLY_EXTRACTION_COMPLETE'else'ALL2036_EXTRACTED_REVIEW_AND_SPATIAL_JOIN_PENDING',
 mode=if(test_mode)'TEST_ONLY'else'PRODUCTION',profiles=nrow(m),primary_years=c(2017,2019,2021),
 roles=as.list(table(pp$role)),missing_or_ambiguous_maize_windows=sum(st$stage_window_status!='PASS'),
 warning_count=nrow(ww),context_key=context_key,artifact_hashes=artifact_hashes,
 scope='Year profiles retain crop-year validity; downstream requires this final marker and all expected rows. Dominant SSURGO only. Profile-wide properties retained for compatibility, not approved association depths. No equilibrium or observed accuracy claim.')
if(!file.exists(file.path(out,'sessionInfo.txt')))atomic(file.path(out,'sessionInfo.txt'),function(p)capture.output(sessionInfo(),file=p))
atomic(file.path(out,'status.json'),function(p)writej(status,p))
cat(if(test_mode)'TEST_ONLY extraction complete'else'All2036 extraction complete','; downstream numerical review required.\\n')
