here<-dirname(normalizePath(sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE)),winslash='/'));source(file.path(here,'common.R'))
a<-args_named();src<-normal(need(a,'source-manifest'));out<-normal(need(a,'out'),FALSE);stopifnot(!dir.exists(out));dir.create(out,recursive=TRUE)
m<-read.csv(src);m<-m[m$source=='DSM'|(m$source=='SSURGO'&grepl('_[0-9]+_d$',m$profile_id)),];stopifnot(nrow(m)==2036,sum(m$source=='DSM')==2030,sum(m$source=='SSURGO')==6)
if(identical(a[['test-pair']],'yes')){stopifnot(is.null(a[['limit']]));m<-m[c(which(m$source=='DSM')[1],which(m$source=='SSURGO')[1]),]}else if(!is.null(a[['limit']]))m<-m[seq_len(as.integer(a[['limit']])),]
dir.create(file.path(out,'weather'));w<-node(fromJSON(m$input_file[1],simplifyVector=FALSE),'Models.Climate.Weather')$FileName;w<-normal(w)
weather<-file.path(out,'weather/forcing.met');stopifnot(file.copy(w,weather,overwrite=FALSE),md5(w)==md5(weather))
manifest<-layers<-list()
for(i in seq_len(nrow(m))){
 stopifnot(md5(m$input_file[i])==m$input_md5[i]);old<-fromJSON(m$input_file[i],simplifyVector=FALSE);p<-node(old,'Models.Soils.Physical');th<-vec(p$Thickness);ad<-airdry(p)
 stopifnot(sum(th)==2000,all(ad<=vec(p$LL15)),all(ad<=vec(p$AirDry)+1e-10),all(vec(node(old,'Models.Soils.Organic')$SoilCNRatio)==12))
 for(crop in Filter(function(z)type_is(z,'Models.Soils.SoilCrop'),p$Children))stopifnot(all(vec(crop$LL)>=ad-.0005))
 stopifnot(md5(node(old,'Models.Climate.Weather')$FileName)==md5(w))
 x<-map_nodes(old,function(z){if(type_is(z,'Models.Soils.Physical'))z$AirDry<-as.list(ad);if(type_is(z,'Models.Climate.Weather'))z$FileName<-weather
  if(type_is(z,'Models.Report')&&z$Name%in%c('WarmupDaily','WarmupStart'))z$VariableNames<-c(z$VariableNames,as.list(c('[SoilWater].Es as AD_Es','[SoilWater].SWmm as AD_SWmm','[SoilWater].Drainage as AD_Drainage','[SoilWater].Runoff as AD_Runoff')));z})
 stopifnot(identical(mask_allowed(old),mask_allowed(x)))
 sw<-vec(node(x,'Models.Soils.Water')$InitialValues);eq(sum(sw*th),609)
 sol<-Filter(function(z)type_is(z,'Models.Soils.Solute')&&z$Name%in%c('NO3','NH4'),walk(x));stopifnot(length(sol)==2);eq(sum(unlist(lapply(sol,function(z){stopifnot(z$InitialValuesUnits==1);vec(z$InitialValues)}))),63.9519166666667)
 rel<-paste0('inputs/',m$source[i],'/',m$profile_id[i],'/simulation.apsimx');f<-file.path(out,rel);dir.create(dirname(f),recursive=TRUE);writej(x,f)
 manifest[[i]]<-data.frame(run_id=paste(m$source[i],m$profile_id[i],sep='_'),source=m$source[i],profile_id=m$profile_id[i],original_input=m$input_file[i],original_md5=m$input_md5[i],relative_input=rel,input_file=f,input_md5=md5(f),expected_db=sub('[.]apsimx$','.db',f))
 layers[[i]]<-data.frame(run_id=manifest[[i]]$run_id,layer=seq_along(th),thickness_mm=th,AirDry=ad,LL15=vec(p$LL15),SW=sw)
 if(i%%200==0){cat('Prepared',i,'/',nrow(m),'\n');flush.console()}
}
write.csv(do.call(rbind,manifest),file.path(out,'manifest.csv'),row.names=FALSE);write.csv(do.call(rbind,layers),file.path(out,'expected_layers.csv'),row.names=FALSE)
writej(list(status='PREPARED',profiles=nrow(m),DSM=sum(m$source=='DSM'),SSURGO=sum(m$source=='SSURGO'),weather=weather,weather_md5=md5(weather),source_manifest=src,source_manifest_md5=md5(src),rule='0-15cm:0.5LL15;15-30cm:0.8LL15;deeper:LL15;overlap-weighted;cropLLunchanged',initial_N_kg_ha=63.9519166666667,initial_water_mm=609,CN=12,scope=if(nrow(m)==2036)'FULLFIELD'else'SMOKE_TEST'),file.path(out,'preparation.json'))
capture.output(sessionInfo(),file=file.path(out,'sessionInfo_prepare.txt'));cat('Preparation complete\n')
