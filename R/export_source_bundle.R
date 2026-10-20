# Export only source model JSON and common weather, never engine binaries or results.
here<-dirname(normalizePath(sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE)),winslash='/'));source(file.path(here,'common.R'))
a<-args_named();src<-normal(need(a,'source-manifest'));out<-normal(need(a,'out'),FALSE)
stopifnot(!dir.exists(out));m<-read.csv(src)
m<-m[m$source=='DSM'|(m$source=='SSURGO'&grepl('_[0-9]+_d$',m$profile_id)),]
stopifnot(nrow(m)==2036,sum(m$source=='DSM')==2030,sum(m$source=='SSURGO')==6)
if(identical(a[['test-mode']],'yes'))m<-m[c(1,which(m$source=='SSURGO')[1]),]
stopifnot(!anyDuplicated(paste(m$source,m$profile_id)),all(grepl('^[A-Za-z0-9_]+$',m$profile_id)))
dir.create(file.path(out,'weather'),recursive=TRUE)
first<-fromJSON(m$input_file[1],simplifyVector=FALSE);weather<-normal(node(first,'Models.Climate.Weather')$FileName)
stopifnot(file.copy(weather,file.path(out,'weather/forcing.met'),overwrite=FALSE))
records<-list()
for(i in seq_len(nrow(m))){
 stopifnot(md5(m$input_file[i])==m$input_md5[i]);x<-fromJSON(m$input_file[i],simplifyVector=FALSE)
 stopifnot(md5(node(x,'Models.Climate.Weather')$FileName)==md5(weather))
 y<-map_nodes(x,function(z){if(type_is(z,'Models.Climate.Weather'))z$FileName<-'../../../weather/forcing.met';z})
 # No other field is changed by this packaging stage.
 mask_weather<-function(z)map_nodes(z,function(q){if(type_is(q,'Models.Climate.Weather'))q$FileName<-'<weather>';q})
 stopifnot(identical(mask_weather(x),mask_weather(y)))
 rel<-paste0('inputs/',m$source[i],'/',m$profile_id[i],'/simulation.apsimx');dest<-file.path(out,rel)
 dir.create(dirname(dest),recursive=TRUE);writej(y,dest)
 records[[i]]<-data.frame(source=m$source[i],profile_id=m$profile_id[i],relative_input=rel,bundle_md5=md5(dest),upstream_md5=m$input_md5[i])
 if(i%%200==0){cat('Bundled',i,'/',nrow(m),'\n');flush.console()}
}
write.csv(do.call(rbind,records),file.path(out,'bundle_manifest.csv'),row.names=FALSE)
writej(list(status='EXPORTED_NOT_PUBLICATION_APPROVED',profiles=nrow(m),mode=if(nrow(m)==2036)'FULLFIELD'else'TEST_ONLY',weather='weather/forcing.met',weather_md5=md5(weather),manifest_md5=md5(file.path(out,'bundle_manifest.csv')),note='Only weather references were relocated. Redistribution permissions must be reviewed before publication.'),file.path(out,'bundle.json'))
