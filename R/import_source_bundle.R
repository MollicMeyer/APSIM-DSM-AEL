# Rebuild an absolute-path source manifest in a new location; never alter the bundle.
here<-dirname(normalizePath(sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE)),winslash='/'));source(file.path(here,'common.R'))
a<-args_named();bundle<-normal(need(a,'bundle'));out<-normal(need(a,'out'),FALSE);stopifnot(!dir.exists(out))
b<-fromJSON(file.path(bundle,'bundle.json'));mp<-file.path(bundle,'bundle_manifest.csv')
stopifnot(md5(mp)==b$manifest_md5,b$status=='EXPORTED_NOT_PUBLICATION_APPROVED')
m<-read.csv(mp);stopifnot(nrow(m)==b$profiles,!anyDuplicated(m$relative_input),all(grepl('^inputs/(DSM|SSURGO)/[A-Za-z0-9_]+/simulation[.]apsimx$',m$relative_input)),identical(b$weather,'weather/forcing.met'))
stopifnot(all(unname(tools::md5sum(file.path(bundle,m$relative_input)))==m$bundle_md5),md5(file.path(bundle,b$weather))==b$weather_md5)
dir.create(file.path(out,'weather'),recursive=TRUE);weather<-file.path(out,'weather/forcing.met')
stopifnot(file.copy(file.path(bundle,b$weather),weather,overwrite=FALSE));records<-list()
stopifnot(md5(weather)==b$weather_md5)
mask_weather<-function(z)map_nodes(z,function(q){if(type_is(q,'Models.Climate.Weather'))q$FileName<-'<weather>';q})
for(i in seq_len(nrow(m))){
 x<-fromJSON(file.path(bundle,m$relative_input[i]),simplifyVector=FALSE)
 stopifnot(node(x,'Models.Climate.Weather')$FileName=='../../../weather/forcing.met')
 y<-map_nodes(x,function(z){if(type_is(z,'Models.Climate.Weather'))z$FileName<-weather;z})
 stopifnot(identical(mask_weather(x),mask_weather(y)))
 dest<-file.path(out,m$relative_input[i]);dir.create(dirname(dest),recursive=TRUE);writej(y,dest)
 records[[i]]<-data.frame(source=m$source[i],profile_id=m$profile_id[i],input_file=dest,input_md5=md5(dest),bundle_md5=m$bundle_md5[i],upstream_md5=m$upstream_md5[i])
 if(i%%200==0){cat('Imported',i,'/',nrow(m),'\n');flush.console()}
}
write.csv(do.call(rbind,records),file.path(out,'manifest.csv'),row.names=FALSE)
writej(list(status='IMPORTED',profiles=nrow(m),bundle_manifest_md5=b$manifest_md5,weather_md5=md5(weather),bundle_mode=b$mode),file.path(out,'import.json'))
