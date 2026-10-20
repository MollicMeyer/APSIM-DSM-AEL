# Exact layer inputs and depth-overlap predictors; no PTF variables in associations.
here<-dirname(normalizePath(sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE)),winslash='/'));source(file.path(here,'common.R'))
a<-args_named();root<-normal(need(a,'experiment'));out<-normal(need(a,'out'),FALSE);test<-identical(a[['test-mode']],'yes')
stopifnot(!dir.exists(out));m<-read.csv(file.path(root,'manifest.csv'));prep<-fromJSON(file.path(root,'preparation.json'))
if(test)stopifnot(nrow(m)>=1,nrow(m)<=6)else stopifnot(nrow(m)==2036,sum(m$source=='DSM')==2030,sum(m$source=='SSURGO')==6)
stopifnot(prep$profiles==nrow(m),!anyDuplicated(m$profile_id));hh<-means<-list()
intervals<-data.frame(label=c('0-30 cm','30-100 cm'),top=c(0,300),bottom=c(300,1000))
for(i in seq_len(nrow(m))){
 stopifnot(md5(m$input_file[i])==m$input_md5[i]);x<-fromJSON(m$input_file[i],simplifyVector=FALSE)
 p<-node(x,'Models.Soils.Physical');o<-node(x,'Models.Soils.Organic');ch<-node(x,'Models.Soils.Chemical');th<-vec(p$Thickness)
 stopifnot(sum(th)==2000,all(vec(o$Thickness)==th),all(vec(ch$Thickness)==th));eq(vec(p$AirDry),airdry(p))
 h<-data.frame(profile_id=m$profile_id[i],source=m$source[i],layer=seq_along(th),thickness_mm=th,bottom_mm=cumsum(th),OC=vec(o$Carbon),pH=vec(ch$PH),KSAT=vec(p$KS),LL15=vec(p$LL15),AirDry=vec(p$AirDry),BD=vec(p$BD),DUL=vec(p$DUL),SAT=vec(p$SAT),sand=vec(p$ParticleSizeSand),silt=vec(p$ParticleSizeSilt),clay=vec(p$ParticleSizeClay))
 fields<-c('OC','pH','KSAT','LL15','AirDry','BD','DUL','SAT','sand','silt','clay')
 stopifnot(all(vapply(h[fields],function(v)all(is.finite(v)),logical(1))))
 hh[[i]]<-h
 for(k in seq_len(nrow(intervals))){
  z<-intervals[k,];w<-pmax(0,pmin(h$bottom_mm,z$bottom)-pmax(h$bottom_mm-th,z$top));eq(sum(w),z$bottom-z$top)
  q<-data.frame(profile_id=m$profile_id[i],source=m$source[i],interval=z$label)
  for(n in c('OC','pH','sand','silt','clay'))q[[n]]<-sum(h[[n]]*w)/sum(w)
  means[[length(means)+1]]<-q
 }
 if(i%%200==0){cat('Read soil inputs',i,'/',nrow(m),'\n');flush.console()}
}
dir.create(out,recursive=TRUE);write.csv(do.call(rbind,hh),file.path(out,'input_horizons.csv'),row.names=FALSE);write.csv(do.call(rbind,means),file.path(out,'depth_predictors.csv'),row.names=FALSE)
writej(list(status=if(test)'TEST_SOIL_INPUTS_COMPLETE'else'SOIL_INPUTS_COMPLETE',profiles=nrow(m),intervals=intervals$label,predictors=c('OC','pH','sand','silt','clay'),manifest_md5=md5(file.path(root,'manifest.csv')),preparation_md5=md5(file.path(root,'preparation.json')),predictors_md5=md5(file.path(out,'depth_predictors.csv')),horizons_md5=md5(file.path(out,'input_horizons.csv'))),file.path(out,'status.json'))
capture.output(sessionInfo(),file=file.path(out,'sessionInfo.txt'))
