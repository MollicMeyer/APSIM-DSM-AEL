here<-dirname(normalizePath(sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE)),winslash='/'));source(file.path(here,'common.R'));source(file.path(here,'lib/variogram_fit.R'))
a<-args_named();paired<-normal(need(a,'paired'));out<-normal(need(a,'out'),FALSE);stopifnot(!dir.exists(out))
st<-fromJSON(file.path(paired,'status.json'));historical<-identical(a[['historical']],'yes')
stopifnot(st$cells==1943,identical(isTRUE(st$historical),historical))
stopifnot(st$status==if(historical)'HISTORICAL_PAIRED_SUMMARIES_COMPLETE'else'PAIRED_SUMMARIES_COMPLETE_FIGURES_PENDING')
stopifnot(!is.null(st$artifact_hashes),all(unname(tools::md5sum(file.path(paired,st$artifact_hashes$path)))==st$artifact_hashes$md5))
em<-candidates<-list();sources<-list()
for(y in c(2017,2019,2021)){
 path<-file.path(paired,paste0('paired_cells_',y,'.csv'));d<-read.csv(path)
 stopifnot(nrow(d)==1943,all(d$year==y),!anyDuplicated(paste(d$x,d$y)),all(is.finite(as.matrix(d[c('x','y','dsm','dominant','delta')]))))
 eq(d$delta,d$dsm-d$dominant)
 sources[[as.character(y)]]<-list(file=basename(path),md5=md5(path))
 distance<-as.numeric(dist(d[c('x','y')]));lag<-cut(distance,seq(0,288,24),labels=FALSE,right=TRUE)
 for(map in c('dsm','dominant','delta')){
  gamma<-.5*as.numeric(dist(d[[map]]))^2
  z<-do.call(rbind,lapply(1:12,function(k){j<-which(lag==k);stopifnot(length(j)>0);data.frame(year=y,map=map,lower_m=(k-1)*24,upper_m=k*24,mean_distance_m=mean(distance[j]),pairs=length(j),semivariance=mean(gamma[j]))}))
  em[[length(em)+1]]<-z
  for(model in c('spherical','exponential','gaussian'))candidates[[length(candidates)+1]]<-fit_one(z,model,'pairs_over_distance_squared',y,map)
 }
 rm(distance,lag,gamma);gc(FALSE)
}
ff<-do.call(rbind,candidates);selected<-do.call(rbind,lapply(split(ff,interaction(ff$year,ff$map,drop=TRUE)),function(z)z[which.min(z$weighted_mean_squared_residual),]))
selected$effective_range_m<-ifelse(selected$model=='spherical',selected$finite_sill_range_m,selected$practical_range95_m)
dir.create(out,recursive=TRUE);write.csv(do.call(rbind,em),file.path(out,'empirical_variograms.csv'),row.names=FALSE);write.csv(ff,file.path(out,'variogram_candidates.csv'),row.names=FALSE);write.csv(selected,file.path(out,'variogram_parameters.csv'),row.names=FALSE)
writej(list(status=if(historical)'HISTORICAL_VARIOGRAMS_COMPLETE'else'VARIOGRAMS_COMPLETE_FIGURES_PENDING',historical=historical,lag_width_m=24,maximum_lag_m=288,sources=sources,solver_md5=md5(file.path(here,'lib/variogram_fit.R')),models=c('spherical','exponential','gaussian'),weighting='pairs_over_distance_squared',range='Spherical finite sill range; exponential and Gaussian distance attaining 95 percent of structured partial sill. Undefined or unresolved ranges remain NA.',scope='Descriptive model selection by minimum weighted residual; no p-values or confidence intervals.'),file.path(out,'status.json'))
capture.output(sessionInfo(),file=file.path(out,'sessionInfo.txt'))
