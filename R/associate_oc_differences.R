# Paired OC/yield differences; descriptive associations, not a loss experiment.
here<-dirname(normalizePath(sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE)),winslash='/'));source(file.path(here,'common.R'))
a<-args_named();paired<-normal(need(a,'paired'));soil<-normal(need(a,'soil-inputs'));out<-normal(need(a,'out'),FALSE);stopifnot(!dir.exists(out))
ps<-fromJSON(file.path(paired,'status.json'));hs<-fromJSON(file.path(soil,'status.json'));stopifnot(ps$status=='PAIRED_SUMMARIES_COMPLETE_FIGURES_PENDING',hs$status=='SOIL_INPUTS_COMPLETE',hs$profiles==2036)
stopifnot(!is.null(ps$manifest_md5),ps$manifest_md5==hs$manifest_md5,ps$preparation_md5==hs$preparation_md5,all(unname(tools::md5sum(file.path(paired,ps$artifact_hashes$path)))==ps$artifact_hashes$md5))
stopifnot(md5(file.path(soil,'depth_predictors.csv'))==hs$predictors_md5)
p<-read.csv(file.path(soil,'depth_predictors.csv'));cells<-fits<-list()
for(depth in c('0-30 cm','30-100 cm')){
 q<-p[p$interval==depth,];stopifnot(nrow(q)==2036);ss<-q[q$source=='SSURGO',];stopifnot(nrow(ss)==6);ss$mukey<-sub('.*_([0-9]+)_d$','\\1',ss$profile_id)
 for(y in c(2017,2019,2021)){
  z<-read.csv(file.path(paired,paste0('paired_cells_',y,'.csv')));stopifnot(nrow(z)==1943,all(z$year==y))
  z$DSM_OC<-q$OC[match(z$profile_id,q$profile_id)];z$SSURGO_OC<-ss$OC[match(as.character(z$mukey),ss$mukey)];stopifnot(all(is.finite(z$DSM_OC)),all(is.finite(z$SSURGO_OC)))
  z$delta_OC<-z$DSM_OC-z$SSURGO_OC;z$interval<-depth;cells[[length(cells)+1]]<-z[c('x','y','DHP','year','interval','DSM_OC','SSURGO_OC','delta_OC','delta')]
  for(g in c('Field','SU','SH','BS','FS','TS')){
   r<-if(g=='Field')z else z[z$DHP==g,];fit<-lm(delta~delta_OC,data=r)
   fits[[length(fits)+1]]<-data.frame(year=y,interval=depth,DHP=g,n=nrow(r),DSM_OC=mean(r$DSM_OC),SSURGO_OC=mean(r$SSURGO_OC),delta_OC=mean(r$delta_OC),delta_yield=mean(r$delta),R2=summary(fit)$r.squared,slope=unname(coef(fit)[2]))
  }
 }
}
dir.create(out,recursive=TRUE);write.csv(do.call(rbind,cells),file.path(out,'paired_OC_yield_differences.csv'),row.names=FALSE);write.csv(do.call(rbind,fits),file.path(out,'OC_difference_associations.csv'),row.names=FALSE)
writej(list(status='OC_DIFFERENCE_ASSOCIATIONS_COMPLETE',paired_status_md5=md5(file.path(paired,'status.json')),soil_status_md5=md5(file.path(soil,'status.json')),scope='Initial mapped OC contrasts, not observed OC loss or isolated causal productivity effects.'),file.path(out,'status.json'))
