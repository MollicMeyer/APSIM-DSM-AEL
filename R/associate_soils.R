# Separate descriptive regressions; do not interpret R-squared as isolated effects.
here<-dirname(normalizePath(sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE)),winslash='/'));source(file.path(here,'common.R'))
a<-args_named();results<-normal(need(a,'results'));soil<-normal(need(a,'soil-inputs'));out<-normal(need(a,'out'),FALSE);stopifnot(!dir.exists(out))
s<-fromJSON(file.path(results,'status.json'));hs<-fromJSON(file.path(soil,'status.json'))
stopifnot(s$status=='ALL2036_EXTRACTED_REVIEW_AND_SPATIAL_JOIN_PENDING',s$profiles==2036,hs$status=='SOIL_INPUTS_COMPLETE',hs$profiles==2036)
ctx<-fromJSON(s$context_key);stopifnot(ctx$manifest_md5==hs$manifest_md5,ctx$preparation_md5==hs$preparation_md5)
stopifnot(md5(file.path(soil,'depth_predictors.csv'))==hs$predictors_md5,all(unname(tools::md5sum(file.path(results,s$artifact_hashes$path)))==s$artifact_hashes$md5))
p<-read.csv(file.path(soil,'depth_predictors.csv'));vars<-c('OC','pH','sand','silt','clay');fits<-corr<-list()
for(depth in c('0-30 cm','30-100 cm'))for(src in c('DSM','SSURGO')){
 q<-p[p$interval==depth&p$source==src,];stopifnot(nrow(q)==if(src=='DSM')2030 else 6,!anyDuplicated(q$profile_id))
 cc<-cor(q[vars]);ix<-which(upper.tri(cc),arr.ind=TRUE);corr[[length(corr)+1]]<-data.frame(source=src,interval=depth,property1=rownames(cc)[ix[,1]],property2=colnames(cc)[ix[,2]],r=cc[ix])
 for(y in c(2017,2019,2021)){
  r<-read.csv(file.path(results,'year_profiles',paste0('profile_results_',y,'.csv')));j<-match(q$profile_id,r$profile_id);stopifnot(!anyNA(j),all(r$crop_year_QA[j]=='PASS'))
  yy<-r$grain_Mg_ha[j];stopifnot(all(is.finite(yy)))
  for(v in vars){xx<-q[[v]];stopifnot(all(is.finite(xx)),sd(xx)>0);fit<-lm(yy~xx)
   loo<-if(src=='SSURGO')sapply(seq_along(yy),function(k)summary(lm(yy[-k]~xx[-k]))$r.squared)else NA_real_
   fits[[length(fits)+1]]<-data.frame(year=y,source=src,interval=depth,property=v,n=length(yy),R2=summary(fit)$r.squared,slope=unname(coef(fit)[2]),loo_min=if(src=='SSURGO')min(loo)else NA_real_,loo_max=if(src=='SSURGO')max(loo)else NA_real_)
  }
 }
}
dir.create(out,recursive=TRUE);write.csv(do.call(rbind,fits),file.path(out,'soil_yield_R2.csv'),row.names=FALSE);write.csv(do.call(rbind,corr),file.path(out,'raw_input_correlations.csv'),row.names=FALSE)
writej(list(status='ASSOCIATIONS_COMPLETE_FIGURES_PENDING',intervals=c('0-30 cm','30-100 cm'),predictors=vars,scope='Associations only; leave-one-profile-out range is a descriptive influence check, not independent validation. No significance tests, causal isolation or overcalibration inference.',results_status_md5=md5(file.path(results,'status.json')),soil_status_md5=md5(file.path(soil,'status.json'))),file.path(out,'status.json'))
