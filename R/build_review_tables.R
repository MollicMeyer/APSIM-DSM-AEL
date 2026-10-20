# Verify native-cell summaries and prepare traceable manuscript numbers in R.
here<-dirname(normalizePath(sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE)),winslash='/'));source(file.path(here,'common.R'))
a<-args_named();root<-normal(need(a,'analysis'));old<-normal(need(a,'historical-paired'));out<-normal(need(a,'out'),FALSE);stopifnot(!dir.exists(out))
stopifnot(fromJSON(file.path(root,'independent_verification/status.json'))$status=='INDEPENDENT_VERIFICATION_PASS',fromJSON(file.path(old,'status.json'))$status=='HISTORICAL_PAIRED_SUMMARIES_COMPLETE')
s<-read.csv(file.path(root,'paired/descriptive_summary.csv'));h<-read.csv(file.path(old,'descriptive_summary.csv'));stopifnot(nrow(s)==18,nrow(h)==18)
pos<-land<-list();checks<-list()
for(y in c(2017,2019,2021)){
 z<-read.csv(file.path(root,'paired',paste0('paired_cells_',y,'.csv')));r<-read.csv(file.path(root,'results/year_profiles',paste0('profile_results_',y,'.csv')))
 stopifnot(nrow(z)==1943,all(z$area_ha==.0144),!anyDuplicated(z$profile_id));eq(z$dsm,r$grain_Mg_ha[match(z$profile_id,r$profile_id)])
 ss<-r[r$source=='SSURGO',];ss$mukey<-sub('.*_([0-9]+)_d$','\\1',ss$profile_id);eq(z$dominant,ss$grain_Mg_ha[match(as.character(z$mukey),ss$mukey)])
 for(g in c('Field','SU','SH','BS','FS','TS')){q<-if(g=='Field')z else z[z$DHP==g,];t<-s[s$year==y&s$DHP==g,];stopifnot(nrow(t)==1,t$n==nrow(q));eq(c(t$DSM_mean,t$SSURGO_mean,t$paired_difference,t$paired_sd,t$percent_difference),c(mean(q$dsm),mean(q$dominant),mean(q$dsm-q$dominant),sd(q$dsm-q$dominant),100*mean(q$dsm-q$dominant)/mean(q$dominant)));checks[[length(checks)+1]]<-data.frame(year=y,DHP=g,n=nrow(q),status='PASS')}
 pos[[length(pos)+1]]<-data.frame(year=y,percent_DSM_above=100*mean(z$delta>0))
 q<-s[s$year==y&s$DHP!='Field',];land[[length(land)+1]]<-data.frame(year=y,DSM_toeslope_minus_summit=q$DSM_mean[q$DHP=='TS']-q$DSM_mean[q$DHP=='SU'],highest=q$DHP[which.max(q$DSM_mean)],lowest=q$DHP[which.min(q$DSM_mean)])
}
comparison<-merge(s,h,by=c('year','DHP'),suffixes=c('_new','_old'));comparison$change_DSM<-comparison$DSM_mean_new-comparison$DSM_mean_old;comparison$change_SSURGO<-comparison$SSURGO_mean_new-comparison$SSURGO_mean_old;comparison$change_paired_difference<-comparison$paired_difference_new-comparison$paired_difference_old
fits<-read.csv(file.path(root,'associations/soil_yield_R2.csv'));oc<-read.csv(file.path(root,'OC_differences/OC_difference_associations.csv'));correlations<-read.csv(file.path(root,'associations/raw_input_correlations.csv'));v<-read.csv(file.path(root,'variograms/variogram_parameters.csv'))
dir.create(out,recursive=TRUE);write.csv(comparison,file.path(out,'AirDry_old_new_comparison.csv'),row.names=FALSE);write.csv(do.call(rbind,checks),file.path(out,'paired_summary_independent_checks.csv'),row.names=FALSE)
writej(list(field=s[s$DHP=='Field',],hillslopes=do.call(rbind,land),fraction_positive=do.call(rbind,pos),ranges=v,clay=fits[fits$source=='DSM'&fits$property=='clay',],OC_field=oc[oc$interval=='0-30 cm'&oc$DHP=='Field',],OC_clay_r=correlations[correlations$source=='SSURGO'&correlations$property1=='OC'&correlations$property2=='clay',]),file.path(out,'manuscript_numbers.json'))
writej(list(status='REVIEW_TABLES_VERIFIED',paired_cells=1943,years=c(2017,2019,2021),checks='18 summaries recomputed; DSM and SSURGO native cells joined independently to verified profile extraction.',analysis_status_md5=md5(file.path(root,'paired/status.json')),historical_status_md5=md5(file.path(old,'status.json')),scope='Old/new changes refer only to this AirDry comparison. No claim of accuracy or causal separation of correlated soil properties.'),file.path(out,'status.json'))
