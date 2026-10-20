# Descriptive native-grid summaries only. No inferential tests or interpolated statistics.
here<-dirname(normalizePath(sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE)),winslash='/'));source(file.path(here,'common.R'))
a<-args_named();results<-normal(need(a,'results'));index_path<-normal(need(a,'cell-index'));out<-normal(need(a,'out'),FALSE)
stopifnot(!dir.exists(out));status<-fromJSON(file.path(results,'status.json'))
historical<-identical(a[['historical']],'yes')
if(historical)stopifnot(status$status=='ALL2050_EXTRACTED_REVIEW_AND_SPATIAL_JOIN_PENDING',status$profiles==2050,status$mode=='PRODUCTION')else stopifnot(status$status=='ALL2036_EXTRACTED_REVIEW_AND_SPATIAL_JOIN_PENDING',status$profiles==2036,status$mode=='PRODUCTION')
h<-status$artifact_hashes;stopifnot(all(unname(tools::md5sum(file.path(results,h$path)))==h$md5))
index<-read.csv(index_path,colClasses=c(mukey='character'));stopifnot(nrow(index)==1943,!anyDuplicated(index$profile_id),all(index$area_ha==.0144))
summaries<-pairs<-list();bu<-1000*.40468564224/(56*.45359237)/(1-.155)
for(y in c(2017,2019,2021)){
 r<-read.csv(file.path(results,'year_profiles',paste0('profile_results_',y,'.csv')))
 if(historical){stopifnot(nrow(r)==2050);r<-r[r$source=='DSM'|(r$source=='SSURGO'&grepl('_[0-9]+_d$',r$profile_id)),]}
 stopifnot(nrow(r)==2036,!anyDuplicated(r$profile_id),all(r$crop_year_QA=='PASS'))
 ss<-r[r$source=='SSURGO',];stopifnot(nrow(ss)==6,all(grepl('_[0-9]+_d$',ss$profile_id)))
 ss$mukey<-sub('.*_([0-9]+)_d$','\\1',ss$profile_id);stopifnot(!anyDuplicated(ss$mukey))
 p<-index;p$year<-y;p$dsm<-r$grain_Mg_ha[match(p$profile_id,r$profile_id)];p$dominant<-ss$grain_Mg_ha[match(p$mukey,ss$mukey)]
 stopifnot(all(is.finite(p$dsm)),all(is.finite(p$dominant)));p$delta<-p$dsm-p$dominant;pairs[[as.character(y)]]<-p
 for(g in c('SU','SH','BS','FS','TS','Field')){
  z<-if(g=='Field')p else p[p$DHP==g,];stopifnot(nrow(z)>1)
  summaries[[length(summaries)+1]]<-data.frame(year=y,DHP=g,Treatment='dominant',n=nrow(z),area_ha=sum(z$area_ha),DSM_mean=mean(z$dsm),SSURGO_mean=mean(z$dominant),DSM_sd=sd(z$dsm),SSURGO_sd=sd(z$dominant),paired_difference=mean(z$delta),paired_sd=sd(z$delta),percent_difference=100*mean(z$delta)/mean(z$dominant),DSM_bu_ac_15_5=mean(z$dsm)*bu,SSURGO_bu_ac_15_5=mean(z$dominant)*bu)
 }
}
dir.create(out,recursive=TRUE)
for(y in names(pairs))write.csv(pairs[[y]],file.path(out,paste0('paired_cells_',y,'.csv')),row.names=FALSE)
write.csv(do.call(rbind,summaries),file.path(out,'descriptive_summary.csv'),row.names=FALSE)
ctx<-fromJSON(status$context_key);files<-list.files(out,pattern='[.]csv$',full.names=TRUE)
writej(list(status=if(historical)'HISTORICAL_PAIRED_SUMMARIES_COMPLETE'else'PAIRED_SUMMARIES_COMPLETE_FIGURES_PENDING',historical=historical,cells=1943,years=c(2017,2019,2021),area_ha=sum(index$area_ha),index_md5=md5(index_path),manifest_md5=ctx$manifest_md5,preparation_md5=ctx$preparation_md5,results_status_md5=md5(file.path(results,'status.json')),artifact_hashes=data.frame(path=basename(files),md5=unname(tools::md5sum(files))),percent_denominator='SSURGO mean',bushels='56-lb bushels at 15.5 percent moisture; APSIM grain dry matter divided by 0.845',scope='Descriptive modeled field; no sampling uncertainty or observed accuracy claim.'),file.path(out,'status.json'))
capture.output(sessionInfo(),file=file.path(out,'sessionInfo.txt'))
