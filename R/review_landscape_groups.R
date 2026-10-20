# Descriptive grouping only; cells and repeated seasons are not independent replicates.
here<-dirname(normalizePath(sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE)),winslash='/'));source(file.path(here,'common.R'))
a<-args_named();paired<-normal(need(a,'paired'));out<-normal(need(a,'out'),FALSE);stopifnot(!dir.exists(out));dir.create(out,recursive=TRUE)
all<-do.call(rbind,lapply(c(2017,2019,2021),function(y)read.csv(file.path(paired,paste0('paired_cells_',y,'.csv')))))
stopifnot(nrow(all)==1943*3)
all$group<-c(SU='Upper',SH='Upper',BS='Middle',FS='Lower',TS='Lower')[all$DHP]
stopifnot(!anyNA(all$group));all$delta<-all$dsm-all$dominant
summary<-do.call(rbind,lapply(split(all,list(all$year,all$group),drop=TRUE),function(d)data.frame(year=d$year[1],group=d$group[1],cells=nrow(d),area_ha=nrow(d)*144/10000,DSM=mean(d$dsm),SSURGO=mean(d$dominant),difference=mean(d$delta),DSM_spatial_sd=sd(d$dsm))))
write.csv(summary,file.path(out,'grouped_landscape_summary.csv'),row.names=FALSE)
within<-do.call(rbind,lapply(split(all,list(all$year,all$mukey),drop=TRUE),function(d)data.frame(year=d$year[1],mukey=d$mukey[1],cells=nrow(d),DSM_sd=sd(d$dsm),DSM_p05=quantile(d$dsm,.05,names=FALSE),DSM_p95=quantile(d$dsm,.95,names=FALSE),SSURGO_values=length(unique(d$dominant)))))
write.csv(within,file.path(out,'within_polygon_spread.csv'),row.names=FALSE)
writej(list(status='DESCRIPTIVE_GROUPING_VERIFIED',cells_per_year=1943,grouping='Upper SU+SH; Middle BS; Lower FS+TS',inference='No tests; repeated spatial cells are not independent replication',source_md5=as.list(tools::md5sum(file.path(paired,paste0('paired_cells_',c(2017,2019,2021),'.csv'))))),file.path(out,'status.json'))
