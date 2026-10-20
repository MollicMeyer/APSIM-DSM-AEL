# Reproduce archived PRISM context separately from the actual APSIM forcing.
here<-dirname(normalizePath(sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE)),winslash='/'));source(file.path(here,'common.R'))
a<-args_named();src<-normal(need(a,'source-data'));metpath<-normal(need(a,'weather'));out<-normal(need(a,'out'),FALSE)
stopifnot(!dir.exists(out));upstream<-fromJSON(file.path(src,'status.json'))
stopifnot(upstream$status=='CLIMATE_CONTEXT_COMPLETE',md5(metpath)==upstream$simulation_weather_md5)
raw_hashes<-read.csv(file.path(src,'raw_hashes.csv'))
stopifnot(all(unname(tools::md5sum(file.path(src,'raw',raw_hashes$file)))==raw_hashes$md5))
dir.create(out,recursive=TRUE)
writecsv<-function(x,name)write.csv(x,file.path(out,name),row.names=FALSE,na='')
read_prism<-function(name) {
 path<-file.path(src,'raw',name);tx<-readLines(path,warn=FALSE)
 start<-grep('^Date,',tx);stopifnot(length(start)==1)
 read.csv(path,skip=start-1,check.names=FALSE,stringsAsFactors=FALSE)
}
norm<-read_prism('monthly_normals.csv');monthly<-read_prism('monthly_201701_202112.csv');annual<-read_prism('yearly_2017_2021.csv')
stopifnot(nrow(norm)==13,nrow(monthly)==60,nrow(annual)==5)
names(norm)<-c('period','precip_mm','tmean_C');names(monthly)<-c('date','precip_mm','tmean_C');names(annual)<-c('year','precip_mm','tmean_C')
monthly$year<-as.integer(substr(monthly$date,1,4));monthly$month<-as.integer(substr(monthly$date,6,7))
stopifnot(!anyDuplicated(monthly$date),all(monthly$precip_mm>=0),all(is.finite(monthly$tmean_C)))
monthly$normal_precip_mm<-norm$precip_mm[monthly$month]
monthly$normal_tmean_C<-norm$tmean_C[monthly$month]
monthly$precip_departure_mm<-monthly$precip_mm-monthly$normal_precip_mm
monthly$precip_departure_percent<-100*monthly$precip_departure_mm/monthly$normal_precip_mm
monthly$tmean_departure_C<-monthly$tmean_C-monthly$normal_tmean_C
writecsv(monthly,'prism_monthly_2017_2021.csv');writecsv(norm,'prism_normals_1991_2020.csv')
years<-c(2017,2019,2021);days<-c(31,28,31,30,31,30,31,31,30,31,30,31)
context<-do.call(rbind,lapply(years,function(y)do.call(rbind,lapply(c('Annual','May-September'),function(period){
 z<-monthly[monthly$year==y,];n<-norm[seq_len(12),]
 if(period=='Annual') {
   a<-annual[annual$year==y,];p<-sum(z$precip_mm);t<-a$tmean_C;np<-sum(n$precip_mm);nt<-norm$tmean_C[13]
 } else {
   z<-z[z$month%in%5:9,];p<-sum(z$precip_mm);t<-weighted.mean(z$tmean_C,days[z$month]);np<-sum(n$precip_mm[5:9]);nt<-weighted.mean(n$tmean_C[5:9],days[5:9])
 }
 data.frame(year=y,period=period,precip_mm=p,normal_precip_mm=np,precip_departure_mm=p-np,precip_departure_percent=100*(p-np)/np,
 tmean_C=t,normal_tmean_C=nt,tmean_departure_C=t-nt,precipitation_aggregation='Sum of monthly values for both selected period and normal',temperature_aggregation=if(period=='Annual')'PRISM annual values' else 'Day-weighted monthly means (153 days)',normal_period='1991-2020',source='PRISM 4-km enclosing grid cell; no interpolation')
}))))
writecsv(context,'prism_climate_context.csv')
# Independent arithmetic check of PRISM annual precipitation vs rounded monthly values.
annual_check<-merge(aggregate(precip_mm~year,monthly,sum),annual,by='year',suffixes=c('_monthly_sum','_annual'))
annual_check$annual_minus_monthly_mm<-annual_check$precip_mm_annual-annual_check$precip_mm_monthly_sum
writecsv(annual_check,'prism_annual_aggregation_check.csv')
print(annual_check)
stopifnot(abs(sum(norm$precip_mm[1:12])-norm$precip_mm[13])<0.12)
# Geometry/location provenance was audited with the source geodatabase in the archived study.
# Exact APSIM weather file, reported separately: this is not PRISM forcing.
tx<-readLines(metpath)
header<-grep('^year day ',tx);stopifnot(length(header)==1)
met<-read.table(metpath,skip=header+1,col.names=strsplit(tx[header],' +')[[1]])
met$date<-as.Date(paste0(met$year,'-01-01'))+met$day-1
met$month<-as.integer(format(met$date,'%m'));met$tmean_C<-(met$maxt+met$mint)/2
stopifnot(nrow(met)==3287,identical(range(met$year),c(2013L,2021L)),!anyDuplicated(met$date),all(diff(met$date)==1))
met_summary<-do.call(rbind,lapply(years,function(y)do.call(rbind,lapply(c('Annual','May-September'),function(period){
 z<-met[met$year==y & (period=='Annual'|met$month%in%5:9),]
 data.frame(year=y,period=period,days=nrow(z),precip_mm=sum(z$rain),tmean_C=mean(z$tmean_C),maxt_C=mean(z$maxt),mint_C=mean(z$mint),radiation_MJ_m2=sum(z$radn),source='APSIM archived IA-IA0200 weather; no 30-year normal available in this file')
}))))
writecsv(met_summary,'apsim_actual_weather_context.csv')
writecsv(merge(context[,c('year','period','precip_mm','tmean_C')],met_summary[,c('year','period','precip_mm','tmean_C')],by=c('year','period'),suffixes=c('_PRISM','_APSIM')),'prism_vs_simulation_weather.csv')

writej(list(status='CLIMATE_CONTEXT_REPRODUCED',archived_source_status_md5=md5(file.path(src,'status.json')),archived_access_date=upstream$access_date,reproduced_at=as.character(Sys.time()),normal_period='1991-2020',years=years,simulation_weather_md5=md5(metpath),source_geometry_check='Preserved in archived source status; not rerun by this arithmetic-only module',annual_precipitation_caveat=upstream$annual_precipitation_caveat,dataset_label_caveat=upstream$dataset_label_caveat,scope='PRISM context is not APSIM forcing; years also differ in evolving soil/crop history.'),file.path(out,'status.json'))
capture.output(sessionInfo(),file=file.path(out,'sessionInfo.txt'))
