# Independent SQL checks of completed outputs; no simulations are launched.
here<-dirname(normalizePath(sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE)),winslash='/'));source(file.path(here,'common.R'));library(DBI);library(RSQLite)
a<-args_named();experiment<-normal(need(a,'experiment'));results<-normal(need(a,'results'));out<-normal(need(a,'out'),FALSE);test<-identical(a[['test-mode']],'yes')
stopifnot(!dir.exists(out));m<-read.csv(file.path(experiment,'manifest.csv'));status<-fromJSON(file.path(results,'status.json'))
stopifnot(status$profiles==nrow(m),status$status==if(test)'TEST_ONLY_EXTRACTION_COMPLETE'else'ALL2036_EXTRACTED_REVIEW_AND_SPATIAL_JOIN_PENDING')
if(test)stopifnot(nrow(m)<=6)else stopifnot(nrow(m)==2036,sum(m$source=='DSM')==2030,sum(m$source=='SSURGO')==6)
ctx<-fromJSON(status$context_key);stopifnot(ctx$manifest_md5==md5(file.path(experiment,'manifest.csv')),ctx$preparation_md5==md5(file.path(experiment,'preparation.json')))
stopifnot(all(unname(tools::md5sum(file.path(results,status$artifact_hashes$path)))==status$artifact_hashes$md5))
annual<-read.csv(file.path(results,'annual_stocks_and_yields.csv'));check<-list();dir.create(out,recursive=TRUE)
for(i in seq_len(nrow(m))){
 rec<-fromJSON(paste0(m$input_file[i],'.verified.json'));stopifnot(rec$status=='PASS',md5(m$expected_db[i])==rec$database_md5,md5(m$input_file[i])==rec$input_md5)
 con<-dbConnect(SQLite(),m$expected_db[i],flags=SQLITE_RO)
 sql_yield<-dbGetQuery(con,"SELECT CAST(substr(Date,1,4) AS INTEGER) AS year, COUNT(*) AS days, MAX(MaizeGrain)*0.01 AS maize, MAX(SoybeanGrain)*0.01 AS soybean FROM WarmupDaily GROUP BY substr(Date,1,4) ORDER BY year")
 stopifnot(identical(sql_yield$year,2013:2021),sum(sql_yield$days)==3287)
 q<-annual[annual$run_id==m$run_id[i],];q<-q[order(q$year),];stopifnot(identical(q$year,sql_yield$year));eq(q$Maize_peak_grain_Mg_ha,sql_yield$maize,1e-10);eq(q$Soybean_peak_grain_Mg_ha,sql_yield$soybean,1e-10)
 raw<-dbListFields(con,'WarmupStart');nn<-make.names(raw);stopifnot(!anyDuplicated(nn));nfields<-raw[grepl('^(NO3|NH4)[.][0-9]+[.]$',nn)]
 stopifnot(length(nfields)>0);nexpr<-paste(as.character(dbQuoteIdentifier(con,nfields)),collapse='+')
 init<-dbGetQuery(con,paste0('SELECT ',nexpr,' AS mineralN FROM WarmupStart ORDER BY Date LIMIT 1'))$mineralN;eq(init,63.9519166666667)
 pools<-raw[grepl('^(TotalC|OrganicN|MineralN|AD_SWmm)[.][0-9]+[.]$',nn)];stopifnot(length(pools)>0)
 diffs<-vapply(seq_along(pools),function(k){col<-as.character(dbQuoteIdentifier(con,pools[k]));paste0('MAX(ABS(s.',col,'-z.',col,')) AS v',k)},character(1))
 continuity<-dbGetQuery(con,paste0('SELECT COUNT(*) AS joins, ',paste(diffs,collapse=',')," FROM WarmupStart s JOIN WarmupDaily z ON substr(s.Date,1,10)=date(substr(z.Date,1,10),'+1 day') WHERE substr(s.Date,6,5)='01-01'"))
 stopifnot(continuity$joins==8,all(is.finite(as.numeric(continuity[1,-1]))),max(abs(as.numeric(continuity[1,-1])))<1e-7)
 normal_end<-dbGetQuery(con,"SELECT COUNT(*) AS n FROM _Messages WHERE Message LIKE '%Simulation terminated normally%'")$n;stopifnot(normal_end>=1)
 dbDisconnect(con)
 check[[i]]<-data.frame(run_id=m$run_id[i],source=m$source[i],days=sum(sql_yield$days),SQL_year_boundary_joins=8,SQL_initial_mineralN=init,max_maize_yield_error=max(abs(q$Maize_peak_grain_Mg_ha-sql_yield$maize)),max_pool_boundary_error=max(abs(as.numeric(continuity[1,-1]))),database_md5=rec$database_md5)
 if(i%%50==0){cat('Independently verified',i,'/',nrow(m),'\n');flush.console();gc(FALSE)}
}
write.csv(do.call(rbind,check),file.path(out,'SQL_independent_checks.csv'),row.names=FALSE)
writej(list(status=if(test)'TEST_INDEPENDENT_VERIFICATION_PASS'else'INDEPENDENT_VERIFICATION_PASS',profiles=nrow(m),years=2013:2021,manifest_md5=ctx$manifest_md5,results_status_md5=md5(file.path(results,'status.json')),checks=c('database hashes','SQL annual grain maxima','SQL initial mineral N','SQL layerwise year-boundary carbon nitrogen water continuity','normal termination')),file.path(out,'status.json'))
