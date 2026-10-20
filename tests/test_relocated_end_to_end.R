# Run a DSM/SSURGO pair from relocated source inputs with the same capped engine.
here<-dirname(normalizePath(sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE)),winslash='/'));source(file.path(here,'../R/common.R'))
a<-args_named();src<-normal(need(a,'source-manifest'));engine<-normal(need(a,'engine'));production<-normal(need(a,'production-results'));soil<-normal(need(a,'production-soil'));out<-normal(need(a,'out'),FALSE)
stopifnot(!dir.exists(out),fromJSON(file.path(production,'status.json'))$profiles==2036,as.integer(tail(ps('@(Get-Process Models -ErrorAction SilentlyContinue).Count'),1))==0);dir.create(out,recursive=TRUE)
root<-normal(file.path(here,'../R'));exe<-file.path(R.home('bin'),'Rscript.exe')
call<-function(script,args){code<-system2(exe,c(shQuote(file.path(root,script)),vapply(args,shQuote,character(1))),stdout=file.path(out,paste0(script,'.stdout.txt')),stderr=file.path(out,paste0(script,'.stderr.txt')));if(code!=0)stop(script,' failed; outputs preserved')}
call('prepare.R',c('--source-manifest',src,'--out',file.path(out,'experiment'),'--test-pair','yes'))
call('run.R',c('--experiment',file.path(out,'experiment'),'--engine',engine))
call('extract.R',c('--experiment',file.path(out,'experiment'),'--out',file.path(out,'results'),'--test-mode','yes'))
call('verify_extraction.R',c('--experiment',file.path(out,'experiment'),'--results',file.path(out,'results'),'--out',file.path(out,'independent'),'--test-mode','yes'))
call('extract_soil_inputs.R',c('--experiment',file.path(out,'experiment'),'--out',file.path(out,'soil'),'--test-mode','yes'))
checks<-list()
for(y in c(2017,2019,2021)){
 x<-read.csv(file.path(out,'results/year_profiles',paste0('profile_results_',y,'.csv')));z<-read.csv(file.path(production,'year_profiles',paste0('profile_results_',y,'.csv')))
 stopifnot(nrow(x)==2,setequal(x$source,c('DSM','SSURGO')));target<-z$grain_Mg_ha[match(x$profile_id,z$profile_id)];eq(x$grain_Mg_ha,target)
 checks[[length(checks)+1]]<-data.frame(year=y,profile_id=x$profile_id,source=x$source,absolute_difference_Mg_ha=abs(x$grain_Mg_ha-target))
}
x<-read.csv(file.path(out,'soil/depth_predictors.csv'));z<-read.csv(file.path(soil,'depth_predictors.csv'));keys<-paste(x$profile_id,x$interval);target<-z[match(keys,paste(z$profile_id,z$interval)),names(x)];rownames(target)<-NULL;stopifnot(isTRUE(all.equal(x,target,tolerance=1e-10)))
write.csv(do.call(rbind,checks),file.path(out,'yield_reproduction.csv'),row.names=FALSE)
writej(list(status='PASS',profiles=2,sources=c('DSM','SSURGO'),years=c(2017,2019,2021),scope='Relocated source bundle through preparation, capped simulation, extraction, independent SQL checks and soil predictor reproduction; full-field figures use separately verified production outputs.',tolerance_Mg_ha=1e-7,source_manifest_md5=md5(src)),file.path(out,'QA.json'))
