# Verify that identifier alignment reproduces the archived numerical map cells.
here<-dirname(normalizePath(sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE)),winslash='/'));source(file.path(here,'../R/common.R'))
a<-args_named();index<-read.csv(need(a,'cell-index'),colClasses=c(mukey='character'));results<-normal(need(a,'historical-results'));spatial<-normal(need(a,'historical-spatial'));out<-normal(need(a,'out'),FALSE);stopifnot(!file.exists(out),nrow(index)==1943)
qa<-list()
for(y in c(2017,2019,2021)){
 r<-read.csv(file.path(results,'year_profiles',paste0('profile_results_',y,'.csv')));old<-read.csv(file.path(spatial,paste0('year_',y),'data/paired_cells.csv'))
 stopifnot(identical(index$x,old$x),identical(index$y,old$y),identical(index$DHP,old$DHP))
 dsm<-r$grain_Mg_ha[match(index$profile_id,r$profile_id)];ss<-r[r$source=='SSURGO'&grepl('_[0-9]+_d$',r$profile_id),];stopifnot(nrow(ss)==6)
 ss$mukey<-sub('.*_([0-9]+)_d$','\\1',ss$profile_id);dom<-ss$grain_Mg_ha[match(index$mukey,ss$mukey)]
 err_dsm<-max(abs(dsm-old$dsm));err_ss<-max(abs(dom-old$dominant));stopifnot(is.finite(err_dsm),is.finite(err_ss),err_dsm<1e-6,err_ss<1e-6)
 qa[[as.character(y)]]<-list(cells=nrow(index),max_DSM_difference_Mg_ha=err_dsm,max_SSURGO_difference_Mg_ha=err_ss)
}
writej(list(status='PASS',years=qa,tolerance_Mg_ha=1e-6,note='SSURGO archived raster values may have single-precision roundoff; new summaries join full-precision profile values.'),out)
