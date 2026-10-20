here<-dirname(normalizePath(sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE)),winslash='/'));source(file.path(here,'../R/common.R'));source(file.path(here,'../R/lib/variogram_fit.R'))
a<-args_named();reference<-normal(need(a,'reference-data'));out<-normal(need(a,'out'),FALSE);stopifnot(!file.exists(out))
em<-read.csv(file.path(reference,'empirical_variograms.csv'));old<-read.csv(file.path(reference,'variogram_candidates.csv'));fits<-list()
for(y in c(2017,2019,2021))for(map in c('dsm','dominant','delta'))for(model in c('spherical','exponential','gaussian')){
 z<-em[em$year==y&em$map==map,];stopifnot(nrow(z)==12,all(z$upper_m-z$lower_m==24))
 fits[[length(fits)+1]]<-fit_one(z,model,'pairs_over_distance_squared',y,map)
}
new<-do.call(rbind,fits);key<-function(z)paste(z$year,z$map,z$model);j<-match(key(new),key(old));stopifnot(!anyNA(j),nrow(new)==27)
fields<-c('nugget','partial_sill','total_sill','fitted_scale_a_m','weighted_mean_squared_residual')
errors<-vapply(fields,function(n)max(abs(new[[n]]-old[[n]][j])),numeric(1))
# CSV round-trip precision perturbs the scalar optimum by approximately 1e-7 m.
# Check scale to 0.00001 m and actual fitted curves to 1e-9 (Mg/ha)^2.
tolerance<-setNames(c(1e-9,1e-9,1e-9,1e-5,1e-12),fields);stopifnot(all(errors<tolerance))
curve_error<-max(vapply(seq_len(nrow(new)),function(i){h<-seq(12,288,12);max(abs(new$nugget[i]+new$partial_sill[i]*shape(h,new$fitted_scale_a_m[i],new$model[i])-old$nugget[j[i]]-old$partial_sill[j[i]]*shape(h,old$fitted_scale_a_m[j[i]],old$model[j[i]])))},numeric(1)))
winners<-function(z)vapply(split(z,interaction(z$year,z$map,drop=TRUE)),function(q)q$model[which.min(q$weighted_mean_squared_residual)],character(1))
stopifnot(curve_error<1e-9,identical(winners(new),winners(old)))
writej(list(status='PASS',fits=27,max_errors=as.list(errors),tolerances=as.list(tolerance),max_curve_error=curve_error,selected_models_identical=TRUE,scope='The portable fitter reproduces archived fits within CSV round-trip precision; new-run variograms remain pending.'),out)
