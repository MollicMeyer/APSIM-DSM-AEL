# Non-AirDry predictors must agree with the approved V13 layer-weighted inputs.
here<-dirname(normalizePath(sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE)),winslash='/'));source(file.path(here,'../R/common.R'))
a<-args_named();soil<-normal(need(a,'soil-inputs'));ref<-normal(need(a,'reference-predictors'));out<-normal(need(a,'out'),FALSE);stopifnot(!file.exists(out))
z<-read.csv(file.path(soil,'depth_predictors.csv'));r<-read.csv(ref);key<-function(d)paste(d$profile_id,d$interval)
stopifnot(!anyDuplicated(key(z)),!anyDuplicated(key(r)));j<-match(key(z),key(r));stopifnot(!anyNA(j))
vars<-c('OC','pH','sand','silt','clay');errors<-vapply(vars,function(v)max(abs(z[[v]]-r[[v]][j])),numeric(1))
stopifnot(all(is.finite(errors)),all(errors<1e-10))
writej(list(status='PASS',profile_depth_rows=nrow(z),max_errors=as.list(errors),reference_md5=md5(ref),scope='Confirms unchanged raw predictors; does not validate full-field yield associations.'),out)
