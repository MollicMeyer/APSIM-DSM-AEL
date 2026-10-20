here<-dirname(normalizePath(sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE)),winslash='/'));source(file.path(here,'../R/common.R'))
a<-args_named();out<-normal(need(a,'out'),FALSE);stopifnot(!dir.exists(out));dir.create(out,recursive=TRUE)
expressions<-parse(file.path(here,'../R/run.R'))
definition<-Filter(function(x)is.call(x)&&identical(x[[1]],as.name('<-'))&&identical(x[[2]],as.name('finish_lock')),as.list(expressions));stopifnot(length(definition)==1)
testcase<-function(name,foreign=FALSE,extra=FALSE){
 env<-new.env(parent=globalenv());env$root<-file.path(out,name);dir.create(env$root);env$lock<-file.path(env$root,'RUNNING.lock');dir.create(env$lock)
 writej(list(pid=if(foreign)-1 else Sys.getpid()),file.path(env$lock,'owner.json'))
 if(extra)writej(list(preserve=TRUE),file.path(env$lock,'extra.json'))
 eval(definition[[1]],envir=env);res<-try(env$finish_lock(),silent=TRUE)
 if(extra)stopifnot(inherits(res,'try-error'),file.exists(file.path(env$lock,'owner.json')),file.exists(file.path(env$lock,'extra.json')))else if(foreign)stopifnot(dir.exists(env$lock),file.exists(file.path(env$lock,'owner.json')))else stopifnot(!dir.exists(env$lock))
 TRUE
}
checks<-c(owned_empty=testcase('owned_empty'),foreign_owner=testcase('foreign_owner',foreign=TRUE),unexpected_content=testcase('unexpected_content',extra=TRUE))
writej(list(status='PASS',checks=as.list(checks)),file.path(out,'QA.json'))
