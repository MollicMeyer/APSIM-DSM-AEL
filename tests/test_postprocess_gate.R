# A live, incomplete experiment must not create an analysis directory or launch stages.
here<-dirname(normalizePath(sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE)),winslash='/'));source(file.path(here,'../R/common.R'))
a<-args_named();experiment<-normal(need(a,'experiment'));index<-normal(need(a,'cell-index'));polygons<-normal(need(a,'polygons'));climate<-normal(need(a,'climate-source'));out<-normal(need(a,'out'),FALSE)
p<-fromJSON(file.path(experiment,'progress.json'));stopifnot(p$completed<2036,!dir.exists(out));dir.create(out,recursive=TRUE)
target<-file.path(out,'must_not_exist')
code<-system2(file.path(R.home('bin'),'Rscript.exe'),c(shQuote(normal(file.path(here,'../R/postprocess.R'))),'--experiment',shQuote(experiment),'--cell-index',shQuote(index),'--polygons',shQuote(polygons),'--climate-source',shQuote(climate),'--out',shQuote(target)),stdout=file.path(out,'stdout.txt'),stderr=file.path(out,'stderr.txt'))
stopifnot(code!=0,!dir.exists(target))
writej(list(status='PASS',incomplete_experiment_rejected=TRUE,no_output_created=TRUE,child_exit=code,scope='Completion gate only; full analysis and rendering integration remain pending.'),file.path(out,'QA.json'))
