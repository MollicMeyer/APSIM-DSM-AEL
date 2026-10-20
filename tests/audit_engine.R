# Read-only audit against the archived complete engine-tree manifest.
here<-dirname(normalizePath(sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE)),winslash='/'));source(file.path(here,'../R/common.R'))
a<-args_named();root<-normal(need(a,'engine-dir'));manifest<-normal(need(a,'manifest'));out<-normal(need(a,'out'),FALSE)
stopifnot(!file.exists(out));m<-read.csv(manifest)
stopifnot(all(!grepl('(^/|^[A-Za-z]:|(^|[/\\\\])[.][.]([/\\\\]|$))',m$relative_path)))
paths<-file.path(root,m$relative_path);stopifnot(all(file.exists(paths)))
observed<-unname(tools::md5sum(paths));stopifnot(all(observed==m$md5),all(file.info(paths)$size==m$bytes))
writej(list(status='PASS',files=nrow(m),manifest_md5=md5(manifest),engine_root=root,checked_at=as.character(Sys.time()),scope='Every file listed in the archived engine tree matches bytes and MD5. Additional unlisted files were not evaluated.'),out)
cat('Engine tree audit PASS:',nrow(m),'files\n')
