# Verify that the lower-memory renderer does not alter native numeric comparisons.
here<-dirname(normalizePath(sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE)),winslash='/'));source(file.path(here,'../R/common.R'))
a<-args_named();candidate<-normal(need(a,'candidate'));reference<-normal(need(a,'reference'));repeat_render<-normal(need(a,'repeat-render'));out<-normal(need(a,'out'),FALSE);stopifnot(!file.exists(out))
x<-read.csv(file.path(candidate,'native_map_audit.csv'));z<-read.csv(file.path(reference,'map_percentile_audit.csv'));j<-match(x$year,z$year)
fields<-c('yield_p05','yield_p95','delta_p05','delta_p95','native_cells','mean_delta','native_delta_min','native_delta_max')
errors<-vapply(fields,function(n)max(abs(x[[n]]-z[[n]][j])),numeric(1));stopifnot(all(errors<1e-10))
pngs<-list.files(candidate,pattern='[.]png$');stopifnot(length(pngs)==4,all(unname(tools::md5sum(file.path(candidate,pngs)))==unname(tools::md5sum(file.path(repeat_render,pngs)))))
st<-fromJSON(file.path(candidate,'status.json'));stopifnot(st$source_vertex_count==600,st$native_resolution_m==12,st$display_resolution_m==3,!st$external_callouts)
writej(list(status='PASS',native_max_errors=as.list(errors),repeat_render_PNGs_identical=TRUE,source_vertices=600,scope='Native color limits and statistics preserved; deterministic capped-helper repeat; original rings retained'),out)
