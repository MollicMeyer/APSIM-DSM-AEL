# Same native values and source vertices; avoid loading sf/raster solely for plotting.
here<-dirname(normalizePath(sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE)),winslash='/'));source(file.path(here,'common.R'))
a<-args_named();paired<-normal(need(a,'paired'));polygon_path<-normal(need(a,'polygons'));out<-normal(need(a,'out'),FALSE)
# The capped caller checks1536MB BEFORE starting R; repeating that check after R's
# own allocation would incorrectly turn a pre-launch guard into a post-launch guard.
library(terra)
stopifnot(!dir.exists(out));st<-fromJSON(file.path(paired,'status.json'));stopifnot(st$status=='PAIRED_SUMMARIES_COMPLETE_FIGURES_PENDING',st$cells==1943)
stopifnot(all(unname(tools::md5sum(file.path(paired,st$artifact_hashes$path)))==st$artifact_hashes$md5))
stopifnot(as.integer(tail(ps('@(Get-Process Models -ErrorAction SilentlyContinue).Count'),1))==0)
if(!identical(a[['resource-job']],'yes')&&as.numeric(tail(ps('(Get-CimInstance Win32_OperatingSystem).FreePhysicalMemory'),1))/1024<1536)stop('At least1536MB free RAM required for map rendering')
for(e in parse(file.path(here,'plot_maps.R')))if(is.call(e)&&identical(e[[1]],as.name('<-'))&&as.character(e[[2]])%in%c('save_plot','diffcols','getcol','draw_raster','panel_legend','render_year'))eval(e)
years<-c(2017,2019,2021);d<-do.call(rbind,lapply(years,function(y)read.csv(file.path(paired,paste0('paired_cells_',y,'.csv')))))
geo<-fromJSON(polygon_path,simplifyVector=FALSE);stopifnot(grepl('32615',geo$crs$properties$name));pieces<-list()
for(f in geo$features){polys<-if(f$geometry$type=='MultiPolygon')f$geometry$coordinates else list(f$geometry$coordinates);for(pol in polys){pieces[[length(pieces)+1]]<-list(mukey=as.character(f$properties$MUKEY),rings=lapply(pol,function(r)do.call(rbind,lapply(r,unlist))))}}
coords<-do.call(rbind,lapply(pieces,function(p)do.call(rbind,p$rings)));bb<-c(xmin=min(coords[,1]),xmax=max(coords[,1]),ymin=min(coords[,2]),ymax=max(coords[,2]))
inside_ring<-function(x,y,r){ans<-rep(FALSE,length(x));j<-nrow(r);for(i in seq_len(nrow(r))){cross<-(r[i,2]>y)!=(r[j,2]>y);hit<-cross&(x<(r[j,1]-r[i,1])*(y-r[i,2])/(r[j,2]-r[i,2]+1e-100)+r[i,1]);ans<-xor(ans,hit);j<-i};ans}
inside<-function(x,y,rings){v<-inside_ring(x,y,rings[[1]]);if(length(rings)>1)for(h in rings[-1])v<-v&!inside_ring(x,y,h);v}
border_dist<-function(x,y,rings){dist<-rep(Inf,length(x));for(r in rings)for(k in seq_len(nrow(r)-1)){v<-r[k+1,]-r[k,];den<-sum(v*v);if(den==0)next;t<-pmax(0,pmin(1,((x-r[k,1])*v[1]+(y-r[k,2])*v[2])/den));dist<-pmin(dist,sqrt((x-r[k,1]-t*v[1])^2+(y-r[k,2]-t*v[2])^2))};dist}
for(k in seq_along(pieces)){
 r<-pieces[[k]]$rings[[1]];g<-expand.grid(x=seq(min(r[,1]),max(r[,1]),length.out=20),y=seq(min(r[,2]),max(r[,2]),length.out=20));g<-g[inside(g$x,g$y,pieces[[k]]$rings),]
 if(nrow(g)){g<-g[order(border_dist(g$x,g$y,pieces[[k]]$rings),decreasing=TRUE),]};pieces[[k]]$candidates<-g
}
dir.create(out,recursive=TRUE);yieldcols<-c('#C75D36','#E7A315','#BCE500','#31CE26','#159370','#0C377C');pal<-colorRampPalette(yieldcols)(256);field<-terra::vect(polygon_path);maps<-list();audit<-list();label_audit<-list()
for(y in years){z<-d[d$year==y,];z$delta<-z$dsm-z$dominant;val<-unique(z[c('mukey','dominant')]);stopifnot(nrow(val)==6)
 nr<-terra::rast(z[,c('x','y','dsm')],type='xyz',crs='EPSG:32615');nd<-terra::rast(z[,c('x','y','delta')],type='xyz',crs='EPSG:32615');stopifnot(all(res(nr)==12))
 display<-terra::rast(ext(nr),resolution=3,crs=crs(nr));rr<-mask(resample(nr,display,method='cubic'),field);dr<-mask(resample(nd,display,method='cubic'),field)
 yl<-as.numeric(quantile(c(z$dsm,z$dominant),c(.05,.95)));dl<-as.numeric(quantile(z$delta,c(.05,.95)));yl<-c(floor(yl[1]/.05)*.05,ceiling(yl[2]/.05)*.05);dl<-c(floor(dl[1]/.05)*.05,ceiling(dl[2]/.05)*.05)
 maps[[as.character(y)]]<-list(rr=rr,dr=dr,yl=yl,dl=dl,dc=diffcols(dl),values=setNames(val$dominant,val$mukey))
 audit[[as.character(y)]]<-data.frame(year=y,yield_p05=yl[1],yield_p95=yl[2],delta_p05=dl[1],delta_p95=dl[2],native_cells=nrow(z),mean_delta=mean(z$delta),native_delta_min=min(z$delta),native_delta_max=max(z$delta))
}
draw_polygons<-function(md,fill=FALSE){for(p in pieces){xy<-do.call(rbind,lapply(p$rings,function(r)rbind(r,c(NA,NA))));xy<-xy[-nrow(xy),,drop=FALSE];polypath(xy[,1],xy[,2],col=if(fill)getcol(md$values[p$mukey],md$yl,pal)else NA,border=if(fill)NA else 'black',lwd=.8,rule='evenodd')}}
panel_map<-function(y,j){md<-maps[[as.character(y)]];par(mar=c(2.3,.25,2.1,.25),family='Arial',xaxs='i',yaxs='i');plot.new();plot.window(xlim=c(bb['xmin']-8,bb['xmax']+35),ylim=c(bb['ymin']-36,bb['ymax']+5),asp=1)
 if(j==1)draw_polygons(md,TRUE);if(j==2)draw_raster(md$rr,md$yl,pal);if(j==3)draw_raster(md$dr,md$dl,md$dc);draw_polygons(md)
 title(main=paste0(letters[(match(y,years)-1)*3+j],') ',y,' ',c('SSURGO','DSM','DSM minus SSURGO')[j]),cex.main=1.65)
 if(j==1)for(k in seq_along(pieces)){p<-pieces[[k]];g<-p$candidates;if(!nrow(g))next;label<-sprintf('%.2f',md$values[p$mukey]);chosen<-NULL
  for(sz in c(1.5,1.35,1.2,1.05)){hw<-strwidth(label,cex=sz,font=2)/2+1;hh<-strheight(label,cex=sz,font=2)/2+1;fits<-rep(TRUE,nrow(g));for(dx in c(-hw,hw))for(dy in c(-hh,hh))fits<-fits&inside(g$x+dx,g$y+dy,p$rings);if(any(fits)){chosen<-g[which(fits)[1],];break}}
  if(is.null(chosen))next
  for(dx in c(-1,0,1))for(dy in c(-1,0,1))if(dx||dy)text(chosen$x+dx*1.15,chosen$y+dy*1.15,label,col='black',cex=sz,font=2);text(chosen$x,chosen$y,label,col='white',cex=sz,font=2)
  label_audit[[length(label_audit)+1]]<<-data.frame(year=y,piece=k,mukey=p$mukey,x=chosen$x,y=chosen$y,cex=sz)
 }
 sx<-bb['xmin']+20;sy<-bb['ymin']-18;if(j!=1){segments(sx,sy,sx+200,sy,lwd=2);segments(sx+c(0,100,200),sy-3,sx+c(0,100,200),sy+3);text(sx+c(0,100,200),sy-14,c('0','100','200 m'),cex=1.2,xpd=NA)}
 nx<-bb['xmax']+18;ny<-bb['ymax']-85;arrows(nx,ny,nx,ny+50,length=.08);text(nx,ny+66,'N')
}
for(y in years)save_plot(paste0('Figure_2_maps_',y),12,4.2,function()render_year(y))
save_plot('Figure_2_multiyear_maps',12,11.4,function(){lay<-do.call(rbind,lapply(0:2,function(i)rbind(c(1,2,3)+5*i,c(4,4,5)+5*i)));layout(lay,heights=rep(c(1,.27),3));for(y in years){for(j in 1:3)panel_map(y,j);panel_legend(y);panel_legend(y,TRUE)}})
write.csv(do.call(rbind,audit),file.path(out,'native_map_audit.csv'),row.names=FALSE);write.csv(unique(do.call(rbind,label_audit)),file.path(out,'interior_labels.csv'),row.names=FALSE)
writej(list(status='MAPS_RENDERED_VISUAL_QA_PENDING',renderer='base polygon paths from original GeoJSON plus terra cubic display',source_vertex_count=nrow(coords),polygon_md5=md5(polygon_path),native_resolution_m=12,display_resolution_m=3,external_callouts=FALSE,guard_MB=1536),file.path(out,'status.json'));capture.output(sessionInfo(),file=file.path(out,'sessionInfo.txt'))
