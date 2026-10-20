# Approved V11/V13 map presentation, parameterized for verified paired results.
here<-dirname(normalizePath(sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE)),winslash='/'));source(file.path(here,'common.R'))
library(sf);library(terra)
a<-args_named();paired<-normal(need(a,'paired'));polygon_path<-normal(need(a,'polygons'));out<-normal(need(a,'out'),FALSE)
stopifnot(!dir.exists(out))
st<-fromJSON(file.path(paired,'status.json'));historical<-identical(a[['historical']],'yes')
stopifnot(st$cells==1943,isTRUE(st$historical)==historical,st$status==if(historical)'HISTORICAL_PAIRED_SUMMARIES_COMPLETE'else'PAIRED_SUMMARIES_COMPLETE_FIGURES_PENDING')
stopifnot(!is.null(st$artifact_hashes),all(unname(tools::md5sum(file.path(paired,st$artifact_hashes$path)))==st$artifact_hashes$md5))
# Render after simulations, not alongside a memory-constrained model batch.
stopifnot(as.integer(tail(ps('@(Get-Process Models -ErrorAction SilentlyContinue).Count'),1))==0)
if(as.numeric(tail(ps('(Get-CimInstance Win32_OperatingSystem).FreePhysicalMemory'),1))/1024<1536)stop('At least1536MB free RAM required for map rendering')
years<-c(2017,2019,2021)
d<-do.call(rbind,lapply(years,function(y){z<-read.csv(file.path(paired,paste0('paired_cells_',y,'.csv')));stopifnot(nrow(z)==1943,all(z$year==y));z}))
dir.create(out,recursive=TRUE)
save_plot <- function(name,w,h,fun){
 png(file.path(out,paste0(name,'.png')),width=w,height=h,units='in',res=600,type='cairo',bg='white');fun();dev.off()
 cairo_pdf(file.path(out,paste0(name,'.pdf')),width=w,height=h,family='Arial',bg='white');fun();dev.off()
}
yieldcols <- c('#C75D36','#E7A315','#BCE500','#31CE26','#159370','#0C377C')
pal <- colorRampPalette(yieldcols)(256)
diffcols <- function(lim){
 v<-seq(lim[1],lim[2],length.out=256)
 # White always denotes zero. One-signed ranges use pale-to-dark signed colors.
 if(lim[1]>=0)return(colorRampPalette(c('#E5F1FF','#075DA8'))(256))
 if(lim[2]<=0)return(colorRampPalette(c('#A51B25','#FDE4E4'))(256))
 rgb(approx(c(lim[1],0,lim[2]),c(165,255,7),xout=v)$y/255,
     approx(c(lim[1],0,lim[2]),c(27,255,93),xout=v)$y/255,
     approx(c(lim[1],0,lim[2]),c(37,255,168),xout=v)$y/255)
}
getcol<-function(v,lim,cols)cols[1+round(255*pmax(0,pmin(1,(v-lim[1])/diff(lim))))]
draw_raster<-function(r,lim,cols){
 v<-terra::values(r)[,1];cc<-getcol(v,lim,cols);cc[is.na(v)]<-'transparent'
 rr<-as.raster(matrix(cc,nrow=terra::nrow(r),ncol=terra::ncol(r),byrow=TRUE))
 ee<-as.vector(terra::ext(r));rasterImage(rr,ee[1],ee[3],ee[2],ee[4],interpolate=FALSE)
}
maps<-list();audit<-list();distribution<-list()
for(y in years){
 z<-d[d$year==y,];z$delta<-z$dsm-z$dominant
 poly<-st_read(polygon_path,quiet=TRUE)
 stopifnot(st_crs(poly)$epsg==32615,all(st_is_valid(poly)))
 val<-unique(z[c('mukey','dominant')]);stopifnot(nrow(val)==6,!anyDuplicated(val$mukey))
 poly$dominant<-val$dominant[match(as.character(poly$MUKEY),as.character(val$mukey))];stopifnot(all(is.finite(poly$dominant)))
 field<-st_union(poly)
 nr<-terra::rast(z[,c('x','y','dsm')],type='xyz',crs='EPSG:32615')
 nd<-terra::rast(z[,c('x','y','delta')],type='xyz',crs='EPSG:32615')
 stopifnot(all(terra::res(nr)==12),all(terra::res(nd)==12))
 display<-terra::rast(terra::ext(nr),resolution=3,crs=terra::crs(nr))
 rr<-terra::mask(terra::resample(nr,display,method='cubic'),terra::vect(field))
 dr<-terra::mask(terra::resample(nd,display,method='cubic'),terra::vect(field))
 # Limits from equal-area native cells, NOT interpolated pixels. Yield sources equally weighted.
 yl<-as.numeric(quantile(c(z$dsm,z$dominant),c(.05,.95)))
 dl<-as.numeric(quantile(z$delta,c(.05,.95)))
 yl<-c(floor(yl[1]/.05)*.05,ceiling(yl[2]/.05)*.05)
 dl<-c(floor(dl[1]/.05)*.05,ceiling(dl[2]/.05)*.05)
 if(diff(yl)==0)yl<-yl+c(-.025,.025)
 if(diff(dl)==0)dl<-dl+c(-.025,.025)
 vals<-sort(unique(poly$dominant));scol<-getcol(vals,yl,pal);names(scol)<-as.character(vals)
 maps[[as.character(y)]]<-list(poly=poly,rr=rr,dr=dr,yl=yl,dl=dl,vals=vals,scol=scol,dc=diffcols(dl))
 audit[[as.character(y)]]<-data.frame(year=y,yield_p05=yl[1],yield_p95=yl[2],delta_p05=dl[1],delta_p95=dl[2],
  DSM_below=sum(z$dsm<yl[1]),DSM_above=sum(z$dsm>yl[2]),SSURGO_below=sum(z$dominant<yl[1]),SSURGO_above=sum(z$dominant>yl[2]),
  delta_below=sum(z$delta<dl[1]),delta_above=sum(z$delta>dl[2]),native_cells=nrow(z),
  native_delta_min=min(z$delta),native_delta_max=max(z$delta),display_delta_min=min(values(dr),na.rm=TRUE),display_delta_max=max(values(dr),na.rm=TRUE),mean_delta=mean(z$delta),units='Mg/ha')
 distribution[[as.character(y)]]<-data.frame(year=y,percentile=c(0,1,5,25,50,75,95,99,100),delta_Mg_ha=as.numeric(quantile(z$delta,c(0,.01,.05,.25,.5,.75,.95,.99,1))))
}
panel_map<-function(y,j){
 md<-maps[[as.character(y)]];bb<-st_bbox(md$poly)
 par(mar=c(2.3,.25,2.1,.25),family='Arial',xaxs='i',yaxs='i')
 plot.new();plot.window(xlim=c(bb['xmin']-8,bb['xmax']+35),ylim=c(bb['ymin']-36,bb['ymax']+5),asp=1)
 if(j==1)plot(st_geometry(md$poly),add=TRUE,col=md$scol[as.character(md$poly$dominant)],border=NA)
 if(j==2)draw_raster(md$rr,md$yl,pal)
 if(j==3)draw_raster(md$dr,md$dl,md$dc)
 plot(st_geometry(md$poly),add=TRUE,col=NA,border='black',lwd=.8)
 title(main=paste0(letters[(match(y,years)-1)*3+j],') ',y,' ',c('SSURGO','DSM','DSM minus SSURGO')[j]),cex.main=1.65)
 if(j==1){
  pieces<-suppressWarnings(st_cast(md$poly,'POLYGON'))
  # Place labels well inside concave delineations rather than at edge-prone centroids.
  candidates<-lapply(seq_len(nrow(pieces)),function(k){
   candidate<-suppressWarnings(st_sample(st_geometry(pieces[k,]),size=300,type='regular'))
   candidate<-c(candidate,suppressWarnings(st_point_on_surface(st_geometry(pieces[k,]))))
   distance<-as.numeric(st_distance(candidate,st_boundary(st_geometry(pieces[k,]))))
   candidate[order(distance,decreasing=TRUE)]
  })
  labels<-sprintf('%.2f',pieces$dominant)
  placed<-list();callouts<-0
  for(k in seq_len(nrow(pieces))){
   done<-FALSE
   for(sz in c(1.5,1.35,1.2,1.05)){
    hw<-strwidth(labels[k],cex=sz,font=2)/2+1;hh<-strheight(labels[k],cex=sz,font=2)/2+1
    coords<-st_coordinates(candidates[[k]])
    boxes<-st_sfc(lapply(seq_len(nrow(coords)),function(i){x<-coords[i,1];yy<-coords[i,2];st_polygon(list(rbind(c(x-hw,yy-hh),c(x+hw,yy-hh),c(x+hw,yy+hh),c(x-hw,yy+hh),c(x-hw,yy-hh))))}),crs=st_crs(pieces))
    fits<-lengths(st_covered_by(boxes,st_geometry(pieces[k,])))>0
    if(any(fits)){a<-coords[which(fits)[1],1:2];done<-TRUE;break}
   }
   # Omit tiny clipped edge fragments rather than placing external callout labels.
   if(!done)next
   for(dx in c(-1,0,1))for(dy in c(-1,0,1))if(dx||dy)text(a[1]+dx*1.15,a[2]+dy*1.15,labels[k],col='black',cex=sz,font=2)
   text(a[1],a[2],labels[k],col='white',cex=sz,font=2)
  }
 }
 sx<-bb['xmin']+20;sy<-bb['ymin']-18
 if(j!=1){segments(sx,sy,sx+200,sy,lwd=2);segments(sx+c(0,100,200),sy-3,sx+c(0,100,200),sy+3)
 text(sx+c(0,100,200),sy-14,c('0','100','200 m'),cex=1.2,xpd=NA)}
 nx<-bb['xmax']+18;ny<-bb['ymax']-85;arrows(nx,ny,nx,ny+50,length=.08);text(nx,ny+66,'N')
}
panel_legend<-function(y,difference=FALSE){
 md<-maps[[as.character(y)]];lim<-if(difference)md$dl else md$yl;cols<-if(difference)md$dc else pal
 par(mar=c(.2,1,.2,1),family='Arial');plot.new();plot.window(xlim=c(0,1),ylim=c(0,1),xaxs='i',yaxs='i')
 text(.5,.86,if(difference)expression(DSM-SSURGO~(Mg~ha^{-1}))else expression(Yield~(Mg~ha^{-1})),cex=1.6)
 rasterImage(as.raster(matrix(cols,1,256)),.06,.34,.94,.63,interpolate=FALSE)
 tt<-seq(lim[1],lim[2],length.out=3);xx<-.06+.88*(tt-lim[1])/diff(lim)
 segments(xx,.34,xx,.24,lwd=1.3)
 for(k in 1:3)text(xx[k],.09,sprintf('%.2f',tt[k]),cex=1.5,adj=c(0,.5,1)[k])
 if(difference&&lim[1]<0&&lim[2]>0)segments(.06+.88*(0-lim[1])/diff(lim),.34,.06+.88*(0-lim[1])/diff(lim),.63,lwd=1)
}
render_year<-function(y){layout(rbind(c(1,2,3),c(4,4,5)),heights=c(1,.27));for(j in 1:3)panel_map(y,j);panel_legend(y);panel_legend(y,TRUE)}
for(y in years)save_plot(paste0('Figure_2_maps_',y),12,4.2,function()render_year(y))
save_plot('Figure_2_multiyear_maps',12,11.4,function(){
 lay<-do.call(rbind,lapply(0:2,function(i)rbind(c(1,2,3)+5*i,c(4,4,5)+5*i)))
 layout(lay,heights=rep(c(1,.27),3));for(y in years){for(j in 1:3)panel_map(y,j);panel_legend(y);panel_legend(y,TRUE)}
})
write.csv(do.call(rbind,audit),file.path(out,'map_percentile_audit.csv'),row.names=FALSE)
write.csv(do.call(rbind,distribution),file.path(out,'native_difference_distribution.csv'),row.names=FALSE)


writej(list(status='MAPS_RENDERED_VISUAL_QA_PENDING',historical=historical,polygon_md5=md5(polygon_path),paired_status_md5=md5(file.path(paired,'status.json')),native_resolution_m=12,display_resolution_m=3,display_resampling='cubic',scope='Native values preserved; display limits saturate tails and do not filter analysis cells. Source polygon vertices unchanged. White SSURGO labels.'),file.path(out,'status.json'))
capture.output(sessionInfo(),file=file.path(out,'sessionInfo.txt'))
