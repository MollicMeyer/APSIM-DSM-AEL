# Draw the scientific figures with ggplot2, aqp, terra and the original polygons.
library(ggplot2)
library(gridExtra)
library(grid)
library(scales)
library(sf)
library(terra)
library(aqp)
library(lattice)
library(dplyr)
source('settings.R')
figure_directory <- file.path(analysis_directory,'figures')
dir.create(figure_directory,recursive=TRUE,showWarnings=FALSE)
paired <- read.csv(file.path(analysis_directory,'paired_cells.csv'))
paired$DHP <- factor(paired$DHP,levels=c('SU','SH','BS','FS','TS'))
source_colors <- c(DSM='darkgoldenrod4',SSURGO='darkslategray')
year_colors <- c('2017'='#0072B2','2019'='#009E73','2021'='#D55E00')
save_figure <- function(plot,name,width,height) {
  ggsave(file.path(figure_directory,paste0(name,'.pdf')),plot,width=width,height=height)
  ggsave(file.path(figure_directory,paste0(name,'.png')),plot,width=width,height=height,dpi=450)
}
dhp_colors <- c(SU='#E41A1C',SH='#FF7F00',BS='#F5DEB3',FS='#ADD8E6',TS='#984EA3',SSURGO='#4DAF4A')
position_summary <- read.csv(file.path(analysis_directory,'hillslope_summary.csv'))
field_summary <- read.csv(file.path(analysis_directory,'field_summary.csv'))
plots <- list()
theme_original <- theme_minimal(base_size=12)+theme(panel.grid.minor=element_blank(),
  legend.position='none',plot.title=element_text(size=12),axis.text=element_text(size=11))
for(year in c(2017,2019,2021)) {
  z <- paired[paired$year==year,]
  boxes <- rbind(data.frame(DHP=z$DHP,yield=z$dsm),data.frame(DHP='SSURGO',yield=z$dominant))
  boxes$DHP <- factor(boxes$DHP,levels=names(dhp_colors))
  means <- position_summary[position_summary$year==year,]
  means$DHP <- factor(means$DHP,levels=names(dhp_colors)[1:5])
  panel <- match(year,c(2017,2019,2021))*2
  plots[[length(plots)+1]] <- ggplot(boxes,aes(DHP,yield,fill=DHP))+
    geom_boxplot(outlier.size=.5)+scale_fill_manual(values=dhp_colors)+theme_original+
    labs(title=paste0(letters[panel-1],') ',year,' Yield distribution'),x=NULL,
      y=expression(Yield~(Mg~ha^{-1})))
  plots[[length(plots)+1]] <- ggplot(means,aes(DHP,mean_difference,fill=DHP))+
    geom_col(width=.65,colour='black',size=.25)+geom_hline(yintercept=0,colour='red',linetype=2)+
    scale_fill_manual(values=dhp_colors)+theme_original+
    labs(title=paste0(letters[panel],') ',year,' Mean paired difference'),x=NULL,
      y=expression(DSM-SSURGO~(Mg~ha^{-1})))
}
save_drawing <- function(name,width,height,draw) {
  png(file.path(figure_directory,paste0(name,'.png')),width=width,height=height,
    units='in',res=450,type='cairo',bg='white');draw();dev.off()
  cairo_pdf(file.path(figure_directory,paste0(name,'.pdf')),width=width,height=height,
    family='Arial');draw();dev.off()
}
draw_summary <- function() {
  grid.newpage();pushViewport(viewport(layout=grid.layout(4,2,
    heights=unit(c(.8,2.55,2.55,2.55),'null'))))
  pushViewport(viewport(layout.pos.row=1,layout.pos.col=1:2,xscale=c(0,5),yscale=c(-.35,2.1)))
  # Schematic hillslope only, matching the approved V16 layout.
  x <- seq(0,5,length.out=501)
  slope <- ifelse(x<1,0,ifelse(x<2,-.7*(1-cos(pi*(x-1)))/2,
    ifelse(x<3,-.7,ifelse(x<4,-.7*(1+cos(pi*(x-3)))/2,0))))
  y <- 1.2+cumsum(slope)*.01
  grid.lines(unit(.15+.94*x,'native'),unit(y,'native'),gp=gpar(col='black',lwd=2.5))
  for(k in 1:5)grid.text(c('Summit (SU)','Shoulder (SH)','Backslope (BS)',
    'Footslope (FS)','Toeslope (TS)')[k],x=unit(.15+.94*(k-.5),'native'),
    y=unit(y[which.min(abs(x-(k-.5)))]+.5,'native'),gp=gpar(fontsize=11))
  popViewport()
  for(i in seq_along(plots)) {
    pushViewport(viewport(layout.pos.row=(i-1)%/%2+2,layout.pos.col=(i-1)%%2+1))
    grid.draw(ggplotGrob(plots[[i]]));popViewport()
  }
  popViewport()
}
save_drawing('Figure_1_multiyear_yields',7.5,8.5,draw_summary)
field_summary$DHP <- 'Field'
differences <- bind_rows(position_summary %>% select(year,DHP,mean_difference,SD_difference),
  field_summary %>% select(year,DHP,mean_difference))
field_SD <- paired %>% group_by(year) %>% summarise(SD_difference=sd(delta),.groups='drop')
differences$SD_difference[differences$DHP=='Field'] <- field_SD$SD_difference[
  match(differences$year[differences$DHP=='Field'],field_SD$year)]
differences$DHP <- factor(differences$DHP,levels=c('SU','SH','BS','FS','TS','Field'))
differences$year <- factor(differences$year)
p <- ggplot(differences,aes(DHP,mean_difference,fill=year))+geom_hline(yintercept=0,colour='grey35')+
  geom_col(width=.65)+geom_errorbar(aes(ymin=mean_difference-SD_difference,
    ymax=mean_difference+SD_difference),width=.15)+facet_wrap(~year,ncol=1)+
  scale_fill_manual(values=year_colors)+theme_bw(base_size=19)+
  theme(legend.position='none',panel.grid.minor=element_blank())+
  labs(x=NULL,y=expression(DSM-SSURGO~yield~difference~(Mg~ha^{-1})))
save_figure(p,'Figure_S2_DHP_differences',9,10)

# Native-grid statistics are already complete. Cubic convolution changes display only.
polygons <- sf::st_read('data/SSURGO/delineations.geojson',quiet=TRUE)
names(polygons)[names(polygons)=='MUKEY'] <- 'mukey'
polygons$mukey <- as.character(polygons$mukey)
# Approved cartographic positions, not soil or yield observations.
label_positions <- read.csv('data/SSURGO/label_positions.csv')
box <- sf::st_bbox(polygons)
yield_palette <- c('#C75D36','#E7A315','#BCE500','#31CE26','#159370','#0C377C')
legend <- function(p) {
  grobs <- ggplotGrob(p)$grobs
  grobs[[which(vapply(grobs,function(x)x$name,character(1))=='guide-box')]]
}
map_base <- function() {
  ggplot()+coord_sf(xlim=c(box['xmin']-5,box['xmax']+30),
    ylim=c(box['ymin']-45,box['ymax']+10),expand=FALSE)+theme_void(base_size=16)+
    theme(plot.title=element_text(size=18,face='bold',hjust=.5),legend.position='bottom',
      legend.title=element_text(size=16,hjust=.5),legend.text=element_text(size=15),
      plot.margin=margin(4,4,4,4))
}
map_annotation <- function(p,scale_bar=TRUE) {
  if(scale_bar)p <- p+
    annotate('segment',x=box['xmin']+20,xend=box['xmin']+220,y=box['ymin']-18,yend=box['ymin']-18,size=1)+
    annotate('segment',x=box['xmin']+c(20,120,220),xend=box['xmin']+c(20,120,220),
      y=box['ymin']-21,yend=box['ymin']-15,size=.4)+
    annotate('text',x=box['xmin']+c(20,120,220),y=box['ymin']-34,label=c('0','100','200 m'),size=4)
  p+
    annotate('segment',x=box['xmax']+16,xend=box['xmax']+16,y=box['ymax']-65,yend=box['ymax']-20,
      arrow=grid::arrow(length=unit(.1,'inches')),size=.5)+
    annotate('text',x=box['xmax']+16,y=box['ymax']-8,label='N',size=4)
}
map_rows <- list()
for(year in c(2017,2019,2021)) {
  z <- paired[paired$year==year,]
  values <- unique(z[c('mukey','dominant')])
  survey <- merge(polygons,values,by='mukey')
  text <- label_positions[label_positions$year==year,]
  text$mukey <- as.character(text$mukey)
  text <- sf::st_as_sf(merge(text,values,by='mukey'),coords=c('x','y'),crs=32615)
  native <- terra::rast(z[c('x','y','dsm')],type='xyz',crs='EPSG:32615')
  difference <- terra::rast(z[c('x','y','delta')],type='xyz',crs='EPSG:32615')
  display_grid <- terra::rast(terra::ext(native),resolution=3,crs=terra::crs(native))
  displayed <- terra::mask(terra::resample(native,display_grid,method='cubic'),terra::vect(polygons))
  displayed_difference <- terra::mask(terra::resample(difference,display_grid,method='cubic'),terra::vect(polygons))
  names(displayed) <- 'yield';names(displayed_difference) <- 'difference'
  a <- as.data.frame(displayed,xy=TRUE,na.rm=TRUE)
  b <- as.data.frame(displayed_difference,xy=TRUE,na.rm=TRUE)
  limits <- quantile(c(z$dsm,z$dominant),c(.05,.95))
  limits <- c(floor(limits[1]/.05),ceiling(limits[2]/.05))*.05
  dlimits <- quantile(z$delta,c(.05,.95))
  dlimits <- c(floor(dlimits[1]/.05),ceiling(dlimits[2]/.05))*.05
  yield_scale <- scale_fill_gradientn(colours=yield_palette,limits=limits,oob=scales::squish,
    breaks=c(limits[1],mean(limits),limits[2]),labels=function(x)sprintf('%.2f',x),
    name=expression(Yield~(Mg~ha^{-1})),guide=guide_colourbar(title.position='top',title.hjust=.5,
      barwidth=unit(9,'inches'),barheight=unit(.27,'inches')))
  pss <- map_annotation(map_base()+geom_sf(data=survey,aes(fill=dominant),colour='black',size=.25)+
    geom_sf_text(data=text,aes(label=sprintf('%.2f',dominant)),colour='black',size=4.8,fontface='bold',check_overlap=TRUE)+
    geom_sf_text(data=text,aes(label=sprintf('%.2f',dominant)),colour='white',size=4.2,fontface='bold',check_overlap=TRUE)+
    yield_scale+labs(title=paste0(letters[match(year,c(2017,2019,2021))*3-2],') ',year,' SSURGO')),scale_bar=FALSE)
  pdsm <- map_annotation(map_base()+geom_raster(data=a,aes(x,y,fill=yield))+
    geom_sf(data=polygons,fill=NA,colour='black',size=.25)+yield_scale+
    labs(title=paste0(letters[match(year,c(2017,2019,2021))*3-1],') ',year,' DSM')))
  # Place white at zero, including years dominated by one sign of the difference.
  anchors <- c(min(dlimits[1],0),0,max(dlimits[2],0))
  difference_scale <- if(dlimits[1]>=0) {
    scale_fill_gradient(low='#c6dbef',high='#08519c',limits=dlimits,oob=scales::squish,
      breaks=c(dlimits[1],mean(dlimits),dlimits[2]),labels=function(x)sprintf('%.2f',x))
  } else if(dlimits[2]<=0) {
    scale_fill_gradient(low='#a50f15',high='#fee5d9',limits=dlimits,oob=scales::squish,
      breaks=c(dlimits[1],mean(dlimits),dlimits[2]),labels=function(x)sprintf('%.2f',x))
  } else scale_fill_gradientn(colours=c('#a50f15','white','#08519c'),
    values=scales::rescale(anchors,from=dlimits),limits=dlimits,oob=scales::squish,
    breaks=c(dlimits[1],mean(dlimits),dlimits[2]),labels=function(x)sprintf('%.2f',x))
  pdiff <- map_annotation(map_base()+geom_raster(data=b,aes(x,y,fill=difference))+
    geom_sf(data=polygons,fill=NA,colour='black',size=.25)+difference_scale+
    labs(title=paste0(letters[match(year,c(2017,2019,2021))*3],') ',year,' DSM minus SSURGO'),
      fill=expression(DSM-SSURGO~(Mg~ha^{-1})))+
    guides(fill=guide_colourbar(title.position='top',title.hjust=.5,
      barwidth=unit(4.3,'inches'),barheight=unit(.27,'inches'))))
  map_rows[[as.character(year)]] <- gridExtra::arrangeGrob(pss+theme(legend.position='none'),
    pdsm+theme(legend.position='none'),pdiff+theme(legend.position='none'),legend(pdsm),legend(pdiff),
    layout_matrix=rbind(c(1,2,3),c(4,4,5)),heights=c(1,.18))
}
save_figure(gridExtra::arrangeGrob(grobs=map_rows,ncol=1),'Figure_2_multiyear_maps',16,15.2)

# Keep the original aqp depth summary and the author's source colors.
dsm <- readRDS('results/DSM/profiles.rds');ssurgo <- readRDS('results/SSURGO/profiles.rds')
aqp::site(dsm)$product <- 'DSM';aqp::site(ssurgo)$product <- 'SSURGO'
combined <- aqp::combine(dsm,ssurgo)
mean.and.sd <- function(values) {
  m <- mean(values,na.rm=TRUE);s <- sd(values,na.rm=TRUE)
  c(mean=m,lower=m-s,upper=m+s)
}
slabs <- aqp::slab(combined,product~AirDry+LL15+DUL+SAT+KS+Carbon+BD+silt+clay+PH,
  slab.structure=c(0,5,15,30,60,100,150,200),slab.fun=mean.and.sd)
slabs$depth <- slabs$bottom
surface <- slabs[slabs$top==0,];surface$depth <- 0
slabs <- bind_rows(surface,slabs) %>% arrange(product,variable,depth)
slabs$variable <- factor(slabs$variable,
  levels=c('Carbon','BD','silt','clay','PH','AirDry','LL15','DUL','SAT','KS'))
property_labels <- c(Carbon='Organic~C~(\"%\")',BD='BD~(g~cm^{-3})',
  silt='Silt~(\"%\")',clay='Clay~(\"%\")',PH='pH',
  AirDry='AirDry~(mm~mm^{-1})',LL15='LL15~(mm~mm^{-1})',
  DUL='DUL~(mm~mm^{-1})',SAT='SAT~(mm~mm^{-1})',KS='K[sat]~(mm~d^{-1})')
write.csv(slabs,file.path(figure_directory,'depth_function_data.csv'),row.names=FALSE)
slabs$product <- factor(slabs$product,levels=c('DSM','SSURGO'))
depth_plot <- lattice::xyplot(depth~mean|variable,data=slabs,lower=slabs$lower,upper=slabs$upper,
  groups=product,sync.colors=TRUE,alpha=.5,ylim=c(200,0),layout=c(5,2),
  ylab='Depth (cm)',xlab='',scales=list(x=list(tick.number=2,relation='free',cex=1),
    y=list(at=seq(0,200,50),alternating=1,cex=1.15)),
  par.settings=list(fontsize=list(text=16,points=12),
    superpose.line=list(lwd=2,col=unname(source_colors),lty=c(2,2))),
  panel=aqp::panel.depth_function,prepanel=aqp::prepanel.depth_function,
  strip=lattice::strip.custom(bg=grey(.8),factor.levels=expression(OC~'%',BD~g~cm^{-3},
    Silt~'%',Clay~'%',pH,AirDry~mm~mm^{-1},LL15~mm~mm^{-1},DUL~mm~mm^{-1},
    SAT~mm~mm^{-1},KSAT~mm~day^{-1})),
  auto.key=list(columns=2,lines=TRUE,points=FALSE,cex=1.25))
save_drawing('Figure_3_depth_functions',11,8.5,function()print(depth_plot))

r2 <- read.csv(file.path(analysis_directory,'soil_yield_associations.csv'))
r2$property <- factor(r2$property,levels=rev(c('OC','pH','sand','silt','clay')))
r2$year <- factor(r2$year)
r2$product <- factor(r2$product,levels=c('DSM','SSURGO'))
r2$interval <- factor(r2$interval,levels=c('0-30 cm','30-100 cm'))
association_plot <- function(z,both=FALSE) {
  p <- ggplot(z,aes(R2,property,fill=year))+geom_col(position=position_dodge(.8),width=.72)+
    scale_fill_manual(values=year_colors)+scale_x_continuous(limits=c(0,1),breaks=c(0,.25,.5,.75,1))+
    labs(x=expression(R^2),y=NULL,fill=NULL)+theme_bw(base_size=19)+
    theme(legend.position='bottom',panel.grid.major.y=element_blank(),
      panel.grid.minor=element_blank(),panel.spacing=unit(1.4,'lines'),
      strip.text=element_text(size=17,margin=margin(8,8,8,8)),
      axis.title.x=element_text(margin=margin(t=12)),plot.margin=margin(14,16,14,16))
  if(both)p+facet_grid(product~interval)else p+facet_wrap(~product,nrow=1)
}
save_figure(association_plot(r2[r2$interval=='0-30 cm',]),'Figure_4_raw_topsoil_R2',9,6.8)
save_figure(association_plot(r2,TRUE),'Figure_S5_topsoil_subsoil_R2',11,11)

v <- read.csv(file.path(analysis_directory,'empirical_variograms.csv'))
fit <- read.csv(file.path(analysis_directory,'fitted_variograms.csv'))
parameters <- read.csv(file.path(analysis_directory,'variogram_parameters.csv'))
draw_variograms <- function() {
  par(mfrow=c(3,3),mar=c(3.7,4.2,2.1,.6),mgp=c(2.6,.8,0),cex=1.15)
  for(year in c(2017,2019,2021))for(map in c('dsm','dominant','delta')) {
    z <- v[v$year==year&v$map==map,]
    curve <- fit[fit$year==year&fit$map==map,]
    parameter <- parameters[parameters$year==year&parameters$map==map,]
    plot(z$dist,z$gamma,pch=16,xlim=c(0,300),ylim=c(0,max(z$gamma,parameter$total_sill)*1.1),
      xlab='Lag distance (m)',ylab=expression(Semivariance~(Mg~ha^{-1})^2),
      main=paste(year,c(dsm='DSM',dominant='SSURGO',delta='Difference')[[map]]))
    # Lines are gstat's fitted curves, not a separate variogram implementation.
    lines(curve$dist,curve$gamma,col='#0072B2',lwd=2)
    abline(h=parameter$total_sill,lty=3,col='grey50')
    abline(v=parameter$practical_range95_m,lty=3,col='#D55E00')
  }
}
save_drawing('Figure_S3_variograms',12,11,draw_variograms)
writeLines(capture.output(sessionInfo()),file.path(figure_directory,'sessionInfo.txt'))
