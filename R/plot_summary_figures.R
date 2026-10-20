# Preserve the approved V12/V13 figure styles using the new verified analysis tables.
here<-dirname(normalizePath(sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE)),winslash='/'));source(file.path(here,'common.R'));source(file.path(here,'lib/variogram_fit.R'))
library(ggplot2);library(grid)
a<-args_named();paired<-normal(need(a,'paired'));soil<-normal(need(a,'soil-inputs'));associations<-normal(need(a,'associations'));variograms<-normal(need(a,'variograms'));out<-normal(need(a,'out'),FALSE)
if(!identical(a[['skip-depth']],'yes')){library(aqp);library(lattice)}
stopifnot(!dir.exists(out),fromJSON(file.path(paired,'status.json'))$status=='PAIRED_SUMMARIES_COMPLETE_FIGURES_PENDING',fromJSON(file.path(soil,'status.json'))$status=='SOIL_INPUTS_COMPLETE',fromJSON(file.path(associations,'status.json'))$status=='ASSOCIATIONS_COMPLETE_FIGURES_PENDING',fromJSON(file.path(variograms,'status.json'))$status=='VARIOGRAMS_COMPLETE_FIGURES_PENDING')
stopifnot(as.integer(tail(ps('@(Get-Process Models -ErrorAction SilentlyContinue).Count'),1))==0)
if(as.numeric(tail(ps('(Get-CimInstance Win32_OperatingSystem).FreePhysicalMemory'),1))/1024<1536)stop('At least1536MB free RAM required for figure rendering')
dir.create(out,recursive=TRUE);years<-c(2017,2019,2021)
saveplot<-function(n,w,h,fun){png(file.path(out,paste0(n,'.png')),width=w,height=h,units='in',res=450,type='cairo',bg='white');fun();dev.off();cairo_pdf(file.path(out,paste0(n,'.pdf')),width=w,height=h,family='Arial');fun();dev.off()}
d<-do.call(rbind,lapply(years,function(y)read.csv(file.path(paired,paste0('paired_cells_',y,'.csv')))))
ds<-read.csv(file.path(paired,'descriptive_summary.csv'));stopifnot(nrow(d)==1943*3,nrow(ds)==18)
dhpcols<-c(SU='#E41A1C',SH='#FF7F00',BS='#F5DEB3',FS='#ADD8E6',TS='#984EA3',SSURGO='#4DAF4A')
yearcols<-c('2017'='#0072B2','2019'='#009E73','2021'='#D55E00')
# Exact source colors in the author's R/APSIM2/Depthfunctions.R (lines100/115).
sourcecols<-c(DSM='darkgoldenrod4',SSURGO='darkslategray')
plots<-list();theme_orig<-theme_minimal(base_size=12)+theme(panel.grid.minor=element_blank(),legend.position='none',plot.title=element_text(size=12),axis.text=element_text(size=11))
for(y in years){
 dd<-d[d$year==y,];b<-rbind(data.frame(DHP=dd$DHP,Yield=dd$dsm),data.frame(DHP='SSURGO',Yield=dd$dominant));b$DHP<-factor(b$DHP,levels=names(dhpcols));ss<-ds[ds$year==y&ds$DHP!='Field',];ss$DHP<-factor(ss$DHP,levels=names(dhpcols)[1:5])
 plots[[length(plots)+1]]<-ggplot(b,aes(DHP,Yield,fill=DHP))+geom_boxplot(outlier.size=.5)+scale_fill_manual(values=dhpcols)+labs(title=paste(paste0(letters[match(y,years)*2-1],')'),y,'Yield distribution'),x=NULL,y=expression(Yield~(Mg~ha^{-1})))+theme_orig
 plots[[length(plots)+1]]<-ggplot(ss,aes(DHP,paired_difference,fill=DHP))+geom_col(width=.65,colour='black',size=.25)+geom_hline(yintercept=0,colour='red',linetype=2)+scale_fill_manual(values=dhpcols)+labs(title=paste(paste0(letters[match(y,years)*2],')'),y,'Mean paired difference'),x=NULL,y=expression(DSM-SSURGO~(Mg~ha^{-1})))+theme_orig
}
drawsummary<-function(){
 grid.newpage();pushViewport(viewport(layout=grid.layout(4,2,heights=unit(c(.8,2.55,2.55,2.55),'null'))))
 pushViewport(viewport(layout.pos.row=1,layout.pos.col=1:2,xscale=c(0,5),yscale=c(-.35,2.1)))
 # Original conceptual profile, not reproduced artwork. Definitions: Miller and Schaetzl (2015).
 xx<-seq(0,5,length.out=501);slope<-ifelse(xx<1,0,ifelse(xx<2,-.7*(1-cos(pi*(xx-1)))/2,ifelse(xx<3,-.7,ifelse(xx<4,-.7*(1+cos(pi*(xx-3)))/2,0))))
 yy<-1.2+cumsum(slope)*.01
 grid.lines(unit(.15+.94*xx,'native'),unit(yy,'native'),gp=gpar(col='black',lwd=2.5))
 for(k in 1:5){gx<-.15+.94*(k-.5);gy<-yy[which.min(abs(xx-(k-.5)))];grid.text(c('Summit (SU)','Shoulder (SH)','Backslope (BS)','Footslope (FS)','Toeslope (TS)')[k],x=unit(gx,'native'),y=unit(gy+.50,'native'),gp=gpar(fontsize=11,col='black'))}
 popViewport()
 for(i in seq_along(plots)){pushViewport(viewport(layout.pos.row=(i-1)%/%2+2,layout.pos.col=(i-1)%%2+1));grid.draw(ggplotGrob(plots[[i]]));popViewport()};popViewport()
}
saveplot('Figure_1_multiyear_yields',7.5,8.5,drawsummary)
# Author's slab/lattice construction, complete surface anchor and0–200cm depth.
if(!identical(a[['skip-depth']],'yes')){
h<-read.csv(file.path(soil,'input_horizons.csv'));stopifnot(length(unique(h$profile_id[h$source=='DSM']))==2030,length(unique(h$profile_id[h$source=='SSURGO']))==6)
h$top<-(h$bottom_mm-h$thickness_mm)/10;h$bottom<-h$bottom_mm/10
sites<-unique(h[c('profile_id','source')]);h$source<-NULL;depths(h)<-profile_id~top+bottom;site(h)<-sites;site(h)$source<-factor(site(h)$source,levels=c('DSM','SSURGO'))
mean.sd<-function(v){m<-mean(v,na.rm=TRUE);s<-sd(v,na.rm=TRUE);c(mean=m,lower=m-s,upper=m+s)}
sl<-slab(h,fm=source~OC+BD+silt+clay+pH+AirDry+LL15+DUL+SAT+KSAT,slab.structure=c(0,5,15,30,60,100,150,200),slab.fun=mean.sd)
surface<-do.call(rbind,lapply(split(sl,list(sl$source,sl$variable),drop=TRUE),function(q){q<-q[order(q$bottom),];q[1,'bottom']<-0;q[1,]}))
curves<-rbind(surface,sl);curves<-curves[order(curves$variable,curves$source,curves$bottom),];curves$variable<-factor(curves$variable,levels=c('OC','BD','silt','clay','pH','AirDry','LL15','DUL','SAT','KSAT'))
dp<-xyplot(bottom~mean|variable,data=curves,lower=curves$lower,upper=curves$upper,groups=source,sync.colors=TRUE,alpha=.5,ylim=c(200,0),layout=c(5,2),ylab='Depth (cm)',xlab='',scales=list(x=list(tick.number=2,relation='free',cex=1),y=list(at=seq(0,200,50),alternating=1,cex=1.15)),par.settings=list(fontsize=list(text=16,points=12),superpose.line=list(lwd=2,col=unname(sourcecols),lty=c(2,2))),panel=panel.depth_function,prepanel=prepanel.depth_function,strip=strip.custom(bg=grey(.8),factor.levels=expression(OC~'%',BD~g~cm^{-3},Silt~'%',Clay~'%',pH,AirDry~mm~mm^{-1},LL15~mm~mm^{-1},DUL~mm~mm^{-1},SAT~mm~mm^{-1},KSAT~mm~day^{-1})),auto.key=list(columns=2,lines=TRUE,points=FALSE,cex=1.25))
saveplot('Figure_3_depth_functions',11,8.5,function()print(dp));write.csv(curves,file.path(out,'depth_slabs.csv'),row.names=FALSE)
}
f<-read.csv(file.path(associations,'soil_yield_R2.csv'));raw<-c('OC','pH','sand','silt','clay');stopifnot(nrow(f)==60,setequal(unique(f$property),raw))
f$property<-factor(f$property,levels=rev(raw));f$source<-factor(f$source,levels=c('DSM','SSURGO'));f$interval<-factor(f$interval,levels=c('0-30 cm','30-100 cm'));f$year<-factor(f$year)
barplot<-function(z,both=FALSE){p<-ggplot(z,aes(R2,property,fill=year))+geom_col(position=position_dodge(.8),width=.72)+scale_fill_manual(values=yearcols)+scale_x_continuous(limits=c(0,1),breaks=c(0,.25,.5,.75,1))+labs(x=expression(R^2),y=NULL,fill=NULL)+theme_bw(base_size=19)+theme(legend.position='bottom',panel.grid.major.y=element_blank(),panel.grid.minor=element_blank(),panel.spacing=unit(1.4,'lines'),strip.text=element_text(size=17,margin=margin(8,8,8,8)),axis.title.x=element_text(margin=margin(t=12)),plot.margin=margin(14,16,14,16));if(both)p+facet_grid(source~interval)else p+facet_wrap(~source,nrow=1)}
saveplot('Figure_4_raw_topsoil_R2',9,6.8,function()print(barplot(f[f$interval=='0-30 cm',])))
saveplot('Figure_S5_topsoil_subsoil_R2',11,11,function()print(barplot(f,TRUE)))
ds$DHP<-factor(ds$DHP,levels=c('SU','SH','BS','FS','TS','Field'));ds$year<-factor(ds$year)
pd<-ggplot(ds,aes(DHP,paired_difference,fill=year))+geom_hline(yintercept=0,color='grey35')+geom_col(width=.65)+geom_errorbar(aes(ymin=paired_difference-paired_sd,ymax=paired_difference+paired_sd),width=.15)+facet_wrap(~year,ncol=1)+scale_fill_manual(values=yearcols)+labs(x=NULL,y=expression(DSM-SSURGO~yield~difference~(Mg~ha^{-1})))+theme_bw(base_size=19)+theme(legend.position='none',panel.grid.minor=element_blank())
saveplot('Figure_S2_DHP_differences',9,10,function()print(pd))
em<-read.csv(file.path(variograms,'empirical_variograms.csv'));selected<-read.csv(file.path(variograms,'variogram_parameters.csv'))
drawvar<-function(){par(mfrow=c(3,3),mar=c(3.7,4.2,2.1,.6),mgp=c(2.6,.8,0),cex=1.15);for(y in years)for(map in c('dsm','dominant','delta')){z<-em[em$year==y&em$map==map,];q<-selected[selected$year==y&selected$map==map,];stopifnot(nrow(q)==1,nrow(z)==12);plot(z$mean_distance_m,z$semivariance,pch=16,xlim=c(0,288),ylim=c(0,max(z$semivariance,q$total_sill)*1.1),xlab='Lag distance (m)',ylab=expression(Semivariance~(Mg~ha^{-1})^2),main=paste(y,c(dsm='DSM',dominant='SSURGO',delta='Difference')[[map]]));xx<-seq(.001,288,length.out=800);lines(xx,q$nugget+q$partial_sill*shape(xx,q$fitted_scale_a_m,q$model),col='#0072B2',lwd=2);abline(h=q$total_sill,lty=3,col='grey50');if(is.finite(q$effective_range_m))abline(v=q$effective_range_m,lty=3,col='#D55E00')}}
saveplot('Figure_S3_variograms',12,11,drawvar)
capture.output(sessionInfo(),file=file.path(out,'sessionInfo.txt'))
writej(list(status='SUMMARY_FIGURES_RENDERED_VISUAL_QA_PENDING',sources=as.list(setNames(unname(tools::md5sum(file.path(c(paired,soil,associations,variograms),'status.json'))),c('paired','soil','associations','variograms'))),source_colors=as.list(sourcecols),figures=c('Figure1','Figure3','Figure4','FigureS2','FigureS3','FigureS5'),note='Not a manuscript release. Compare all rendered figures with approved V13 and inspect labels/legibility before use.'),file.path(out,'status.json'))
