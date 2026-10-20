# Portable version of the approved blue/orange PRISM context graphic.
here<-dirname(normalizePath(sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE)),winslash='/'));source(file.path(here,'common.R'))
a<-args_named();src<-normal(need(a,'climate'));out<-normal(need(a,'out'),FALSE)
stopifnot(!dir.exists(out));st<-fromJSON(file.path(src,'status.json'));stopifnot(st$status=='CLIMATE_CONTEXT_REPRODUCED')
stopifnot(as.integer(tail(ps('@(Get-Process Models -ErrorAction SilentlyContinue).Count'),1))==0)
if(as.numeric(tail(ps('(Get-CimInstance Win32_OperatingSystem).FreePhysicalMemory'),1))/1024<1536)stop('At least 1536 MB free RAM required for rendering')
library(ggplot2)
z<-read.csv(file.path(src,'prism_monthly_2017_2021.csv'));s<-read.csv(file.path(src,'prism_climate_context.csv'))
z<-z[z$year%in%c(2017,2019,2021)&z$month%in%5:9,];s<-s[s$period=='May-September',]
stopifnot(nrow(z)==15,nrow(s)==3,!anyNA(z$precip_departure_percent));dir.create(out,recursive=TRUE)
labs<-setNames(sprintf('%d\n%.0f mm (%+.1f%%)',s$year,s$precip_mm,s$precip_departure_percent),s$year)
z$panel<-factor(labs[as.character(z$year)],levels=unname(labs[as.character(c(2017,2019,2021))]));z$Month<-factor(month.abb[z$month],levels=month.abb[5:9])
write.csv(z,file.path(out,'climate_figure_data.csv'),row.names=FALSE)
p<-ggplot(z,aes(Month,precip_departure_percent))+geom_hline(yintercept=0,colour='black',size=.35)+
 geom_col(aes(fill=precip_departure_percent>=0),width=.68,colour='grey20',size=.25)+
 scale_fill_manual(values=c('FALSE'='#D55E00','TRUE'='#0072B2'),guide='none')+facet_wrap(~panel,nrow=1)+
 scale_y_continuous(breaks=scales::pretty_breaks(n=6),expand=expansion(mult=c(.05,.08)))+
 labs(x=NULL,y='Precipitation departure from normal (%)',subtitle='May-September totals and departures shown above each panel',caption=paste0('PRISM 4-km grid cell at the field; 1991-2020 normal.\nPRISM Group, Oregon State University; accessed ',st$archived_access_date,'.'))+
 theme_bw(base_size=11,base_family='sans')+theme(panel.grid.minor=element_blank(),panel.grid.major.x=element_blank(),strip.background=element_rect(fill='grey93',colour='grey40'),strip.text=element_text(size=11,lineheight=1.1),plot.subtitle=element_text(size=10,margin=margin(b=8)),plot.caption=element_text(size=8.5,hjust=0),axis.title.y=element_text(size=10),axis.text=element_text(colour='black'),plot.margin=margin(8,10,8,8),panel.spacing=grid::unit(.18,'in'))
temp_labels<-setNames(sprintf('%d\n%.2f \u00b0C (%+.2f \u00b0C)',s$year,s$tmean_C,s$tmean_departure_C),s$year)
z$temp_panel<-factor(temp_labels[as.character(z$year)],levels=unname(temp_labels[as.character(c(2017,2019,2021))]))
pt<-ggplot(z,aes(Month,tmean_departure_C))+geom_hline(yintercept=0,colour='black',size=.35)+geom_col(aes(fill=tmean_departure_C>=0),width=.68,colour='grey20',size=.25)+scale_fill_manual(values=c('FALSE'='#0072B2','TRUE'='#D55E00'),guide='none')+facet_wrap(~temp_panel,nrow=1)+labs(x=NULL,y='Temperature departure (\u00b0C)',subtitle='b) Mean temperature; seasonal means and departures above each panel')+theme_bw(base_size=13)+theme(panel.grid.minor=element_blank(),panel.grid.major.x=element_blank(),strip.text=element_text(size=12),plot.subtitle=element_text(size=11),plot.margin=margin(10,12,10,12))
p<-p+labs(y='Precipitation departure (%)',subtitle='a) Precipitation; seasonal totals and departures above each panel',caption=NULL)+theme(text=element_text(size=13),axis.title.y=element_text(size=12))
draw<-function(){grid::grid.newpage();grid::pushViewport(grid::viewport(layout=grid::grid.layout(2,1)));print(p,vp=grid::viewport(layout.pos.row=1));print(pt,vp=grid::viewport(layout.pos.row=2));grid::popViewport()}
png(file.path(out,'Climate_departures.png'),width=8,height=7.5,units='in',res=600,type='cairo',bg='white');draw();dev.off()
cairo_pdf(file.path(out,'Climate_departures.pdf'),width=8,height=7.5,family='Arial');draw();dev.off()
writej(list(status='CLIMATE_FIGURE_RENDERED_VISUAL_QA_PENDING',climate_status_md5=md5(file.path(src,'status.json')),positive='#0072B2',negative='#D55E00'),file.path(out,'status.json'))
capture.output(sessionInfo(),file=file.path(out,'sessionInfo.txt'))
