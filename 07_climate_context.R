# PRISM departures describe the three crop years; they do not replace APSIM weather.
library(ggplot2)
library(dplyr)
library(gridExtra)
source('settings.R')
dir.create(file.path(analysis_directory,'climate'),recursive=TRUE,showWarnings=FALSE)
read_PRISM <- function(file) {
  path <- file.path('data/climate',file)
  header <- grep('^Date,',readLines(path,warn=FALSE))
  stopifnot(length(header)==1)
  read.csv(path,skip=header-1,check.names=FALSE)
}
normal <- read_PRISM('monthly_normals.csv');names(normal) <- c('period','precip_mm','tmean_C')
monthly <- read_PRISM('monthly_201701_202112.csv');names(monthly) <- c('date','precip_mm','tmean_C')
annual <- read_PRISM('yearly_2017_2021.csv');names(annual) <- c('year','precip_mm','tmean_C')
stopifnot(nrow(normal)==13,nrow(monthly)==60,nrow(annual)==5)
monthly$year <- as.integer(substr(monthly$date,1,4))
monthly$month <- as.integer(substr(monthly$date,6,7))
monthly$precip_departure_percent <- 100*(monthly$precip_mm-normal$precip_mm[monthly$month])/normal$precip_mm[monthly$month]
monthly$tmean_departure_C <- monthly$tmean_C-normal$tmean_C[monthly$month]
days <- c(31,28,31,30,31,30,31,31,30,31,30,31)
seasons <- list()
for(year in c(2017,2019,2021))for(period in c('Annual','May-September')) {
  selected <- if(period=='Annual')1:12 else 5:9
  z <- monthly[monthly$year==year & monthly$month %in% selected,]
  p <- sum(z$precip_mm);np <- sum(normal$precip_mm[selected])
  t <- if(period=='Annual')annual$tmean_C[annual$year==year] else weighted.mean(z$tmean_C,days[z$month])
  nt <- if(period=='Annual')normal$tmean_C[13]else weighted.mean(normal$tmean_C[selected],days[selected])
  seasons[[paste(year,period)]] <- data.frame(year=year,period=period,precip_mm=p,
    normal_precip_mm=np,precip_departure_percent=100*(p-np)/np,
    tmean_C=t,normal_tmean_C=nt,tmean_departure_C=t-nt)
}
write.csv(monthly,file.path(analysis_directory,'climate/monthly_departures.csv'),row.names=FALSE)
seasonal <- bind_rows(seasons)
write.csv(seasonal,file.path(analysis_directory,'climate/seasonal_departures.csv'),row.names=FALSE)
z <- monthly %>% filter(year %in% c(2017,2019,2021),month %in% 5:9) %>%
  mutate(Month=factor(month.abb[month],levels=month.abb[5:9]))
context <- seasonal[seasonal$period=='May-September',]
precip_labels <- setNames(sprintf('%d\n%.0f mm (%+.1f%%)',context$year,
  context$precip_mm,context$precip_departure_percent),context$year)
temperature_labels <- setNames(sprintf('%d\n%.2f \u00b0C (%+.2f \u00b0C)',context$year,
  context$tmean_C,context$tmean_departure_C),context$year)
z$precip_panel <- factor(precip_labels[as.character(z$year)],levels=unname(precip_labels))
z$temp_panel <- factor(temperature_labels[as.character(z$year)],levels=unname(temperature_labels))
precip <- ggplot(z,aes(Month,precip_departure_percent))+geom_hline(yintercept=0)+
  geom_col(aes(fill=precip_departure_percent>=0),width=.68,colour='grey20',size=.25)+
  scale_fill_manual(values=c('FALSE'='#D55E00','TRUE'='#0072B2'),guide='none')+
  facet_wrap(~precip_panel,nrow=1)+theme_bw(base_size=13)+
  scale_y_continuous(breaks=scales::pretty_breaks(n=6),expand=expansion(mult=c(.05,.08)))+
  theme(panel.grid.minor=element_blank(),panel.grid.major.x=element_blank(),
    strip.background=element_rect(fill='grey93',colour='grey40'),
    strip.text=element_text(size=11,lineheight=1.1),plot.subtitle=element_text(size=10),
    axis.title.y=element_text(size=12),axis.text=element_text(colour='black'))+
  labs(x=NULL,y='Precipitation departure (%)',
    subtitle='a) Precipitation; seasonal totals and departures above each panel')
temperature <- ggplot(z,aes(Month,tmean_departure_C))+geom_hline(yintercept=0)+
  geom_col(aes(fill=tmean_departure_C>=0),width=.68,colour='grey20',size=.25)+
  scale_fill_manual(values=c('FALSE'='#0072B2','TRUE'='#D55E00'),guide='none')+
  facet_wrap(~temp_panel,nrow=1)+theme_bw(base_size=13)+
  theme(panel.grid.minor=element_blank(),panel.grid.major.x=element_blank(),
    strip.text=element_text(size=12),plot.subtitle=element_text(size=11))+
  labs(x=NULL,y=expression(Temperature~departure~(degree*C)),
    subtitle='b) Mean temperature; seasonal means and departures above each panel')
figure <- gridExtra::arrangeGrob(precip,temperature,ncol=1)
ggsave(file.path(analysis_directory,'climate/Climate_departures.pdf'),figure,width=8,height=7.5)
ggsave(file.path(analysis_directory,'climate/Climate_departures.png'),figure,width=8,height=7.5,dpi=600)
