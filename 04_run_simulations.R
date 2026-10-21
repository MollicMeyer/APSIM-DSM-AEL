# Run the prepared profiles with the APSIM R package, one profile at a time.
# Optional command-line arguments select profile IDs for a small diagnostic run.
library(apsimx)
library(DBI)
library(RSQLite)
library(jsonlite)
source('settings.R')
source('R/align_urea_layers.R')
stopifnot(file.exists(apsim_executable))
engine_version <- system2(apsim_executable,'--version',stdout=TRUE)
stopifnot(any(trimws(engine_version)=='APSIM 2025.3.7681.0'))
apsimx::apsimx_options(exe.path=apsim_executable)
manifest_path <- file.path(simulation_directory,'manifest.csv')
manifest <- read.csv(manifest_path,stringsAsFactors=FALSE)
stopifnot(nrow(manifest)==2036)
manifest <- align_urea_layers(manifest)
write.csv(manifest,manifest_path,row.names=FALSE)
selection <- commandArgs(trailingOnly=TRUE)
if(length(selection)) {
  stopifnot(all(selection %in% manifest$profile_id))
  manifest <- manifest[manifest$profile_id %in% selection,]
}
engine_md5 <- unname(tools::md5sum(apsim_executable))
weather_md5 <- unname(tools::md5sum('data/weather.met'))
for(i in seq_len(nrow(manifest))) {
  file <- manifest$input[i]
  db <- sub('[.]apsimx$','.db',file)
  receipt <- paste0(file,'.complete.json')
  input_md5 <- unname(tools::md5sum(file))
  stopifnot(input_md5==manifest$md5[i])
  if(file.exists(receipt)) {
    completed <- jsonlite::read_json(receipt,simplifyVector=TRUE)
    stopifnot(completed$input_md5==input_md5,completed$engine_md5==engine_md5,
      completed$weather_md5==weather_md5,completed$database_md5==unname(tools::md5sum(db)))
    next
  }
  if(file.exists(db)) stop('Unverified output already exists: ',db)
  started <- Sys.time()
  # Multiple daily reports have different columns, so retain a named list.
  reports <- apsimx::apsimx(file=basename(file),src.dir=dirname(file),
    value='all',simplify=FALSE,silent=TRUE,cleanup=FALSE)
  stopifnot(nrow(reports$WarmupDaily)==3287,nrow(reports$WarmupStart)==3287,
    all(is.finite(reports$WarmupDaily$MaizeGrain)))
  con <- DBI::dbConnect(RSQLite::SQLite(),db,flags=RSQLite::SQLITE_RO)
  messages <- DBI::dbReadTable(con,'_Messages')
  DBI::dbDisconnect(con)
  stopifnot(any(grepl('Simulation terminated normally',messages$Message,fixed=TRUE)))
  jsonlite::write_json(list(profile_id=manifest$profile_id[i],status='COMPLETE',
    input_md5=input_md5,engine_md5=engine_md5,weather_md5=weather_md5,
    database_md5=unname(tools::md5sum(db)),started=as.character(started),
    finished=as.character(Sys.time()),elapsed_seconds=as.numeric(difftime(Sys.time(),started,units='secs'))),
    receipt,pretty=TRUE,auto_unbox=TRUE,digits=NA)
  cat(i,'/',nrow(manifest),manifest$profile_id[i],'complete\n')
  flush.console()
  rm(reports);gc()
}
cat('Selected simulations completed. Run 05 after all 2,036 profiles finish.\n')
