# Run each prepared soil profile through APSIM directly from R.
# Existing databases are not overwritten; each result is checked before extraction.
run_simulations <- function(experiment, engine) {
  library(DBI)
  library(RSQLite)
  root <- normal(experiment)
  engine <- normal(engine)
  m <- read.csv(file.path(root, 'manifest.csv'))
  prep <- fromJSON(file.path(root, 'preparation.json'))
  stopifnot(nrow(m) == prep$profiles)
  engine_version <- system2(engine, '--version', stdout = TRUE, stderr = TRUE)
  version_status <- attr(engine_version, 'status')
  stopifnot(is.null(version_status) || version_status == 0L,
    any(trimws(engine_version) == 'APSIM 2025.3.7681.0'),
    md5(prep$weather) == prep$weather_md5)
  dates <- seq(as.Date('2013-01-01'), as.Date('2021-12-31'), by = 'day')

  # Check initial mineral N/water, the AirDry rule, crop rotation, and year-to-year
  # continuity before including each profile in the analysis.
  validate <- function(i) {
    con <- dbConnect(SQLite(), m$expected_db[i], flags = SQLITE_RO)
    on.exit(dbDisconnect(con))
    z <- normcols(dbReadTable(con, 'WarmupDaily'))
    s <- normcols(dbReadTable(con, 'WarmupStart'))
    msg <- dbReadTable(con, '_Messages')
    ic <- dbReadTable(con, '_InitialConditions')
    stopifnot(nrow(z) == 3287, nrow(s) == 3287,
      identical(as.Date(substr(z$Date, 1, 10)), dates),
      identical(as.Date(substr(s$Date, 1, 10)), dates),
      any(grepl('Simulation terminated normally', msg$Message, fixed = TRUE)))
    stopifnot(all(is.finite(z$MaizeGrain)), all(z$MaizeGrain >= 0),
      all(is.finite(z$AD_Es)), all(z$AD_Es >= 0))
    x <- fromJSON(m$input_file[i], simplifyVector = FALSE)
    p <- node(x, 'Models.Soils.Physical')
    th <- vec(p$Thickness)
    iv <- function(n, path) {
      v <- ic$Value[ic$Name == n & grepl(path, ic$ModelPath)]
      stopifnot(length(v) == 1)
      as.numeric(strsplit(v, ',', fixed = TRUE)[[1]])
    }
    eq(iv('AirDry', '[.]Physical$'), airdry(p))
    eq(iv('SoilCNRatio', '[.]Organic$'), rep(12, length(th)))
    eq(iv('InitialValues', '[.]Water$'), vec(node(x, 'Models.Soils.Water')$InitialValues))
    eq(sum(iv('InitialValues', '[.]Water$') * th), 609)
    for (sp in c('NO3', 'NH4')) {
      q <- Filter(function(z) type_is(z, 'Models.Soils.Solute') && z$Name == sp, walk(x))[[1]]
      eq(as.numeric(s[1, cols(s, sp)]), vec(q$InitialValues))
    }
    eq(sum(as.numeric(s[1, c(cols(s, 'NO3'), cols(s, 'NH4'))])), 63.9519166666667)
    jan <- which(format(dates, '%m-%d') == '01-01')[-1]
    for (n in c('TotalC', 'OrganicN', 'MineralN', 'AD_SWmm')) {
      eq(as.numeric(as.matrix(s[jan, cols(s, n)])),
         as.numeric(as.matrix(z[jan - 1, cols(z, n)])))
    }
    sow <- msg[grepl('A crop of (Maize|Soybean)', msg$Message), ]
    stopifnot(nrow(sow) == 9,
      identical(sort(as.integer(substr(sow$Date[grepl('A crop of Maize', sow$Message)], 1, 4))),
        seq(2013L, 2021L, 2L)))
    list(status = 'PASS', normal_termination = TRUE, days = 3287,
      initialization = 'PASS', continuous_rotation = 'PASS',
      warnings = sum(msg$MessageType == 1))
  }

  # The manifest identifies one model file and one output database per profile.
  for (i in seq_len(nrow(m))) {
    stopifnot(md5(m$input_file[i]) == m$input_md5[i],
      md5(m$original_input[i]) == m$original_md5[i])
    verified <- paste0(m$input_file[i], '.verified.json')
    if (file.exists(verified)) {
      old <- fromJSON(verified)
      stopifnot(old$status == 'PASS', old$engine_md5 == md5(engine),
        old$input_md5 == m$input_md5[i], old$database_md5 == md5(m$expected_db[i]))
      next
    }
    if (file.exists(m$expected_db[i])) {
      stop('An unverified database already exists: ', m$expected_db[i])
    }
    log <- paste0(m$input_file[i], '.log')
    if (file.exists(log)) stop('A previous attempt log already exists: ', log)
    exit_code <- system2(engine, shQuote(m$input_file[i]), stdout = log, stderr = log)
    if (exit_code != 0) stop('APSIM failed for ', m$run_id[i], '; see ', log)
    checks <- validate(i)
    writej(c(checks, list(input_md5 = m$input_md5[i], original_md5 = m$original_md5[i],
      database_md5 = md5(m$expected_db[i]), engine_md5 = md5(engine),
      engine_version = trimws(engine_version))), verified)
    writej(list(status = 'RUNNING', completed = i, total = nrow(m)), file.path(root, 'progress.json'))
    cat(i, '/', nrow(m), m$run_id[i], '\n')
  }
  writej(list(status = 'ALL_MODELS_COMPLETE_VALIDATED_EXTRACTION_PENDING',
    completed = nrow(m), total = nrow(m)), file.path(root, 'progress.json'))
}
