# Final scientific workflow functions.
# Called by the three numbered scripts in the repository root.


# Read validated APSIM databases and extract annual yields, soil stocks and crop stress.
# options is a named list of input/output paths supplied by the numbered entry script.
extract_simulated_yields <- function (options) 
{
    here <- .workflow_directory
    args_named <- function() options
    commandArgs <- function(trailingOnly = FALSE) {
        if (trailingOnly) 
            return(unlist(Map(function(n, v) c(paste0("--", n), v), names(options), options)))
        paste0("--file=", file.path(here, "yield_analysis.R"))
    }
    library(DBI)
    library(RSQLite)
    stopifnot(requireNamespace("digest", quietly = TRUE))
    a <- args_named()
    root <- normal(need(a, "experiment"))
    test_mode <- identical(a[["test-mode"]], "yes")
    resume <- identical(a[["resume"]], "yes")
    test_stop <- if (is.null(a[["test-stop-after"]])) 
        Inf
    else as.integer(a[["test-stop-after"]])
    stopifnot(is.finite(test_stop) || is.infinite(test_stop), test_mode || is.infinite(test_stop))
    out <- normal(need(a, "out"), FALSE)
    root_abs <- normal(root)
    stopifnot(out != root_abs)
    manifest_path <- normal(file.path(root, "manifest.csv"))
    m <- read.csv(manifest_path)
    prep_path <- file.path(root, "preparation.json")
    prep <- fromJSON(prep_path)
    progress <- fromJSON(file.path(root, "progress.json"))
    stopifnot(progress$status == "ALL_MODELS_COMPLETE_VALIDATED_EXTRACTION_PENDING", progress$completed == 
        nrow(m), prep$profiles == nrow(m))
    if (test_mode) 
        stopifnot(nrow(m) >= 1, nrow(m) <= 6)
    else stopifnot(nrow(m) == 2036, sum(m$source == "DSM") == 2030, sum(m$source == "SSURGO") == 
        6)
    stopifnot(all(grepl("_[0-9]+_d$", m$profile_id[m$source == "SSURGO"])))
    stopifnot(!anyDuplicated(m$run_id), !anyDuplicated(m$profile_id), all(unname(tools::md5sum(m$input_file)) == 
        m$input_md5), all(unname(tools::md5sum(m$original_input)) == m$original_md5), md5(prep$weather) == 
        prep$weather_md5)
    self <- normal(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)))
    stopifnot(length(self) == 1)
    context <- list(mode = if (test_mode) "TEST_ONLY" else "PRODUCTION", profiles = nrow(m), manifest_md5 = md5(manifest_path), 
        preparation_md5 = md5(prep_path), weather_md5 = prep$weather_md5, extractor_md5 = md5(self), 
        common_md5 = md5(file.path(here, "common.R")))
    context_key <- as.character(toJSON(context, auto_unbox = TRUE, digits = NA))
    atomic <- function(path, writer) {
        temp <- tempfile(pattern = paste0(basename(path), ".partial_"), tmpdir = dirname(path))
        writer(temp)
        stopifnot(file.exists(temp))
        if (file.exists(path)) {
            if (md5(temp) != md5(path)) 
                stop("Existing output differs; preserved old and candidate: ", path, " / ", temp)
            stopifnot(file.remove(temp))
        }
        else stopifnot(file.rename(temp, path))
        invisible(path)
    }
    context_path <- file.path(out, "context.json")
    if (dir.exists(out)) {
        stopifnot(resume, file.exists(context_path))
        existing <- fromJSON(context_path, simplifyVector = FALSE)
        stopifnot(identical(existing$key, context_key))
    }
    else {
        stopifnot(!resume)
        dir.create(out, recursive = TRUE)
        atomic(context_path, function(p) writej(list(key = context_key, context = context), p))
    }
    dir.create(file.path(out, "checkpoints"), showWarnings = FALSE)
    dir.create(file.path(out, "year_profiles"), showWarnings = FALSE)
    expected_years <- 2013:2021
    payload_hash <- function(cp) digest::digest(cp[c("annual", "stress", "warnings", "properties", 
        "checks")], algo = "sha256", serialize = TRUE)
    validate_checkpoint <- function(cp, i, key) {
        stopifnot(is.list(cp), identical(cp$key, key), identical(cp$payload_sha256, payload_hash(cp)), 
            nrow(cp$annual) == 9, nrow(cp$stress) == 5, nrow(cp$properties) == 1, nrow(cp$checks) == 
                1, identical(cp$annual$year, expected_years), identical(cp$stress$year, seq(2013, 
                2021, 2)), all(cp$annual$run_id == m$run_id[i]), all(cp$stress$run_id == m$run_id[i]), 
            cp$checks$input_md5 == m$input_md5[i])
        invisible(TRUE)
    }
    cols <- function(z, n) {
        v <- grep(paste0("^", n, "[.][0-9]+[.]$"), names(z), value = TRUE)
        stopifnot(length(v) > 0)
        v[order(as.integer(sub(paste0("^", n, "[.]([0-9]+)[.]$"), "\\1", v)))]
    }
    sums <- function(z, n) rowSums(as.matrix(z[, cols(z, n), drop = FALSE]))
    eq <- function(x, y, tol = 1e-07) stopifnot(length(x) == length(y), all(is.finite(x)), all(is.finite(y)), 
        max(abs(x - y)) < tol)
    dates <- seq(as.Date("2013-01-01"), as.Date("2021-12-31"), by = "day")
    years <- as.integer(format(dates, "%Y"))
    metlines <- readLines(prep$weather)
    h <- grep("^year day ", metlines)
    stopifnot(length(h) == 1)
    met <- read.table(prep$weather, skip = h + 1, col.names = c("year", "day", "Radn", "MaxT", "MinT", 
        "Rain"))
    met$Date <- as.Date(paste0(met$year, "-01-01")) + met$day - 1
    stopifnot(identical(met$Date, dates))
    f32 <- function(x) readBin(writeBin(as.double(x), raw(), size = 4), numeric(), n = length(x), 
        size = 4)
    annual <- stress <- warnings <- checks <- properties <- list()
    for (i in seq_len(nrow(m))) {
        recordfile <- paste0(m$input_file[i], ".verified.json")
        rec <- fromJSON(recordfile)
        stopifnot(rec$status == "PASS", rec$input_md5 == m$input_md5[i], rec$original_md5 == m$original_md5[i], 
            md5(m$expected_db[i]) == rec$database_md5)
        checkpoint <- file.path(out, "checkpoints", paste0(m$run_id[i], ".rds"))
        key <- paste(context_key, m$run_id[i], rec$database_md5, md5(recordfile), sep = "|")
        if (file.exists(checkpoint)) {
            cp <- tryCatch(readRDS(checkpoint), error = function(e) stop("Unreadable checkpoint preserved: ", 
                checkpoint, "; ", conditionMessage(e)))
            validate_checkpoint(cp, i, key)
            annual[[i]] <- cp$annual
            stress[[i]] <- cp$stress
            warnings[[i]] <- cp$warnings
            properties[[i]] <- cp$properties
            checks[[i]] <- cp$checks
            next
        }
        con <- dbConnect(SQLite(), m$expected_db[i], flags = SQLITE_RO)
        z <- normcols(dbReadTable(con, "WarmupDaily"))
        s <- normcols(dbReadTable(con, "WarmupStart"))
        r <- normcols(dbReadTable(con, "Report"))
        msg <- dbReadTable(con, "_Messages")
        ic <- dbReadTable(con, "_InitialConditions")
        dbDisconnect(con)
        z <- z[order(z$Date), ]
        s <- s[order(s$Date), ]
        stopifnot(nrow(z) == 3287, nrow(s) == 3287, identical(as.Date(substr(z$Date, 1, 10)), dates), 
            identical(as.Date(substr(s$Date, 1, 10)), dates))
        stopifnot(any(grepl("Simulation terminated normally", msg$Message, fixed = TRUE)), all(is.finite(z$MaizeGrain)), 
            all(z$MaizeGrain >= 0), all(is.finite(z$SoybeanGrain)), all(z$SoybeanGrain >= 0), all(is.finite(z$MaizeLAI)), 
            all(z$MaizeLAI >= 0), all(z$MaizeAlive %in% c("True", "False")))
        sow <- msg[grepl("A crop of (Maize|Soybean)", msg$Message), , drop = FALSE]
        stopifnot(nrow(sow) == 9, identical(sort(as.integer(substr(sow$Date[grepl("A crop of Maize", 
            sow$Message)], 1, 4))), seq(2013L, 2021L, 2L)), identical(sort(as.integer(substr(sow$Date[grepl("A crop of Soybean", 
            sow$Message)], 1, 4))), seq(2014L, 2020L, 2L)))
        x <- fromJSON(m$input_file[i], simplifyVector = FALSE)
        p <- node(x, "Models.Soils.Physical")
        o <- node(x, "Models.Soils.Organic")
        c <- node(x, "Models.Soils.Chemical")
        th <- vec(p$Thickness)
        iv <- function(n, path) {
            v <- ic$Value[ic$Name == n & grepl(path, ic$ModelPath)]
            stopifnot(length(v) == 1)
            as.numeric(strsplit(v, ",", fixed = TRUE)[[1]])
        }
        eq(iv("InitialValues", "[.]Water$"), vec(node(x, "Models.Soils.Water")$InitialValues))
        eq(iv("SoilCNRatio", "[.]Organic$"), rep(12, length(th)))
        eq(iv("AirDry", "[.]Physical$"), airdry(p))
        eq(sum(iv("InitialValues", "[.]Water$") * th), 609)
        sols <- Filter(function(v) type_is(v, "Models.Soils.Solute"), walk(x))
        for (species in c("NO3", "NH4", "Urea")) {
            q <- Filter(function(v) v$Name == species, sols)
            stopifnot(length(q) == 1, q[[1]]$InitialValuesUnits == 1)
            eq(as.numeric(s[1, cols(s, species)]), vec(q[[1]]$InitialValues))
        }
        carbon <- vec(o$Carbon) * 100 * vec(p$BD) * th
        inert <- carbon * vec(o$FInert)
        microbial <- (carbon - inert) * vec(o$FBiom)/(1 + vec(o$FBiom))
        humic <- carbon - inert - microbial
        eq(as.numeric(s[1, cols(s, "OrganicN")]), (inert + humic)/12 + microbial/8 + 0.4 * vec(o$FOM)/vec(o$FOMCNRatio))
        for (n in c("Rain", "Radn", "MaxT", "MinT")) eq(z[[n]], f32(met[[n]]), 1e-12)
        eq(sums(z, "TotalN"), sums(z, "OrganicN") + sums(z, "MineralN"))
        eq(sums(z, "OrganicN"), sums(z, "InertN") + sums(z, "MicrobialN") + sums(z, "HumicN") + 
            sums(z, "FOMN"))
        jan <- which(format(dates, "%m-%d") == "01-01")[-1]
        for (n in c("TotalC", "OrganicN", "MineralN", "AD_SWmm")) eq(as.numeric(as.matrix(s[jan, 
            cols(s, n)])), as.numeric(as.matrix(z[jan - 1, cols(z, n)])))
        eq(sum(as.numeric(s[1, c(cols(s, "NO3"), cols(s, "NH4"))])), 63.9519166666667)
        role <- if (m$source[i] == "DSM") 
            "DSM_grid_cell"
        else "SSURGO_dominant_component"
        id <- data.frame(run_id = m$run_id[i], source = m$source[i], profile_id = m$profile_id[i], 
            role = role)
        annual[[i]] <- do.call(rbind, lapply(2013:2021, function(y) {
            q <- which(years == y)
            j <- q[1]
            k <- tail(q, 1)
            data.frame(id, year = y, crop = if (y%%2 == 1) 
                "Maize"
            else "Soybean", primary_year = y %in% c(2017, 2019, 2021), C_Jan1_start_kg_ha = sums(s[j, 
                , drop = FALSE], "TotalC"), C_Dec31_end_kg_ha = sums(z[k, , drop = FALSE], "TotalC"), 
                OrganicN_Jan1_start_kg_ha = sums(s[j, , drop = FALSE], "OrganicN"), OrganicN_Dec31_end_kg_ha = sums(z[k, 
                  , drop = FALSE], "OrganicN"), MineralN_Jan1_start_kg_ha = sums(s[j, , drop = FALSE], 
                  "MineralN"), MineralN_Dec31_end_kg_ha = sums(z[k, , drop = FALSE], "MineralN"), 
                annual_net_mineralised_N_kg_ha = sum(sums(z[q, , drop = FALSE], "NetMineralisedN")), 
                Maize_peak_grain_Mg_ha = max(z$MaizeGrain[q]) * 0.01, Soybean_peak_grain_Mg_ha = max(z$SoybeanGrain[q]) * 
                  0.01, annual_soil_evaporation_mm = sum(z$AD_Es[q]), annual_drainage_mm = sum(z$AD_Drainage[q]), 
                annual_runoff_mm = sum(z$AD_Runoff[q]))
        }))
        stress[[i]] <- do.call(rbind, lapply(seq(2013, 2021, 2), function(y) {
            rr <- r[substr(r$Clock.Today, 1, 4) == as.character(y), ]
            first <- as.Date(substr(rr$Clock.Today[which(rr$Maize.Phenology.CurrentStageName == 
                "Emergence")], 1, 10))
            last <- as.Date(substr(rr$Clock.Today[which(rr$Maize.Phenology.CurrentStageName == "HarvestRipe")], 
                1, 10))
            if (length(first) != 1 || length(last) != 1 || last < first) 
                return(data.frame(id, year = y, emergence = NA_character_, harvest_ripe = NA_character_, 
                  stage_window_status = "MISSING_OR_AMBIGUOUS", active_green_days = 0L, N_factor_min = NA_real_, 
                  N_factor_mean = NA_real_, N_factor_below1_days = NA_integer_, water_factor_min = NA_real_, 
                  water_factor_mean = NA_real_, water_factor_below1_days = NA_integer_, zero_canopy_zero_N_days = NA_integer_))
            q <- z[dates >= first & dates <= last, ]
            active <- tolower(as.character(q$MaizeAlive)) == "true" & q$MaizeLAI > 0.01
            g <- q[active, ]
            if (nrow(g) == 0) 
                return(data.frame(id, year = y, emergence = as.character(first), harvest_ripe = as.character(last), 
                  stage_window_status = "NO_ACTIVE_GREEN_CANOPY", active_green_days = 0L, N_factor_min = NA_real_, 
                  N_factor_mean = NA_real_, N_factor_below1_days = NA_integer_, water_factor_min = NA_real_, 
                  water_factor_mean = NA_real_, water_factor_below1_days = NA_integer_, zero_canopy_zero_N_days = sum(q$MaizeLAI == 
                    0 & q$MaizeNFactor == 0)))
            stopifnot(all(is.finite(g$MaizeNFactor)), all(is.finite(g$MaizeWaterFactor)))
            data.frame(id, year = y, emergence = as.character(first), harvest_ripe = as.character(last), 
                stage_window_status = "PASS", active_green_days = nrow(g), N_factor_min = min(g$MaizeNFactor), 
                N_factor_mean = mean(g$MaizeNFactor), N_factor_below1_days = sum(g$MaizeNFactor < 
                  1 - 1e-08), water_factor_min = min(g$MaizeWaterFactor), water_factor_mean = mean(g$MaizeWaterFactor), 
                water_factor_below1_days = sum(g$MaizeWaterFactor < 1 - 1e-08), zero_canopy_zero_N_days = sum(q$MaizeLAI == 
                  0 & q$MaizeNFactor == 0))
        }))
        w <- msg[msg$MessageType == 1, , drop = FALSE]
        w$run_id <- rep(m$run_id[i], nrow(w))
        warnings[[i]] <- w
        x <- fromJSON(m$input_file[i], simplifyVector = FALSE)
        p <- node(x, "Models.Soils.Physical")
        o <- node(x, "Models.Soils.Organic")
        c <- node(x, "Models.Soils.Chemical")
        th <- vec(p$Thickness)
        avg <- function(v) sum(vec(v) * th)/sum(th)
        properties[[i]] <- data.frame(id, BD = avg(p$BD), Clay = avg(p$ParticleSizeClay), Sand = avg(p$ParticleSizeSand), 
            Silt = avg(p$ParticleSizeSilt), OC = avg(o$Carbon), pH = avg(c$PH))
        checks[[i]] <- data.frame(id, input_md5 = m$input_md5[i], database_md5 = rec$database_md5, 
            engine_md5 = rec$engine_md5, dates = 3287, weather_exact = TRUE, pool_identity = TRUE, 
            year_boundary_continuity = TRUE)
        cp <- list(key = key, annual = annual[[i]], stress = stress[[i]], warnings = w, properties = properties[[i]], 
            checks = checks[[i]])
        cp$payload_sha256 <- payload_hash(cp)
        validate_checkpoint(cp, i, key)
        atomic(checkpoint, function(p) saveRDS(cp, p))
        if (test_mode && i >= test_stop) 
            stop("TEST_ONLY intentional interruption after checkpoint ", i)
        rm(z, s, r, msg, x)
        if (i%%50 == 0) {
            gc(FALSE)
            cat("Extracted", i, "of", nrow(m), "\n")
            flush.console()
        }
    }
    aa <- do.call(rbind, annual)
    st <- do.call(rbind, stress)
    ww <- do.call(rbind, warnings)
    pp <- do.call(rbind, properties)
    stopifnot(nrow(aa) == nrow(m) * 9, nrow(st) == nrow(m) * 5, nrow(pp) == nrow(m), !anyDuplicated(paste(aa$run_id, 
        aa$year)), !anyDuplicated(paste(st$run_id, st$year)), !anyDuplicated(pp$run_id))
    csv <- function(z, path) atomic(path, function(p) write.csv(z, p, row.names = FALSE))
    csv(aa, file.path(out, "annual_stocks_and_yields.csv"))
    csv(st, file.path(out, "active_green_canopy_stress.csv"))
    csv(ww, file.path(out, "warnings.csv"))
    csv(pp, file.path(out, "depth_weighted_soil_properties.csv"))
    csv(do.call(rbind, checks), file.path(out, "verification_and_provenance.csv"))
    for (y in c(2017, 2019, 2021)) {
        q <- aa[aa$year == y, ]
        q$grain_Mg_ha <- q$Maize_peak_grain_Mg_ha
        q$grain_bu_ac_15_5 <- q$grain_Mg_ha * 1000 * 0.40468564224/(56 * 0.45359237)/(1 - 0.155)
        q <- merge(q, pp, by = c("run_id", "source", "profile_id", "role"), sort = FALSE)
        ss <- st[st$year == y, c("run_id", "year", "stage_window_status", "active_green_days")]
        q <- merge(q, ss, by = c("run_id", "year"), sort = FALSE)
        q$crop_year_QA <- ifelse(q$stage_window_status == "PASS", "PASS", "REVIEW_REQUIRED")
        stopifnot(nrow(q) == nrow(m), !anyDuplicated(q$run_id), !anyNA(q$crop_year_QA))
        csv(q, file.path(out, "year_profiles", paste0("profile_results_", y, ".csv")))
    }
    weather <- do.call(rbind, lapply(2013:2021, function(y) {
        q <- met[met$year == y & format(met$Date, "%m-%d") >= "05-01" & format(met$Date, "%m-%d") <= 
            "09-30", ]
        data.frame(year = y, period = "May1_Sep30", days = nrow(q), rain_mm = sum(q$Rain), radiation_MJ_m2 = sum(q$Radn), 
            mean_maxT_C = mean(q$MaxT), mean_minT_C = mean(q$MinT))
    }))
    csv(weather, file.path(out, "common_weather_summary.csv"))
    artifacts <- list.files(out, pattern = "[.]csv$", recursive = TRUE, full.names = TRUE)
    artifact_hashes <- data.frame(path = substring(artifacts, nchar(out) + 2), md5 = unname(tools::md5sum(artifacts)))
    status <- list(status = if (test_mode) "TEST_ONLY_EXTRACTION_COMPLETE" else "ALL2036_EXTRACTED_REVIEW_AND_SPATIAL_JOIN_PENDING", 
        mode = if (test_mode) "TEST_ONLY" else "PRODUCTION", profiles = nrow(m), primary_years = c(2017, 
            2019, 2021), roles = as.list(table(pp$role)), missing_or_ambiguous_maize_windows = sum(st$stage_window_status != 
            "PASS"), warning_count = nrow(ww), context_key = context_key, artifact_hashes = artifact_hashes, 
        scope = "Year profiles retain crop-year validity; downstream requires this final marker and all expected rows. Dominant SSURGO only. Profile-wide properties retained for compatibility, not approved association depths. No equilibrium or observed accuracy claim.")
    if (!file.exists(file.path(out, "sessionInfo.txt"))) 
        atomic(file.path(out, "sessionInfo.txt"), function(p) capture.output(sessionInfo(), file = p))
    atomic(file.path(out, "status.json"), function(p) writej(status, p))
    cat(if (test_mode) 
        "TEST_ONLY extraction complete"
    else "All2036 extraction complete", "; downstream numerical review required.\\n")
}

# Independently compare extracted values with direct database queries.
# options is a named list of input/output paths supplied by the numbered entry script.
verify_simulated_yields <- function (options) 
{
    here <- .workflow_directory
    args_named <- function() options
    commandArgs <- function(trailingOnly = FALSE) {
        if (trailingOnly) 
            return(unlist(Map(function(n, v) c(paste0("--", n), v), names(options), options)))
        paste0("--file=", file.path(here, "yield_analysis.R"))
    }
    library(DBI)
    library(RSQLite)
    a <- args_named()
    experiment <- normal(need(a, "experiment"))
    results <- normal(need(a, "results"))
    out <- normal(need(a, "out"), FALSE)
    test <- identical(a[["test-mode"]], "yes")
    stopifnot(!dir.exists(out))
    m <- read.csv(file.path(experiment, "manifest.csv"))
    status <- fromJSON(file.path(results, "status.json"))
    stopifnot(status$profiles == nrow(m), status$status == if (test) 
        "TEST_ONLY_EXTRACTION_COMPLETE"
    else "ALL2036_EXTRACTED_REVIEW_AND_SPATIAL_JOIN_PENDING")
    if (test) 
        stopifnot(nrow(m) <= 6)
    else stopifnot(nrow(m) == 2036, sum(m$source == "DSM") == 2030, sum(m$source == "SSURGO") == 
        6)
    ctx <- fromJSON(status$context_key)
    stopifnot(ctx$manifest_md5 == md5(file.path(experiment, "manifest.csv")), ctx$preparation_md5 == 
        md5(file.path(experiment, "preparation.json")))
    stopifnot(all(unname(tools::md5sum(file.path(results, status$artifact_hashes$path))) == status$artifact_hashes$md5))
    annual <- read.csv(file.path(results, "annual_stocks_and_yields.csv"))
    check <- list()
    dir.create(out, recursive = TRUE)
    for (i in seq_len(nrow(m))) {
        rec <- fromJSON(paste0(m$input_file[i], ".verified.json"))
        stopifnot(rec$status == "PASS", md5(m$expected_db[i]) == rec$database_md5, md5(m$input_file[i]) == 
            rec$input_md5)
        con <- dbConnect(SQLite(), m$expected_db[i], flags = SQLITE_RO)
        sql_yield <- dbGetQuery(con, "SELECT CAST(substr(Date,1,4) AS INTEGER) AS year, COUNT(*) AS days, MAX(MaizeGrain)*0.01 AS maize, MAX(SoybeanGrain)*0.01 AS soybean FROM WarmupDaily GROUP BY substr(Date,1,4) ORDER BY year")
        stopifnot(identical(sql_yield$year, 2013:2021), sum(sql_yield$days) == 3287)
        q <- annual[annual$run_id == m$run_id[i], ]
        q <- q[order(q$year), ]
        stopifnot(identical(q$year, sql_yield$year))
        eq(q$Maize_peak_grain_Mg_ha, sql_yield$maize, 1e-10)
        eq(q$Soybean_peak_grain_Mg_ha, sql_yield$soybean, 1e-10)
        raw <- dbListFields(con, "WarmupStart")
        nn <- make.names(raw)
        stopifnot(!anyDuplicated(nn))
        nfields <- raw[grepl("^(NO3|NH4)[.][0-9]+[.]$", nn)]
        stopifnot(length(nfields) > 0)
        nexpr <- paste(as.character(dbQuoteIdentifier(con, nfields)), collapse = "+")
        init <- dbGetQuery(con, paste0("SELECT ", nexpr, " AS mineralN FROM WarmupStart ORDER BY Date LIMIT 1"))$mineralN
        eq(init, 63.9519166666667)
        pools <- raw[grepl("^(TotalC|OrganicN|MineralN|AD_SWmm)[.][0-9]+[.]$", nn)]
        stopifnot(length(pools) > 0)
        diffs <- vapply(seq_along(pools), function(k) {
            col <- as.character(dbQuoteIdentifier(con, pools[k]))
            paste0("MAX(ABS(s.", col, "-z.", col, ")) AS v", k)
        }, character(1))
        continuity <- dbGetQuery(con, paste0("SELECT COUNT(*) AS joins, ", paste(diffs, collapse = ","), 
            " FROM WarmupStart s JOIN WarmupDaily z ON substr(s.Date,1,10)=date(substr(z.Date,1,10),'+1 day') WHERE substr(s.Date,6,5)='01-01'"))
        stopifnot(continuity$joins == 8, all(is.finite(as.numeric(continuity[1, -1]))), max(abs(as.numeric(continuity[1, 
            -1]))) < 1e-07)
        normal_end <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM _Messages WHERE Message LIKE '%Simulation terminated normally%'")$n
        stopifnot(normal_end >= 1)
        dbDisconnect(con)
        check[[i]] <- data.frame(run_id = m$run_id[i], source = m$source[i], days = sum(sql_yield$days), 
            SQL_year_boundary_joins = 8, SQL_initial_mineralN = init, max_maize_yield_error = max(abs(q$Maize_peak_grain_Mg_ha - 
                sql_yield$maize)), max_pool_boundary_error = max(abs(as.numeric(continuity[1, -1]))), 
            database_md5 = rec$database_md5)
        if (i%%50 == 0) {
            cat("Independently verified", i, "/", nrow(m), "\n")
            flush.console()
            gc(FALSE)
        }
    }
    write.csv(do.call(rbind, check), file.path(out, "SQL_independent_checks.csv"), row.names = FALSE)
    writej(list(status = if (test) "TEST_INDEPENDENT_VERIFICATION_PASS" else "INDEPENDENT_VERIFICATION_PASS", 
        profiles = nrow(m), years = 2013:2021, manifest_md5 = ctx$manifest_md5, results_status_md5 = md5(file.path(results, 
            "status.json")), checks = c("database hashes", "SQL annual grain maxima", "SQL initial mineral N", 
            "SQL layerwise year-boundary carbon nitrogen water continuity", "normal termination")), 
        file.path(out, "status.json"))
}

# Align profile IDs to the native 12 m hillslope grid; do not interpolate yield for statistics.
# options is a named list of input/output paths supplied by the numbered entry script.
align_field_cells <- function (options) 
{
    here <- .workflow_directory
    args_named <- function() options
    commandArgs <- function(trailingOnly = FALSE) {
        if (trailingOnly) 
            return(unlist(Map(function(n, v) c(paste0("--", n), v), names(options), options)))
        paste0("--file=", file.path(here, "yield_analysis.R"))
    }
    library(raster)
    a <- args_named()
    out <- normal(need(a, "out"), FALSE)
    stopifnot(!dir.exists(out))
    xy_path <- normal(need(a, "dsm-xy"))
    native_path <- normal(need(a, "native-grid"))
    ref_path <- normal(need(a, "reference-grid"))
    paired_path <- normal(need(a, "paired-cells"))
    xy <- read.csv(xy_path)
    xy$profile_id <- paste0("Maize_", as.integer(xy$site))
    stopifnot(nrow(xy) == 2030, !anyDuplicated(xy$profile_id))
    native <- raster(native_path)
    ref <- raster(ref_path)
    stopifnot(compareCRS(native, ref), all(res(native) == 12), all(res(ref) == 12))
    ix <- cellFromXY(native, xy[, c("x", "y")])
    stopifnot(!anyNA(ix), !anyDuplicated(ix))
    values(native) <- NA
    native[ix] <- seq_len(nrow(xy))
    aligned <- resample(native, ref, method = "ngb")
    p <- read.csv(paired_path)
    stopifnot(nrow(p) == 1943, all(p$area_ha == 0.0144))
    j <- extract(aligned, p[, c("x", "y")])
    stopifnot(!anyNA(j), all(j == as.integer(j)))
    index <- data.frame(x = p$x, y = p$y, DHP = p$DHP, mukey = as.character(p$mukey), area_ha = p$area_ha, 
        profile_id = xy$profile_id[j])
    stopifnot(!anyDuplicated(paste(index$x, index$y)), !anyDuplicated(index$profile_id), length(unique(index$mukey)) == 
        6, all(index$DHP %in% c("SU", "SH", "BS", "FS", "TS")))
    dir.create(out, recursive = TRUE)
    write.csv(index, file.path(out, "cell_index.csv"), row.names = FALSE)
    writej(list(status = "INDEX_READY_YIELD_JOIN_CHECK_PENDING", cells = 1943, DSM_profiles = 2030, 
        mapunits = 6, area_ha = sum(index$area_ha), method = "Original native DSM grid to reference DHP grid by nearest neighbor; no cubic interpolation for statistics.", 
        source_md5 = as.list(setNames(unname(tools::md5sum(c(xy_path, native_path, ref_path, paired_path))), 
            c("dsm_xy", "native_grid", "reference_grid", "paired_cells"))), index_md5 = md5(file.path(out, 
            "cell_index.csv"))), file.path(out, "status.json"))
    capture.output(sessionInfo(), file = file.path(out, "sessionInfo.txt"))
}

# Calculate exact layer inputs and thickness-weighted soil properties at 0-30 and 30-100 cm.
# options is a named list of input/output paths supplied by the numbered entry script.
summarize_soil_depths <- function (options) 
{
    here <- .workflow_directory
    args_named <- function() options
    commandArgs <- function(trailingOnly = FALSE) {
        if (trailingOnly) 
            return(unlist(Map(function(n, v) c(paste0("--", n), v), names(options), options)))
        paste0("--file=", file.path(here, "yield_analysis.R"))
    }
    a <- args_named()
    root <- normal(need(a, "experiment"))
    out <- normal(need(a, "out"), FALSE)
    test <- identical(a[["test-mode"]], "yes")
    stopifnot(!dir.exists(out))
    m <- read.csv(file.path(root, "manifest.csv"))
    prep <- fromJSON(file.path(root, "preparation.json"))
    if (test) 
        stopifnot(nrow(m) >= 1, nrow(m) <= 6)
    else stopifnot(nrow(m) == 2036, sum(m$source == "DSM") == 2030, sum(m$source == "SSURGO") == 
        6)
    stopifnot(prep$profiles == nrow(m), !anyDuplicated(m$profile_id))
    hh <- means <- list()
    intervals <- data.frame(label = c("0-30 cm", "30-100 cm"), top = c(0, 300), bottom = c(300, 
        1000))
    for (i in seq_len(nrow(m))) {
        stopifnot(md5(m$input_file[i]) == m$input_md5[i])
        x <- fromJSON(m$input_file[i], simplifyVector = FALSE)
        p <- node(x, "Models.Soils.Physical")
        o <- node(x, "Models.Soils.Organic")
        ch <- node(x, "Models.Soils.Chemical")
        th <- vec(p$Thickness)
        stopifnot(sum(th) == 2000, all(vec(o$Thickness) == th), all(vec(ch$Thickness) == th))
        eq(vec(p$AirDry), airdry(p))
        h <- data.frame(profile_id = m$profile_id[i], source = m$source[i], layer = seq_along(th), 
            thickness_mm = th, bottom_mm = cumsum(th), OC = vec(o$Carbon), pH = vec(ch$PH), KSAT = vec(p$KS), 
            LL15 = vec(p$LL15), AirDry = vec(p$AirDry), BD = vec(p$BD), DUL = vec(p$DUL), SAT = vec(p$SAT), 
            sand = vec(p$ParticleSizeSand), silt = vec(p$ParticleSizeSilt), clay = vec(p$ParticleSizeClay))
        fields <- c("OC", "pH", "KSAT", "LL15", "AirDry", "BD", "DUL", "SAT", "sand", "silt", "clay")
        stopifnot(all(vapply(h[fields], function(v) all(is.finite(v)), logical(1))))
        hh[[i]] <- h
        for (k in seq_len(nrow(intervals))) {
            z <- intervals[k, ]
            w <- pmax(0, pmin(h$bottom_mm, z$bottom) - pmax(h$bottom_mm - th, z$top))
            eq(sum(w), z$bottom - z$top)
            q <- data.frame(profile_id = m$profile_id[i], source = m$source[i], interval = z$label)
            for (n in c("OC", "pH", "sand", "silt", "clay")) q[[n]] <- sum(h[[n]] * w)/sum(w)
            means[[length(means) + 1]] <- q
        }
        if (i%%200 == 0) {
            cat("Read soil inputs", i, "/", nrow(m), "\n")
            flush.console()
        }
    }
    dir.create(out, recursive = TRUE)
    write.csv(do.call(rbind, hh), file.path(out, "input_horizons.csv"), row.names = FALSE)
    write.csv(do.call(rbind, means), file.path(out, "depth_predictors.csv"), row.names = FALSE)
    writej(list(status = if (test) "TEST_SOIL_INPUTS_COMPLETE" else "SOIL_INPUTS_COMPLETE", profiles = nrow(m), 
        intervals = intervals$label, predictors = c("OC", "pH", "sand", "silt", "clay"), manifest_md5 = md5(file.path(root, 
            "manifest.csv")), preparation_md5 = md5(file.path(root, "preparation.json")), predictors_md5 = md5(file.path(out, 
            "depth_predictors.csv")), horizons_md5 = md5(file.path(out, "input_horizons.csv"))), 
        file.path(out, "status.json"))
    capture.output(sessionInfo(), file = file.path(out, "sessionInfo.txt"))
}

# Join each DSM cell to its dominant SSURGO map unit and summarize paired yield differences.
# options is a named list of input/output paths supplied by the numbered entry script.
summarize_yield_differences <- function (options) 
{
    here <- .workflow_directory
    args_named <- function() options
    commandArgs <- function(trailingOnly = FALSE) {
        if (trailingOnly) 
            return(unlist(Map(function(n, v) c(paste0("--", n), v), names(options), options)))
        paste0("--file=", file.path(here, "yield_analysis.R"))
    }
    a <- args_named()
    results <- normal(need(a, "results"))
    index_path <- normal(need(a, "cell-index"))
    out <- normal(need(a, "out"), FALSE)
    stopifnot(!dir.exists(out))
    status <- fromJSON(file.path(results, "status.json"))
    stopifnot(status$status == "ALL2036_EXTRACTED_REVIEW_AND_SPATIAL_JOIN_PENDING", status$profiles == 
        2036, status$mode == "PRODUCTION")
    h <- status$artifact_hashes
    stopifnot(all(unname(tools::md5sum(file.path(results, h$path))) == h$md5))
    index <- read.csv(index_path, colClasses = c(mukey = "character"))
    stopifnot(nrow(index) == 1943, !anyDuplicated(index$profile_id), all(index$area_ha == 0.0144))
    summaries <- pairs <- list()
    bu <- 1000 * 0.40468564224/(56 * 0.45359237)/(1 - 0.155)
    for (y in c(2017, 2019, 2021)) {
        r <- read.csv(file.path(results, "year_profiles", paste0("profile_results_", y, ".csv")))
        stopifnot(nrow(r) == 2036, !anyDuplicated(r$profile_id), all(r$crop_year_QA == "PASS"))
        ss <- r[r$source == "SSURGO", ]
        stopifnot(nrow(ss) == 6, all(grepl("_[0-9]+_d$", ss$profile_id)))
        ss$mukey <- sub(".*_([0-9]+)_d$", "\\1", ss$profile_id)
        stopifnot(!anyDuplicated(ss$mukey))
        p <- index
        p$year <- y
        p$dsm <- r$grain_Mg_ha[match(p$profile_id, r$profile_id)]
        p$dominant <- ss$grain_Mg_ha[match(p$mukey, ss$mukey)]
        stopifnot(all(is.finite(p$dsm)), all(is.finite(p$dominant)))
        p$delta <- p$dsm - p$dominant
        pairs[[as.character(y)]] <- p
        for (g in c("SU", "SH", "BS", "FS", "TS", "Field")) {
            z <- if (g == "Field") 
                p
            else p[p$DHP == g, ]
            stopifnot(nrow(z) > 1)
            summaries[[length(summaries) + 1]] <- data.frame(year = y, DHP = g, Treatment = "dominant", 
                n = nrow(z), area_ha = sum(z$area_ha), DSM_mean = mean(z$dsm), SSURGO_mean = mean(z$dominant), 
                DSM_sd = sd(z$dsm), SSURGO_sd = sd(z$dominant), paired_difference = mean(z$delta), 
                paired_sd = sd(z$delta), percent_difference = 100 * mean(z$delta)/mean(z$dominant), 
                DSM_bu_ac_15_5 = mean(z$dsm) * bu, SSURGO_bu_ac_15_5 = mean(z$dominant) * bu)
        }
    }
    dir.create(out, recursive = TRUE)
    for (y in names(pairs)) write.csv(pairs[[y]], file.path(out, paste0("paired_cells_", y, ".csv")), 
        row.names = FALSE)
    write.csv(do.call(rbind, summaries), file.path(out, "descriptive_summary.csv"), row.names = FALSE)
    ctx <- fromJSON(status$context_key)
    files <- list.files(out, pattern = "[.]csv$", full.names = TRUE)
    writej(list(status = "PAIRED_SUMMARIES_COMPLETE_FIGURES_PENDING", historical = FALSE, cells = 1943, 
        years = c(2017, 2019, 2021), area_ha = sum(index$area_ha), index_md5 = md5(index_path), 
        manifest_md5 = ctx$manifest_md5, preparation_md5 = ctx$preparation_md5, results_status_md5 = md5(file.path(results, 
            "status.json")), artifact_hashes = data.frame(path = basename(files), md5 = unname(tools::md5sum(files))), 
        percent_denominator = "SSURGO mean", bushels = "56-lb bushels at 15.5 percent moisture; APSIM grain dry matter divided by 0.845", 
        scope = "Descriptive modeled field; no sampling uncertainty or observed accuracy claim."), 
        file.path(out, "status.json"))
    capture.output(sessionInfo(), file = file.path(out, "sessionInfo.txt"))
}

# Fit descriptive regressions using raw soil inputs, separately by year, depth and soil map.
# options is a named list of input/output paths supplied by the numbered entry script.
relate_soils_to_yield <- function (options) 
{
    here <- .workflow_directory
    args_named <- function() options
    commandArgs <- function(trailingOnly = FALSE) {
        if (trailingOnly) 
            return(unlist(Map(function(n, v) c(paste0("--", n), v), names(options), options)))
        paste0("--file=", file.path(here, "yield_analysis.R"))
    }
    a <- args_named()
    results <- normal(need(a, "results"))
    soil <- normal(need(a, "soil-inputs"))
    out <- normal(need(a, "out"), FALSE)
    stopifnot(!dir.exists(out))
    s <- fromJSON(file.path(results, "status.json"))
    hs <- fromJSON(file.path(soil, "status.json"))
    stopifnot(s$status == "ALL2036_EXTRACTED_REVIEW_AND_SPATIAL_JOIN_PENDING", s$profiles == 2036, 
        hs$status == "SOIL_INPUTS_COMPLETE", hs$profiles == 2036)
    ctx <- fromJSON(s$context_key)
    stopifnot(ctx$manifest_md5 == hs$manifest_md5, ctx$preparation_md5 == hs$preparation_md5)
    stopifnot(md5(file.path(soil, "depth_predictors.csv")) == hs$predictors_md5, all(unname(tools::md5sum(file.path(results, 
        s$artifact_hashes$path))) == s$artifact_hashes$md5))
    p <- read.csv(file.path(soil, "depth_predictors.csv"))
    vars <- c("OC", "pH", "sand", "silt", "clay")
    fits <- corr <- list()
    for (depth in c("0-30 cm", "30-100 cm")) for (src in c("DSM", "SSURGO")) {
        q <- p[p$interval == depth & p$source == src, ]
        stopifnot(nrow(q) == if (src == "DSM") 
            2030
        else 6, !anyDuplicated(q$profile_id))
        cc <- cor(q[vars])
        ix <- which(upper.tri(cc), arr.ind = TRUE)
        corr[[length(corr) + 1]] <- data.frame(source = src, interval = depth, property1 = rownames(cc)[ix[, 
            1]], property2 = colnames(cc)[ix[, 2]], r = cc[ix])
        for (y in c(2017, 2019, 2021)) {
            r <- read.csv(file.path(results, "year_profiles", paste0("profile_results_", y, ".csv")))
            j <- match(q$profile_id, r$profile_id)
            stopifnot(!anyNA(j), all(r$crop_year_QA[j] == "PASS"))
            yy <- r$grain_Mg_ha[j]
            stopifnot(all(is.finite(yy)))
            for (v in vars) {
                xx <- q[[v]]
                stopifnot(all(is.finite(xx)), sd(xx) > 0)
                fit <- lm(yy ~ xx)
                loo <- if (src == "SSURGO") 
                  sapply(seq_along(yy), function(k) summary(lm(yy[-k] ~ xx[-k]))$r.squared)
                else NA_real_
                fits[[length(fits) + 1]] <- data.frame(year = y, source = src, interval = depth, 
                  property = v, n = length(yy), R2 = summary(fit)$r.squared, slope = unname(coef(fit)[2]), 
                  loo_min = if (src == "SSURGO") 
                    min(loo)
                  else NA_real_, loo_max = if (src == "SSURGO") 
                    max(loo)
                  else NA_real_)
            }
        }
    }
    dir.create(out, recursive = TRUE)
    write.csv(do.call(rbind, fits), file.path(out, "soil_yield_R2.csv"), row.names = FALSE)
    write.csv(do.call(rbind, corr), file.path(out, "raw_input_correlations.csv"), row.names = FALSE)
    writej(list(status = "ASSOCIATIONS_COMPLETE_FIGURES_PENDING", intervals = c("0-30 cm", "30-100 cm"), 
        predictors = vars, scope = "Associations only; leave-one-profile-out range is a descriptive influence check, not independent validation. No significance tests, causal isolation or overcalibration inference.", 
        results_status_md5 = md5(file.path(results, "status.json")), soil_status_md5 = md5(file.path(soil, 
            "status.json"))), file.path(out, "status.json"))
}

# Describe paired OC-input and yield differences without attributing causation to OC loss.
# options is a named list of input/output paths supplied by the numbered entry script.
relate_carbon_and_yield_differences <- function (options) 
{
    here <- .workflow_directory
    args_named <- function() options
    commandArgs <- function(trailingOnly = FALSE) {
        if (trailingOnly) 
            return(unlist(Map(function(n, v) c(paste0("--", n), v), names(options), options)))
        paste0("--file=", file.path(here, "yield_analysis.R"))
    }
    a <- args_named()
    paired <- normal(need(a, "paired"))
    soil <- normal(need(a, "soil-inputs"))
    out <- normal(need(a, "out"), FALSE)
    stopifnot(!dir.exists(out))
    ps <- fromJSON(file.path(paired, "status.json"))
    hs <- fromJSON(file.path(soil, "status.json"))
    stopifnot(ps$status == "PAIRED_SUMMARIES_COMPLETE_FIGURES_PENDING", hs$status == "SOIL_INPUTS_COMPLETE", 
        hs$profiles == 2036)
    stopifnot(!is.null(ps$manifest_md5), ps$manifest_md5 == hs$manifest_md5, ps$preparation_md5 == 
        hs$preparation_md5, all(unname(tools::md5sum(file.path(paired, ps$artifact_hashes$path))) == 
        ps$artifact_hashes$md5))
    stopifnot(md5(file.path(soil, "depth_predictors.csv")) == hs$predictors_md5)
    p <- read.csv(file.path(soil, "depth_predictors.csv"))
    cells <- fits <- list()
    for (depth in c("0-30 cm", "30-100 cm")) {
        q <- p[p$interval == depth, ]
        stopifnot(nrow(q) == 2036)
        ss <- q[q$source == "SSURGO", ]
        stopifnot(nrow(ss) == 6)
        ss$mukey <- sub(".*_([0-9]+)_d$", "\\1", ss$profile_id)
        for (y in c(2017, 2019, 2021)) {
            z <- read.csv(file.path(paired, paste0("paired_cells_", y, ".csv")))
            stopifnot(nrow(z) == 1943, all(z$year == y))
            z$DSM_OC <- q$OC[match(z$profile_id, q$profile_id)]
            z$SSURGO_OC <- ss$OC[match(as.character(z$mukey), ss$mukey)]
            stopifnot(all(is.finite(z$DSM_OC)), all(is.finite(z$SSURGO_OC)))
            z$delta_OC <- z$DSM_OC - z$SSURGO_OC
            z$interval <- depth
            cells[[length(cells) + 1]] <- z[c("x", "y", "DHP", "year", "interval", "DSM_OC", "SSURGO_OC", 
                "delta_OC", "delta")]
            for (g in c("Field", "SU", "SH", "BS", "FS", "TS")) {
                r <- if (g == "Field") 
                  z
                else z[z$DHP == g, ]
                fit <- lm(delta ~ delta_OC, data = r)
                fits[[length(fits) + 1]] <- data.frame(year = y, interval = depth, DHP = g, n = nrow(r), 
                  DSM_OC = mean(r$DSM_OC), SSURGO_OC = mean(r$SSURGO_OC), delta_OC = mean(r$delta_OC), 
                  delta_yield = mean(r$delta), R2 = summary(fit)$r.squared, slope = unname(coef(fit)[2]))
            }
        }
    }
    dir.create(out, recursive = TRUE)
    write.csv(do.call(rbind, cells), file.path(out, "paired_OC_yield_differences.csv"), row.names = FALSE)
    write.csv(do.call(rbind, fits), file.path(out, "OC_difference_associations.csv"), row.names = FALSE)
    writej(list(status = "OC_DIFFERENCE_ASSOCIATIONS_COMPLETE", paired_status_md5 = md5(file.path(paired, 
        "status.json")), soil_status_md5 = md5(file.path(soil, "status.json")), scope = "Initial mapped OC contrasts, not observed OC loss or isolated causal productivity effects."), 
        file.path(out, "status.json"))
}

# Compute native-grid empirical semivariograms and fit spherical, exponential and Gaussian models.
# options is a named list of input/output paths supplied by the numbered entry script.
describe_spatial_dependence <- function (options) 
{
    here <- .workflow_directory
    args_named <- function() options
    commandArgs <- function(trailingOnly = FALSE) {
        if (trailingOnly) 
            return(unlist(Map(function(n, v) c(paste0("--", n), v), names(options), options)))
        paste0("--file=", file.path(here, "yield_analysis.R"))
    }
    a <- args_named()
    paired <- normal(need(a, "paired"))
    out <- normal(need(a, "out"), FALSE)
    stopifnot(!dir.exists(out))
    st <- fromJSON(file.path(paired, "status.json"))
    stopifnot(st$cells == 1943, identical(isTRUE(st$historical), FALSE))
    stopifnot(st$status == "PAIRED_SUMMARIES_COMPLETE_FIGURES_PENDING")
    stopifnot(!is.null(st$artifact_hashes), all(unname(tools::md5sum(file.path(paired, st$artifact_hashes$path))) == 
        st$artifact_hashes$md5))
    em <- candidates <- list()
    sources <- list()
    for (y in c(2017, 2019, 2021)) {
        path <- file.path(paired, paste0("paired_cells_", y, ".csv"))
        d <- read.csv(path)
        stopifnot(nrow(d) == 1943, all(d$year == y), !anyDuplicated(paste(d$x, d$y)), all(is.finite(as.matrix(d[c("x", 
            "y", "dsm", "dominant", "delta")]))))
        eq(d$delta, d$dsm - d$dominant)
        sources[[as.character(y)]] <- list(file = basename(path), md5 = md5(path))
        distance <- as.numeric(dist(d[c("x", "y")]))
        lag <- cut(distance, seq(0, 288, 24), labels = FALSE, right = TRUE)
        for (map in c("dsm", "dominant", "delta")) {
            gamma <- 0.5 * as.numeric(dist(d[[map]]))^2
            z <- do.call(rbind, lapply(1:12, function(k) {
                j <- which(lag == k)
                stopifnot(length(j) > 0)
                data.frame(year = y, map = map, lower_m = (k - 1) * 24, upper_m = k * 24, mean_distance_m = mean(distance[j]), 
                  pairs = length(j), semivariance = mean(gamma[j]))
            }))
            em[[length(em) + 1]] <- z
            for (model in c("spherical", "exponential", "gaussian")) candidates[[length(candidates) + 
                1]] <- fit_one(z, model, "pairs_over_distance_squared", y, map)
        }
        rm(distance, lag, gamma)
        gc(FALSE)
    }
    ff <- do.call(rbind, candidates)
    selected <- do.call(rbind, lapply(split(ff, interaction(ff$year, ff$map, drop = TRUE)), function(z) z[which.min(z$weighted_mean_squared_residual), 
        ]))
    selected$effective_range_m <- ifelse(selected$model == "spherical", selected$finite_sill_range_m, 
        selected$practical_range95_m)
    dir.create(out, recursive = TRUE)
    write.csv(do.call(rbind, em), file.path(out, "empirical_variograms.csv"), row.names = FALSE)
    write.csv(ff, file.path(out, "variogram_candidates.csv"), row.names = FALSE)
    write.csv(selected, file.path(out, "variogram_parameters.csv"), row.names = FALSE)
    writej(list(status = "VARIOGRAMS_COMPLETE_FIGURES_PENDING", historical = FALSE, lag_width_m = 24, 
        maximum_lag_m = 288, sources = sources, solver_md5 = md5(file.path(here, "variogram_functions.R")), 
        models = c("spherical", "exponential", "gaussian"), weighting = "pairs_over_distance_squared", 
        range = "Spherical finite sill range; exponential and Gaussian distance attaining 95 percent of structured partial sill. Undefined or unresolved ranges remain NA.", 
        scope = "Descriptive model selection by minimum weighted residual; no p-values or confidence intervals."), 
        file.path(out, "status.json"))
    capture.output(sessionInfo(), file = file.path(out, "sessionInfo.txt"))
}

# Calculate seasonal PRISM departures from archived normals; this does not replace APSIM weather.
# options is a named list of input/output paths supplied by the numbered entry script.
summarize_seasonal_climate <- function (options) 
{
    here <- .workflow_directory
    args_named <- function() options
    commandArgs <- function(trailingOnly = FALSE) {
        if (trailingOnly) 
            return(unlist(Map(function(n, v) c(paste0("--", n), v), names(options), options)))
        paste0("--file=", file.path(here, "yield_analysis.R"))
    }
    a <- args_named()
    src <- normal(need(a, "source-data"))
    metpath <- normal(need(a, "weather"))
    out <- normal(need(a, "out"), FALSE)
    stopifnot(!dir.exists(out))
    upstream <- fromJSON(file.path(src, "status.json"))
    stopifnot(upstream$status == "CLIMATE_CONTEXT_COMPLETE", md5(metpath) == upstream$simulation_weather_md5)
    raw_hashes <- read.csv(file.path(src, "raw_hashes.csv"))
    stopifnot(all(unname(tools::md5sum(file.path(src, "raw", raw_hashes$file))) == raw_hashes$md5))
    dir.create(out, recursive = TRUE)
    writecsv <- function(x, name) write.csv(x, file.path(out, name), row.names = FALSE, na = "")
    read_prism <- function(name) {
        path <- file.path(src, "raw", name)
        tx <- readLines(path, warn = FALSE)
        start <- grep("^Date,", tx)
        stopifnot(length(start) == 1)
        read.csv(path, skip = start - 1, check.names = FALSE, stringsAsFactors = FALSE)
    }
    norm <- read_prism("monthly_normals.csv")
    monthly <- read_prism("monthly_201701_202112.csv")
    annual <- read_prism("yearly_2017_2021.csv")
    stopifnot(nrow(norm) == 13, nrow(monthly) == 60, nrow(annual) == 5)
    names(norm) <- c("period", "precip_mm", "tmean_C")
    names(monthly) <- c("date", "precip_mm", "tmean_C")
    names(annual) <- c("year", "precip_mm", "tmean_C")
    monthly$year <- as.integer(substr(monthly$date, 1, 4))
    monthly$month <- as.integer(substr(monthly$date, 6, 7))
    stopifnot(!anyDuplicated(monthly$date), all(monthly$precip_mm >= 0), all(is.finite(monthly$tmean_C)))
    monthly$normal_precip_mm <- norm$precip_mm[monthly$month]
    monthly$normal_tmean_C <- norm$tmean_C[monthly$month]
    monthly$precip_departure_mm <- monthly$precip_mm - monthly$normal_precip_mm
    monthly$precip_departure_percent <- 100 * monthly$precip_departure_mm/monthly$normal_precip_mm
    monthly$tmean_departure_C <- monthly$tmean_C - monthly$normal_tmean_C
    writecsv(monthly, "prism_monthly_2017_2021.csv")
    writecsv(norm, "prism_normals_1991_2020.csv")
    years <- c(2017, 2019, 2021)
    days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    context <- do.call(rbind, lapply(years, function(y) do.call(rbind, lapply(c("Annual", "May-September"), 
        function(period) {
            z <- monthly[monthly$year == y, ]
            n <- norm[seq_len(12), ]
            if (period == "Annual") {
                a <- annual[annual$year == y, ]
                p <- sum(z$precip_mm)
                t <- a$tmean_C
                np <- sum(n$precip_mm)
                nt <- norm$tmean_C[13]
            }
            else {
                z <- z[z$month %in% 5:9, ]
                p <- sum(z$precip_mm)
                t <- weighted.mean(z$tmean_C, days[z$month])
                np <- sum(n$precip_mm[5:9])
                nt <- weighted.mean(n$tmean_C[5:9], days[5:9])
            }
            data.frame(year = y, period = period, precip_mm = p, normal_precip_mm = np, precip_departure_mm = p - 
                np, precip_departure_percent = 100 * (p - np)/np, tmean_C = t, normal_tmean_C = nt, 
                tmean_departure_C = t - nt, precipitation_aggregation = "Sum of monthly values for both selected period and normal", 
                temperature_aggregation = if (period == "Annual") 
                  "PRISM annual values"
                else "Day-weighted monthly means (153 days)", normal_period = "1991-2020", source = "PRISM 4-km enclosing grid cell; no interpolation")
        }))))
    writecsv(context, "prism_climate_context.csv")
    annual_check <- merge(aggregate(precip_mm ~ year, monthly, sum), annual, by = "year", suffixes = c("_monthly_sum", 
        "_annual"))
    annual_check$annual_minus_monthly_mm <- annual_check$precip_mm_annual - annual_check$precip_mm_monthly_sum
    writecsv(annual_check, "prism_annual_aggregation_check.csv")
    print(annual_check)
    stopifnot(abs(sum(norm$precip_mm[1:12]) - norm$precip_mm[13]) < 0.12)
    tx <- readLines(metpath)
    header <- grep("^year day ", tx)
    stopifnot(length(header) == 1)
    met <- read.table(metpath, skip = header + 1, col.names = strsplit(tx[header], " +")[[1]])
    met$date <- as.Date(paste0(met$year, "-01-01")) + met$day - 1
    met$month <- as.integer(format(met$date, "%m"))
    met$tmean_C <- (met$maxt + met$mint)/2
    stopifnot(nrow(met) == 3287, identical(range(met$year), c(2013L, 2021L)), !anyDuplicated(met$date), 
        all(diff(met$date) == 1))
    met_summary <- do.call(rbind, lapply(years, function(y) do.call(rbind, lapply(c("Annual", "May-September"), 
        function(period) {
            z <- met[met$year == y & (period == "Annual" | met$month %in% 5:9), ]
            data.frame(year = y, period = period, days = nrow(z), precip_mm = sum(z$rain), tmean_C = mean(z$tmean_C), 
                maxt_C = mean(z$maxt), mint_C = mean(z$mint), radiation_MJ_m2 = sum(z$radn), source = "APSIM archived IA-IA0200 weather; no 30-year normal available in this file")
        }))))
    writecsv(met_summary, "apsim_actual_weather_context.csv")
    writecsv(merge(context[, c("year", "period", "precip_mm", "tmean_C")], met_summary[, c("year", 
        "period", "precip_mm", "tmean_C")], by = c("year", "period"), suffixes = c("_PRISM", "_APSIM")), 
        "prism_vs_simulation_weather.csv")
    writej(list(status = "CLIMATE_CONTEXT_REPRODUCED", archived_source_status_md5 = md5(file.path(src, 
        "status.json")), archived_access_date = upstream$access_date, reproduced_at = as.character(Sys.time()), 
        normal_period = "1991-2020", years = years, simulation_weather_md5 = md5(metpath), source_geometry_check = "Preserved in archived source status; not rerun by this arithmetic-only module", 
        annual_precipitation_caveat = upstream$annual_precipitation_caveat, dataset_label_caveat = upstream$dataset_label_caveat, 
        scope = "PRISM context is not APSIM forcing; years also differ in evolving soil/crop history."), 
        file.path(out, "status.json"))
    capture.output(sessionInfo(), file = file.path(out, "sessionInfo.txt"))
}

# Summarize upper, middle and lower positions and within-map-unit yield variation.
# options is a named list of input/output paths supplied by the numbered entry script.
summarize_hillslope_positions <- function (options) 
{
    here <- .workflow_directory
    args_named <- function() options
    commandArgs <- function(trailingOnly = FALSE) {
        if (trailingOnly) 
            return(unlist(Map(function(n, v) c(paste0("--", n), v), names(options), options)))
        paste0("--file=", file.path(here, "yield_analysis.R"))
    }
    a <- args_named()
    paired <- normal(need(a, "paired"))
    out <- normal(need(a, "out"), FALSE)
    stopifnot(!dir.exists(out))
    dir.create(out, recursive = TRUE)
    all <- do.call(rbind, lapply(c(2017, 2019, 2021), function(y) read.csv(file.path(paired, paste0("paired_cells_", 
        y, ".csv")))))
    stopifnot(nrow(all) == 1943 * 3)
    all$group <- c(SU = "Upper", SH = "Upper", BS = "Middle", FS = "Lower", TS = "Lower")[all$DHP]
    stopifnot(!anyNA(all$group))
    all$delta <- all$dsm - all$dominant
    summary <- do.call(rbind, lapply(split(all, list(all$year, all$group), drop = TRUE), function(d) data.frame(year = d$year[1], 
        group = d$group[1], cells = nrow(d), area_ha = nrow(d) * 144/10000, DSM = mean(d$dsm), SSURGO = mean(d$dominant), 
        difference = mean(d$delta), DSM_spatial_sd = sd(d$dsm))))
    write.csv(summary, file.path(out, "grouped_landscape_summary.csv"), row.names = FALSE)
    within <- do.call(rbind, lapply(split(all, list(all$year, all$mukey), drop = TRUE), function(d) data.frame(year = d$year[1], 
        mukey = d$mukey[1], cells = nrow(d), DSM_sd = sd(d$dsm), DSM_p05 = quantile(d$dsm, 0.05, 
            names = FALSE), DSM_p95 = quantile(d$dsm, 0.95, names = FALSE), SSURGO_values = length(unique(d$dominant)))))
    write.csv(within, file.path(out, "within_polygon_spread.csv"), row.names = FALSE)
    writej(list(status = "DESCRIPTIVE_GROUPING_VERIFIED", cells_per_year = 1943, grouping = "Upper SU+SH; Middle BS; Lower FS+TS", 
        inference = "No tests; repeated spatial cells are not independent replication", source_md5 = as.list(tools::md5sum(file.path(paired, 
            paste0("paired_cells_", c(2017, 2019, 2021), ".csv"))))), file.path(out, "status.json"))
}
