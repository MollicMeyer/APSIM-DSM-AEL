# Final scientific workflow functions.
# Called by the three numbered scripts in the repository root.


# Relocate the portable model inputs and shared weather without changing soil values.
# options is a named list of input/output paths supplied by the numbered entry script.
import_soil_inputs <- function (options) 
{
    here <- .workflow_directory
    args_named <- function() options
    commandArgs <- function(trailingOnly = FALSE) {
        if (trailingOnly) 
            return(unlist(Map(function(n, v) c(paste0("--", n), v), names(options), options)))
        paste0("--file=", file.path(here, "profile_preparation.R"))
    }
    a <- args_named()
    bundle <- normal(need(a, "bundle"))
    out <- normal(need(a, "out"), FALSE)
    stopifnot(!dir.exists(out))
    b <- fromJSON(file.path(bundle, "bundle.json"))
    mp <- file.path(bundle, "bundle_manifest.csv")
    stopifnot(md5(mp) == b$manifest_md5, b$status == "EXPORTED_NOT_PUBLICATION_APPROVED")
    m <- read.csv(mp)
    stopifnot(nrow(m) == b$profiles, !anyDuplicated(m$relative_input), all(grepl("^inputs/(DSM|SSURGO)/[A-Za-z0-9_]+/simulation[.]apsimx$", 
        m$relative_input)), identical(b$weather, "weather/forcing.met"))
    stopifnot(all(unname(tools::md5sum(file.path(bundle, m$relative_input))) == m$bundle_md5), md5(file.path(bundle, 
        b$weather)) == b$weather_md5)
    dir.create(file.path(out, "weather"), recursive = TRUE)
    weather <- file.path(out, "weather/forcing.met")
    stopifnot(file.copy(file.path(bundle, b$weather), weather, overwrite = FALSE))
    records <- list()
    stopifnot(md5(weather) == b$weather_md5)
    mask_weather <- function(z) map_nodes(z, function(q) {
        if (type_is(q, "Models.Climate.Weather")) 
            q$FileName <- "<weather>"
        q
    })
    for (i in seq_len(nrow(m))) {
        x <- fromJSON(file.path(bundle, m$relative_input[i]), simplifyVector = FALSE)
        stopifnot(node(x, "Models.Climate.Weather")$FileName == "../../../weather/forcing.met")
        y <- map_nodes(x, function(z) {
            if (type_is(z, "Models.Climate.Weather")) 
                z$FileName <- weather
            z
        })
        stopifnot(identical(mask_weather(x), mask_weather(y)))
        dest <- file.path(out, m$relative_input[i])
        dir.create(dirname(dest), recursive = TRUE)
        writej(y, dest)
        records[[i]] <- data.frame(source = m$source[i], profile_id = m$profile_id[i], input_file = dest, 
            input_md5 = md5(dest), bundle_md5 = m$bundle_md5[i], upstream_md5 = m$upstream_md5[i])
        if (i%%200 == 0) {
            cat("Imported", i, "/", nrow(m), "\n")
            flush.console()
        }
    }
    write.csv(do.call(rbind, records), file.path(out, "manifest.csv"), row.names = FALSE)
    writej(list(status = "IMPORTED", profiles = nrow(m), bundle_manifest_md5 = b$manifest_md5, weather_md5 = md5(weather), 
        bundle_mode = b$mode), file.path(out, "import.json"))
}

# Apply the common AirDry rule; check layer depths, crop limits, mineral N, water and C:N.
# options is a named list of input/output paths supplied by the numbered entry script.
prepare_soil_profiles <- function (options) 
{
    here <- .workflow_directory
    args_named <- function() options
    commandArgs <- function(trailingOnly = FALSE) {
        if (trailingOnly) 
            return(unlist(Map(function(n, v) c(paste0("--", n), v), names(options), options)))
        paste0("--file=", file.path(here, "profile_preparation.R"))
    }
    a <- args_named()
    src <- normal(need(a, "source-manifest"))
    out <- normal(need(a, "out"), FALSE)
    stopifnot(!dir.exists(out))
    dir.create(out, recursive = TRUE)
    m <- read.csv(src)
    m <- m[m$source == "DSM" | (m$source == "SSURGO" & grepl("_[0-9]+_d$", m$profile_id)), ]
    stopifnot(nrow(m) == 2036, sum(m$source == "DSM") == 2030, sum(m$source == "SSURGO") == 6)
    if (identical(a[["test-pair"]], "yes")) {
        stopifnot(is.null(a[["limit"]]))
        m <- m[c(which(m$source == "DSM")[1], which(m$source == "SSURGO")[1]), ]
    }
    else if (!is.null(a[["limit"]])) 
        m <- m[seq_len(as.integer(a[["limit"]])), ]
    dir.create(file.path(out, "weather"))
    w <- node(fromJSON(m$input_file[1], simplifyVector = FALSE), "Models.Climate.Weather")$FileName
    w <- normal(w)
    weather <- file.path(out, "weather/forcing.met")
    stopifnot(file.copy(w, weather, overwrite = FALSE), md5(w) == md5(weather))
    manifest <- layers <- list()
    for (i in seq_len(nrow(m))) {
        stopifnot(md5(m$input_file[i]) == m$input_md5[i])
        old <- fromJSON(m$input_file[i], simplifyVector = FALSE)
        p <- node(old, "Models.Soils.Physical")
        th <- vec(p$Thickness)
        ad <- airdry(p)
        stopifnot(sum(th) == 2000, all(ad <= vec(p$LL15)), all(ad <= vec(p$AirDry) + 1e-10), all(vec(node(old, 
            "Models.Soils.Organic")$SoilCNRatio) == 12))
        for (crop in Filter(function(z) type_is(z, "Models.Soils.SoilCrop"), p$Children)) stopifnot(all(vec(crop$LL) >= 
            ad - 5e-04))
        stopifnot(md5(node(old, "Models.Climate.Weather")$FileName) == md5(w))
        x <- map_nodes(old, function(z) {
            if (type_is(z, "Models.Soils.Physical")) 
                z$AirDry <- as.list(ad)
            if (type_is(z, "Models.Climate.Weather")) 
                z$FileName <- weather
            if (type_is(z, "Models.Report") && z$Name %in% c("WarmupDaily", "WarmupStart")) 
                z$VariableNames <- c(z$VariableNames, as.list(c("[SoilWater].Es as AD_Es", "[SoilWater].SWmm as AD_SWmm", 
                  "[SoilWater].Drainage as AD_Drainage", "[SoilWater].Runoff as AD_Runoff")))
            z
        })
        stopifnot(identical(mask_allowed(old), mask_allowed(x)))
        sw <- vec(node(x, "Models.Soils.Water")$InitialValues)
        eq(sum(sw * th), 609)
        sol <- Filter(function(z) type_is(z, "Models.Soils.Solute") && z$Name %in% c("NO3", "NH4"), 
            walk(x))
        stopifnot(length(sol) == 2)
        eq(sum(unlist(lapply(sol, function(z) {
            stopifnot(z$InitialValuesUnits == 1)
            vec(z$InitialValues)
        }))), 63.9519166666667)
        rel <- paste0("inputs/", m$source[i], "/", m$profile_id[i], "/simulation.apsimx")
        f <- file.path(out, rel)
        dir.create(dirname(f), recursive = TRUE)
        writej(x, f)
        manifest[[i]] <- data.frame(run_id = paste(m$source[i], m$profile_id[i], sep = "_"), source = m$source[i], 
            profile_id = m$profile_id[i], original_input = m$input_file[i], original_md5 = m$input_md5[i], 
            relative_input = rel, input_file = f, input_md5 = md5(f), expected_db = sub("[.]apsimx$", 
                ".db", f))
        layers[[i]] <- data.frame(run_id = manifest[[i]]$run_id, layer = seq_along(th), thickness_mm = th, 
            AirDry = ad, LL15 = vec(p$LL15), SW = sw)
        if (i%%200 == 0) {
            cat("Prepared", i, "/", nrow(m), "\n")
            flush.console()
        }
    }
    write.csv(do.call(rbind, manifest), file.path(out, "manifest.csv"), row.names = FALSE)
    write.csv(do.call(rbind, layers), file.path(out, "expected_layers.csv"), row.names = FALSE)
    writej(list(status = "PREPARED", profiles = nrow(m), DSM = sum(m$source == "DSM"), SSURGO = sum(m$source == 
        "SSURGO"), weather = weather, weather_md5 = md5(weather), source_manifest = src, source_manifest_md5 = md5(src), 
        rule = "0-15cm:0.5LL15;15-30cm:0.8LL15;deeper:LL15;overlap-weighted;cropLLunchanged", initial_N_kg_ha = 63.9519166666667, 
        initial_water_mm = 609, CN = 12, scope = if (nrow(m) == 2036) "FULLFIELD" else "SMOKE_TEST"), 
        file.path(out, "preparation.json"))
    capture.output(sessionInfo(), file = file.path(out, "sessionInfo_prepare.txt"))
    cat("Preparation complete\n")
}
