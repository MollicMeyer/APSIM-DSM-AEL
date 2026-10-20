# Final scientific workflow functions.
# Called by the three numbered scripts in the repository root.


# Draw the final yield, soil-depth, soil-yield association and semivariogram figures.
# options is a named list of input/output paths supplied by the numbered entry script.
plot_scientific_figures <- function (options) 
{
    here <- .workflow_directory
    args_named <- function() options
    commandArgs <- function(trailingOnly = FALSE) {
        if (trailingOnly) 
            return(unlist(Map(function(n, v) c(paste0("--", n), v), names(options), options)))
        paste0("--file=", file.path(here, "figures.R"))
    }
    library(ggplot2)
    library(grid)
    a <- args_named()
    paired <- normal(need(a, "paired"))
    soil <- normal(need(a, "soil-inputs"))
    associations <- normal(need(a, "associations"))
    variograms <- normal(need(a, "variograms"))
    out <- normal(need(a, "out"), FALSE)
    if (!identical(a[["skip-depth"]], "yes")) {
        library(aqp)
        library(lattice)
    }
    stopifnot(!dir.exists(out), fromJSON(file.path(paired, "status.json"))$status == "PAIRED_SUMMARIES_COMPLETE_FIGURES_PENDING", 
        fromJSON(file.path(soil, "status.json"))$status == "SOIL_INPUTS_COMPLETE", fromJSON(file.path(associations, 
            "status.json"))$status == "ASSOCIATIONS_COMPLETE_FIGURES_PENDING", fromJSON(file.path(variograms, 
            "status.json"))$status == "VARIOGRAMS_COMPLETE_FIGURES_PENDING")
    dir.create(out, recursive = TRUE)
    years <- c(2017, 2019, 2021)
    saveplot <- function(n, w, h, fun) {
        png(file.path(out, paste0(n, ".png")), width = w, height = h, units = "in", res = 450, type = "cairo", 
            bg = "white")
        fun()
        dev.off()
        cairo_pdf(file.path(out, paste0(n, ".pdf")), width = w, height = h, family = "Arial")
        fun()
        dev.off()
    }
    d <- do.call(rbind, lapply(years, function(y) read.csv(file.path(paired, paste0("paired_cells_", 
        y, ".csv")))))
    ds <- read.csv(file.path(paired, "descriptive_summary.csv"))
    stopifnot(nrow(d) == 1943 * 3, nrow(ds) == 18)
    dhpcols <- c(SU = "#E41A1C", SH = "#FF7F00", BS = "#F5DEB3", FS = "#ADD8E6", TS = "#984EA3", 
        SSURGO = "#4DAF4A")
    yearcols <- c(`2017` = "#0072B2", `2019` = "#009E73", `2021` = "#D55E00")
    sourcecols <- c(DSM = "darkgoldenrod4", SSURGO = "darkslategray")
    plots <- list()
    theme_orig <- theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank(), legend.position = "none", 
        plot.title = element_text(size = 12), axis.text = element_text(size = 11))
    for (y in years) {
        dd <- d[d$year == y, ]
        b <- rbind(data.frame(DHP = dd$DHP, Yield = dd$dsm), data.frame(DHP = "SSURGO", Yield = dd$dominant))
        b$DHP <- factor(b$DHP, levels = names(dhpcols))
        ss <- ds[ds$year == y & ds$DHP != "Field", ]
        ss$DHP <- factor(ss$DHP, levels = names(dhpcols)[1:5])
        plots[[length(plots) + 1]] <- ggplot(b, aes(DHP, Yield, fill = DHP)) + geom_boxplot(outlier.size = 0.5) + 
            scale_fill_manual(values = dhpcols) + labs(title = paste(paste0(letters[match(y, years) * 
            2 - 1], ")"), y, "Yield distribution"), x = NULL, y = expression(Yield ~ (Mg ~ ha^{
            -1
        }))) + theme_orig
        plots[[length(plots) + 1]] <- ggplot(ss, aes(DHP, paired_difference, fill = DHP)) + geom_col(width = 0.65, 
            colour = "black", size = 0.25) + geom_hline(yintercept = 0, colour = "red", linetype = 2) + 
            scale_fill_manual(values = dhpcols) + labs(title = paste(paste0(letters[match(y, years) * 
            2], ")"), y, "Mean paired difference"), x = NULL, y = expression(DSM - SSURGO ~ (Mg ~ 
            ha^{
                -1
            }))) + theme_orig
    }
    drawsummary <- function() {
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(4, 2, heights = unit(c(0.8, 2.55, 2.55, 2.55), 
            "null"))))
        pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1:2, xscale = c(0, 5), yscale = c(-0.35, 
            2.1)))
        xx <- seq(0, 5, length.out = 501)
        slope <- ifelse(xx < 1, 0, ifelse(xx < 2, -0.7 * (1 - cos(pi * (xx - 1)))/2, ifelse(xx < 
            3, -0.7, ifelse(xx < 4, -0.7 * (1 + cos(pi * (xx - 3)))/2, 0))))
        yy <- 1.2 + cumsum(slope) * 0.01
        grid.lines(unit(0.15 + 0.94 * xx, "native"), unit(yy, "native"), gp = gpar(col = "black", 
            lwd = 2.5))
        for (k in 1:5) {
            gx <- 0.15 + 0.94 * (k - 0.5)
            gy <- yy[which.min(abs(xx - (k - 0.5)))]
            grid.text(c("Summit (SU)", "Shoulder (SH)", "Backslope (BS)", "Footslope (FS)", "Toeslope (TS)")[k], 
                x = unit(gx, "native"), y = unit(gy + 0.5, "native"), gp = gpar(fontsize = 11, col = "black"))
        }
        popViewport()
        for (i in seq_along(plots)) {
            pushViewport(viewport(layout.pos.row = (i - 1)%/%2 + 2, layout.pos.col = (i - 1)%%2 + 
                1))
            grid.draw(ggplotGrob(plots[[i]]))
            popViewport()
        }
        popViewport()
    }
    saveplot("Figure_1_multiyear_yields", 7.5, 8.5, drawsummary)
    if (!identical(a[["skip-depth"]], "yes")) {
        h <- read.csv(file.path(soil, "input_horizons.csv"))
        stopifnot(length(unique(h$profile_id[h$source == "DSM"])) == 2030, length(unique(h$profile_id[h$source == 
            "SSURGO"])) == 6)
        h$top <- (h$bottom_mm - h$thickness_mm)/10
        h$bottom <- h$bottom_mm/10
        sites <- unique(h[c("profile_id", "source")])
        h$source <- NULL
        depths(h) <- profile_id ~ top + bottom
        site(h) <- sites
        site(h)$source <- factor(site(h)$source, levels = c("DSM", "SSURGO"))
        mean.sd <- function(v) {
            m <- mean(v, na.rm = TRUE)
            s <- sd(v, na.rm = TRUE)
            c(mean = m, lower = m - s, upper = m + s)
        }
        sl <- slab(h, fm = source ~ OC + BD + silt + clay + pH + AirDry + LL15 + DUL + SAT + KSAT, 
            slab.structure = c(0, 5, 15, 30, 60, 100, 150, 200), slab.fun = mean.sd)
        surface <- do.call(rbind, lapply(split(sl, list(sl$source, sl$variable), drop = TRUE), function(q) {
            q <- q[order(q$bottom), ]
            q[1, "bottom"] <- 0
            q[1, ]
        }))
        curves <- rbind(surface, sl)
        curves <- curves[order(curves$variable, curves$source, curves$bottom), ]
        curves$variable <- factor(curves$variable, levels = c("OC", "BD", "silt", "clay", "pH", 
            "AirDry", "LL15", "DUL", "SAT", "KSAT"))
        dp <- xyplot(bottom ~ mean | variable, data = curves, lower = curves$lower, upper = curves$upper, 
            groups = source, sync.colors = TRUE, alpha = 0.5, ylim = c(200, 0), layout = c(5, 2), 
            ylab = "Depth (cm)", xlab = "", scales = list(x = list(tick.number = 2, relation = "free", 
                cex = 1), y = list(at = seq(0, 200, 50), alternating = 1, cex = 1.15)), par.settings = list(fontsize = list(text = 16, 
                points = 12), superpose.line = list(lwd = 2, col = unname(sourcecols), lty = c(2, 
                2))), panel = panel.depth_function, prepanel = prepanel.depth_function, strip = strip.custom(bg = grey(0.8), 
                factor.levels = expression(OC ~ "%", BD ~ g ~ cm^{
                  -3
                }, Silt ~ "%", Clay ~ "%", pH, AirDry ~ mm ~ mm^{
                  -1
                }, LL15 ~ mm ~ mm^{
                  -1
                }, DUL ~ mm ~ mm^{
                  -1
                }, SAT ~ mm ~ mm^{
                  -1
                }, KSAT ~ mm ~ day^{
                  -1
                })), auto.key = list(columns = 2, lines = TRUE, points = FALSE, cex = 1.25))
        saveplot("Figure_3_depth_functions", 11, 8.5, function() print(dp))
        write.csv(curves, file.path(out, "depth_slabs.csv"), row.names = FALSE)
    }
    f <- read.csv(file.path(associations, "soil_yield_R2.csv"))
    raw <- c("OC", "pH", "sand", "silt", "clay")
    stopifnot(nrow(f) == 60, setequal(unique(f$property), raw))
    f$property <- factor(f$property, levels = rev(raw))
    f$source <- factor(f$source, levels = c("DSM", "SSURGO"))
    f$interval <- factor(f$interval, levels = c("0-30 cm", "30-100 cm"))
    f$year <- factor(f$year)
    barplot <- function(z, both = FALSE) {
        p <- ggplot(z, aes(R2, property, fill = year)) + geom_col(position = position_dodge(0.8), 
            width = 0.72) + scale_fill_manual(values = yearcols) + scale_x_continuous(limits = c(0, 
            1), breaks = c(0, 0.25, 0.5, 0.75, 1)) + labs(x = expression(R^2), y = NULL, fill = NULL) + 
            theme_bw(base_size = 19) + theme(legend.position = "bottom", panel.grid.major.y = element_blank(), 
            panel.grid.minor = element_blank(), panel.spacing = unit(1.4, "lines"), strip.text = element_text(size = 17, 
                margin = margin(8, 8, 8, 8)), axis.title.x = element_text(margin = margin(t = 12)), 
            plot.margin = margin(14, 16, 14, 16))
        if (both) 
            p + facet_grid(source ~ interval)
        else p + facet_wrap(~source, nrow = 1)
    }
    saveplot("Figure_4_raw_topsoil_R2", 9, 6.8, function() print(barplot(f[f$interval == "0-30 cm", 
        ])))
    saveplot("Figure_S5_topsoil_subsoil_R2", 11, 11, function() print(barplot(f, TRUE)))
    ds$DHP <- factor(ds$DHP, levels = c("SU", "SH", "BS", "FS", "TS", "Field"))
    ds$year <- factor(ds$year)
    pd <- ggplot(ds, aes(DHP, paired_difference, fill = year)) + geom_hline(yintercept = 0, color = "grey35") + 
        geom_col(width = 0.65) + geom_errorbar(aes(ymin = paired_difference - paired_sd, ymax = paired_difference + 
        paired_sd), width = 0.15) + facet_wrap(~year, ncol = 1) + scale_fill_manual(values = yearcols) + 
        labs(x = NULL, y = expression(DSM - SSURGO ~ yield ~ difference ~ (Mg ~ ha^{
            -1
        }))) + theme_bw(base_size = 19) + theme(legend.position = "none", panel.grid.minor = element_blank())
    saveplot("Figure_S2_DHP_differences", 9, 10, function() print(pd))
    em <- read.csv(file.path(variograms, "empirical_variograms.csv"))
    selected <- read.csv(file.path(variograms, "variogram_parameters.csv"))
    drawvar <- function() {
        par(mfrow = c(3, 3), mar = c(3.7, 4.2, 2.1, 0.6), mgp = c(2.6, 0.8, 0), cex = 1.15)
        for (y in years) for (map in c("dsm", "dominant", "delta")) {
            z <- em[em$year == y & em$map == map, ]
            q <- selected[selected$year == y & selected$map == map, ]
            stopifnot(nrow(q) == 1, nrow(z) == 12)
            plot(z$mean_distance_m, z$semivariance, pch = 16, xlim = c(0, 288), ylim = c(0, max(z$semivariance, 
                q$total_sill) * 1.1), xlab = "Lag distance (m)", ylab = expression(Semivariance ~ 
                (Mg ~ ha^{
                  -1
                })^2), main = paste(y, c(dsm = "DSM", dominant = "SSURGO", delta = "Difference")[[map]]))
            xx <- seq(0.001, 288, length.out = 800)
            lines(xx, q$nugget + q$partial_sill * shape(xx, q$fitted_scale_a_m, q$model), col = "#0072B2", 
                lwd = 2)
            abline(h = q$total_sill, lty = 3, col = "grey50")
            if (is.finite(q$effective_range_m)) 
                abline(v = q$effective_range_m, lty = 3, col = "#D55E00")
        }
    }
    saveplot("Figure_S3_variograms", 12, 11, drawvar)
    capture.output(sessionInfo(), file = file.path(out, "sessionInfo.txt"))
    writej(list(status = "SUMMARY_FIGURES_RENDERED_VISUAL_QA_PENDING", sources = as.list(setNames(unname(tools::md5sum(file.path(c(paired, 
        soil, associations, variograms), "status.json"))), c("paired", "soil", "associations", "variograms"))), 
        source_colors = as.list(sourcecols), figures = c("Figure1", "Figure3", "Figure4", "FigureS2", 
            "FigureS3", "FigureS5"), note = "Final scientific figures for the three-season soil input comparison."), 
        file.path(out, "status.json"))
}

# Draw polygon SSURGO, continuous DSM and difference maps with cubic interpolation for display only.
# options is a named list of input/output paths supplied by the numbered entry script.
plot_yield_maps <- function (options) 
{
    here <- .workflow_directory
    args_named <- function() options
    commandArgs <- function(trailingOnly = FALSE) {
        if (trailingOnly) 
            return(unlist(Map(function(n, v) c(paste0("--", n), v), names(options), options)))
        paste0("--file=", file.path(here, "figures.R"))
    }
    a <- args_named()
    paired <- normal(need(a, "paired"))
    polygon_path <- normal(need(a, "polygons"))
    out <- normal(need(a, "out"), FALSE)
    library(terra)
    stopifnot(!dir.exists(out))
    st <- fromJSON(file.path(paired, "status.json"))
    stopifnot(st$status == "PAIRED_SUMMARIES_COMPLETE_FIGURES_PENDING", st$cells == 1943)
    stopifnot(all(unname(tools::md5sum(file.path(paired, st$artifact_hashes$path))) == st$artifact_hashes$md5))
    for (e in parse(file.path(here, "map_functions.R"))) if (is.call(e) && identical(e[[1]], as.name("<-")) && 
        as.character(e[[2]]) %in% c("save_plot", "diffcols", "getcol", "draw_raster", "panel_legend", 
            "render_year")) 
        eval(e)
    years <- c(2017, 2019, 2021)
    d <- do.call(rbind, lapply(years, function(y) read.csv(file.path(paired, paste0("paired_cells_", 
        y, ".csv")))))
    geo <- fromJSON(polygon_path, simplifyVector = FALSE)
    stopifnot(grepl("32615", geo$crs$properties$name))
    pieces <- list()
    for (f in geo$features) {
        polys <- if (f$geometry$type == "MultiPolygon") 
            f$geometry$coordinates
        else list(f$geometry$coordinates)
        for (pol in polys) {
            pieces[[length(pieces) + 1]] <- list(mukey = as.character(f$properties$MUKEY), rings = lapply(pol, 
                function(r) do.call(rbind, lapply(r, unlist))))
        }
    }
    coords <- do.call(rbind, lapply(pieces, function(p) do.call(rbind, p$rings)))
    bb <- c(xmin = min(coords[, 1]), xmax = max(coords[, 1]), ymin = min(coords[, 2]), ymax = max(coords[, 
        2]))
    inside_ring <- function(x, y, r) {
        ans <- rep(FALSE, length(x))
        j <- nrow(r)
        for (i in seq_len(nrow(r))) {
            cross <- (r[i, 2] > y) != (r[j, 2] > y)
            hit <- cross & (x < (r[j, 1] - r[i, 1]) * (y - r[i, 2])/(r[j, 2] - r[i, 2] + 1e-100) + 
                r[i, 1])
            ans <- xor(ans, hit)
            j <- i
        }
        ans
    }
    inside <- function(x, y, rings) {
        v <- inside_ring(x, y, rings[[1]])
        if (length(rings) > 1) 
            for (h in rings[-1]) v <- v & !inside_ring(x, y, h)
        v
    }
    border_dist <- function(x, y, rings) {
        dist <- rep(Inf, length(x))
        for (r in rings) for (k in seq_len(nrow(r) - 1)) {
            v <- r[k + 1, ] - r[k, ]
            den <- sum(v * v)
            if (den == 0) 
                next
            t <- pmax(0, pmin(1, ((x - r[k, 1]) * v[1] + (y - r[k, 2]) * v[2])/den))
            dist <- pmin(dist, sqrt((x - r[k, 1] - t * v[1])^2 + (y - r[k, 2] - t * v[2])^2))
        }
        dist
    }
    for (k in seq_along(pieces)) {
        r <- pieces[[k]]$rings[[1]]
        g <- expand.grid(x = seq(min(r[, 1]), max(r[, 1]), length.out = 20), y = seq(min(r[, 2]), 
            max(r[, 2]), length.out = 20))
        g <- g[inside(g$x, g$y, pieces[[k]]$rings), ]
        if (nrow(g)) {
            g <- g[order(border_dist(g$x, g$y, pieces[[k]]$rings), decreasing = TRUE), ]
        }
        pieces[[k]]$candidates <- g
    }
    dir.create(out, recursive = TRUE)
    yieldcols <- c("#C75D36", "#E7A315", "#BCE500", "#31CE26", "#159370", "#0C377C")
    pal <- colorRampPalette(yieldcols)(256)
    field <- terra::vect(polygon_path)
    maps <- list()
    audit <- list()
    label_audit <- list()
    for (y in years) {
        z <- d[d$year == y, ]
        z$delta <- z$dsm - z$dominant
        val <- unique(z[c("mukey", "dominant")])
        stopifnot(nrow(val) == 6)
        nr <- terra::rast(z[, c("x", "y", "dsm")], type = "xyz", crs = "EPSG:32615")
        nd <- terra::rast(z[, c("x", "y", "delta")], type = "xyz", crs = "EPSG:32615")
        stopifnot(all(res(nr) == 12))
        display <- terra::rast(ext(nr), resolution = 3, crs = crs(nr))
        rr <- mask(resample(nr, display, method = "cubic"), field)
        dr <- mask(resample(nd, display, method = "cubic"), field)
        yl <- as.numeric(quantile(c(z$dsm, z$dominant), c(0.05, 0.95)))
        dl <- as.numeric(quantile(z$delta, c(0.05, 0.95)))
        yl <- c(floor(yl[1]/0.05) * 0.05, ceiling(yl[2]/0.05) * 0.05)
        dl <- c(floor(dl[1]/0.05) * 0.05, ceiling(dl[2]/0.05) * 0.05)
        maps[[as.character(y)]] <- list(rr = rr, dr = dr, yl = yl, dl = dl, dc = diffcols(dl), values = setNames(val$dominant, 
            val$mukey))
        audit[[as.character(y)]] <- data.frame(year = y, yield_p05 = yl[1], yield_p95 = yl[2], delta_p05 = dl[1], 
            delta_p95 = dl[2], native_cells = nrow(z), mean_delta = mean(z$delta), native_delta_min = min(z$delta), 
            native_delta_max = max(z$delta))
    }
    draw_polygons <- function(md, fill = FALSE) {
        for (p in pieces) {
            xy <- do.call(rbind, lapply(p$rings, function(r) rbind(r, c(NA, NA))))
            xy <- xy[-nrow(xy), , drop = FALSE]
            polypath(xy[, 1], xy[, 2], col = if (fill) 
                getcol(md$values[p$mukey], md$yl, pal)
            else NA, border = if (fill) 
                NA
            else "black", lwd = 0.8, rule = "evenodd")
        }
    }
    panel_map <- function(y, j) {
        md <- maps[[as.character(y)]]
        par(mar = c(2.3, 0.25, 2.1, 0.25), family = "Arial", xaxs = "i", yaxs = "i")
        plot.new()
        plot.window(xlim = c(bb["xmin"] - 8, bb["xmax"] + 35), ylim = c(bb["ymin"] - 36, bb["ymax"] + 
            5), asp = 1)
        if (j == 1) 
            draw_polygons(md, TRUE)
        if (j == 2) 
            draw_raster(md$rr, md$yl, pal)
        if (j == 3) 
            draw_raster(md$dr, md$dl, md$dc)
        draw_polygons(md)
        title(main = paste0(letters[(match(y, years) - 1) * 3 + j], ") ", y, " ", c("SSURGO", "DSM", 
            "DSM minus SSURGO")[j]), cex.main = 1.65)
        if (j == 1) 
            for (k in seq_along(pieces)) {
                p <- pieces[[k]]
                g <- p$candidates
                if (!nrow(g)) 
                  next
                label <- sprintf("%.2f", md$values[p$mukey])
                chosen <- NULL
                for (sz in c(1.5, 1.35, 1.2, 1.05)) {
                  hw <- strwidth(label, cex = sz, font = 2)/2 + 1
                  hh <- strheight(label, cex = sz, font = 2)/2 + 1
                  fits <- rep(TRUE, nrow(g))
                  for (dx in c(-hw, hw)) for (dy in c(-hh, hh)) fits <- fits & inside(g$x + dx, 
                    g$y + dy, p$rings)
                  if (any(fits)) {
                    chosen <- g[which(fits)[1], ]
                    break
                  }
                }
                if (is.null(chosen)) 
                  next
                for (dx in c(-1, 0, 1)) for (dy in c(-1, 0, 1)) if (dx || dy) 
                  text(chosen$x + dx * 1.15, chosen$y + dy * 1.15, label, col = "black", cex = sz, 
                    font = 2)
                text(chosen$x, chosen$y, label, col = "white", cex = sz, font = 2)
                label_audit[[length(label_audit) + 1]] <<- data.frame(year = y, piece = k, mukey = p$mukey, 
                  x = chosen$x, y = chosen$y, cex = sz)
            }
        sx <- bb["xmin"] + 20
        sy <- bb["ymin"] - 18
        if (j != 1) {
            segments(sx, sy, sx + 200, sy, lwd = 2)
            segments(sx + c(0, 100, 200), sy - 3, sx + c(0, 100, 200), sy + 3)
            text(sx + c(0, 100, 200), sy - 14, c("0", "100", "200 m"), cex = 1.2, xpd = NA)
        }
        nx <- bb["xmax"] + 18
        ny <- bb["ymax"] - 85
        arrows(nx, ny, nx, ny + 50, length = 0.08)
        text(nx, ny + 66, "N")
    }
    for (y in years) save_plot(paste0("Figure_2_maps_", y), 12, 4.2, function() render_year(y))
    save_plot("Figure_2_multiyear_maps", 12, 11.4, function() {
        lay <- do.call(rbind, lapply(0:2, function(i) rbind(c(1, 2, 3) + 5 * i, c(4, 4, 5) + 5 * 
            i)))
        layout(lay, heights = rep(c(1, 0.27), 3))
        for (y in years) {
            for (j in 1:3) panel_map(y, j)
            panel_legend(y)
            panel_legend(y, TRUE)
        }
    })
    write.csv(do.call(rbind, audit), file.path(out, "native_map_audit.csv"), row.names = FALSE)
    write.csv(unique(do.call(rbind, label_audit)), file.path(out, "interior_labels.csv"), row.names = FALSE)
    writej(list(status = "MAPS_RENDERED_VISUAL_QA_PENDING", renderer = "base polygon paths from original GeoJSON plus terra cubic display", 
        source_vertex_count = nrow(coords), polygon_md5 = md5(polygon_path), native_resolution_m = 12, 
        display_resolution_m = 3, external_callouts = FALSE), file.path(out, "status.json"))
    capture.output(sessionInfo(), file = file.path(out, "sessionInfo.txt"))
}

# Draw precipitation and temperature departures using the final blue/orange palette.
# options is a named list of input/output paths supplied by the numbered entry script.
plot_seasonal_climate <- function (options) 
{
    here <- .workflow_directory
    args_named <- function() options
    commandArgs <- function(trailingOnly = FALSE) {
        if (trailingOnly) 
            return(unlist(Map(function(n, v) c(paste0("--", n), v), names(options), options)))
        paste0("--file=", file.path(here, "figures.R"))
    }
    a <- args_named()
    src <- normal(need(a, "climate"))
    out <- normal(need(a, "out"), FALSE)
    stopifnot(!dir.exists(out))
    st <- fromJSON(file.path(src, "status.json"))
    stopifnot(st$status == "CLIMATE_CONTEXT_REPRODUCED")
    library(ggplot2)
    z <- read.csv(file.path(src, "prism_monthly_2017_2021.csv"))
    s <- read.csv(file.path(src, "prism_climate_context.csv"))
    z <- z[z$year %in% c(2017, 2019, 2021) & z$month %in% 5:9, ]
    s <- s[s$period == "May-September", ]
    stopifnot(nrow(z) == 15, nrow(s) == 3, !anyNA(z$precip_departure_percent))
    dir.create(out, recursive = TRUE)
    labs <- setNames(sprintf("%d\n%.0f mm (%+.1f%%)", s$year, s$precip_mm, s$precip_departure_percent), 
        s$year)
    z$panel <- factor(labs[as.character(z$year)], levels = unname(labs[as.character(c(2017, 2019, 
        2021))]))
    z$Month <- factor(month.abb[z$month], levels = month.abb[5:9])
    write.csv(z, file.path(out, "climate_figure_data.csv"), row.names = FALSE)
    p <- ggplot(z, aes(Month, precip_departure_percent)) + geom_hline(yintercept = 0, colour = "black", 
        size = 0.35) + geom_col(aes(fill = precip_departure_percent >= 0), width = 0.68, colour = "grey20", 
        size = 0.25) + scale_fill_manual(values = c(`FALSE` = "#D55E00", `TRUE` = "#0072B2"), guide = "none") + 
        facet_wrap(~panel, nrow = 1) + scale_y_continuous(breaks = scales::pretty_breaks(n = 6), 
        expand = expansion(mult = c(0.05, 0.08))) + labs(x = NULL, y = "Precipitation departure from normal (%)", 
        subtitle = "May-September totals and departures shown above each panel", caption = paste0("PRISM 4-km grid cell at the field; 1991-2020 normal.\nPRISM Group, Oregon State University; accessed ", 
            st$archived_access_date, ".")) + theme_bw(base_size = 11, base_family = "sans") + theme(panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), strip.background = element_rect(fill = "grey93", colour = "grey40"), 
        strip.text = element_text(size = 11, lineheight = 1.1), plot.subtitle = element_text(size = 10, 
            margin = margin(b = 8)), plot.caption = element_text(size = 8.5, hjust = 0), axis.title.y = element_text(size = 10), 
        axis.text = element_text(colour = "black"), plot.margin = margin(8, 10, 8, 8), panel.spacing = grid::unit(0.18, 
            "in"))
    temp_labels <- setNames(sprintf("%d\n%.2f \260C (%+.2f \260C)", s$year, s$tmean_C, s$tmean_departure_C), 
        s$year)
    z$temp_panel <- factor(temp_labels[as.character(z$year)], levels = unname(temp_labels[as.character(c(2017, 
        2019, 2021))]))
    pt <- ggplot(z, aes(Month, tmean_departure_C)) + geom_hline(yintercept = 0, colour = "black", 
        size = 0.35) + geom_col(aes(fill = tmean_departure_C >= 0), width = 0.68, colour = "grey20", 
        size = 0.25) + scale_fill_manual(values = c(`FALSE` = "#0072B2", `TRUE` = "#D55E00"), guide = "none") + 
        facet_wrap(~temp_panel, nrow = 1) + labs(x = NULL, y = "Temperature departure (\260C)", 
        subtitle = "b) Mean temperature; seasonal means and departures above each panel") + theme_bw(base_size = 13) + 
        theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), strip.text = element_text(size = 12), 
            plot.subtitle = element_text(size = 11), plot.margin = margin(10, 12, 10, 12))
    p <- p + labs(y = "Precipitation departure (%)", subtitle = "a) Precipitation; seasonal totals and departures above each panel", 
        caption = NULL) + theme(text = element_text(size = 13), axis.title.y = element_text(size = 12))
    draw <- function() {
        grid::grid.newpage()
        grid::pushViewport(grid::viewport(layout = grid::grid.layout(2, 1)))
        print(p, vp = grid::viewport(layout.pos.row = 1))
        print(pt, vp = grid::viewport(layout.pos.row = 2))
        grid::popViewport()
    }
    png(file.path(out, "Climate_departures.png"), width = 8, height = 7.5, units = "in", res = 600, 
        type = "cairo", bg = "white")
    draw()
    dev.off()
    cairo_pdf(file.path(out, "Climate_departures.pdf"), width = 8, height = 7.5, family = "Arial")
    draw()
    dev.off()
    writej(list(status = "CLIMATE_FIGURE_RENDERED_VISUAL_QA_PENDING", climate_status_md5 = md5(file.path(src, 
        "status.json")), positive = "#0072B2", negative = "#D55E00"), file.path(out, "status.json"))
    capture.output(sessionInfo(), file = file.path(out, "sessionInfo.txt"))
}
