# Map drawing helpers; evaluated inside each map-rendering function.
save_plot <- function(name, w, h, fun) {
    png(file.path(out, paste0(name, ".png")), width = w, height = h, units = "in", res = 600, type = "cairo", 
        bg = "white")
    fun()
    dev.off()
    cairo_pdf(file.path(out, paste0(name, ".pdf")), width = w, height = h, family = "Arial", bg = "white")
    fun()
    dev.off()
}
diffcols <- function(lim) {
    v <- seq(lim[1], lim[2], length.out = 256)
    if (lim[1] >= 0) 
        return(colorRampPalette(c("#E5F1FF", "#075DA8"))(256))
    if (lim[2] <= 0) 
        return(colorRampPalette(c("#A51B25", "#FDE4E4"))(256))
    rgb(approx(c(lim[1], 0, lim[2]), c(165, 255, 7), xout = v)$y/255, approx(c(lim[1], 0, lim[2]), 
        c(27, 255, 93), xout = v)$y/255, approx(c(lim[1], 0, lim[2]), c(37, 255, 168), xout = v)$y/255)
}
getcol <- function(v, lim, cols) cols[1 + round(255 * pmax(0, pmin(1, (v - lim[1])/diff(lim))))]
draw_raster <- function(r, lim, cols) {
    v <- terra::values(r)[, 1]
    cc <- getcol(v, lim, cols)
    cc[is.na(v)] <- "transparent"
    rr <- as.raster(matrix(cc, nrow = terra::nrow(r), ncol = terra::ncol(r), byrow = TRUE))
    ee <- as.vector(terra::ext(r))
    rasterImage(rr, ee[1], ee[3], ee[2], ee[4], interpolate = FALSE)
}
panel_legend <- function(y, difference = FALSE) {
    md <- maps[[as.character(y)]]
    lim <- if (difference) 
        md$dl
    else md$yl
    cols <- if (difference) 
        md$dc
    else pal
    par(mar = c(0.2, 1, 0.2, 1), family = "Arial")
    plot.new()
    plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i")
    text(0.5, 0.86, if (difference) 
        expression(DSM - SSURGO ~ (Mg ~ ha^{
            -1
        }))
    else expression(Yield ~ (Mg ~ ha^{
        -1
    })), cex = 1.6)
    rasterImage(as.raster(matrix(cols, 1, 256)), 0.06, 0.34, 0.94, 0.63, interpolate = FALSE)
    tt <- seq(lim[1], lim[2], length.out = 3)
    xx <- 0.06 + 0.88 * (tt - lim[1])/diff(lim)
    segments(xx, 0.34, xx, 0.24, lwd = 1.3)
    for (k in 1:3) text(xx[k], 0.09, sprintf("%.2f", tt[k]), cex = 1.5, adj = c(0, 0.5, 1)[k])
    if (difference && lim[1] < 0 && lim[2] > 0) 
        segments(0.06 + 0.88 * (0 - lim[1])/diff(lim), 0.34, 0.06 + 0.88 * (0 - lim[1])/diff(lim), 
            0.63, lwd = 1)
}
render_year <- function(y) {
    layout(rbind(c(1, 2, 3), c(4, 4, 5)), heights = c(1, 0.27))
    for (j in 1:3) panel_map(y, j)
    panel_legend(y)
    panel_legend(y, TRUE)
}
