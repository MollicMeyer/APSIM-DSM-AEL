# Shared cartography for the manuscript maps and graphical abstract.
# Elevation affects appearance only, never the simulated yields or summaries.
map_relief <- function(polygons, output_directory) {
  dem_file <- 'data/landscape/DEM_3m.tif'
  stopifnot(file.exists(dem_file))
  shade_file <- tempfile('hillshade_', tmpdir=output_directory, fileext='.tif')
  sf::gdal_utils('demprocessing', source=dem_file, destination=shade_file,
    processing='hillshade', options=c('-multidirectional','-alt','35','-z','10','-compute_edges'), quiet=TRUE)
  shade <- terra::project(terra::rast(shade_file), sf::st_crs(polygons)$wkt, method='bilinear')
  shade <- terra::mask(shade, terra::vect(polygons))
  v <- terra::values(shade)
  bounds <- stats::quantile(v,c(.02,.98),na.rm=TRUE)
  v <- pmin(1,pmax(0,(v-bounds[1])/diff(bounds)))
  v[is.na(v)] <- 1
  pixels <- matrix(grDevices::gray(v),nrow=nrow(shade),ncol=ncol(shade),byrow=TRUE)
  pixels[is.na(as.matrix(shade,wide=TRUE))] <- NA
  list(pixels=pixels,extent=as.vector(terra::ext(shade)))
}
relief_layer <- function(relief) {
  ggplot2::annotation_raster(relief$pixels,xmin=relief$extent[1],xmax=relief$extent[2],
    ymin=relief$extent[3],ymax=relief$extent[4])
}
# Nominal HCL lightness is held constant so color does not mimic illuminated relief.
yield_map_colors <- grDevices::hcl(h=c(20,75,140,200,260),c=60,l=65)
difference_map_colors <- grDevices::hcl(h=c(20,20,240),c=c(75,0,75),l=65)
map_color_opacity <- .77
signed_difference_fill <- function(limits, breaks=limits) {
  colors <- if(limits[1]>=0) grDevices::hcl(h=240,c=c(20,75),l=65) else
    if(limits[2]<=0) grDevices::hcl(h=20,c=c(75,20),l=65) else difference_map_colors
  stops <- if(limits[1]<0 && limits[2]>0) scales::rescale(c(limits[1],0,limits[2]),from=limits) else c(0,1)
  ggplot2::scale_fill_gradientn(colours=colors,values=stops,limits=limits,
    oob=scales::squish,breaks=breaks,
    labels=function(x)sprintf('%.2f',x))
}
