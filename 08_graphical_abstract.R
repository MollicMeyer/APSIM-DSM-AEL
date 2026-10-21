# Graphical abstract from the final paired results. Run after steps 05 and 06.
# No new simulation, aggregation or inferential analysis is performed here.
library(ggplot2)
library(sf)
library(terra)
library(grid)
library(gridExtra)
library(scales)
source('R/map_relief.R')

out <- 'results/analysis/graphical_abstract'
dir.create(out, recursive = TRUE, showWarnings = FALSE)
cells <- read.csv('results/analysis/paired_cells.csv')
field <- read.csv('results/analysis/field_summary.csv')
hillslope <- read.csv('results/analysis/hillslope_summary.csv')
polygons <- st_read('data/SSURGO/delineations.geojson', quiet = TRUE)
box <- st_bbox(polygons)
relief <- map_relief(polygons,out)
years <- c(2017, 2019, 2021)
panels <- list()

for (year in years) {
  z <- cells[cells$year == year, ]
  summary <- field[field$year == year, ]
  stopifnot(nrow(z) == 1943, nrow(summary) == 1,
            abs(mean(z$delta) - summary$mean_difference) < 1e-10)
  # Statistics remain on the native 12 m grid. Cubic interpolation is display only.
  native <- rast(z[c('x', 'y', 'delta')], type = 'xyz', crs = 'EPSG:32615')
  display <- rast(ext(native), resolution = 3, crs = crs(native))
  display <- mask(resample(native, display, method = 'cubic'), vect(polygons))
  names(display) <- 'difference'
  values <- as.data.frame(display, xy = TRUE, na.rm = TRUE)
  # Retain the approved Figure 2 palette and bulk-distribution display limits.
  limits <- quantile(z$delta, c(.05, .95))
  limits <- c(floor(limits[1] / .05), ceiling(limits[2] / .05)) * .05
  scale <- signed_difference_fill(limits)
  panels[[as.character(year)]] <- ggplot(values, aes(x, y, fill = difference)) +
    relief_layer(relief) + geom_raster(alpha=map_color_opacity) +
    geom_sf(data = polygons, inherit.aes = FALSE, fill = NA, colour = '#303030', size = .25) +
    coord_sf(xlim = c(box['xmin'] - 8, box['xmax'] + 8),
             ylim = c(box['ymin'] - 8, box['ymax'] + 8), expand = FALSE) + scale +
    labs(fill = NULL) + theme_void(base_family = 'Segoe UI', base_size = 12) +
    theme(legend.position = 'bottom', legend.text = element_text(size = 11),
          legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 7, 0, 7)) +
    guides(fill = guide_colourbar(barwidth = unit(1.75, 'in'), barheight = unit(.15, 'in')))
}

# Check the second, landscape-level finding before printing it in the graphic.
contrast <- vapply(years, function(year) {
  z <- hillslope[hillslope$year == year, ]
  z$mean_DSM[z$DHP == 'TS'] - z$mean_DSM[z$DHP == 'SU']
}, numeric(1))
stopifnot(contrast[1] > 0, contrast[2] > 0, contrast[3] < 0)

draw_abstract <- function() {
  grid.newpage()
  ink <- '#193c48'
  text <- function(label,x,y,size=12,bold=FALSE,col=ink,just='centre')
    grid.text(label,x,y,just=just,gp=gpar(fontfamily='Segoe UI',fontsize=size,
      fontface=if(bold)'bold' else 'plain',col=col,lineheight=1.15))
  connect <- function(x0,y0,x1,y1) grid.lines(c(x0,x1),c(y0,y1),
    arrow=grid::arrow(length=unit(.07,'in'),type='closed'),gp=gpar(col='#84949b',lwd=1.6))
  # Editable process diagram; the maps below depict simulation results.
  text('Local soil information changes seasonal yield patterns',.5,.958,21,TRUE)
  text('SOIL INFORMATION',.19,.887,10,TRUE,col='#667c85')
  text('SOIL PROFILES',.53,.887,10,TRUE,col='#667c85')
  text('CROP SIMULATION',.82,.887,10,TRUE,col='#667c85')
  grid.roundrect(.19,.79,width=.31,height=.13,r=unit(.06,'in'),
    gp=gpar(fill='#edf5f4',col=NA))
  text('Locally enhanced DSM',.19,.814,15,TRUE)
  text('2019 samples + terrain + imagery',.19,.774,11)
  grid.roundrect(.19,.675,width=.31,height=.08,r=unit(.06,'in'),
    gp=gpar(fill='#f3f1ed',col=NA))
  text('SSURGO',.19,.69,14,TRUE)
  text('Dominant soil components',.19,.659,11)
  connect(.355,.79,.425,.752)
  connect(.355,.675,.425,.728)
  # Simple depth-layer icon, not a fabricated soil measurement.
  for(j in 1:4) grid.rect(.468,.81-(j-1)*.028,width=.035,height=.028,
    gp=gpar(fill=c('#a88c68','#c2a884','#d7c1a1','#e8daca')[j],col='white',lwd=.6))
  text('Depth-specific\nsoil properties',.565,.778,12)
  text('Common initialization',.53,.676,10,col='#536a75')
  connect(.635,.75,.695,.75)
  text('APSIM',.82,.807,21,TRUE)
  text('Common weather and management',.82,.76,10.5)
  text('Continuous 2013\u20132021 rotation',.82,.72,11)
  grid.lines(c(.035,.965),c(.606,.606),gp=gpar(col='#cfd9dc',lwd=1))
  text('SIMULATED YIELD DIFFERENCE',.04,.572,11,TRUE,just='left')
  text(expression('Locally enhanced DSM minus SSURGO (Mg ha'^{-1}*')'),
       .96,.572,11,just='right')
  for (i in seq_along(years)) {
    x <- .18+(i-1)*.32
    delta <- field$mean_difference[field$year == years[i]]
    text(as.character(years[i]),x,.523,16,TRUE)
    label <- sprintf('%+.2f', delta)
    label <- sub('-', '\u2212', label, fixed = TRUE)
    pushViewport(viewport(x = x, y = .353, width = .32, height = .30))
    grid.draw(ggplotGrob(panels[[as.character(years[i])]]))
    popViewport()
    text(paste('Field mean',label),x,.185,13,TRUE,
         col=if(delta>0)'#176b9a' else '#aa5438')
  }
  text('Orange: SSURGO higher   |   Blue: locally enhanced DSM higher',.5,.13,11)
  grid.rect(.5,.055,width=1,height=.08,gp=gpar(fill='#eef3f4',col=NA))
  text('Soil representation changes yield contrasts across seasons and hillslope positions.',
       .5,.055,14,TRUE)
}

png(file.path(out, 'graphical_abstract.png'), width = 10, height = 7,
    units = 'in', res = 450, type = 'cairo', bg = 'white')
draw_abstract(); dev.off()
tiff(file.path(out, 'graphical_abstract.tif'), width = 10, height = 7,
     units = 'in', res = 450, compression = 'lzw', type = 'cairo', bg = 'white')
draw_abstract(); dev.off()
cairo_pdf(file.path(out, 'graphical_abstract.pdf'), width = 10, height = 7, family = 'Segoe UI')
draw_abstract(); dev.off()
writeLines(c('Graphical abstract caption',
  'Local 2019 samples, terrain and imagery informed locally enhanced digital soil mapping (DSM). DSM and SSURGO profiles supplied APSIM under common settings. Maps show simulated DSM minus SSURGO maize yield, with paired field means below. Orange indicates higher SSURGO yield and blue higher DSM yield. Hillshade shows terrain; legends vary by year. Prepared in R with OpenAI Codex assistance for layout and code.'),
  file.path(out, 'graphical_abstract_caption.txt'))
