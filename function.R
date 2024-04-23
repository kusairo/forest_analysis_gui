library(ggplot2)
library(lidR)
library(terra)
library(RCSF)

plot_crossection <- function(las,
                             p1 = c(min(las@data$X), mean(las@data$Y)),
                             p2 = c(max(las@data$X), mean(las@data$Y)),
                             width = 4, colour_by = NULL) {
  colour_by <- rlang::enquo(colour_by)
  data_clip <- clip_transect(las, p1, p2, width)
  p <- ggplot(data_clip@data, aes(X,Z)) + geom_point(size = 0.5) + coord_equal() + theme_minimal()
  
  if (!is.null(colour_by))
    p <- p + aes(color = !!colour_by) + labs(color = "")
  
  return(p)
}

fill.na <- function(x, i=5) { if (is.na(x)[i]) { return(mean(x, na.rm = TRUE)) } else { return(x[i]) }}

# ---- generate dtm from pcd ----
generate_dtm <- function(las,
                         res = 0.33,
                         sloop_smooth = FALSE,
                         class_threshold = 0.5,
                         cloth_resolution = 0.15,
                         time_step = 0.65,
                         crs = 6675) {
  las_last <- filter_last(las)
  
  # classify method setting
  mycsf <- csf(sloop_smooth = sloop_smooth, class_threshold = class_threshold, cloth_resolution = cloth_resolution, time_step = time_step)
  las_dtm <- classify_ground(las_last, algorithm = mycsf)
  las_dtm <- filter_ground(las_dtm)
  
  dtm <- rasterize_terrain(las = las_dtm, res = res, algorithm = tin(), pkg = "terra")
  w <- matrix(1, res * 10, res * 10)
  dtm <- terra::focal(dtm, w, fun = fill.na)
  crs(dtm) <- paste0('epsg:', crs)
  return(dtm)
}

# ---- generate dsm ----
generate_dsm <- function(las,
                         res = 0.33,
                         crs = 6675) {
  dsm <- rasterize_canopy(las, res = res, algorithm = dsmtin(), pkg = "terra")
  w <- matrix(1, res * 10, res * 10)
  dsm <- terra::focal(dsm, w, fun = fill.na)
  crs(dsm) <- paste0('epsg:', crs)
  return(dsm)
}

# ---- tree detection ----
detect_trees <- function(
    chm_dsm,
    ws = 3,
    crs = 6675) {
  
  f <- function(x) {x * 0.1 + 3}
  ttops <- locate_trees(chm_dsm, algorithm = lmf(ws))
  return (ttops)
}

# ---- tree segmentation ----
segment_crowns <- function(
    chm_dsm, 
    ttops,
    crs = 6675) {
  algo <- dalponte2016(chm_dsm, ttops)
  crowns <- algo()
  crs(crowns) <- paste0('epsg:', crs)
  return (crowns)
}

# ---- calculate difference ----
calc_differ <- function(raster1, raster2) {
  if (mean(res(raster1)) <= mean(res(raster2))) {
    tmp_raster2 <- resample(raster2, raster1)
    diff <- raster1 - raster2
  }
  else {
    tmp_raster1 <- resample(raster1, raster2)
    diff <- raster2 - raster1
  }
  return(diff)
}

