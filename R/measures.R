bff_area_distortion <- function(x) {
  
  areas_orig <- Rvcg::vcgArea(x$mesh_orig, perface = TRUE)
  areas_flat <- Rvcg::vcgArea(x$mesh_flat, perface = TRUE)
  
  areas_orig <- areas_orig$pertriangle / (areas_orig$area / 2)
  areas_flat <- areas_flat$pertriangle / (areas_flat$area / 2)
  
  area_distort <- areas_flat - areas_orig
  
  area_distort
  
}

bff_vert_dens <- function(x) {
  areas <- Rvcg::vcgArea(x, perface = TRUE)
  vert_dens <- 1 / areas$pertriangle
  vert_dens
}

bff_curvature <- function(x) {
  curvature <- Rvcg::vcgCurve(x$mesh_orig)$meanvb
  curvature
}