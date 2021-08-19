#' Visualise the result of flattening a 3d mesh.
#'
#' @param x A `bff_flattened` or `bff_sphered` object
#' @param metric Which metric to display. One of "area distortion", 
#' "vertice density", "curvature", or "mesh"
#'
#' @return None.
#' @export
#'
#' @examples
#' data(face)
#' face_flat <- bff_flatten(face)
#' bff_vis_metric(face_flat)
bff_vis_metrics <- function(x, 
                            metric = c("area distortion", 
                                       "vertice density",
                                       "curvature",
                                       "mesh"),
                            ...) {
  
  metric <- match.arg(metric)
  
  measure <- switch(metric,
    `area distortion` = bff_area_distortion(x),
    `vertice density` = bff_vert_dens(x),
    curvature = bff_curvature(x),
    mesh = NA
  )
  
  if(metric == "curvature") {
    mesh_col <- "vertices"
  } else {
    mesh_col <- "faces"
  }
  
  open3d()
  mfrow3d(1, 2, mouseMode = "replace")
  next3d()
  if(metric != "mesh") {
    shade3d(face_flat$mesh_orig, polygon_offset = 1,
            col = colourvalues::color_values(-measure),
            meshColor = mesh_col,
            ...)
  } else {
    shade3d(face_flat$mesh_orig, polygon_offset = 1,
            col = "white",
            ...)
  }
  wire3d(face_flat$mesh_orig, ...)
  next3d()
  view3d(0, 0)
  par3d(mouseMode = c("none", "zAxis", "fov", "none", "pull"))
  if(metric != "mesh") {
    id <- shade3d(face_flat$mesh_flat, polygon_offset = 1,
            col = colourvalues::color_values(-measure),
            specular = "black",
            meshColor = mesh_col,
            ...)
  } else {
    id <- wire3d(face_flat$mesh_flat, specular = "black",
                 ...)
  }
  
}