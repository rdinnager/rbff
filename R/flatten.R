#' Automatically flatten a 3d mesh using boundary first flattening
#' 
#' Flattening will only work with closed objects if cones are inserted (cuts or seams).
#' Closed objects can instead be mapped to a sphere however. 
#' If both `to_disk` and `to_sphere` are `FALSE`, a target boundary shape will be determined
#' automatically that minimized area and angle distortion. If you want to
#' flatten to a particular target boundary shape, use [bff_flatten_to_shape()]
#'
#' @param mesh `mesh3d` object to flatten
#' @param n_cones Number of cone singularities to insert into mesh to reduce distortion. Ignored
#' if `to_disk = TRUE`
#' @param to_disk Should the mesh be flattened to a disk?
#' @param to_sphere Should the mesh be mapped to a sphere instead (only works with closed objects).
#' @param normalise Should the 2d mapping be normalised between 0 and 1 on the x and y axes?
#'
#' @return A `bff_flattened` object containing the original mesh and its flattened version with
#' corresponding vertices unless `to_sphere = TRUE`, in which case a `bff_sphered` object is returned.
#' @export
#'
#' @examples
#' data(face)
#' flat_face <- bff_flatten(face)
bff_flatten <- function(mesh, n_cones = 0, to_disk = FALSE, to_sphere = FALSE, normalise = TRUE) {
  
  temp_obj1 <- tempfile(fileext = ".obj")
  temp_obj2 <- tempfile(fileext = ".obj")
  
  if(!inherits(mesh, "mesh3d")) {
    stop("mesh is not a mesh3d object. bff_flatten currently only supports mesh3d objects.")
  } 
  
  Rvcg::vcgObjWrite(mesh, temp_obj1, writeNormals = FALSE)
  
  auto_flatten(temp_obj1,
               nCones = n_cones,
               flattenToDisk = to_disk,
               mapToSphere = to_sphere,
               normalizeUVs = normalise,
               temp_obj2)
  
  new_mesh <- readobj::read.obj(temp_obj2, convert.rgl = TRUE)[[1]]
  if(!to_sphere) {
    flat_mesh <- rgl::mesh3d(cbind(t(new_mesh$texcoords), 0),
                             triangles = new_mesh$it)
    res <- list(mesh_orig = new_mesh,
                mesh_flat = flat_mesh)
    
    class(res) <- "bff_flattened"
  } else {
    lon <- new_mesh$texcoords[1, ] 
    lat <- new_mesh$texcoords[2, ] 
    lon <- (lon - 0.5) * 2 * pi
    lat <- (lat - 0.5) * pi
    
    x <- cos(lat) * cos(lon)
    y <- cos(lat) * sin(lon)
    z <- sin(lat)
    
    sphere_mesh <- rgl::mesh3d(x, y, z,
                               triangles = new_mesh$it)
    
    res <- list(mesh_orig = new_mesh,
                mesh_sphere = sphere_mesh)
    
    class(res) <- "bff_sphered"
  }
  
  res
  
}