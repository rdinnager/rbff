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
#' options(rgl.useNULL = TRUE)
#' flat_face <- bff_flatten(face)
bff_flatten <- function(mesh, n_cones = 0, to_disk = FALSE, to_sphere = FALSE, normalise = TRUE) {

  if(!inherits(mesh, "mesh3d")) {
    stop("mesh is not a mesh3d object. bff_flatten currently only supports mesh3d objects.")
  }

  temp_obj1 <- tempfile(fileext = ".obj")
  temp_obj2 <- tempfile(fileext = ".obj")
  on.exit(unlink(c(temp_obj1, temp_obj2)))

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

#' Flatten a mesh to a particular boundary shape
#'
#' @param mesh `mesh3d` object to flatten. Can also be a two-column `data.frame`
#' or `matrix` with 2d coordinates, or an `sf` object representing a polygon,
#' in which case it will be converted to a mesh, and then flattened (but since
#' it is already flat, this will be a mapping of one shape to another instead).
#' @param boundary_shape A two-column `data.frame` or `matrix` with 2d coordinates,
#' or an `sf` object representing a polygon. The shape to be flattened to.
#' @param normalise Should the 2d mapping be normalised between 0 and 1 on the x and y axes?
#'
#' @return A `bff_flattened` object containing the original mesh and its flattened version with
#' corresponding vertices.
#' @export
#'
#' @examples
#' data(face)
#' options(rgl.useNULL = TRUE)
#' flat_face <- bff_flatten_to_shape(face, matrix(c(0, 1, 1, 0, 0,
#'                                                  0, 0, 1, 1, 0),
#'                                                ncol = 2))
bff_flatten_to_shape <- function(mesh,
                                 boundary_shape,
                                 normalise = TRUE) {

  if(!inherits(mesh, "mesh3d")) {
    if(inherits("sf")) {
      if(!requireNamespace("sfdct", quietly = TRUE)) {
        stop('Using a 2d polygon as an argument to mesh requires the sfdct package.
You can install it using install.packages("sfdct")')
      }
    }
    stop("mesh is not a mesh3d object. bff_flatten currently only supports mesh3d objects.")
  }
  if(inherits(boundary_shape, "sf")) {
    boundary_shape <- convert_to_polygon(boundary_shape)
  } else {
    if(inherits(boundary_shape, "matrix") || inherits(boundary_shape, "data.frame")) {
      if(ncol(boundary_shape) > 2) {
        warning("boundary_shape had more than 2 columns, only the first two are being used as x and y coordinates")
        boundary_shape <- boundary_shape[ , 1:2]
      }
      if(ncol(boundary_shape) < 2) {
        stop("boundary shape does not have at least two columns and so is not a valid set of x and y coordinates")
      }
    } else {
      stop("boundary_shape must be a two-column matrix or data.frame with x and y polygon coordinates, or an sf object")
    }
  }


  temp_obj1 <- tempfile(fileext = ".obj")
  temp_obj2 <- tempfile(fileext = ".obj")
  on.exit(unlink(c(temp_obj1, temp_obj2)))

  Rvcg::vcgObjWrite(mesh, temp_obj1, writeNormals = FALSE)
  flatten_to_shape(temp_obj1,
                   boundary_shape,
                   normalise,
                   temp_obj2)

  new_mesh <- readobj::read.obj(temp_obj2, convert.rgl = TRUE)[[1]]

  flat_mesh <- rgl::mesh3d(cbind(t(new_mesh$texcoords), 0),
                           triangles = new_mesh$it)
  res <- list(mesh_orig = new_mesh,
              mesh_flat = flat_mesh)

  class(res) <- "bff_flattened"

  res

}
