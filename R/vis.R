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
#' bff_vis_metrics(face_flat)
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
    shade3d(x$mesh_orig, polygon_offset = 1,
            col = colourvalues::color_values(-measure),
            meshColor = mesh_col,
            ...)
  } else {
    shade3d(x$mesh_orig, polygon_offset = 1,
            col = "white",
            ...)
  }
  wire3d(face_flat$mesh_orig, ...)
  next3d()
  view3d(0, 0)
  par3d(mouseMode = c("none", "zAxis", "fov", "none", "pull"))
  if(metric != "mesh") {
    id <- shade3d(x$mesh_flat, polygon_offset = 1,
            col = colourvalues::color_values(-measure),
            specular = "black",
            meshColor = mesh_col,
            ...)
  } else {
    id <- wire3d(x$mesh_flat, specular = "black",
                 ...)
  }

}

bff_place_image <- function(x, expression, filename = NULL, wraparound = TRUE) {

  rotate_mesh <- function(button, dev = rgl::cur3d(), subscene = rgl::currentSubscene3d(), ...) {

    start <- list()

    begin <- function(x, y) {

      init <- list()
      activeSubscene <- par3d("activeSubscene", dev = dev)
      start$listeners <<- par3d("listeners", dev = dev, subscene = activeSubscene)
      for (sub in start$listeners) {
        init <- list()
        init$viewport <- par3d("viewport", dev = dev, subscene = sub)
        init$pos <- c(x/init$viewport[3], 1 - y/init$viewport[4])
        start[[as.character(sub)]] <<- init
      }

    }

    update <- function(x, y) {

      for (sub in start$listeners) {

        init <- start[[as.character(sub)]]

        xlat <- 2*(c(x/init$viewport[3], 1 - y/init$viewport[4]) - init$pos)

        mesh <<- rgl::translate3d(mesh, -mesh_centre[1], -mesh_centre[2], -mesh_centre[3]) %>%
          rgl::rotate3d(angle = xlat[1], x = 0, y = 0, z = 1) %>%
          rgl::translate3d(mesh_centre[1], mesh_centre[2], mesh_centre[3])

        par3d(skipRedraw = TRUE)
        useSubscene3d(par3d("activeSubscene", dev = dev))
        rgl.pop(id = id)
        id <<- wire3d(mesh, ...)
        par3d(skipRedraw = FALSE)
      }

    }


    rgl.setMouseCallbacks(button, begin, update, dev = dev, subscene = subscene)

  }

  move_mesh <- function(button, dev = rgl::cur3d(), subscene = rgl::currentSubscene3d(), ...) {

    #depth <- 0.763019
    active <- par3d("activeSubscene", dev = dev)

    begin <- function(x, y) {

      depth <- rgl::rgl.user2window(mesh_centre[1], mesh_centre[2], 0)[1, 3]

      active <<- par3d("activeSubscene", dev = dev)
      viewport <- par3d("viewport", dev = dev, subscene = active)
      win_x <- x/viewport[3]
      win_y <- 1 - y/viewport[4]

      new_centre <- as.vector(rgl::rgl.window2user(win_x, win_y, depth))
      move <- new_centre - mesh_centre
      mesh_centre <<- new_centre

      mesh <<- rgl::translate3d(mesh, move[1], move[2], 0)

      par3d(skipRedraw = TRUE)
      par3d(mouseMode = c("none", "none", "none", "none", "none"))
      useSubscene3d(active)
      rgl.pop(id = id)
      id <<- wire3d(mesh, ...)
      par3d(skipRedraw = FALSE)
      par3d(mouseMode = c("none", "user", "user", "none", "user2"))


    }

    update <- function(x, y) {

      depth <- rgl::rgl.user2window(mesh_centre[1], mesh_centre[2], 0)[1, 3]

      viewport <- par3d("viewport", dev = dev, subscene = subscene)
      win_x <- x/viewport[3]
      win_y <- 1 - y/viewport[4]

      new_centre <- as.vector(rgl::rgl.window2user(win_x, win_y, depth))
      move <- new_centre - mesh_centre
      mesh_centre <<- new_centre

      mesh <<- rgl::translate3d(mesh, move[1], move[2], 0)

      par3d(skipRedraw = TRUE)
      useSubscene3d(active)
      rgl.pop(id = id)
      id <<- wire3d(mesh, ...)
      par3d(skipRedraw = FALSE)

    }

    rgl.setMouseCallbacks(button, begin, update, dev = dev, subscene = subscene)

  }

  zoom_mesh <- function(dev = rgl::cur3d(), subscene = rgl::currentSubscene3d(), ...) {

    zoom <- function(wheel) {
      if(wheel == 1) {
        scale <- 0.9
      }
      if(wheel == 2) {
        scale <- 1.1
      }
      mesh <<- rgl::translate3d(mesh, -mesh_centre[1], -mesh_centre[2], -mesh_centre[3]) %>%
        rgl::scale3d(x = scale, y = scale, z = 1) %>%
        rgl::translate3d(mesh_centre[1], mesh_centre[2], mesh_centre[3])
      par3d(skipRedraw = TRUE)
      par3d(mouseMode = c("none", "none", "none", "none", "none"))
      useSubscene3d(subsceneList()[1])
      rgl.pop(id = id)
      id <<- wire3d(mesh, ...)
      par3d(skipRedraw = FALSE)
      par3d(mouseMode = c("none", "user", "user", "none", "user2"))

    }

    rgl.setWheelCallback(zoom, dev = dev, subscene = subscene)

  }

  rgl::open3d()

  mfrow3d(1, 2, mouseMode = "replace")
  next3d()

  rgl::view3d(0, 0, zoom = 1)
  par3d(mouseMode = c("none", "none", "none", "none", "none"))
  if(!is.null(filename)) {
    show2d(filename = filename, x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1), z = c(0, 0, 0, 0),
           ignoreExtent = FALSE)
  }

  mesh <- x$mesh_flat %>% rgl::translate3d(0, 0, 0.01)
  id <- wire3d(mesh, specular = "black")
  mesh_centre <- find_mesh_centre(mesh)

  move_mesh(1, specular = "black")
  zoom_mesh(specular = "black")
  rotate_mesh(2, specular = "black")

  mesh_orig <- x$mesh_orig

  plot_texmap <- function() {
    mesh_orig$texcoords <- t(asEuclidean(t(mesh$vb))[ , 1:2] + 1) / 2
    shade3d(mesh_orig, col = "white", texture = filename, textype = "rgb", texmipmap = TRUE,
            texminfilter = "linear.mipmap.linear", specular = "grey")
  }

  next3d()

  plot_texmap()


}

find_mesh_centre <- function(mesh) {

  apply(asEuclidean(t(mesh$vb)), 2, mean)

}
