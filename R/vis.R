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

  rgl::open3d()
  rgl::mfrow3d(1, 2, mouseMode = "replace")
  rgl::next3d()
  if(metric != "mesh") {
    rgl::shade3d(x$mesh_orig, polygon_offset = 1,
            col = colourvalues::color_values(-measure),
            meshColor = mesh_col,
            ...)
  } else {
    rgl::shade3d(x$mesh_orig, polygon_offset = 1,
            col = "white",
            ...)
  }
  rgl::wire3d(face_flat$mesh_orig, ...)
  rgl::next3d()
  rgl::view3d(0, 0)
  rgl::par3d(mouseMode = c("none", "zAxis", "fov", "none", "pull"))
  if(metric != "mesh") {
    id <- rgl::shade3d(x$mesh_flat, polygon_offset = 1,
            col = colourvalues::color_values(-measure),
            specular = "black",
            meshColor = mesh_col,
            ...)
  } else {
    id <- rgl::wire3d(x$mesh_flat, specular = "black",
                 ...)
  }

}

#' Title
#'
#' @param x A `bff_flattened` object
#' @param expression An R expression that generates an image
#' @param filename Alternative to expression: provide the file name of a png image directly
#' @param tile If the image does not cover the whole flattened mesh, should it be tiled
#' (e.g repeated) so that it fills the whole mesh?
#'
#' @return A `bff_textured` object containing the original mesh with updated textcoords,
#' its flattened version, and the image for texture mapping
#'
#' @export
bff_place_image <- function(x, expression, filename = NULL, wraparound = TRUE) {

  rotate_mesh <- function(button, dev = rgl::cur3d(), subscene = rgl::currentSubscene3d(), ...) {

    start <- list()

    begin <- function(x, y) {

      init <- list()
      activeSubscene <- rgl::par3d("activeSubscene", dev = dev)
      start$listeners <<- rgl::par3d("listeners", dev = dev, subscene = activeSubscene)
      for (sub in start$listeners) {
        init <- list()
        init$viewport <- rgl::par3d("viewport", dev = dev, subscene = sub)
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

        rgl::par3d(skipRedraw = TRUE)
        rgl::useSubscene3d(subs[1])
        rgl::rgl.pop(id = id)
        id <<- rgl::wire3d(mesh, ...)
        rgl::par3d(skipRedraw = FALSE)
      }

    }

    end <- function() {
      last_manip <<- Sys.time()
      manip <<- TRUE
    }


    rgl::rgl.setMouseCallbacks(button, begin, update, end = end, dev = dev, subscene = subscene)

  }

  move_mesh <- function(button, dev = rgl::cur3d(), subscene = rgl::currentSubscene3d(), ...) {

    #depth <- 0.763019
    #active <- rgl::par3d("activeSubscene", dev = dev)

    begin <- function(x, y) {

      depth <- rgl::rgl.user2window(mesh_centre[1], mesh_centre[2], 0)[1, 3]

      active <- subs[1]
      viewport <- rgl::par3d("viewport", dev = dev, subscene = subscene)
      win_x <- x/viewport[3]
      win_y <- 1 - y/viewport[4]

      new_centre <- as.vector(rgl::rgl.window2user(win_x, win_y, depth))
      move <- new_centre - mesh_centre
      mesh_centre <<- new_centre

      mesh <<- rgl::translate3d(mesh, move[1], move[2], 0)

      rgl::par3d(skipRedraw = TRUE)
      rgl::par3d(mouseMode = c("none", "none", "none", "none", "none"),
                 subscene = subs[1])
      rgl::useSubscene3d(subs[1])
      rgl::rgl.pop(id = id)
      id <<- rgl::wire3d(mesh, ...)
      rgl::par3d(skipRedraw = FALSE)
      rgl::par3d(mouseMode = c("none", "user", "user", "none", "user2"),
                 subscene = subs[1])


    }

    update <- function(x, y) {

      depth <- rgl::rgl.user2window(mesh_centre[1], mesh_centre[2], 0)[1, 3]

      viewport <- rgl::par3d("viewport", dev = dev, subscene = subscene)
      win_x <- x/viewport[3]
      win_y <- 1 - y/viewport[4]

      new_centre <- as.vector(rgl::rgl.window2user(win_x, win_y, depth))
      move <- new_centre - mesh_centre
      mesh_centre <<- new_centre

      mesh <<- rgl::translate3d(mesh, move[1], move[2], 0)

      rgl::par3d(skipRedraw = TRUE)
      rgl::useSubscene3d(subs[1])
      rgl::rgl.pop(id = id)
      id <<- rgl::wire3d(mesh, ...)
      rgl::par3d(skipRedraw = FALSE)

    }

    end <- function() {
      last_manip <<- Sys.time()
      manip <<- TRUE
    }

    rgl::rgl.setMouseCallbacks(button, begin, update, end = end, dev = dev, subscene = subscene)

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
      rgl::par3d(skipRedraw = TRUE)
      rgl::par3d(mouseMode = c("none", "none", "none", "none", "none"),
                 subscene = subs[1])
      rgl::useSubscene3d(subs[1])
      rgl::rgl.pop(id = id)
      id <<- rgl::wire3d(mesh, ...)
      rgl::par3d(skipRedraw = FALSE)
      rgl::par3d(mouseMode = c("none", "user", "user", "none", "user2"),
                 subscene = subs[1])

      last_manip <<- Sys.time()
      manip <<- TRUE

    }

    rgl::rgl.setWheelCallback(zoom, dev = dev, subscene = subscene)

  }

  rgl::open3d()

  rgl::mfrow3d(1, 2, mouseMode = "replace")
  rgl::next3d()

  rgl::view3d(0, 0, zoom = 1)
  rgl::par3d(mouseMode = c("none", "none", "none", "none", "none"))
  if(!is.null(filename)) {
    rgl::show2d(filename = filename, x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1), z = c(0, 0, 0, 0),
           ignoreExtent = FALSE)
  }

  mesh <- x$mesh_flat
  mesh$vb[1:2, ] <- rgl::asHomogeneous2((rgl::asEuclidean2(mesh$vb) - 0.5) * 2)[1:2, ]
  mesh <- mesh %>% rgl::translate3d(0, 0, 0.01)
  id <- rgl::wire3d(mesh, specular = "black")
  mesh_centre <- find_mesh_centre(mesh)

  move_mesh(1, specular = "black")
  zoom_mesh(specular = "black")
  rotate_mesh(2, specular = "black")

  mesh_orig <- x$mesh_orig

  rgl::next3d()

  mesh_orig$texcoords <- t(rgl::asEuclidean(t(mesh$vb))[ , 1:2] + 1) / 2
  mapped <- rgl::shade3d(mesh_orig, col = "white", texture = filename, textype = "rgb", texmipmap = TRUE,
                         texminfilter = "linear.mipmap.linear", specular = "grey")

  title3d("close this window when you are satisfied to save results")

  subs <- rgl::subsceneList()
  device <- rgl::cur3d()

  wait_time <- 1
  last_manip <- Sys.time()
  device_open <- TRUE
  manip <- TRUE
  while(device_open) {

    device_open <- any(rgl::rgl.dev.list() == device)

    if(device_open) {
      since_manip <- Sys.time() - last_manip

      if((since_manip > wait_time) & manip) {
        rgl::par3d(skipRedraw = TRUE)
        rgl::useSubscene3d(subs[2])
        rgl::rgl.pop(id = mapped)
        mesh_orig$texcoords <- t(rgl::asEuclidean(t(mesh$vb))[ , 1:2] + 1) / 2
        mapped <- rgl::shade3d(mesh_orig, col = "white", texture = filename, textype = "rgb", texmipmap = TRUE,
                               texminfilter = "linear.mipmap.linear", specular = "grey")
        rgl::par3d(skipRedraw = FALSE)
        manip <- FALSE

      }
    }

  }

  message("Texture map complete!")

  mesh_orig$texcoords <- (rgl::asEuclidean2(mesh$vb)[ , 1:2] + 1) / 2
  mesh_flat <- mesh
  mesh$vb <- rgl::asHomogeneous2((rgl::asEuclidean2(mesh$vb) + 1) / 2)

  res <- list(mesh_flat = mesh_flat, mesh_orig = mesh_orig, texture = filename)
  class(res) <- "bff_textured"
  res

}

find_mesh_centre <- function(mesh) {

  apply(rgl::asEuclidean(t(mesh$vb)), 2, mean)

}
