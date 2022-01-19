#' Get the R logo png file path.
#'
#' @return A file path to the R logo png
#' @export
#'
#' @examples
#' plot(imager::load.image(r_logo_png()))
r_logo_png <- function() {
  system.file("extdata/Rlogo.png", package = "rbff")
}

convert_to_polygon <- function(x) {
  if(!inherits(x, "sf")) {
    x <- try(sf::st_as_sf(x, crs = NA))
  }

  if(inherits(x, "try-error")) {
    stop("x must be an sf object or coercable to one.")
  }

  if(nrow(x) > 1) {
    stop("sf object must consist of a single polygon (e.g. only have one row")
  } else {
    if(!inherits(st_geometry(oz)[[1]], "POLYGON")) {
      stop("sf object must consist of a POLYGON")
    }
  }

  poly <- sf::st_coordinates(x)[ , c(1, 2)]

  poly

}
