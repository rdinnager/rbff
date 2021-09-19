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
