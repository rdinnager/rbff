% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vis.R
\name{bff_place_image}
\alias{bff_place_image}
\title{Place an image interactively on the flattened mesh and see it mapped to the
original 3D mesh.}
\usage{
bff_place_image(
  x,
  expression = NULL,
  filename = NULL,
  tile = TRUE,
  bg = "white",
  alpha_flatten = FALSE,
  ...
)
}
\arguments{
\item{x}{A \code{bff_flattened} object}

\item{expression}{An R expression that generates an image.}

\item{filename}{Alternative to expression: provide the file name of a png image directly}

\item{tile}{If the image does not cover the whole flattened mesh, should it be tiled
(e.g repeated) so that it fills the whole mesh? \emph{Also not implemented yet}}

\item{bg}{Background colour for if \code{tile = FALSE}.}

\item{alpha_flatten}{Should the alpha channel be flattened? By default, parts of the mesh with
completely transparent colours will be invisible. Flattening removes the alpha channel
by layering the image over the \code{bg} colour}

\item{...}{Further arguments passed to the graphic device is \code{expression} is not \code{NULL}}
}
\value{
A \code{bff_textured} object containing the original mesh with updated textcoords,
its flattened version, and the image for texture mapping
}
\description{
Place an image interactively on the flattened mesh and see it mapped to the
original 3D mesh.
}
