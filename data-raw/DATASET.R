## code to prepare `DATASET` dataset goes here
beetle <- readobj::read.obj(system.file("extdata", "beetle.obj", package = "rbff"), convert.rgl = TRUE)[[1]]
box <- readobj::read.obj(system.file("extdata", "box.obj", package = "rbff"), convert.rgl = TRUE)[[1]]
bunny <- readobj::read.obj(system.file("extdata", "bunny.obj", package = "rbff"), convert.rgl = TRUE)[[1]]
cowhead <- readobj::read.obj(system.file("extdata", "cowhead.obj", package = "rbff"), convert.rgl = TRUE)[[1]]
face <- readobj::read.obj(system.file("extdata", "face.obj", package = "rbff"), convert.rgl = TRUE)[[1]]
fish <- readobj::read.obj(system.file("extdata", "fish.obj", package = "rbff"), convert.rgl = TRUE)[[1]]
hemisphere <- readobj::read.obj(system.file("extdata", "hemisphere.obj", package = "rbff"), convert.rgl = TRUE)[[1]]
kitten <- readobj::read.obj(system.file("extdata", "kitten.obj", package = "rbff"), convert.rgl = TRUE)[[1]]
maze <- readobj::read.obj(system.file("extdata", "maze.obj", package = "rbff"), convert.rgl = TRUE)[[1]]
punctured_torus <- readobj::read.obj(system.file("extdata", "punctured-torus.obj", package = "rbff"), convert.rgl = TRUE)[[1]]
spothead <- readobj::read.obj(system.file("extdata", "spothead.obj", package = "rbff"), convert.rgl = TRUE)[[1]]
teapot <- readobj::read.obj(system.file("extdata", "teapot.obj", package = "rbff"), convert.rgl = TRUE)[[1]]

usethis::use_data(beetle, 
                  box,
                  bunny,
                  cowhead,
                  face,
                  fish,
                  hemisphere,
                  kitten,
                  maze,
                  punctured_torus,
                  spothead,
                  teapot,
                  overwrite = TRUE)
