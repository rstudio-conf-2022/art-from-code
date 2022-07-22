library(rayshader)
library(tibble)
library(ambient)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tictoc)
library(dplyr)

sample_canva2 <- function(seed = NULL, n = 4) {
  if(!is.null(seed)) set.seed(seed)
  sample(ggthemes::canva_palettes, 1)[[1]] |>
    (\(x) colorRampPalette(x)(n))()  
}

ridge_art <- function(seed = NULL, pixels = 2000) {
  
  if(!is.null(seed)) set.seed(seed)
  long_grid(
    x = seq(from = 0, to = 1, length.out = pixels),
    y = seq(from = 0, to = 1, length.out = pixels)
  ) |> 
    mutate(
      paint = fracture(
        x = x, 
        y = y,
        noise = gen_simplex,
        fractal = ridged,
        octaves = 8,
        frequency = 10,
        seed = seed
      ),
      paint = normalise(paint)
    ) |>
    as.array(value = paint)
}

shaded_ridge_art <- function(seed = NULL) {
  
  art <- ridge_art(seed) 
  height_shade(
    heightmap = art,
    texture = sample_canva2(seed, 256)
  ) |>
    add_shadow(
      shadowmap = ray_shade(
        heightmap = art, 
        sunaltitude = 30, 
        sunangle = 90,
        multicore = TRUE, 
        zscale = .05
      ), 
      max_darken = .1
    ) |>
    plot_map()
}

tic()
shaded_ridge_art(100)
toc()
