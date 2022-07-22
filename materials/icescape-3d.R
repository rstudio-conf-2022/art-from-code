
library(rayshader)
library(tibble)
library(ambient)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tictoc)
library(here)

sample_canva2 <- function(seed = NULL, n = 4) {
  
  if(!is.null(seed)) set.seed(seed)
  sample(ggthemes::canva_palettes, 1)[[1]] |>
    (\(x) colorRampPalette(x)(n))()  
}

transform_to_curl_space <- function(x, y, frequency = 1, octaves = 10) {
  curl_noise(
    generator = fracture,
    noise = gen_simplex,
    fractal = fbm,
    octaves = octaves,
    frequency = frequency,
    x = x,
    y = y
  )
}


define_worley_cells <- function(x, y, frequency = 3, octaves = 6) {
  fracture(
    noise = gen_worley,
    fractal = billow,
    octaves = octaves,
    frequency = frequency,
    value = "cell",
    x = x,
    y = y
  ) |>
    rank() |> 
    normalise()
}


simplex_noise <- function(x, y, frequency = .1, octaves = 10) {
  fracture(
    noise = gen_simplex,
    fractal = ridged,
    octaves = octaves,
    frequency = frequency,
    x = x,
    y = y
  ) |>
    normalise()
}


ice_floe <- function(seed) {
  
  set.seed(seed)
  
  grid <- long_grid(
    x = seq(0, 1, length.out = 2000),
    y = seq(0, 1, length.out = 2000)
  )
  
  coords <- transform_to_curl_space(grid$x, grid$y)
  
  grid |>
    mutate(
      cells = define_worley_cells(coords$x, coords$y),
      paint = simplex_noise(x + cells, y + cells),
      paint = normalise(paint)
    ) |>
    as.array(value = paint)
}

icescape_3d <- function(seed) {
  
  ice_height <- matrix(0, 2500, 2500)
  ice_height[251:2250, 251:2250] <- ice_floe(seed)
  
  ice_scape <- height_shade(
    heightmap = ice_height,
    texture = sample_canva2(seed, 256)
  ) |>
    add_shadow(
      shadowmap = ray_shade(
        heightmap = ice_height, 
        sunaltitude = 30, 
        sunangle = 90,
        multicore = TRUE, 
        zscale = .005
      ), 
      max_darken = .05
    )
  
  plot_3d(
    hillshade = ice_scape,
    heightmap = ice_height,
    theta = 45,
    phi = 30,
    zoom = .75,
    zscale = .001,
    background = "#222222",
    shadow = FALSE,
    soliddepth = .5,
    solidcolor = "#222222",
    windowsize = c(2500, 1500)
  )
  
  render_snapshot(
    filename = here("output", paste0("icescape_3d_", seed, ".png")), 
    clear = TRUE
  )
}

tic()
icescape_3d(123)
toc()

