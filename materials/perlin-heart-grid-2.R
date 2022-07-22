library(dplyr)
library(purrr)
library(tidyr)
library(tibble)
library(ggplot2)
library(ambient)
library(tictoc)
library(ggthemes)
library(gifski)

sample_canva <- function(seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  sample(ggthemes::canva_palettes, 1)[[1]]
}

show_polygon <- function(polygon, show_vertices = TRUE, ...) {
  
  pic <- ggplot(polygon, aes(x, y)) +
    geom_polygon(colour = "black", fill = NA, show.legend = FALSE, ...) + 
    coord_equal() + 
    theme_void()
  
  if(show_vertices == TRUE) {
    pic <- pic + geom_point(colour = "black", size = 2)
  }
  return(pic)
}

heart_x <- function(angle) {
  x <- (16 * sin(angle) ^ 3) / 17
  return(x - mean(x))
}

heart_y <- function(angle) {
  y <- (13 * cos(angle) - 5 * cos(2 * angle) - 2 * cos(3 * angle) -
          cos(4 * angle)) / 17
  return(y - mean(y))
}

normalise_radius <- function(x, min, max) {
  normalise(x, from = c(-0.5, 0.5), to = c(min, max))
}

perlin_heart2 <- function(n = 100, 
                          freq_init = 0.3,
                          octaves = 2, 
                          r_min = 0.5, 
                          r_max = 1,
                          w_min = 0,
                          w_max = 4,
                          rot = 0,
                          x_shift = 0,
                          y_shift = 0,
                          id = NA,
                          seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  tibble(
    angle = seq(0, 2*pi, length.out = n),
    
    radius = fracture(
      x = cos(angle), 
      y = sin(angle), 
      freq_init = freq_init,
      noise = gen_perlin, 
      fractal = fbm, 
      octaves = octaves
    ) |>
      normalise_radius(r_min, r_max),
    
    x = radius * heart_x(angle) + x_shift,
    y = radius * heart_y(angle) + y_shift,
    
    width = fracture(
      x = cos(angle + rot), 
      y = sin(angle + rot), 
      freq_init = freq_init,
      noise = gen_perlin, 
      fractal = fbm, 
      octaves = octaves
    ) |>
      normalise(to = c(w_min, w_max)),
    
    id = id
  )
}

perlin_heart_grid2 <- function(nx = 4, ny = 2, seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  
  heart_settings <- expand_grid(
    r_min = .3, 
    r_max = .4, 
    w_min = .01,
    w_max = 6,
    x_shift = 1:nx, 
    y_shift = 1:ny
  ) |>
    mutate(
      n = 200,
      x_shift = x_shift + runif(n(), -.1, .1),
      y_shift = y_shift + runif(n(), -.1, .1),
      rot = runif(n(), -.1, .1),
      id = row_number()
    ) 
  
  heart_data <-  pmap_dfr(heart_settings, perlin_heart2)
  
  heart_data |>
    ggplot(aes(x, y, group = id, colour = sample(id), size = width)) +
    geom_path(show.legend = FALSE) +
    theme_void() +
    scale_size_identity() +
    scale_colour_gradientn(colours = sample_canva(seed)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_fixed(xlim = c(0, nx + 1), ylim = c(0, ny + 1))
}

pic <- perlin_heart_grid2(seed = 666)
plot(pic)