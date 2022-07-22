library(dplyr)
library(purrr)
library(tidyr)
library(tibble)
library(ggplot2)
library(ambient)
library(tictoc)
library(ggthemes)
library(gifski)

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

perlin_heart <- function(n = 100, 
                         freq_init = 0.3,
                         octaves = 2, 
                         r_min = 0.5, 
                         r_max = 1,
                         x_shift = 0,
                         y_shift = 0,
                         id = NA,
                         seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  tibble(
    angle = seq(0, 2*pi, length.out = n),
    x_base = cos(angle),
    y_base = sin(angle),
    radius = fracture(
      x = x_base, 
      y = y_base, 
      freq_init = freq_init,
      noise = gen_perlin, 
      fractal = fbm, 
      octaves = octaves
    ) |>
      normalise_radius(r_min, r_max),
    x = radius * heart_x(angle) + x_shift,
    y = radius * heart_y(angle) + y_shift,
    id = id
  )
}

set.seed(3); 
pic <- perlin_heart(freq_init = .4) |> show_polygon(FALSE)
plot(pic)