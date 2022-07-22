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

normalise_radius <- function(x, min, max) {
  normalise(x, from = c(-0.5, 0.5), to = c(min, max))
}

perlin_blob <- function(n = 100, 
                        freq_init = 0.3,
                        octaves = 2, 
                        r_min = 0.5, 
                        r_max = 1) {
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
    x = radius * x_base,
    y = radius * y_base
  )
}

set.seed(3); 
pic <- perlin_blob(freq_init = .4) |> show_polygon(FALSE)
plot(pic)