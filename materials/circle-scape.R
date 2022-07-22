library(rayshader)
library(tibble)
library(ambient)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tictoc)
library(dplyr)

is_within_circle <- function(x_coord, y_coord, x_center, y_center, radius) {
  (x_coord - x_center)^2 + (y_coord - y_center)^2 < radius^2
}

additive_circles <- function(n = 5, pixels = 1000, seed = NULL) {
  
  if(!is.null(seed)) set.seed(seed)
  
  # setup canvas
  art <- long_grid(
    x = seq(0, 1, length.out = pixels),
    y = seq(0, 1, length.out = pixels)
  )
  art$paint <- 0
  
  for(i in 1:n) {
    
    # sample a random circle
    x_center <- runif(1, min = .3, max = .7)
    y_center <- runif(1, min = .3, max = .7)
    radius <- runif(1, min = .05, max = .25)
    
    # add +1 to all points inside the circle
    art <- art |>
      mutate(
        paint = paint + is_within_circle(
          x, y, x_center, y_center, radius
        )
      )
  }
  
  # normalise paint to [0, 1] range and return
  art$paint <- normalise(art$paint)
  return(art)
}

circle_art <- additive_circles(seed = 99)

circle_array <- circle_art |>
  as.array(value = paint) 

circle_shadow <- ray_shade(
  heightmap = circle_array,
  sunaltitude = 15, 
  sunangle = 135,
  zscale = .01,
  multicore = TRUE
)

circle_scape <- circle_array |> 
  height_shade() |>
  add_shadow(
    shadowmap = circle_shadow,
    max_darken = .1
  )

tic()
plot_map(circle_scape, rotate = 270)
toc()
