library(dplyr)
library(purrr)
library(tidyr)
library(tibble)
library(ggplot2)
library(ambient)
library(tictoc)
library(ggthemes)
library(here)

edge_length <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

edge_noise <- function(size) {
  runif(1, min = -size/2, max = size/2)
}

sample_edge_l <- function(polygon) {
  sample(length(polygon), 1, prob = map_dbl(polygon, ~ .x$seg_len))
}

insert_edge_l <- function(polygon, noise) {
  
  ind <- sample_edge_l(polygon)
  len <- polygon[[ind]]$seg_len
  
  last_x <- polygon[[ind]]$x
  last_y <- polygon[[ind]]$y
  
  next_x <- polygon[[ind + 1]]$x
  next_y <- polygon[[ind + 1]]$y
  
  new_x <- (last_x + next_x) / 2 + edge_noise(len * noise)
  new_y <- (last_y + next_y) / 2 + edge_noise(len * noise)
  
  new_point <- list(
    x = new_x,
    y = new_y,
    seg_len = edge_length(new_x, new_y, next_x, next_y)
  )
  
  polygon[[ind]]$seg_len <- edge_length(
    last_x, last_y, new_x, new_y
  )
  
  c(
    polygon[1:ind],
    list(new_point),
    polygon[-(1:ind)]
  )
}

grow_polygon_l <- function(polygon, iterations, noise, seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  for(i in 1:iterations) polygon <- insert_edge_l(polygon, noise)
  return(polygon)
}

grow_multipolygon_l <- function(base_shape, n, seed = NULL, ...) {
  if(!is.null(seed)) set.seed(seed)
  polygons <- list()
  for(i in 1:n) {
    polygons[[i]] <- grow_polygon_l(base_shape, ...) |>
      transpose() |>
      as_tibble() |>
      mutate(across(.fn = unlist))
  }
  polygons <- bind_rows(polygons, .id = "id")
  polygons
}

show_multipolygon <- function(polygon, fill, alpha = .02, ...) {
  ggplot(polygon, aes(x, y, group = id)) +
    geom_polygon(colour = NA, alpha = alpha, fill = fill, ...) + 
    coord_equal() + 
    theme_void()
}

splotch <- function(seed, layers = 10) {
  set.seed(seed)
  square_l <- transpose(tibble(
    x = c(0, 1, 1, 0, 0),
    y = c(0, 0, 1, 1, 0),
    seg_len = c(1, 1, 1, 1, 0)
  ))
  square_l |> 
    grow_polygon_l(iterations = 10, noise = .5, seed = seed) |>
    grow_multipolygon_l(n = layers, iterations = 500, noise = .8, seed = seed) 
}

tic()
dat <- splotch(seed = 1)
pic <- dat |> show_multipolygon(fill = "white", alpha = .2)
ggsave(
  filename = here("output", "splotch.png"), 
  plot = pic,
  width = 2000,
  height = 2000,
  units = "px",
  dpi = 300,
  bg = "black"
)
toc()



