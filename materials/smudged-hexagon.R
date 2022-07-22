library(dplyr)
library(purrr)
library(tidyr)
library(tibble)
library(ggplot2)
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

smudged_hexagon <- function(seed, noise1 = 0, noise2 = 2, noise3 = 0.5) {
  set.seed(seed)
  
  # define hexagonal base shape
  theta <- (0:6) * pi / 3
  hexagon <- tibble(
    x = sin(theta),
    y = cos(theta),
    seg_len = edge_length(x, y, lead(x), lead(y))
  )
  hexagon$seg_len[7] <- 0
  hexagon <- transpose(hexagon)
  base <- hexagon |> 
    grow_polygon_l(
      iterations = 60, 
      noise = noise1
    )
  
  # define intermediate-base-shapes in clusters
  polygons <- list()
  ijk <- 0
  for(i in 1:3) {
    base_i <- base |> 
      grow_polygon_l(
        iterations = 50, 
        noise = noise2
      )
    
    for(j in 1:3) {
      base_j <- base_i |> 
        grow_polygon_l(
          iterations = 50, 
          noise = noise2
        )
      
      # grow 10 polygons per intermediate-base
      for(k in 1:10) {
        ijk <- ijk + 1
        polygons[[ijk]] <- base_j |>
          grow_polygon_l(
            iterations = 500, 
            noise = noise3
          ) |>
          transpose() |>
          as_tibble() |>
          mutate(across(.fn = unlist))
      }
    }
  }
  
  # return as data frame
  bind_rows(polygons, .id = "id")
}


tic()
dat <- smudged_hexagon(seed = 88)
pic <- dat |> show_multipolygon(fill = "#d4379005")
ggsave(
  filename = here("output", "smudged-hexagon.png"), 
  plot = pic,
  width = 2000,
  height = 2000,
  units = "px",
  dpi = 300,
  bg = "#222222"
)
toc()



