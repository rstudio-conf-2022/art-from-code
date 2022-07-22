library(dplyr)
library(purrr)
library(tidyr)
library(tibble)
library(ggplot2)
library(ambient)
library(tictoc)
library(ggthemes)
library(gifski)

square <- tibble(
  x = c(0, 1, 1, 0, 0),
  y = c(0, 0, 1, 1, 0),
  seg_len = c(1, 1, 1, 1, 0)
)

show_polygon <- function(polygon, show_vertices = TRUE, colour = "black", ...) {
  
  pic <- ggplot(polygon, aes(x, y)) +
    geom_polygon(fill = NA, colour = colour, show.legend = FALSE, ...) + 
    coord_equal() + 
    theme_void()
  
  if(show_vertices == TRUE) {
    pic <- pic + geom_point(size = 2, colour = colour)
  }
  return(pic)
}

sample_edge <- function(polygon) {
  sample(nrow(polygon), 1, prob = polygon$seg_len)
}

edge_length <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

edge_noise <- function(size) {
  runif(1, min = -size/2, max = size/2)
}

insert_edge <- function(polygon, noise) {
  
  # sample and edge and remember its length
  ind <- sample_edge(polygon)
  len <- polygon$seg_len[ind]
  
  # one endpoint of the old edge
  last_x <- polygon$x[ind]
  last_y <- polygon$y[ind]
  
  # the other endpoint of the old edge
  next_x <- polygon$x[ind + 1]
  next_y <- polygon$y[ind + 1]
  
  # location of the new point to be inserted: noise 
  # is scaled proportional to the length of the old edge
  new_x <- (last_x + next_x) / 2 + edge_noise(len * noise)
  new_y <- (last_y + next_y) / 2 + edge_noise(len * noise)
  
  # the new row for insertion into the tibble, 
  # containing coords and length of the 'new' edge
  new_row <- tibble(
    x = new_x,
    y = new_y,
    seg_len = edge_length(new_x, new_y, next_x, next_y)
  )
  
  # update the length of the 'old' edge
  polygon$seg_len[ind] <- edge_length(
    last_x, last_y, new_x, new_y
  )
  
  # insert a row into the tibble
  bind_rows(
    polygon[1:ind, ],
    new_row,
    polygon[-(1:ind), ]
  )
}

grow_polygon <- function(polygon, iterations, noise, seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  for(i in 1:iterations) polygon <- insert_edge(polygon, noise)
  return(polygon)
}

grow_multipolygon <- function(base_shape, n, seed = NULL, ...) {
  if(!is.null(seed)) set.seed(seed)
  polygons <- list()
  for(i in 1:n) {
    polygons[[i]] <- grow_polygon(base_shape, ...)
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

# simplified version of the one in the workshop
tic()
dat <- square |> 
  grow_polygon(iterations = 100, noise = .5, seed = 2) |>
  grow_multipolygon(n = 10, iterations = 300, noise = 1, seed = 2)
pic <- show_multipolygon(dat, fill = "#d43790", alpha = .2)
plot(pic)
toc()
