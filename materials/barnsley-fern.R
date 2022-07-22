
library(ggplot2)
library(tibble)
library(purrr)
library(dplyr)

fern_transform <- function(coord, ind) {
  
  # coefficients for the stem function f_1
  if(ind == 1) {
    mat <- matrix(c(0, 0, 0, .16), 2, 2) # matrix to multiply
    off <- c(0, 0)                       # offset vector to add
  }
  
  # coefficients for the small leaflet function f_2
  if(ind == 2) {
    mat <- matrix(c(.85, -.04, .04, .85), 2, 2)
    off <- c(0, 1.6)                      
  }
  # coefficients for the right-side function f_3
  if(ind == 3) {
    mat <- matrix(c(.2, .23, -.26, .22), 2, 2)
    off <- c(0, 1.6)                      
  }
  
  # coefficients for the left-side function f_4
  if(ind == 4) {
    mat <- matrix(c(-.15, .26, .28, .24), 2, 2)
    off <- c(0, .44)                     
  }
  
  # return the affine transformed coords
  coord <- mat %*% coord + off
  return(coord)
}

fern_chaos <- function(iterations = 10000, seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  
  # which transformation to apply at each iteration
  transform_index <- sample(
    x = 1:4, 
    size = iterations, 
    replace= TRUE, 
    prob = c(.01, .85, .07, .07)
  )
  
  # initialise chaos game at the origin
  start <- matrix(c(0, 0))
  
  # helper function to collapse accumulated output
  bind_to_column_matrix <- function(lst) {
    do.call(cbind, lst)
  }
  
  # iterate until done!
  coord_matrix <- transform_index |>
    accumulate(fern_transform, .init = start) |>
    bind_to_column_matrix() 
  
  # tidy the output, add extra columns, and return
  coord_df <- t(coord_matrix) |> 
    as.data.frame() 
  names(coord_df) <- c("x", "y")
  coord_df <- coord_df |>
    as_tibble() |>
    mutate(
      transform = c(0, transform_index),
      iteration = row_number() - 1
    )
  return(coord_df)
}

fern_dat <- fern_chaos(seed = 1)

pic <- ggplot(fern_dat, aes(x, y, colour = factor(transform))) +
  geom_point(size = 1, stroke = 0) +
  coord_equal() +
  theme_void() + 
  guides(colour = guide_legend(
    title = "transformation", 
    override.aes = list(size = 5))
  )

plot(pic)