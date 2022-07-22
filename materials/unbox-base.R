
library(ggplot2)
library(tibble)
library(purrr)
library(dplyr)
library(tictoc)
library(ggthemes)
library(here)

sample_canva2 <- function(seed = NULL, n = 4) {
  
  if(!is.null(seed)) set.seed(seed)
  sample(ggthemes::canva_palettes, 1)[[1]] |>
    (\(x) colorRampPalette(x)(n))()  
}

funs <- list(
  function(point) point + (sum(point ^ 2)) ^ (1/3),
  function(point) sin(point),
  function(point) 2 * sin(point)
)

unboxer_base <- function(iterations, layers, seed = NULL) {
  
  if(!is.null(seed)) set.seed(seed)
  
  # coefficients defining affine layer transforms, A_i
  coeffs <- array(
    data = runif(9 * layers, min = -1, max = 1), 
    dim = c(3, 3, layers)
  )
  
  # list of variant functions, g_j
  funs <- list(
    function(point) point + (sum(point ^ 2)) ^ (1/3),
    function(point) sin(point),
    function(point) 2 * sin(point)
  )
  
  # updater function: apply the layer, then the function
  # (the weirdness with point[3] is me treating colour as special)
  update <- function(point, layer, transform) {
    f <- funs[[transform]]
    z <- point[3]
    point[3] <- 1
    point <- f(point %*% coeffs[,,layer])
    point[3] <- (point[3] + z)/2
    return(point)
  }
  
  # initial point
  point0 <- matrix(
    data = runif(3, min = -1, max = 1), 
    nrow = 1,
    ncol = 3
  )
  
  # sample points
  layer_ind <- sample(layers, iterations, replace = TRUE)  
  trans_ind <- sample(length(funs), iterations, replace = TRUE)  
  points <- accumulate2(layer_ind, trans_ind, update, .init = point0)
  
  # tidy up, add columns, and return
  points <- matrix(unlist(points), ncol = 3, byrow = TRUE)
  points <- cbind(
    points,
    c(0, layer_ind),
    c(0, trans_ind)
  )
  return(points)
}

unbox_art <- function(data, seed = NULL, size = 1) {
  
  # convert to data frame and sample a palette
  data <- data |> as.data.frame() |> as_tibble()
  names(data) <- c("x", "y", "c", "l", "t")[1:ncol(data)]
  shades <- sample_canva2(seed)
  
  # render image as a scatter plot
  ggplot(data, aes(x, y, colour = c)) +
    geom_point(
      size = size,
      stroke = 0,
      show.legend = FALSE
    ) + 
    theme_void() + 
    coord_equal(xlim = c(-4, 4), ylim = c(-4, 4)) + 
    scale_colour_gradientn(colours = shades) + 
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(panel.background = element_rect(
      fill = shades[1], colour = shades[1]
    ))
}

tic()

seed <- 1234
layers <- 5

pic <- unboxer_base(1000000, layers = layers, seed = seed) |> 
  unbox_art(seed = seed, size = .2) 
fname <- paste0("unboxer-base-", layers, "-", seed, ".png")

ggsave(
  filename = here("output", fname), 
  plot = pic,
  width = 2000,
  height = 2000,
  units = "px",
  dpi = 300
)

toc()



