
library(tibble)
library(purrr)
library(ggplot2)
library(ggforce)
library(voronoise)
library(dplyr)

sample_canva2 <- function(seed = NULL, n = 4) {
  if(!is.null(seed)) set.seed(seed)
  sample(ggthemes::canva_palettes, 1)[[1]] |>
    (\(x) colorRampPalette(x)(n))()  
}

unboxy <- function(iterations, layers) {
  
  coeffs <- array(
    data = runif(9 * layers, min = -1, max = 1), 
    dim = c(3, 3, layers)
  )
  
  point0 <- matrix(
    data = runif(3, min = -1, max = 1), 
    nrow = 1,
    ncol = 3
  )
  
  funs <- list(
    function(point) point + (sum(point ^ 2)) ^ (1/3),
    function(point) sin(point),
    function(point) 2 * sin(point)
  )
  
  update <- function(point, t) {
    l <- sample(layers, 1)
    f <- sample(funs, 1)[[1]]
    z <- point[3]
    point[3] <- 1
    point <- f(point %*% coeffs[,,l])
    point[3] <- (point[3] + z)/2
    return(point)
  }
  
  points <- accumulate(1:iterations, update, .init = point0)
  points <- matrix(unlist(points), ncol = 3, byrow = TRUE)
  points <- as_tibble(as.data.frame(points)) 
  names(points) <- c("x", "y", "val")
  return(points)
}

sift <- function(data) {
  data <- data |>
    group_by(group) |>
    mutate(
      tilesize = (max(x) - min(x)) * (max(y) - min(y)),
      x = if_else(tilesize > .02, x, x + rnorm(1)/10), 
      y = if_else(tilesize > .02, y, y + rnorm(1)/10)
    ) |>
    ungroup()
  return(data)
}

shake <- function(data) {
  data |> 
    group_by(group) |>
    mutate(
      x = x + runif(1)/10, 
      y = y + runif(1)/10
    ) |>
    ungroup()
}

voronoi_baroque <- function(
    seed, 
    perturb, 
    max.radius = NULL, 
    radius = 0, 
    expand = 0,
    ...
) {
  
  set.seed(seed)
  
  blank <- ggplot(mapping = aes(x, y, fill = val)) +
    theme_void() + 
    coord_equal(xlim = c(-2.75, 2.75), ylim = c(-2.75, 2.75)) + 
    guides(fill = guide_none(), alpha = guide_none()) +
    scale_fill_gradientn(colours = sample_canva2(seed)) + 
    scale_alpha_identity() + 
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))
  
  blank + 
    geom_voronoise(
      data = unboxy(iterations = 10000, layers = 5),
      perturb = perturb,
      max.radius = max.radius,
      radius = radius,
      expand = expand,
      ...,
      show.legend = FALSE
    )
}

pic <- voronoi_baroque(43, shake)
plot(pic)
