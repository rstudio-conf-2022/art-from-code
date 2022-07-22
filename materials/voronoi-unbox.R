
library(tibble)
library(purrr)
library(ggplot2)
library(ggforce)

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

set.seed(1)
dat <- unboxy(iterations = 1000, layers = 5) 

pic <- ggplot(dat, aes(x, y, fill = val)) +
  theme_void() + 
  coord_equal(xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5)) + 
  scale_fill_gradientn(colours = sample_canva2()) + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

pic2 <- pic +
  geom_voronoi_tile(
    colour = "#222222",
    size = .2, 
    show.legend = FALSE
  )

plot(pic2)
