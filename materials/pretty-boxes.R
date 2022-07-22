
library(tictoc)
library(Rcpp)
library(here)

sourceCpp(file = here("materials", "unbox-grid.cpp"))

sample_canva2 <- function(seed = NULL, n = 4) {
  if(!is.null(seed)) set.seed(seed)
  sample(ggthemes::canva_palettes, 1)[[1]] |>
    (\(x) colorRampPalette(x)(n))()  
}

pretty_boxes <- function(
    seed,
    iterations = 100000000, 
    layers = 5, 
    pixels = 4000, 
    background = "black",
    border = 4,
    trim = .001
) {
  
  set.seed(seed)
  
  mat <- unboxer_grid(
    iterations = iterations, 
    layers = layers, 
    pixels = pixels, 
    border = border
  )
  
  shades <- c(background, sample_canva2(seed, n = 1023))
  
  zlim <- quantile(mat, c(trim, 1 - trim))
  mat[mat < zlim[1]] <- zlim[1]
  mat[mat > zlim[2]] <- zlim[2]
  
  op <- par(mar = c(0, 0, 0, 0))
  image(
    z = mat, 
    axes = FALSE, 
    asp = 1, 
    useRaster = TRUE, 
    col = shades
  )
  par(op)
}

tic()
pretty_boxes(100, iterations = 10000000)
toc()

