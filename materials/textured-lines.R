library(dplyr)
library(tibble)
library(ggplot2)
library(here)
library(e1071)

smooth_loess <- function(x, span) {
  n <- length(x)
  dat <- tibble(time = 1:n, walk = x)
  mod <- loess(walk ~ time, dat, span = span)
  predict(mod, tibble(time = 1:n))
}

smooth_path <- function(n = 1000, smoothing = .4, seed = NULL) { 
  if(!is.null(seed)) set.seed(seed)
  tibble(
    x = smooth_loess(rbridge(1, n), span = smoothing),
    y = smooth_loess(rbridge(1, n), span = smoothing),
    stroke = 1
  )
}

perturb <- function(path, noise = .01, span = .1) {
  path |> 
    group_by(stroke) |>
    mutate(
      x = x + rnorm(n(), 0, noise),
      y = y + rnorm(n(), 0, noise),
      x = smooth_loess(x, span),
      y = smooth_loess(y, span),
      alpha = runif(n()) > .5,
      size = runif(n(), 0, .2)
    )
}

brush <- function(path, bristles = 100, seed = 1, ...) {
  set.seed(seed)
  dat <- list()
  for(i in 1:bristles) {
    dat[[i]] <- perturb(path, ...)
  }
  return(bind_rows(dat, .id = "id"))
}

stroke <- function(dat, geom = geom_path, colour = "white", ...) {
  dat |>  
    ggplot(aes(
      x = x, 
      y = y, 
      alpha = alpha, 
      size = size, 
      group = paste0(stroke, id)
    )) + 
    geom(
      colour = colour, 
      show.legend = FALSE,
      ...
    ) + 
    coord_equal() +
    scale_alpha_identity() +
    scale_size_identity() +
    theme_void() + 
    theme(plot.background = element_rect(
      fill = "#222222", 
      colour = "#222222"
    ))
}

path <- smooth_path(seed = 123)
pic <- path |> brush() |> stroke()
plot(pic)

