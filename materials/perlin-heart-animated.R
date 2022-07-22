library(dplyr)
library(purrr)
library(tidyr)
library(tibble)
library(ggplot2)
library(ambient)
library(tictoc)
library(ggthemes)
library(gifski)
library(here)

sample_canva <- function(seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  sample(ggthemes::canva_palettes, 1)[[1]]
}

show_polygon <- function(polygon, show_vertices = TRUE, ...) {
  
  pic <- ggplot(polygon, aes(x, y)) +
    geom_polygon(colour = "black", fill = NA, show.legend = FALSE, ...) + 
    coord_equal() + 
    theme_void()
  
  if(show_vertices == TRUE) {
    pic <- pic + geom_point(colour = "black", size = 2)
  }
  return(pic)
}

heart_x <- function(angle) {
  x <- (16 * sin(angle) ^ 3) / 17
  return(x - mean(x))
}

heart_y <- function(angle) {
  y <- (13 * cos(angle) - 5 * cos(2 * angle) - 2 * cos(3 * angle) -
          cos(4 * angle)) / 17
  return(y - mean(y))
}

normalise_radius <- function(x, min, max) {
  normalise(x, from = c(-0.5, 0.5), to = c(min, max))
}

perlin_heart2 <- function(n = 100, 
                          freq_init = 0.3,
                          octaves = 2, 
                          r_min = 0.5, 
                          r_max = 1,
                          w_min = 0,
                          w_max = 4,
                          rot = 0,
                          x_shift = 0,
                          y_shift = 0,
                          id = NA,
                          seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  tibble(
    angle = seq(0, 2*pi, length.out = n),
    
    radius = fracture(
      x = cos(angle), 
      y = sin(angle), 
      freq_init = freq_init,
      noise = gen_perlin, 
      fractal = fbm, 
      octaves = octaves
    ) |>
      normalise_radius(r_min, r_max),
    
    x = radius * heart_x(angle) + x_shift,
    y = radius * heart_y(angle) + y_shift,
    
    width = fracture(
      x = cos(angle + rot), 
      y = sin(angle + rot), 
      freq_init = freq_init,
      noise = gen_perlin, 
      fractal = fbm, 
      octaves = octaves
    ) |>
      normalise(to = c(w_min, w_max)),
    
    id = id
  )
}

perlin_heart_data <- function(nhearts = 10, scatter = .05, seed = NULL) {
  
  if(!is.null(seed)) set.seed(seed)
  
  palette <- sample_canva(seed) |>
    (\(x) colorRampPalette(x)(nhearts))()
  
  heart_settings <- tibble(
    id = 1:nhearts,
    n = 500,
    r_min = .35, 
    r_max = .4,
    w_min = -10, 
    w_max = 10,
    x_shift = runif(nhearts, -scatter/2, scatter/2),
    y_shift = runif(nhearts, -scatter/2, scatter/2),
    rot = runif(nhearts, -pi, pi)
  )
  
  heart_settings |>
    pmap_dfr(perlin_heart2) |>
    group_by(id) |>
    mutate(
      shade = sample(palette, 1),
      width = abs(width)
    )
}

generate_one_frame <- function(dat) {
  
  pic <- dat |>
    ggplot(aes(x, y, group = id, size = width, colour = shade)) +
    geom_path(show.legend = FALSE) +
    theme_void() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_colour_identity() +
    scale_size_identity() +
    coord_fixed(xlim = c(-.6, .6), ylim = c(-.6, .6))
  
  print(pic)
}

rotate_vector <- function(x, percent) {
  
  len <- length(x)
  ind <- ceiling(len * percent)
  if(ind == 0) return(x)
  if(ind == len) return(x)
  c(x[(ind+1):len], x[1:ind])
}

generate_all_frames <- function(dat, nframes = 100) {
  
  for(frame in 1:nframes) {
    dat |>
      group_by(id) |>
      mutate(width = width |> rotate_vector(frame / nframes)) |>
      generate_one_frame()
  }
}

animated_perlin_heart <- function(seed, ...) {
  
  gif_file <- paste0("animated-perlin-heart-", seed, ".gif")
  save_gif(
    expr = perlin_heart_data(seed = seed, ...) |> generate_all_frames(),
    gif_file = here("output", gif_file),
    height = 1000,
    width = 1000,
    delay = .1,
    progress = TRUE,
    bg = "#222222"
  )
  invisible(NULL)
}

tic()
animated_perlin_heart(seed = 99)
toc()

