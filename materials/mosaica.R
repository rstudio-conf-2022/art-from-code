
library(ggplot2)
library(tibble)
library(purrr)
library(dplyr)
library(ambient)
library(tidyr)

sample_canva2 <- function(seed = NULL, n = 4) {
  if(!is.null(seed)) set.seed(seed)
  sample(ggthemes::canva_palettes, 1)[[1]] |>
    (\(x) colorRampPalette(x)(n))()  
}

choose_rectangle <- function(blocks) {
  sample(nrow(blocks), 1, prob = blocks$area)
}

choose_break <- function(lower, upper) {
  round((upper - lower) * runif(1))
}

create_rectangles <- function(left, right, bottom, top, value) {
  tibble(
    left = left,
    right = right,
    bottom = bottom,
    top = top,
    width = right - left,
    height = top - bottom,
    area = width * height,
    value = value
  )
}

split_rectangle_x <- function(rectangle, new_value) {
  with(rectangle, {
    split <- choose_break(left, right)
    new_left  <- c(left, left + split)
    new_right <- c(left + split, right)
    new_value <- c(value, new_value)
    create_rectangles(new_left, new_right, bottom, top, new_value)
  })
}

split_rectangle_y <- function(rectangle, new_value) {
  with(rectangle, {
    split <- choose_break(bottom, top)
    new_bottom <- c(bottom, bottom + split)
    new_top <- c(bottom + split, top)
    new_value <- c(value, new_value)
    create_rectangles(left, right, new_bottom, new_top, new_value)
  })
}

split_rectangle <- function(rectangle, value) {
  if(runif(1) < .5) {
    return(split_rectangle_x(rectangle, value))
  }
  split_rectangle_y(rectangle, value)
}

split_block <- function(blocks, value) {
  old <- choose_rectangle(blocks) 
  new <- split_rectangle(blocks[old, ], value)
  bind_rows(blocks[-old, ], new)
}

subdivision <- function(ncol = 1000, 
                        nrow = 1000, 
                        nsplits = 50, 
                        seed = NULL) {
  
  if(!is.null(seed)) set.seed(seed)
  blocks <- create_rectangles(
    left = 1, 
    right = ncol, 
    bottom = 1, 
    top = nrow, 
    value = 0
  )
  reduce(1:nsplits, split_block, .init = blocks)
}

fill_rectangle <- function(left, right, bottom, top, width, 
                           height, area, value, nshades = 100) {
  
  set.seed(value)
  fractals <- list(billow, fbm, ridged)
  generators <- list(gen_simplex, gen_perlin, gen_worley)
  
  expand_grid(
    x = left:right, 
    y = bottom:top, 
  ) |>
    mutate(
      fill = 10 * value + fracture(
        x = x * sample(-3:3, 1),
        y = y * sample(-3:3, 1),
        noise = sample(generators, 1)[[1]],
        fractal = sample(fractals, 1)[[1]],
        octaves = sample(10, 1),
        frequency = sample(10, 1) / 20,
        value = "distance2"
      ) |>
        normalise(to = c(1, nshades)) |> 
        round()
    )
}

draw_mosaic <- function(dat, palette) {
  background <- sample(palette, 1)
  dat |>
    ggplot(aes(x, y, fill = fill)) +
    geom_tile(show.legend = FALSE, colour = background, size = .2) +
    scale_size_identity() +
    scale_colour_gradientn(colours = palette) +
    scale_fill_gradientn(colours = palette) +
    scale_x_continuous(expand = expansion(add = 5)) +
    scale_y_continuous(expand = expansion(add = 5)) +
    coord_equal() +
    theme_void() +
    theme(plot.background = element_rect(fill = background)) 
}

mosaica <- function(ncol = 60, 
                    nrow = 60, 
                    nsplits = 30, 
                    seed = NULL) {
  
  subdivision(ncol, nrow, nsplits, seed) |>
    pmap_dfr(fill_rectangle) |> 
    slice_sample(prop = .995) |>
    filter(!is.na(fill)) |>
    draw_mosaic(palette = sample_canva2(seed))
}

pic <- mosaica(ncol = 100, nrow = 100, nsplits = 200, seed = 1302)
plot(pic)
