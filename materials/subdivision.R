
library(ggplot2)
library(tibble)
library(purrr)
library(dplyr)

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

develop <- function(div, seed = NULL) {
  
  div |> 
    ggplot(aes(
      xmin = left, 
      xmax = right, 
      ymin = bottom, 
      ymax = top,
      fill = value
    )) +
    geom_rect(
      colour = "#ffffff", 
      size = 3,
      show.legend = FALSE
    ) +
    scale_fill_gradientn(
      colours = sample_canva2(seed)
    ) +
    coord_equal() +
    theme_void()
}

pic <- subdivision(seed = 1) |> develop() 
plot(pic)
