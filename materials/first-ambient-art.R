library(dplyr)
library(purrr)
library(tibble)
library(ggplot2)
library(ggthemes)
library(ambient)

sample_canva <- function(seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  sample(ggthemes::canva_palettes, 1)[[1]]
}

x_coords <- seq(from = 0, to = 1, length.out = 800)
y_coords <- seq(from = 0, to = 1, length.out = 800)
canvas <- long_grid(x = x_coords, y = y_coords) 

freq_spatial <- 10
seed_spatial <- 100
seed_palette <- 101

dat <- canvas |> 
  mutate(
    paint = gen_perlin(
      x = x, 
      y = y, 
      frequency = freq_spatial, 
      seed = seed_spatial
    )
  )
  
pic <- dat |>
  ggplot(aes(x, y, fill = paint)) + 
  geom_raster(show.legend = FALSE) +
  theme_void() + 
  coord_equal() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(
    colours = sample_canva(seed_palette)
  )

plot(pic)
