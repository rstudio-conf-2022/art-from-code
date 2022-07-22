library(dplyr)
library(purrr)
library(tibble)
library(ggplot2)
library(ggthemes)
library(ambient)
library(here)

sample_canva <- function(seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  sample(ggthemes::canva_palettes, 1)[[1]]
}

make_noise_art <- function(
    generator = gen_perlin, 
    frequency = 10, 
    seed = 1234,
    pixels = 2000,
    palette = c("#e5ddc8", "#01949a", "#004369", "#db1f48"), 
    ...
) {
  
  # define the grid
  canvas <- long_grid(
    x = seq(from = 0, to = 1, length.out = pixels),
    y = seq(from = 0, to = 1, length.out = pixels)
  ) 
  
  # use the generator to add paint
  canvas <- canvas |>
    mutate(
      paint = generator(
        x, y, 
        frequency = frequency, 
        seed = seed, 
        ...
      )
    )
  
  # use ggplot2 to draw the picture
  art <- canvas |> 
    ggplot(aes(x, y, fill = paint)) + 
    geom_raster(show.legend = FALSE) +
    theme_void() +
    coord_equal() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_gradientn(colours = palette)
  
  return(art)
}

# call make_noise_art with idiosyncratic parameters
art <- make_noise_art(
  generator = gen_worley,
  seed = 1000, 
  palette = sample_canva(100),
  value = "distance",
  pixels = 1000
)

# save the plot to file with a generic file name
ggsave(
  filename = here("output", "noise-art.png"), 
  plot = art,
  width = 1000,
  height = 1000,
  units = "px",
  dpi = 300
)

