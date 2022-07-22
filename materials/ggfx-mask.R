
library(dplyr)
library(tibble)
library(ggplot2)
library(ggfx)
library(ambient)

texture <- geom_raster(
  mapping = aes(x, y, fill = paint),
  data = long_grid(
    x = seq(from = -1, to = 1, length.out = 1000),
    y = seq(from = -1, to = 1, length.out = 1000)
  ) |> 
    mutate(
      lf_noise = gen_simplex(x, y, frequency = 2, seed = 1234),
      mf_noise = gen_simplex(x, y, frequency = 20, seed = 1234),
      hf_noise = gen_simplex(x, y, frequency = 99, seed = 1234),
      paint = lf_noise + mf_noise + hf_noise
    )
)

hex <- tibble(x = sin((0:6)/6 * 2 * pi), y = cos((0:6)/6 * 2 * pi))
mask <- geom_polygon(aes(x, y), hex, fill = "white")

base <- ggplot() + 
  theme_void() +
  coord_equal() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(
    colours = c("#222222","#e83e8c"), 
    guide = guide_none()
  )

border <- geom_path(aes(x, y), hex, colour = "white", size = 15)

text <- geom_text(
  mapping = aes(x, y, label = text), 
  dat = tibble(x = 0, y = 0, text = "ART"), 
  size = 36,
  colour = "white", 
  fontface = "bold"
) 

plot(
  base + 
    as_group(texture, text, border, id = "content") +
    as_reference(mask, id = "mask") + 
    with_mask("content", "mask")
)