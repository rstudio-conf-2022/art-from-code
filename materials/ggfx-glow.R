library(dplyr)
library(tibble)
library(ggplot2)
library(ggforce)
library(flametree)
library(ggfx)

tree <- flametree_grow(
  seed = 1, 
  time = 9, 
  angle = c(-15, 15, 30)
)

leaf <- tree |> filter(id_leaf == TRUE)

base <- ggplot() + 
  scale_size_identity() + 
  theme_void() + 
  coord_equal()

leaves <- geom_point(
  mapping = aes(coord_x, coord_y),
  data = leaf, 
  size = 1.3, 
  stroke = 0, 
  colour = "#e38b75"
)

trunk <- geom_bezier(
  mapping = aes(coord_x, coord_y, group = id_pathtree, size = seg_wid),
  data = tree, 
  lineend = "round", 
  colour = "#555555",
  show.legend = FALSE
)

plot(
  base +   
    trunk + 
    with_outer_glow(leaves, colour = "#555555", sigma = 5, expand = 3)
)
