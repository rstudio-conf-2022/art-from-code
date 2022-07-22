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

leaves <- geom_point(
  data = leaf, 
  mapping = aes(coord_x, coord_y, colour = seg_col),
  colour = "white",
  size = 2, 
  stroke = 0
)

trunk <- geom_bezier(
  data = tree,
  mapping = aes(
    x = coord_x, 
    y = coord_y, 
    size = seg_wid,
    group = id_pathtree
  ),
  colour = "white",
  lineend = "round"
)

polygon_layer <- function(x, y, fill = "white", alpha = .5) {
  geom_polygon(aes(x, y), fill = fill, alpha = alpha)
}

triangle <- polygon_layer(
  x = c(-4, 2, 2), 
  y = c(0, 0, 6), 
  fill = "white",
  alpha = 1
)

base <- ggplot() + 
  theme_void() +
  theme(panel.background = element_rect(
    fill = "black", colour = "black"
  )) + 
  coord_equal(xlim = c(-3, 1), ylim = c(1, 5)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_size_identity(guide = guide_none())

plot(
  base +
    as_group(trunk, leaves, id = "tree") + 
    with_blend(triangle, "tree", blend_type = "xor")
)
