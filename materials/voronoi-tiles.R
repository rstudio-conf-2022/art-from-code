
library(ggplot2)
library(ggforce)
library(tibble)

set.seed(61)

dat <- tibble(
  x = runif(20),
  y = runif(20),
  val = runif(20)
)

bg_dat <- tibble(
  x = runif(500, min = -.5, max = 1.5),
  y = runif(500, min = -.5, max = 1.5)
)

pic <- ggplot(dat, aes(x, y, fill = val)) +
  coord_equal(xlim = c(-.3, 1.3), ylim = c(-.3, 1.3)) +
  guides(fill = guide_none()) +
  theme_void() + 
  theme(panel.background = element_rect(
    fill = "#222222", colour = "#222222"
  ))

pic2 <- pic + 
  geom_voronoi_tile(
    data = bg_dat,
    fill = "#333333", 
    radius = .01,
    expand = -.0025
  ) +
  geom_voronoi_tile(
    colour = "white", 
    max.radius = .2, 
    radius = .02,
    expand = -.005
  )

plot(pic2)
