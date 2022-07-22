
library(truchet)
library(ggplot2)

set.seed(123)
mosaic <- st_truchet_ms(
  tiles = c("dr", "tn", "ane"), 
  p1 = 0.2, # scale 1 
  p2 = 0.6, # scale 2
  p3 = 0.2, # scale 3
  xlim = c(1, 6),
  ylim = c(1, 6)
)

pic <- mosaic |> 
  ggplot(aes(fill = color)) +
  geom_sf(color = NA, show.legend = FALSE) + 
  scale_fill_gradientn(colours = c("#222222", "#ffffff")) + 
  theme_void()

plot(pic)
