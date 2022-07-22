library(dplyr)
library(purrr)
library(tibble)
library(ggplot2)
library(ggthemes)
library(ambient)
library(here)
library(tictoc)

curl_data <- function(
    data, 
    iterations = 50,
    step_size = .001,
    ...
) {
  
  update <- function(current_state, iteration, ...) {
    curl <- curl_noise(
      x = current_state$x, 
      y = current_state$y,
      generator = fracture,
      ...
    )
    next_state <- current_state |>
      mutate(
        x = x + curl$x * step_size,
        y = y + curl$y * step_size,
        time = time + 1
      )
    return(next_state)
  }
  
  data |> 
    mutate(id = row_number(), time = 1) |>
    accumulate(1:iterations, update, .init = _, ...) |>
    bind_rows()
}

curl_art <- function(...) {
  curl_data(...) |> 
    ggplot(aes(x, y, group = id)) + 
    geom_path() +
    theme_void() + 
    coord_equal() 
}

smol_grid <- long_grid(x = 1:20, y = 1:20)

pic <- smol_grid |>
  mutate(x = normalise(x), y = normalise(y)) |>
  curl_art(noise = gen_simplex, fractal = fbm, octaves = 4, freq_init = .5)

tic()
ggsave(
  filename = here("output", "curl-art-1.png"), 
  plot = pic,
  width = 2000,
  height = 2000,
  units = "px",
  dpi = 300,
  bg = "white"
)
toc()

