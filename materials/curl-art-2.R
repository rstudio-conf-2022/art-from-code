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

custom_curl_data <- function(data) {
  curl_data(
    data = data,
    iterations = 80, 
    octaves = 10,
    fractal = ridged,
    noise = gen_cubic,
    freq_init = 1,
    frequency = ~ . * 1.2,
    gain_init = 1,
    gain = ~ . * .9,
    seed = 1
  )
}

circle <- function(n = 100) {
  tibble(
    theta = 2 * pi * (1:n) / n, 
    x = cos(theta),
    y = sin(theta)
  )
}

dat1 <- circle(5000) |> 
  custom_curl_data()

dat2 <- circle(5000) |>
  mutate(x = x * .99, y = y * .99) |>
  custom_curl_data()

pic <- ggplot(mapping = aes(x, y, group = time)) +
  geom_polygon(data = dat1, fill = "#ffffff10") +
  geom_polygon(data = dat2, fill = "#22222205") +
  theme_void() +
  coord_equal()

tic()
ggsave(
  filename = here("output", "curl-art-2.png"), 
  plot = pic,
  width = 2000,
  height = 2000,
  units = "px",
  dpi = 300,
  bg = "white"
)
toc()
