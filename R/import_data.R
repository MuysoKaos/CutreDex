options(scipen = 1111)

type <- toupper(c(
  "grass", "fire", "water", "bug", "normal", "poison", "electric", "ground", "fairy", "fighting", "psychic", "rock",
  "ghost", "ice", "dragon", "dark", "steel", "flying"
))

coltype <- c(
  "#3fa129", "#e62829", "#2980ef", "#91a119", "#9fa19f", "#9141cb", "#fac000", "#915121", "#ef70ef", "#ff8000", "#ef4179", "#afa981",
  "#704170", "#3dcef3", "#5060e1", "#624d4e", "#60a1b8", "#81b9ef"
)

dataset <- read.csv("data/pokemon.csv") |>
  dplyr::filter(generation <= 6)

image_files <- list.files("img")

data_image <- data.frame(image_files = image_files) |>
  dplyr::mutate(name = gsub(".png", "", image_files)) |>
  tidyr::separate(name, into = c("name", "complement"), sep = "-", extra = "merge") |>
  dplyr::mutate(complement = ifelse(is.na(complement), "common", complement))

means <- dataset |>
  dplyr::select(attack, defense, hp, sp_attack, sp_defense, speed) |>
  dplyr::summarise(across(everything(), ~ round(mean(.), 2))) |>
  dplyr::mutate(name = "Average")

# Definir los valores máximos y mínimos
max_vals <- dataset |>
  dplyr::select(attack, defense, hp, sp_attack, sp_defense, speed) |>
  dplyr::summarise(across(everything(), ~ round(max(.), 2) + 10))

min_vals <- rep(0, 6)
