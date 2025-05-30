# Quiero obtener un data.frame con las combinaciones faltantes de horas y fechas
# para cada uno de los grupos de locacion (amba o baires) y
# tipo_de_raster (pc o 7dpc)

library(dplyr)
library(purrr)
library(quadkeyr)
library(usethis)

rasters_faltantes <- base_raster |>
  group_by(locacion, tipo_de_raster) |>
  group_split() |>
  map_dfr(~ missing_combinations(.x, hour_col = "hora", date_col = "fecha") |> 
            mutate(locacion = unique(.x$locacion), 
                   tipo_de_raster = unique(.x$tipo_de_raster))) |>
  dplyr::mutate(momento = dplyr::case_when(hora == 0 ~ "noche",
                                           hora == 8 ~ "mañana",
                                           hora == 16 ~ "tarde")) 

fechas_rasters_faltantes <- rasters_faltantes |>
  group_by(fecha, locacion, tipo_de_raster) |>
  count() |> 
  filter(n == 3) |> 
  select(-n)
  

usethis::use_data(fechas_rasters_faltantes, internal = TRUE, overwrite = TRUE)

momentos_rasters_faltantes <- rasters_faltantes |> 
  filter(!fecha %in% fechas_rasters_faltantes$fecha) |>
  group_by(fecha, locacion, tipo_de_raster) |>
  summarize(momentos = list(unique(momento)), .groups = "drop") 

usethis::use_data(momentos_rasters_faltantes, internal = TRUE, overwrite = TRUE)
