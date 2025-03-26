# Genera un tibble con informacion de los rasters
# va a ser agregado al paquete como base_raster.rda
library(tidyverse)
library(pool)
library(config)
library(usethis)

db <- config::get("database")

pool <- pool::dbPool(
  drv = RPostgres::Postgres(),
  dbname = db$dbname,
  user = db$user,
  password = db$password,
  port = db$port,
  host = db$host
)
onStop(function() {
  pool::poolClose(pool)
})

# leo todos los archivos
amba_files <- pool::dbGetQuery(pool,
                              "SELECT filename FROM raster_schema.rasters_geo")
baires_files <- pool::dbGetQuery(pool,
                               "SELECT filename FROM raster_schema.raster_geo_baires") 

# Si quisiese solo sacar los de un tipo en particular
# |> filter(str_detect(filename, "pc"))

all_files <- rbind(baires_files, amba_files)

# Lista todos los archivos en la carpeta
# all_files <- list.files("inst/rasters/")

# Fintra los archivos que terminan con '.tif'
base_raster <- all_files |>
  tibble::as_tibble() |>
  dplyr::mutate(file_info = str_remove(filename, "\\.tif$")
               ) |>
  tidyr::separate(file_info,
                  into = c('locacion',
                           'tipo_de_raster',
                           'fecha',
                           'hora'),
                  sep = '_',
                  remove = FALSE) |>
  dplyr::mutate(fecha = as.Date(fecha),
                hora = as.numeric(hora)) |>
  dplyr::mutate(momento = dplyr::case_when(hora == 0 ~ "noche",
                                           hora == 8 ~ "mañana",
                                           hora == 16 ~ "tarde"))

# problema: missing_combinations reconoce day y hour, en ingles
# corregi esto en el paquete para que pudiese leer el dataset en castellano
usethis::use_data(base_raster, overwrite = TRUE)

# bsas -----------------
st_geometry(bsas) <- "geometry"
usethis::use_data(bsas, overwrite = TRUE)

# bsas_comunas ---------
st_geometry(bsas_comunas) <- "geometry"
usethis::use_data(bsas_comunas, overwrite = TRUE)

# bbox amba ------------
amba <- bsas[bsas$partido %in% amba_reducido_names, ]
sf::st_geometry(amba) <- "geometry"
amba_bbox <- sf::st_bbox(amba)

usethis::use_data(amba_bbox, overwrite = TRUE)
