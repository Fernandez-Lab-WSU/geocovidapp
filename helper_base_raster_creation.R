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

print(raster_data)

# leo todos los archivos
amba_files <- pool::dbGetQuery(pool,
                              "SELECT filename FROM raster_schema.rasters_geo")
baires_files <- pool::dbGetQuery(pool,
                               "SELECT filename FROM raster_schema.raster_geo_baires") |> 
  filter(str_detect(filename, "pc"))

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


usethis::use_data(base_raster, overwrite = TRUE)

