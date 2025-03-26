# Cargar bases de datos
# Los datos estan en una carpeta interna dentro de inst/
 
# Instalo el paquete con los modulos
 source("install.R")

 load("data/bsas.rda")
 load("data/bsas_comunas.rda")
 load("data/centroides_mapa.rda")
 load("data/base_raster.rda")
 load("data/data_sisa.rda")

#load(system.file("geocovidapp", "data", "bsas.rda",
#                 package = "geocovidapp"))
#load(system.file("geocovidapp", "data", "bsas_comunas.rda",
#                 package = "geocovidapp"))
#load(system.file("geocovidapp", "data", "centroides_mapa.rda",
#                 package = "geocovidapp"))
#load(system.file("geocovidapp", "data", "base_raster.rda",
#                 package = "geocovidapp"))
#load(system.file("geocovidapp", "data", "data_sisa.rda",
#                 package = "geocovidapp"))

# Para chequar la reactividad, descomentar
#library(reactlog)
#reactlog_enable()

# Cargo datasets ------

amba_reducido_names <- c('Almirante Brown',
                         'Avellaneda',
                         'Berazategui',
                         paste('Comuna', 1:15), # CABA
                         'Esteban Echeverría', 'Escobar', 'Ezeiza',
                         'Florencio Varela',
                         'General San Martín',
                         'Hurlingham',
                         'Ituzaingó',
                         'José C. Paz',
                         'La Matanza',  'Lanús', 'Lomas de Zamora',
                         'Malvinas Argentinas', 'Merlo', 'Moreno', 'Morón',
                         'Quilmes', 'Pilar', 'Presidente Perón',
                         'San Fernando', 'San Isidro', 'San Miguel',
                         'Tigre', 'Tres de Febrero',
                         'Vicente López')

amba_caba <- c('Almirante Brown',
               'Avellaneda',
               'Berazategui',
               'Capital Federal', # CABA
               'Esteban Echeverría', 'Escobar', 'Ezeiza',
               'Florencio Varela',
               'General San Martín',
               'Hurlingham',
               'Ituzaingó',
               'José C. Paz',
               'La Matanza',  'Lanús', 'Lomas de Zamora',
               'Malvinas Argentinas', 'Merlo', 'Moreno', 'Morón',
               'Quilmes', 'Pilar', 'Presidente Perón',
               'San Fernando', 'San Isidro', 'San Miguel',
               'Tigre', 'Tres de Febrero',
               'Vicente López')


# Conecto con la base de datos
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

# 
# # PRIMERA VERSION
# # Esta version de la app no tiene conectados todos los datos.
# # mini_data_sisa <- data_sisa |> filter(fecha_enfermo >= as.Date(min(base_raster$fecha)),
# #                                       fecha_enfermo <= as.Date(max(base_raster$fecha)))
# #
# # write_csv(mini_data_sisa, "mini_data_sisa_deploy.csv")
# 
# data_sisa <- dplyr::tbl(pool, 'tabla_covid') |>
#   dplyr::collect()
# 
# # data_sisa <- readr::read_csv("data/mini_data_sisa_deploy.csv") |>
# #   dplyr::filter(fecha_enfermo >= "2020-05-09" &
# #                   "2020-05-14" >= fecha_enfermo) |>
# #   dplyr::mutate(residencia_departamento_nombre = stringr::str_to_title(residencia_departamento_nombre),
# #                 residencia_departamento_nombre = stringr::str_remove(residencia_departamento_nombre, "0(?!$)"))
# 
# all_files <- pool::dbGetQuery(pool,
#                               "SELECT filename FROM raster_schema.rasters_geo")
#  HELPER
# 
# # Lista todos los archivos en la carpeta
# # all_files <- list.files("inst/rasters/")
# 
# # Fintra los archivos que terminan con '.tif'
# base_raster <- all_files |>
#   dplyr::mutate(file_info = stringr::str_replace_all(string = filename,
#                            pattern =  "\\.tif",
#                            replacement = ""),
#                 file_info = stringr::str_trim(filename, 'right')) |>
#   tibble::as_tibble() |>
#   tidyr::separate(file_info,
#                   into = c('locacion',
#                            'tipo_de_raster',
#                            'fecha',
#                            'hora'),
#                   sep = '_',
#                   remove = FALSE) |>
#   dplyr::mutate(fecha = as.Date(fecha),
#                 hora = as.numeric(stringr::str_sub(hora, end= -4))) |>
#   dplyr::mutate(momento = dplyr::case_when(hora == 0 ~ "noche",
#                                            hora == 8 ~ "mañana",
#                                            hora == 16 ~ "tarde"))



# Permite leer el directorio imagenes dentro de www/'
#shiny::addResourcePath(prefix = "imagenes",
#                directoryPath = system.file("www/imagenes",
#                                            package = "geocovidapp"))

