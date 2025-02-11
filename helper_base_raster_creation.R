
library(tidyverse)
library(pool)
library(config)

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


use_data(base_raster, overwrite = TRUE)

pool <- pool::dbPool(
  drv = RPostgres::Postgres(),
  dbname = "geocovidapp_db",
  user = Sys.getenv("USER"),
  password = Sys.getenv("PASSWORD"),
  port = "5432",
  host = "44.226.244.60"
)
onStop(function() {
  pool::poolClose(pool)
})

# PRIMERA VERSION
# Esta version de la app no tiene conectados todos los datos.
# mini_data_sisa <- data_sisa |> filter(fecha_enfermo >= as.Date(min(base_raster$fecha)),
#                                       fecha_enfermo <= as.Date(max(base_raster$fecha)))
#
# write_csv(mini_data_sisa, "mini_data_sisa_deploy.csv")

data_sisa <- dplyr::tbl(pool, 'tabla_covid') |> 
  dplyr::collect()
use_data(data_sisa)

# Otras formas de leer rasters
# https://stackoverflow.com/questions/60606191/how-to-download-images-from-postgres-bytea-column-using-r-or-python

pool <- pool::dbPool(
  drv = RPostgres::Postgres(),
  dbname = "geocovidapp_db",
  user = Sys.getenv("USER"),
  password = Sys.getenv("PASSWORD"),
  port = "5432",
  host = "44.226.244.60"
)
onStop(function() {
  pool::poolClose(pool)
})

# llamo el raster
raster <- base_raster |>
  dplyr::filter(fecha == as.Date('2020-04-30',
                                 origin = "1970-01-01"),
                tipo_de_raster ==  'pc',
                momento == 'tarde', # es un valor no reactivo
                locacion ==  'amba')
thisQ = paste0("SELECT rast AS image from raster_schema.rasters_geo WHERE filename='",
               raster$filename,"';")
# Execute the SELECT query and fetch the results


## THIS WOR
query <- paste0("SELECT ST_AsGDALRaster(rast, 'GTiff') AS rast FROM raster_schema.rasters_geo WHERE filename='",
raster$filename,"';")
result <- dbGetQuery(pool, query)
# Save the binary raster data to a temporary file
temp_file <- tempfile(fileext = ".tif")
writeBin(result$rast[[1]], temp_file)

# Load the raster package and read the temporary file
library(terra)
raster_data <- rast(temp_file)
plot(raster_data)


resultSet = pool::dbGetQuery(pool, thisQ)

#resultData <- fetch(resultSet, n=-1)

blob2string <- function(blob){
  hex_raw <- wkb::hex2raw(blob)
  rawToChar(as.raw(unlist(hex_raw)))
}
blob2string(result)

# probando
library(hexView)
#https://stackoverflow.com/questions/50361059/import-raw-image-in-r
imageDataDecoded <- base64enc::base64decode(resultSet$image)
#raw_image <- readRaw(imageDataDecoded)
image_matrix <- matrix(imageDataDecoded,
                       nrow = 286, 
                       ncol = 225,
                       byrow = TRUE
                       )
plot(as.raster(image_matrix))
rast(image_matrix)

dim(imageDataDecoded)


# Create a file connection and write the binary data to disk using mode "wb".
write.filename = file("test.png", "wb")
writeBin(imageDataDecoded, write.filename)
close(write.filename)
imageDataDecoded <-jsonlite::base64_dec(resultSet$image)

conn <- file("img2.png","wb")
writeBin(imageDataDecoded, conn, useBytes=TRUE)
close(conn)
inconn <- file("w.bin","rb")
outconn <- file("img2.jpg","wb")
base64enc::base64decode(what=inconn, output=outconn)
close(inconn)
close(outconn)

decode <- function(input, output = tempfile()){
  input <- normalizePath(input, mustWork = TRUE)
  buf <- readBin(input, raw(), file.info(input)$size)
  bin <- base64_decode(buf)
  writeBin(bin, output)
  output
}


writeBin(imageDataDecoded, "imageDataDecoded.tiff")

terra::rast(rawToBits(imageDataDecoded))

# Create a file connection and write the binary data to disk using mode "wb".
# Create a file connection and write the binary data to disk using mode "wb".
write.filename = file("file.png", "wb")
writeBin(imageDataDecoded, write.filename)
close(write.filename)
