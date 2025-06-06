#' Carga de raster desde la base de datos
#'
#' Esta función se encarga de cargar un archivo raster desde la base de datos según el área especificada 
#' (AMBA o Buenos Aires). Utiliza una consulta SQL para recuperar el raster como un archivo binario 
#' y lo guarda temporalmente como un archivo `.tif`, que luego es cargado como un objeto raster 
#' utilizando el paquete `terra`.
#'
#' @param pool Conexión al pool de base de datos.
#' @param raster_data Un dataframe que contiene la información del raster, incluyendo el nombre del archivo (filename).
#' @param area Una cadena de texto que indica el área de interés. Puede ser "amba" o "baires".
#'
#' @return Un objeto `rast` del paquete `terra`, que representa el archivo raster cargado desde la base de datos.
#' @export
rasterLoader <- function(pool,
                         raster_data, 
                         area){
  
 # con <- pool::poolCheckout(pool)  # Obtienes la conexión
 # on.exit(pool::poolReturn(con))   # Aseguras que se devuelva al pool cuando la función termine
  
  # Los rasters de amba y baires estan en diferentes tablas dentro de la base
  # porque tienen distintos tamaños
  if (area == "amba") {
    query <- paste0(
      "SELECT ST_AsGDALRaster(rast, 'GTiff') AS rast FROM raster_schema.rasters_geo WHERE filename='",
      raster_data$filename, "';"
    )
  } else {
    query <- paste0(
      "SELECT ST_AsGDALRaster(rast, 'GTiff') AS rast FROM raster_schema.raster_geo_baires WHERE filename='",
      raster_data$filename, "';"
    )

  }
  
  result <- pool::dbGetQuery(pool, query)
  
  if (nrow(result) == 0) {
    warning("Error: No raster found for the specified filename.")
  }

  # Guarda el binario a un archivo temporario
  temp_file <- tempfile(fileext = ".tif")
  writeBin(result$rast[[1]], temp_file)
  
  # Carga el raster leyendolo desde el archivo temporario
  return(terra::rast(temp_file))
    }
