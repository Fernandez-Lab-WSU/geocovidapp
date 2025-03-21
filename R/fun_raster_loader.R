#' Title
#'
#' @param raster_data 
#' @param area 
#'
#' @returns
#' @export
#'
#' @examples
rasterLoader <- function(pool,
                         raster_data, 
                         area){
  
  # Los rasters de amba y baires estan en diferentes bases de datos
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
  print(result)
  # Save the binary raster data to a temporary file
  temp_file <- tempfile(fileext = ".tif")
  writeBin(result$rast[[1]], temp_file)
  
  # Load the raster package and read the temporary file
  return(terra::rast(temp_file))
    }
