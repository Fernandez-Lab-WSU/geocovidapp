#' Cargar pixeles raster desde base de datos
#'
#' @description
#' Conecta a una base de datos PostgreSQL/PostGIS y extrae pixeles raster
#' según el partido y el tipo de raster especificados.
#'
#' @param partido `character`. Nombre del partido para filtrar.
#' @param tipo_de_raster `character`. Tipo de raster a consultar (por ejemplo, `pc` o `7dpc`).
#'
#' @details
#' La conexión se configura usando los parámetros definidos en el archivo `config.yml`.
#' Utiliza `pool` para gestionar la conexión y `sf::st_read` para leer datos espaciales.
#'
#' @return Un objeto `sf` con los pixeles raster filtrados.
#'
#' @export
pixel_loader <- function(partido, tipo_de_raster){

  db <- config::get("database")
  
  pool2 <- pool::dbPool(
    drv = RPostgres::Postgres(),
    dbname = db$dbname,
    user = db$user,
    password = db$password,
    port = db$port,
    host = db$host
  )

  query <- paste(
    "
  select * from px_prom",
  "where partido = ",  partido,
  "and tipo_de_raster = ", tipo_de_raster
  )

  data_px <- sf::st_read(pool2, layer = 'pixeles',
                       DBI::Id(schema="px_prom", table = "px_prom")) 

return(data_px)
}