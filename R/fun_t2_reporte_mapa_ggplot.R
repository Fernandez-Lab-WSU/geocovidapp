#' Obtener Mapa Base con Tiles
#'
#' Descarga un mapa base en formato raster (tiles) para un partido especificado.
#' Usa la extensión geográfica del partido como área de recorte. Ideal para usar como capa de fondo en visualizaciones con `ggplot2` o `terra`.
#'
#' @param part Nombre del partido a visualizar. Debe coincidir con la columna `partido` del objeto `geocovidapp::bsas_comunas`.
#' @param zoom Nivel de zoom (entero) para los tiles del mapa. Valores más altos generan más detalle.
#'
#' @return Un objeto `SpatRaster` que contiene los tiles del mapa base para la región especificada.
#' @export
#'
#' @note A la fecha de hoy es imposible usar otro método para obtener un mapa base que respete la extensión precisa del área de interés.
mapa_base_ggplot <- function(part, zoom) {

  shape_partido <- geocovidapp::bsas_comunas |> 
    dplyr::filter(partido == part) |>
    dplyr::pull(geometry)
  
  shape_partido <- terra::vect(shape_partido)
  
  map_ext <- terra::ext(shape_partido)
  
  map_tiles <- maptiles::get_tiles(x = map_ext, zoom = zoom, crop = T)
  
  return(map_tiles)

}
