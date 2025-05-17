#' Title
#'
#' @param part 
#' @param zoom 
#'
#' @returns
#' @export
#'
#' @examples
#'
# a la fecha de hoy es imposible usar otro metodo
mapa_base_ggplot <- function(part, zoom) {

  shape_partido <- geocovidapp::bsas_comunas |> 
    dplyr::filter(partido == part) |>
    dplyr::pull(geometry)
  
  shape_partido <- terra::vect(shape_partido)
  
  map_ext <- terra::ext(shape_partido)
  
  map_tiles <- maptiles::get_tiles(x = map_ext, zoom = zoom, crop = T)
  
  return(map_tiles)

}
