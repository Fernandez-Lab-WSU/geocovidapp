#' Genera un mapa interactivo de un partido con datos ráster
#'
#' @description Esta función crea un mapa interactivo utilizando `leaflet`, 
#' donde se visualiza un partido (unidad geopolítica) y un ráster superpuesto. 
#' Se utiliza una paleta de colores para representar los valores del ráster.
#'
#' @param partido Un objeto `sf` que representa los límites geográficos del partido.
#' @param raster Un objeto ráster que contiene los valores a visualizar en el mapa.
#' @param opacidad Un valor numérico entre 0 y 1 que define la opacidad de la capa ráster.
#'
#' @return Un objeto `leaflet` con el mapa interactivo.
#' @export
#'
#' @examples
#' \dontrun{
#' mapa <- mapa_partido(partido = mi_partido_sf, 
#'                      raster = mi_raster, 
#'                      opacidad = 0.5)
#' mapa
#' }
mapa_partido <- function(partido,
                         raster,
                         opacidad){
  
  
  
  pal <- leaflet::colorBin(palette = c("#0000FF", "#0040FF",
                                       "#0080FF", "#00BFFF",
                                       "#00FFFF", "#FFFFFF",
                                       "#FFCC00", "#FF9900",
                                       "#FF6600", "#FF3300",
                                       "#FF0000"),
                           bins = c(50, 40, 30 , 20 , 10, 1, -1,
                                    -10, -20, -30, -40, -50),
                           na.color = "transparent")
  
  bbx <- sf::st_bbox(partido)
  
  # mapa mañana
  leaf_map <- leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = FALSE)) |>
    leaflet::addTiles() |>
    leaflet::addRasterImage(shiny::isolate(raster),
                            colors = pal,
                            opacity = shiny::isolate(opacidad),
                            group = "basic",
                            layerId = "raster",
                            project = FALSE) |> # mejora performance evitando que el mapa se reproyecte
    leaflet::addPolygons(data = shiny::isolate(partido)[, 'geom'],
                         label = shiny::isolate(partido)[, 'partido'],
                         layerId = shiny::isolate(partido)[, 'partido'],
                         color = "black",
                         fillColor = "transparent",
                         weight = 1,
                         stroke = TRUE,
                         fillOpacity = 0.1,
                         smoothFactor = 0.5,
                         group = "basic") |>
    leaflet::fitBounds(lng1 = bbx$xmin[[1]],
                       lat1= bbx$ymin[[1]],
                       lng2 = bbx$xmax[[1]],
                       lat2 = bbx$ymax[[1]])
  
  
  return(leaf_map)
}