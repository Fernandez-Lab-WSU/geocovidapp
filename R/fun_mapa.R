#' Agrega un mapa base con relieve de Esri
#'
#' @description
#' Agrega un mapa base de relieve (Esri World Imagery) al mapa Leaflet,
#' utilizado por defecto en el mapa de la provincia de Buenos Aires.
#'
#' @param map Mapa Leaflet al que se le añadirá el mapa base con relieve.
#' 
#' @return El mapa Leaflet con el mapa base añadido.
#' @export
#'
#' @examples
addBasemapTiles <- function(map) {
  map |>
    leaflet::addProviderTiles("Esri.WorldImagery",
                              group = "Esri.WorldImagery",
                              layerId = 'esri',
                              options = leaflet::providerTileOptions(attribution = 'Powered by Esri')) |> 
    leaflet::groupOptions("polys", zoomLevels = 0:6) |> # Evita que el raster se vea cuando se visualiza el mapa de la provincia al principio
    leaflet::groupOptions("basic", zoomLevels = 7:20)
}

#' Agrega polígonos de un conjunto de datos al mapa Leaflet
#'
#' Esta función agrega una capa de polígonos representando los partidos de la provincia de Buenos Aires
#' al mapa Leaflet. Cada polígono tiene un estilo de borde y relleno configurable.
#'
#' @param map Mapa Leaflet al que se le añadirán los polígonos.
#' @param data Conjunto de datos de clase `sf` que contiene la geometría de los polígonos.
#' @param fillopacity_poly Valor numérico entre 0 y 1 que especifica la opacidad de relleno de los polígonos.
#'
#' @return El mapa Leaflet con la capa de polígonos añadida.
#' @export
#'
#' @examples
addPolygonsLayer <- function(map, data, fillopacity_poly) {
  map |>
    leaflet::addPolygons(
      data = data,
      layerId = data$partido,
      label = data$partido,
      color = "black",
      fillColor = "blue",
      weight = 1,
      stroke = TRUE,
      fillOpacity = fillopacity_poly,
      smoothFactor = 0.5                     )
}


