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
#' @param mapa Mapa Leaflet al que se le añadirán los polígonos.
#' @param data Conjunto de datos de clase `sf` que contiene la geometría de los polígonos.
#' @param fillopacity_poly Valor numérico entre 0 y 1 que especifica la opacidad de relleno de los polígonos.
#'
#' @return El mapa Leaflet con la capa de polígonos añadida.
#' @export
#'
#' @examples
addPolygonsLayer <- function(mapa, data, fillopacity_poly) {
  mapa |>
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

#' Agregar leyenda al mapa raster
#'
#' @description
#' Esta función agrega una imagen raster al mapa interactivo utilizando `leaflet` y agrega una leyenda que muestra el porcentaje de cambio
#' en los valores del raster. La leyenda es configurada con colores binarios según los valores del raster y se posiciona en la esquina superior derecha.
#'
#' @param mapa Mapa Leaflet al que se le añadirán los polígonos.
#' @param imagen Objeto `raster` que representa la imagen raster a ser agregada al mapa.
#' @param opacidad Valor de opacidad que controla la transparencia de la imagen raster en el mapa.
#' @param pal Objeto de tipo `colorBin` o `colorNumeric` de `leaflet` que define la paleta de colores que se usará para la visualización de los valores en el raster.
#'
#' @return Un objeto `leaflet` con la imagen raster agregada y una leyenda que muestra el porcentaje de cambio.
#' 
#' @export
#'
#' @examples
#' # Ejemplo de uso:
#' # Supongamos que 'imagen' es un objeto raster previamente cargado y 'opacidad' es un valor entre 0 y 1.
#' # Definir la paleta de colores:
#' pal <- leaflet::colorBin(
#'   palette = c("#0000FF", "#0040FF", "#0080FF", "#00BFFF", "#00FFFF", "#FFFFFF", "#FFCC00", "#FF9900", "#FF6600", "#FF3300", "#FF0000"),
#'   bins = c(50, 40, 30, 20, 10, 1, -1, -10, -20, -30, -40, -50),
#'   na.color = "transparent"
#' )
#' mapa |> addRasterLegend(imagen = imagen, opacidad = 0.7, pal = pal)
addRasterLegend <- function(mapa, imagen, opacidad, paleta, etiquetas){

mapa |> 
leaflet::removeImage(layerId = "raster") |>
leaflet::addRasterImage(imagen,
                        colors = paleta,
                        opacity = opacidad,
                        group = "basic",
                        layerId = "raster",
                        project = FALSE
) |>
  leaflet::removeControl("legend") |> # Evita que se duplique la leyenda
  leaflet::addLegend(
    pal = paleta, values = terra::values(imagen),
    title = "Porcentaje de cambio",
    position = "topright",
    group = "basic",
    layerId = "legend",
    labFormat = function(type, cuts, p) {
      paste0(etiquetas)
    }
  )
}