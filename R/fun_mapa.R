# Creo una funcion para agregar el mapa base con relieve
addBasemapTiles <- function(map) {
  map |>
    leaflet::addProviderTiles("Esri.WorldImagery",
                              group = "Esri.WorldImagery",
                              layerId = 'esri',
                              options = leaflet::providerTileOptions(attribution = 'Powered by Esri')) |> 
    leaflet::groupOptions("polys", zoomLevels = 0:6) |> # Evita que el raster se vea cuando se visualiza el mapa de la provincia al principio
    leaflet::groupOptions("basic", zoomLevels = 7:20)
}

# Creo una funcion auxiliar para agregar poligonos
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


