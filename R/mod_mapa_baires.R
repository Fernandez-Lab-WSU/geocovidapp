#' IU: Mapa raster de la provincia de Buenos Aires
#' 
#' @description
#' Este mapa se encuentra el tab 1 correspondiente al 
#' Mapa provincia de Buenos Aires. 
#'
#' @param id Module name
#'
#' @return posición del mapa leaflet de Buenos Aires con proxy.
#' También retorna el valor de zoom a medida que cambia durante la navegación.
#' @export
MapaBaires_UI <- function(id) {
  ns <- NS(id)

  shiny::tagList(
     shiny::fillPage(

    leaflet::leafletOutput(ns("inter_mapa"),
                            height="90vh"
                                        ),
                            header=tags$style(HTML("
                                        .container-fluid{
                                          padding: 0px !important;
                                        }

                                        .navbar{
                                          margin-bottom: 0px !important;
                                        }"))
))

}


#' Servidor: Mapa raster de la provincia de Buenos Aires
#' 
#' @description
#' Este mapa se encuentra el tab 1 correspondiente al 
#' Mapa provincia de Buenos Aires. 
#'
#'
#' @param id Module name
#' @param bsas Dataset de clase sf con los partidos de Buenos Aires.
#' @param imagen Imagen raster representando la movilidad ciudadana.
#' @param area El raster puede corresponder a Buenos Aires provincia o a AMBA.
#' @param opacidad Double. Valor de opacidad del raster.
#' @param basemap Estilo de mapa Leaflet. Puede mostrar las calles o el relieve.
#' @param amba_reducido_names String. Vector con los nombres de los partidos 
#' que conforman el AMBA.
#'
#' @return Mapa leaflet de Buenos Aires con proxy. También retorna el valor de
#' zoom a medida que cambia durante la navegación.
#' @export
MapaBaires_Server <- function(id, bsas,
                              imagen, area,
                              opacidad,
                              basemap,
                              amba_reducido_names){
  moduleServer(id,
  function(input, output, session) {


    sf::st_geometry(bsas) <- "geom"
    amba <- bsas[bsas$partido %in% amba_reducido_names, ]
  
    amba_bbox <- sf::st_bbox(amba)

    output$inter_mapa <- leaflet::renderLeaflet({

    pal <- leaflet::colorBin(palette = c("#0000FF", "#0040FF",
                                         "#0080FF", "#00BFFF",
                                         "#00FFFF", "#FFFFFF",
                                         "#FFCC00", "#FF9900",
                                         "#FF6600", "#FF3300",
                                         "#FF0000"),
                             bins = c(50, 40,
                                      30, 20,
                                      10, 1,
                                      -1, -10,
                                      -20, -30,
                                      -40, -50), 
                             na.color = "transparent")

    labels <- c("Aumento más de 40", "40 - 30",
                "30 - 20", "20 - 10",
                "10 - 1",
                "Sin cambio",
                "-1 - -10",
                "-10 - -20", "-20 - -30",
                "-30 - -40", "Dismunuyo bajo -40")

   #https://leaflet-extras.github.io/leaflet-providers/preview/


  if(area() == 'amba'){

  leaflet::leaflet() |>
  leaflet::addProviderTiles(
           "Esri.WorldImagery",
           group = "Esri.WorldImagery",
           layerId = 'esri',
           options = leaflet::providerTileOptions(attribution = paste('Tiles',
          '&copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX,',
          'GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User',
          'Community - Powered by Esri'))) |>
        leaflet::groupOptions("polys",
                              zoomLevels = 0:6) |>
        leaflet::groupOptions("basic",
                              zoomLevels = 7:20) |>
        leaflet::addPolygons(data = bsas,
                    layerId = bsas$partido, # esto fue importante agregarlo
                    label = bsas$partido,
                    color = "black",
                    fillColor = "blue",
                    weight = 1,
                    stroke = TRUE,
                    fillOpacity = 0.1,
                    smoothFactor = 0.5,
                    group = "polys"
        ) |>
        leaflet.extras::addResetMapButton() |>
        leaflet::addMeasure(position = "bottomleft",
                   primaryLengthUnit = "kilometers",
                   primaryAreaUnit = "hectares",
                   localization = "es",
                   activeColor = "#3D535D",
                   completedColor = "#7D4479")|>

        leaflet.extras::addFullscreenControl() |>
        leaflet::addScaleBar(options = leaflet::scaleBarOptions(imperial = FALSE),
                             position = c("topright")) |>
        leafem::addMouseCoordinates() |>
        leaflet::setView( lat=-34.72,
                 lng=-59.12,
                 zoom=10)

    }else if(area() == 'baires'){
print(imagen())
      leaflet::leaflet() |>
        leaflet::addProviderTiles("Esri.WorldImagery",
                         group = "Esri.WorldImagery",
                         layerId = 'esri',
                         options = leaflet::providerTileOptions(
                                  attribution = 'Powered by Esri')) |>
        leaflet::groupOptions("polys",
                              zoomLevels = 0:6) |>
        leaflet::groupOptions("basic",
                              zoomLevels = 7:20) |>
        leaflet::addPolygons(data = bsas,
                    layerId = bsas$partido, # esto fue importante agregarlo
                    label = bsas$partido,
                    color = "black",
                    fillColor = "blue",
                    weight = 1,
                    stroke = TRUE,
                    fillOpacity = 0.1,
                    smoothFactor = 0.5,
                    group = "polys"
        ) |>
        leaflet.extras::addResetMapButton() |>
        leaflet::addMeasure(position = "bottomleft",
                   primaryLengthUnit = "kilometers",
                   primaryAreaUnit = "hectares",
                   localization = "es",
                   activeColor = "#3D535D",
                   completedColor = "#7D4479")|>
        leaflet.extras::addFullscreenControl() |>
        leaflet::addScaleBar(options = leaflet::scaleBarOptions(imperial = FALSE),
                             position = c("topright")) |>
        leafem::addMouseCoordinates() |>
        leaflet::setView( lat=-36.94,
                 lng=-63.94 ,
                 zoom=6)
    }
 })



  # actualizo el mapa cuando la persona elige la opacidad
  # https://stackoverflow.com/questions/28393310/how-to-prevent-leaflet-map-from-resetting-zoom-in-shiny-app

  shiny::observe({
    
    print(imagen())

    pal <- leaflet::colorBin(palette = c("#0000FF", "#0040FF",
                                         "#0080FF", "#00BFFF",
                                         "#00FFFF", "#FFFFFF",
                                         "#FFCC00", "#FF9900",
                                         "#FF6600", "#FF3300",
                                         "#FF0000"),
                             bins = c(50,40, 30 , 20 , 10, 1, -1,
                                      -10, -20, -30,-40, -50), 
                             na.color = "transparent")

    labels <- c("Aumento más de 40", "40 - 30",
                "30 - 20", "20 - 10", "10 - 1",
                "Sin cambio",
                "-1 - -10", "-10 - -20", "-20 - -30",
                "-30 - -40", "Dismunuyo bajo -40")

if(basemap() == 'calles'){
    # Agregar la imagen de nuevo con la nueva opacidad
   leafprox <-  leaflet::leafletProxy(mapId ="inter_mapa",
                 session = session) |>
     leaflet::removeTiles(layerId = 'esri') |>
     leaflet::addProviderTiles("OpenStreetMap",
              group = "OpenStreetMap",
              layerId = 'open',
              options = leaflet::providerTileOptions(attribution = 'Powered by OpenStreetMaps')) |>
     leaflet::addPolygons(data = bsas,
                  layerId = bsas$partido, # esto fue importante agregarlo
                  label = bsas$partido,
                  color = "black",
                  fillColor = "blue",
                  weight = 1,
                  stroke = TRUE,
                  fillOpacity = 0.1,
                  smoothFactor = 0.5,
                  group = "polys"
      ) |>
     leaflet::addRasterImage(imagen(),
                    colors = pal,
                    opacity = opacidad(),
                    group = "basic",
                    layerId = "raster",
                    project = FALSE) |> # evita que el mapa se reproyecte / mejora de performance
     leaflet::addLegend(pal = pal,
                values = terra::values(imagen()),
                title = "Porcentaje de cambio",
                position = "topright",
                group = "basic",
                layerId = "raster2",
                labFormat = function(type, cuts, p) {  # Here's the trick
                  paste0(labels)
                }) |>
     leaflet::addPolygons(data = bsas,
                  layerId = abbreviate(bsas$partido),
                  color = "black",
                  fillColor = "transparent",
                  weight = 1,
                  stroke = TRUE,
                  fillOpacity = 0.1,
                  smoothFactor = 0.5,
                  group = "basic") }else{


                    leafprox <-  leaflet::leafletProxy(mapId ="inter_mapa",
                                              session = session) |>
                      leaflet::addPolygons(data = bsas,
                                  layerId = bsas$partido, # esto fue importante agregarlo
                                  label = bsas$partido,
                                  color = "black",
                                  fillColor = "blue",
                                  weight = 1,
                                  stroke = TRUE,
                                  fillOpacity = 0.1,
                                  smoothFactor = 0.5,
                                  group = "polys"
                      ) |>
                      leaflet::addRasterImage(imagen(),
                                     colors = pal,
                                     opacity = opacidad(),
                                     group = "basic",
                                     layerId = "raster") |>
                      leaflet::addLegend(pal = pal,
                                values = terra::values(imagen()),
                                title = "Porcentaje de cambio",
                                position = "topright",
                                group = "basic",
                                layerId = "raster2",
                                labFormat = function(type, cuts, p) {  
                                  paste0(labels)
                                }) |>
                      leaflet::addPolygons(data = bsas,
                                  layerId = abbreviate(bsas$partido),
                                  color = "black",
                                  fillColor = "transparent",
                                  weight = 1,
                                  stroke = TRUE,
                                  fillOpacity = 0.1,
                                  smoothFactor = 0.5,
                                  group = "basic")

                  }

   leafprox
     })


  # actualizo el mapa con el click sin tener que cargarlo de cero devuelta.
  shiny::observeEvent(input$inter_mapa_shape_click, {
    click <- input$inter_mapa_shape_click

    mapa_proxy <- leaflet::leafletProxy(mapId ="inter_mapa") |>
      leaflet::setView(lng = click$lng,
              lat = click$lat,
              zoom = 11)

     mapa_proxy
  })

  return(
    list(
      mapa_zoom = reactive({ input$inter_mapa_zoom })
    )
  )

})}