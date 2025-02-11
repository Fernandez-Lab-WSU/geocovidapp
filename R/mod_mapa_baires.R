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
                 
                 # Paleta de color
                 pal <- leaflet::colorBin(
                   palette = c("#0000FF", "#0040FF", "#0080FF", "#00BFFF", "#00FFFF", 
                               "#FFFFFF",
                               "#FFCC00", "#FF9900", "#FF6600", "#FF3300", "#FF0000"),
                   bins = c(50, 40, 30, 20, 10, 1, -1, -10, -20, -30, -40, -50),
                   na.color = "transparent"
                 )
                 
                 labels <- c("Aumento más de 40", 
                             "40 - 30", "30 - 20", "20 - 10", "10 - 1",
                             "Sin cambio",
                             "-1 - -10", "-10 - -20", "-20 - -30", "-30 - -40", 
                             "Disminuyó bajo -40")
                 
                 # Selecciono los partidos de AMBA
                 amba <- bsas[bsas$partido %in% amba_reducido_names, ]
                 amba_bbox <- sf::st_bbox(amba)
                 

                 output$inter_mapa <- leaflet::renderLeaflet({
                   base_map <- leaflet::leaflet() |>
                     addBasemapTiles() |>
                     leaflet.extras::addResetMapButton() |>
                     leaflet::addMeasure(position = "bottomleft",
                                         primaryLengthUnit = "kilometers",
                                         primaryAreaUnit = "hectares",
                                         localization = "es") |>
                     leaflet.extras::addFullscreenControl() |>
                     leaflet::addScaleBar(position = "topright") |>
                     leafem::addMouseCoordinates()
                   
                   # El zoom va a ser diferente al inicio para AMBA y provincia.
                   # De todas formas es codigo innecesario,  ya que solo corre
                   # Baires porque es la opcion definida por defaukt
                   # Lo dejo, por si en algun momento se quiere cambiar esto
                   if (area() == 'amba') {
                     base_map |>
                       addPolygonsLayer(bsas, 
                                        fillopacity_poly = 0) |>
                       leaflet::setView(lat = -34.72, lng = -59.12, zoom = 10)
                   } else {
                     base_map |>
                       addPolygonsLayer(bsas,
                                        fillopacity_poly = 0.1) |>
                       leaflet::setView(lat = -36.94, lng = -63.94, zoom = 6)
                   }
                 })
                 
                 # Ahora observo cambios en la opacidad y mapa de base
                 shiny::observe({
                   leafprox <- leaflet::leafletProxy(mapId = "inter_mapa",
                                                     session = session)
                   
                   
                   if (basemap() == 'calles') {
                     leafprox |>
                       leaflet::clearTiles() |>
                       leaflet::addProviderTiles("OpenStreetMap",
                                                 group = "OpenStreetMap",
                                                 layerId = 'open')
                   }
                   
                   # si cambio imagen() o opacidad() el mapa se regenera
                   leafprox |>
                     leaflet::clearShapes() |> 
                     addPolygonsLayer(bsas,
                                      fillopacity_poly = 0.1) |>
                     leaflet::addRasterImage(imagen(), 
                                             colors = pal,
                                             opacity = opacidad(),
                                             group = "basic", 
                                             layerId = "raster",
                                             project = FALSE) |>
                     leaflet::removeControl("legend") |>  # Evita que se duplique la leyenda
                     leaflet::addLegend(
                       pal = pal, values = terra::values(imagen()),
                       title = "Porcentaje de cambio",
                       position = "topright",
                       group = "basic", 
                       layerId = "legend",
                       labFormat = function(type, cuts, p) { paste0(labels) }
                     )
                     
                 })
                 
                 # Click en el mapa de la provincia de BsAs permite acercarme al mapa
                 shiny::observeEvent(input$inter_mapa_shape_click, {
                   click <- input$inter_mapa_shape_click
                   leaflet::leafletProxy("inter_mapa") |>
                     leaflet::setView(lng = click$lng, 
                                      lat = click$lat,
                                      zoom = 10)
                 })
                 
                 return(list(mapa_zoom = reactive({ input$inter_mapa_zoom })))
               })
}
