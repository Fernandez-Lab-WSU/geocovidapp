#' IU: Mapa raster de la provincia de Buenos Aires
#'
#' @description
#' Esta función crea la interfaz de usuario (UI) para el mapa interactivo
#' de la provincia de Buenos Aires, que se encuentra en el primer tab de la aplicación.
#' Utiliza el paquete `leaflet` para mostrar un mapa interactivo con un alto grado de interactividad,
#' permitiendo a los usuarios visualizar y explorar los datos geoespaciales de la región.
#'
#' @param id Nombre del módulo. Este parámetro es necesario para la correcta
#' integración del módulo UI dentro de la aplicación Shiny.
#'
#' @return Un objeto UI de Shiny que contiene un mapa interactivo de la provincia de Buenos Aires,
#' renderizado con `leaflet`. El mapa es capaz de manejar eventos de interacción como zoom y desplazamiento.
#'
#' @export
MapaBaires_UI <- function(id) {
  ns <- NS(id)

  shiny::tagList(
    shiny::fillPage(
      leaflet::leafletOutput(ns("tab1_leaflet_baires"),
        height = "90vh"
      ),
      BarraInferior_UI(ns('tab1_texto_info')),
      header = tags$style(HTML("
                                        .container-fluid{
                                          padding: 0px !important;
                                        }

                                        .navbar{
                                          margin-bottom: 0px !important;
                                        }"))
    )
  )
}


#' Servidor: Mapa raster de la provincia de Buenos Aires
#'
#' @description
#' Este módulo crea la lógica del servidor para renderizar un mapa interactivo de la provincia de Buenos Aires.
#' El mapa incluye un raster que representa la movilidad ciudadana y permite visualizar de forma dinámica
#' los cambios geoespaciales. Incluye también controles interactivos como escalas, medidas, coordenadas y leyendas.
#'
#' @param id Nombre del módulo. Necesario para la integración dentro de una aplicación Shiny.
#' @param imagen Reactive. Objeto `SpatRaster` que representa la movilidad ciudadana y se visualiza sobre el mapa.
#' @param area Reactive. String que indica el área geográfica de interés: puede ser `'baires'` (provincia) o `'amba'`.
#' @param opacidad Reactive. Valor numérico entre 0 y 1 que determina la transparencia del raster.
#' @param fecha Reactive. Fecha seleccionada por el usuario.
#' @param momento Reactive. Entero que representa el momento del día (e.g., 0, 8, 16 horas).
#' @param porcentaje Reactive. Tipo de porcentaje comparado: `"pc"` (prepandemia) o `"7dpc"` (últimos 7 días).
#' @param basemap Reactive. String que define el estilo del mapa base: `"calles"` o `"relieve"`.
#' @param boton Reactive. Trigger que se activa al presionar un botón de actualización.
#' @param amba_reducido_names Vector con los nombres de los partidos que conforman el AMBA (Área Metropolitana de Buenos Aires).
#'
#' @return Una lista con un único elemento:
#' \describe{
#'   \item{`mapa_zoom`}{Reactive que devuelve el nivel de zoom actual del mapa Leaflet.}
#' }
#'
#' @import leaflet
#' @export
MapaBaires_Server <- function(id, 
                              imagen, area,
                              opacidad,
                              fecha,
                              momento,
                              porcentaje,
                              basemap,
                              boton,
                              amba_reducido_names) {
  moduleServer(
    id,
    function(input, output, session) {
      # Paleta de color
      pal <- leaflet::colorBin(
        palette = c(
          "#0000FF", "#0040FF", "#0080FF",
          "#00BFFF", "#00FFFF",
          "#FFFFFF",
          "#FFCC00", "#FF9900",
          "#FF6600", "#FF3300", "#FF0000"
        ),
        bins = c(
          50, 40, 30, 20, 10, 1,
          -1, -10, -20, -30, -40, -50
        ),
        na.color = "transparent"
      )

      labels <- c(
        "Aumento más de 40",
        "40 - 30", "30 - 20", "20 - 10", "10 - 1",
        "Sin cambio",
        "-1 - -10", "-10 - -20", "-20 - -30", "-30 - -40",
        "Disminuyó bajo -40"
      )

      # Renderiza el mapa de la provincia de Buenos Aires
      output$tab1_leaflet_baires <- leaflet::renderLeaflet({
        
        base_map <- leaflet::leaflet() |>
          geocovidapp::addBasemapTiles() |>
          leaflet.extras::addResetMapButton() |>
          leaflet::addMeasure(
            position = "bottomleft",
            primaryLengthUnit = "kilometers",
            primaryAreaUnit = "hectares",
            localization = "es"
          ) |>
          leaflet.extras::addFullscreenControl() |>
          leaflet::addScaleBar(position = "topright") |>
          leafem::addMouseCoordinates() |>
          geocovidapp::addPolygonsLayer(geocovidapp::bsas, fillopacity_poly = 0.05) |>
          leaflet::setView(
            lat = ifelse(area() == "baires", -36.94, -34.72),
            lng = ifelse(area() == "baires", -63.94, -59.12),
            zoom = ifelse(area() == "baires", 6, 10)
          )
      })


      # Ahora observo cambios en la opacidad y mapa de base
      shiny::observe({
        leafprox <- leaflet::leafletProxy(
          mapId = "tab1_leaflet_baires",
          session = session
        )

        # Cambio el mapa de base
        if (basemap() == "calles") {
          lm <- leafprox |>
            leaflet::clearTiles() |>
            leaflet::addProviderTiles("OpenStreetMap",
              group = "OpenStreetMap",
              layerId = "open"
            ) |>
            addRasterLegend(imagen = imagen(),
                            opacidad = opacidad(),
                            pal = pal,
                            etiquetas = labels)

        } else if (basemap() == "relieve") {
          lm <- leafprox |>
            leaflet::clearTiles() |>
            addBasemapTiles() |>
            addRasterLegend(imagen = imagen(),
                            opacidad = opacidad(),
                            pal = pal,
                            etiquetas = labels)
        }
      })

      # Click en el mapa de la provincia de BsAs permite acercarme al mapa
      shiny::observeEvent(input$tab1_leaflet_baires_shape_click, {
        click <- input$tab1_leaflet_baires_shape_click

        leaflet::leafletProxy("tab1_leaflet_baires") |>
          leaflet::setView(
            lng = click$lng,
            lat = click$lat,
            zoom = 10
          )
      })


BarraInferior_Server('tab1_texto_info',
                     boton = boton,
                     area = area,
                     fecha = fecha,
                     momento = momento,
                     porcentaje = porcentaje )

      return(list(mapa_zoom = reactive({
        input$tab1_leaflet_baires_zoom
      })))
    }
  )
}