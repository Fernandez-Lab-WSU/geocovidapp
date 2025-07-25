#' IU: Mapa raster por partido
#'
#' @description
#' Este mapa se encuentra en el tab 2, "por partido" de GeoCovid app
#'
#' @param id Module name
#'
#' @return Mapa leaflet en la IU
#' @export
MapaPartido_UI <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shinycssloaders::withSpinner(
      leaflet::leafletOutput(ns("mapa_partido"),
        height = 350
      ),
      type = 2,
      color = "lightgrey",
      color.background = "white"
    )
  )
}

#' Server: Módulo de mapa raster por partido
#'
#' @description
#' Lógica del servidor para el módulo que muestra un mapa raster por partido.
#' Este módulo corresponde a la pestaña 2, "Por partido", de la aplicación GeoCovid.
#'
#' @param id `character`. Identificador del módulo Shiny.
#' @param act_mapas `reactive`. Trigger reactivo para actualizar el mapa.
#' @param amba_reducido_names `character`. Vector de nombres de partidos que componen el AMBA.
#' @param imagen `reactive`. Lista de rasters.
#' @param bsas_comunas `sf`. Dataset de clase `sf` con los polígonos de comunas de Buenos Aires.
#' @param area `character`. Indica si el raster corresponde a Buenos Aires o AMBA.
#' @param fecha `Date` o `reactive`. Fecha seleccionada.
#' @param tipo_de_raster `character`. Define si el raster es de cambio porcentual prepandemia (`pc`) o semanal (`7dpc`).
#' @param opacidad `numeric`. Valor de opacidad del raster.
#' @param partido `reactive`. Partido seleccionado (provincia de Buenos Aires).
#' @param momento_dia `numeric`. Hora del día representada: 0, 8 o 16 horas.
#'
#' @return Una lista de reactivos con el raster seleccionado (`mapa_partido`) y el nivel de zoom (`zoom_mapa_partido`).
#' @export
MapaPartido_Server <- function(id,
                               act_mapas,
                               amba_reducido_names,
                               imagen,
                               bsas_comunas,
                               area,
                               fecha,
                               tipo_de_raster,
                               opacidad, partido,
                               momento_dia) {
  moduleServer(
    id,
    session = getDefaultReactiveDomain(),
    function(input, output, session) {
      
      # Accedés al raster según el momento indicado
      imagen_momento <- reactive({
        req(imagen())
        imagen()[[momento_dia]]
      })

      filter_partido <- eventReactive(act_mapas(), ignoreNULL = TRUE, {
    
        
        if (partido() %in% amba_reducido_names) {
          # ver Partidos_Input.R
          amba <- dplyr::filter(
            bsas_comunas,
            .data$partido %in% amba_reducido_names
          )

          sf::st_as_sf(base::subset(
            amba,
            partido == partido()
          ))
        } else if (!(partido() %in% amba_reducido_names)) { # baires
       
          # ver Partidos_Input.R
          prov <- dplyr::filter(
            bsas,
            !.data$partido %in% amba_reducido_names
          )

          # recorto por poligono
          sf::st_as_sf(base::subset(
            prov,
            partido == partido()
          ))
        }
      })


      # Hago esto porque necesito sacar el mapa para el reporte
      mapa_leaflet <- reactive({
        req(imagen_momento())

        mapa_partido(
          partido = filter_partido(),
          raster = imagen_momento(),
          opacidad = opacidad()
        )
      })

      output$mapa_partido <- leaflet::renderLeaflet({
        # Mensaje si no existen rasters disponibles
        if (is.null(imagen_momento())) {
          leaflet::leaflet() |>
            leaflet::addTiles() |>
            leaflet::addPopups(
              lng = -58.5, lat = -34.6,
              popup = "No hay datos disponibles para la fecha seleccionada."
            )
        } else {
          mapa_leaflet()
        }
      })


      shiny::observe({
        req(mapa_leaflet())

        pal <- leaflet::colorBin(
          palette = c(
            "#0000FF", "#0040FF",
            "#0080FF", "#00BFFF",
            "#00FFFF", "#FFFFFF",
            "#FFCC00", "#FF9900",
            "#FF6600", "#FF3300",
            "#FF0000"
          ),
          bins = c(
            Inf, 40, 30, 20, 10, 1, -1,
            -10, -20, -30, -40, -Inf
          ),
          na.color = "transparent"
        )


        leaflet::leafletProxy("mapa_partido") |>
          leaflet::addRasterImage(imagen_momento(),
            colors = pal,
            opacity = opacidad(),
            group = "basic",
            layerId = "raster"
          ) |>
          leaflet::clearShapes() |>
          leaflet::addPolygons(
            data = filter_partido()[, "geom"],
            label = filter_partido()[, "partido"],
            layerId = filter_partido()[, "partido"],
            color = "black",
            fillColor = "transparent",
            weight = 1,
            stroke = TRUE,
            fillOpacity = 0.1,
            smoothFactor = 0.5,
            group = "basic"
          ) |>
          leaflet::fitBounds(
            lng1 = filter_partido()$lng1,
            lat1 = filter_partido()$lat1,
            lng2 = filter_partido()$lng2,
            lat2 = filter_partido()$lat2
          )
      })

      return(list(
        mapa_partido = reactive({ # Esto va al reporte
          imagen_momento() # Estoy devolviendo el raster seleccionado, NO el mapa de leaflet
        }),
        zoom_mapa_partido = reactive({
          input$mapa_partido_zoom
        })
      ))
    }
  )
}