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

#' Servidor: Mapa raster por partido
#'
#' @description
#' Este mapa se encuentra en el tab 2, "por partido" de GeoCovid app
#'
#' @param id Module name
#' @param amba_reducido_names String. Vector con los nombres de los partidos
#' que conforman el AMBA.
#' @param base_raster Dataframe que lista todos los rasters y desagrega en
#' sus columnas características de interes, como si son rasters de
#' AMBA o Buenos Aires, si el cambio porcentual es semanal o prepandemia
#' o el momento del día que representan.
#' @param fecha Fecha seleccionada.
#' @param bsas Dataset de clase sf con los partidos de Buenos Aires.
#' @param area El raster puede corresponder a Buenos Aires provincia o a AMBA.
#' @param part Partido de la provincia de Buenos Aires, seleccionado en otro
#' módulo.
#' @param momento_dia Entero. Hora del dia representada en el raster, puede ser
#' 0am, 8 am o 4 pm.
#' @param tipo_de_raster String. Si el raster corresponde a el cambio porcentual
#' prepandemia (pc) o semanal (7dpc).
#' @param opacidad Double. Valor de opacidad del raster.
#'
#' @return Mapa raster por partido creado con leaflet.
#' @export
MapaPartido_Server <- function(id,
                               pool,
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
            50, 40, 30, 20, 10, 1, -1,
            -10, -20, -30, -40, -50
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