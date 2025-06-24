#' IU: Histograma de la movilidad ciudadana por departamento
#'
#' @param id Module name
#' @return Histograma de la movilidad ciudadana por partido, respetando la
#' escala de colores del raster.
#' @export
HistogramaRaster_UI <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    tags$div(
      style = "padding:0px;margin-top:0px",
      shinycssloaders::withSpinner(
        shiny::plotOutput(ns("histograma"),
          width = 150,
          height = 150
        ),
        type = 2,
        color = "lightgrey",
        color.background = "white"
      )
    )
  )
}

#' Servidor: Histograma de la movilidad ciudadana por departamento
#'
#' @param id Identificador del módulo Shiny.
#' @param pool Objeto de conexión a base de datos (actualmente no utilizado directamente).
#' @param act_mapas Reactive que se activa para recalcular el histograma (trigger externo).
#' @param imagen Lista de objetos `SpatRaster`, uno por cada momento del día, usados para calcular el histograma.
#' @param amba_reducido_names Vector de strings. Nombres de los partidos que forman parte del AMBA.
#' @param bsas_comunas Objeto `sf` o `SpatialPolygonsDataFrame` simplificado con los polígonos de los partidos.
#' @param fecha Fecha seleccionada.
#' @param tipo_de_raster String. Tipo de raster: cambio porcentual semanal ("7dpc") o prepandemia ("pc").
#' @param partido String. Nombre del partido seleccionado.
#' @param momento_dia Entero. Momento del día representado (e.g. 0, 8, 16).
#'
#' @return Un histograma de la movilidad para el partido seleccionado, respetando los colores del raster.
#'
#' @importFrom graphics hist par
#' @export
HistogramaRaster_Server <- function(id,
                                    pool,
                                    act_mapas,
                                    imagen,
                                    amba_reducido_names,
                                    bsas_comunas, 
                                    fecha,
                                    tipo_de_raster,
                                    partido,
                                    momento_dia) {
  moduleServer(
    id,
    function(input, output, session) {
      # Evitamos conflicto con el nombre `imagen`
      imagen_momento <- reactive({
        req(imagen())
        imagen()[[momento_dia]]
      })
      
      
      raster_hist <- eventReactive(act_mapas(), ignoreNULL = TRUE, {
        

        if (partido() %in% amba_reducido_names) {
          # ver Partidos_Input.R
          amba <- dplyr::filter(
            bsas_comunas,
            partido %in% amba_reducido_names
          )

          # recorto por poligono
          poli <- sf::st_as_sf(subset(
            amba,
            partido == partido()
          ))

          # Me aseguro que el CRS sea el mismo para el poligono y el raster
          poli <- sf::st_transform(poli, terra::crs(imagen_momento()))

          imagen2 <- imagen_momento() |>
            terra::mask(poli) |>
            terra::crop(poli)

          # no quiero que considere valores por arriba de 50 o debajo de 50
          imagen2[imagen2 > 50] <- 50
          imagen2[imagen2 < -50] <- -50
          imagen2
        } else if (!(partido() %in% amba_reducido_names)) {
          # ver Patidos_Input.R
          prov <- dplyr::filter(
            bsas_comunas,
            !partido %in% amba_reducido_names
          )

          # recorto por poligono
          poli <- sf::st_as_sf(subset(
            prov,
            partido == partido()
          ))

          # Me aseguro que el CRS sea el mismo para el poligono y el raster
          poli <- sf::st_transform(poli, terra::crs(imagen_momento()))

          imagen2 <- imagen_momento() |>
            terra::mask(poli) |>
            terra::crop(poli)

          # No quiero que considere valores por arriba de 50 o debajo de 50
          imagen2[imagen2 > 50] <- 50
          imagen2[imagen2 < -50] <- -50
          imagen2
        }
      })



      output$histograma <- renderPlot({
        req(imagen_momento())
          validate(need(!is.null(raster_hist()), "No hay datos disponibles para la fecha seleccionada."))

          par(mar = c(4, 1, 3, 1)) # bottom, left, top, right

          hist(terra::values(raster_hist()),
            breaks = c(
              50, 40, 30,
              20, 10, 1, -1,
              -10, -20, -30,
              -40, -50
            ),
            main = NULL,
            col = c(
              "#0000FF", "#0040FF",
              "#0080FF", "#00BFFF",
              "#00FFFF", "#FFFFFF",
              "#FFCC00", "#FF9900",
              "#FF6600", "#FF3300",
              "#FF0000"
            ), # changes bin color
            xlim = c(50, -50),
            freq = FALSE,
            yaxs = "i",
            xaxs = "i",
            lty = "blank",
            # ylab = paste0("% de pixeles")
            yaxt = "n", ylab = "",
            xlab = paste("Movilidad", momento_dia)
          )
        },
        bg = "transparent"
      )
    }
  )
}