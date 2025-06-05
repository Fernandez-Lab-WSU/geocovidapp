#' Interfaz de usuario: Gráficos de casos de covid por provincia y departamento
#'
#' @param id Module name
#' @import shiny
#' @return Dos gráficos de casos de COVID-19 en el tiempo.
#' @export
Dygraph_UI <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    fluidRow(
      shinycssloaders::withSpinner(
        dygraphs::dygraphOutput(ns("casos_prov"),
          width = 700,
          height = 120
        ),
        type = 2,
        color = "lightgrey",
        color.background = "white"
      ),
      shinycssloaders::withSpinner(
        dygraphs::dygraphOutput(ns("casos_dpto"),
          width = 700,
          height = 100
        ),
        type = 2,
        color = "lightgrey",
        color.background = "white"
      ),
      htmltools::tags$p(
        style = "font-size: 12px; color: #666; margin-top: 10px;",
        HTML("<br> a - Aislamiento por segmentacion geografica, <br> b - Aislamiento con reapertura progresiva, <br> c - Nueva normalidad.")
      )
    )
  )
}

#' Servidor: Gráficos de casos de covid por provincia y departamento
#'
#' @param id Module name
#' @param data_sisa Dataframe con los casos diarios reportados de COVID-19.
#' @param amba_reducido_names String. Vector con los nombres de los partidos
#' que conforman el AMBA.
#' @param base_raster Dataframe que lista todos los rasters y desagrega en
#' sus columnas características de interes, como si son rasters de
#' AMBA o Buenos Aires, si el cambio porcentual es semanal o prepandemia
#' o el momento del día que representan.
#' @param area El raster puede corresponder a Buenos Aires provincia o a AMBA.
#' @param part Partido de la provincia de Buenos Aires, seleccionado en otro
#' módulo.
#'
#' @return La fecha seleccionada por el usuario en el grafico de COVID-19
#' @export
Dygraph_Server <- function(id,
                           data_sisa,
                           amba_reducido_names,
                           base_raster,
                           area,
                           part) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      # Convierto los datos al formato que necesita dygraph
      data_xts <- reactive({
        data_xts_bsas <- convertir_xts_serie(
          serie = "Buenos Aires",
          nombre_serie = "BsAs"
        )

        data_xts_caba <- convertir_xts_serie(
          serie = "CABA",
          nombre_serie = "CABA"
        )


        # Creo una version que combine las series de caba y baires
        data_xts_combinado <- cbind(data_xts_caba, data_xts_bsas)
        data_xts_combinado$fecha_enfermo <- NULL # Remuevo las fechas duplicadas
        data_xts_combinado$fecha_enfermo.1 <- NULL


        # Si el usuario elige una comuna, no se va a visualizar una tercera linea
        # Ya que no hay muchos datos de covid reportados por comuna, consideramos CABA en general
        # Pero, se va apoder visualizar la comuna en el mapa si se quiere

        if (part() %in% amba_reducido_names &
          stringr::str_detect(part(),
            pattern = "^Comuna"
          ) | # si quiero visualizar la comuna
          dim(dplyr::filter(
            data_sisa,
            .data$residencia_departamento_nombre == part()
          ))[1] == 0) { # si no hay casos en el partido

          list(
            caba = data_xts_caba,
            bsas = data_xts_bsas,
            cabaybsas = data_xts_combined
          )
        } else {
          dpartido <- data_sisa |>
            dplyr::filter(.data$residencia_departamento_nombre == part()) |>
            dplyr::count(.data$fecha_enfermo) |>
            dplyr::filter(
              .data$fecha_enfermo >= base::min(as.Date(base_raster$fecha, origin = "1970-01-01")),
              .data$fecha_enfermo <= base::max(as.Date(base_raster$fecha, origin = "1970-01-01"))
            ) |>
            dplyr::mutate(fecha_enfermo = as.Date(.data$fecha_enfermo))

          # Que en la leyenda aparezca el nombre del partido
          base::colnames(dpartido)[2] <- as.character(part())

          data_xts_partido <- xts::xts(dpartido,
            order.by = dpartido$fecha_enfermo
          ) # serie temporal


          list(
            partido = data_xts_partido, # esto solo aparece si no quiero visualizar la comuna
            caba = data_xts_caba,
            bsas = data_xts_bsas,
            cabaybsas = data_xts_combinado
          )
        }
      })

      # Primer grafico: Casos por provincia y/o por CABA
      grafico_casos_prov <- reactive({
        if (part() %in% amba_reducido_names &
          stringr::str_detect(as.character(part()),
            pattern = "^Comuna"
          ) | # si quiero ver datos de comuna
          dim(dplyr::filter(
            data_sisa,
            .data$residencia_departamento_nombre == part()
          ))[1] == 0) { # si no hay casos en el partido

          data_plot <- data_xts()
          # Si el usuario elige una comuna, no se va a visualizar una tercera linea
          dygraphs::dygraph(data_plot$bsas,
            group = "A"
          ) |>
            dygraphs::dySeries("BsAs", color = "#186E8B") |>
            dygraphs_events()
        } else {
          data_plot <- data_xts()
          # Si no es un dato de comuna, que muestre tambien una linea para CABA
          dygraphs::dygraph(data_plot$cabaybsas,
            group = "A"
          ) |>
            dygraphs::dySeries("BsAs", color = "#186E8B") |>
            dygraphs::dySeries("CABA", color = "#301A4B") |>
            dygraphs_events()
        }
      })

      output$casos_prov <- dygraphs::renderDygraph({
        grafico_casos_prov()
      })

      # Segundo grafico: Casos por departamento
      grafico_casos_dpto <- reactive({
        if (as.character(area()) == "amba" &
          stringr::str_detect(as.character(part()),
            pattern = "^Comuna"
          ) || # si quiero ver datos de comuna
          dim(dplyr::filter(
            data_sisa,
            .data$residencia_departamento_nombre == part()
          ))[1] == 0) { # si no hay casos en el partido

          dygraphs::dygraph(data_xts()$caba,
            group = "A"
          ) |>
            dygraphs::dySeries("CABA",
              color = "#301A4B"
            ) |>
            dygraphs_events()
        } else {
          dygraphs::dygraph(data_xts()$partido,
            group = "A"
          ) |>
            dygraphs::dySeries(colnames(data_xts()$partido)[2], # nombre del partido
              color = "#6C9AC6"
            ) |>
            dygraphs_events()
        }
      })

      output$casos_dpto <- dygraphs::renderDygraph({
        grafico_casos_dpto()
      })

      return(
        list(
          casos_covid = reactive({
            input$casos_prov_click$x
          }),
          grafico_casos_prov = reactive({
            grafico_casos_prov()
          }),
          grafico_casos_dpto = reactive({
            grafico_casos_dpto()
          })
        )
      )
    }
  )
}

   