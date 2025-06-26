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
          width = 800,
          height = 120
        ),
        type = 2,
        color = "lightgrey",
        color.background = "white"
      ),
      shinycssloaders::withSpinner(
        dygraphs::dygraphOutput(ns("casos_dpto"),
          width = 800,
          height = 100
        ),
        type = 2,
        color = "lightgrey",
        color.background = "white"
      ),
      htmltools::tags$p(
        style = "font-size: 12px; color: #666; margin-top: 10px;",
        HTML("<br> a - Aislamiento con reapertura progresiva, <br> b - Nueva normalidad.")
      )
    )
  )
}

#' Servidor: Gráficos de casos de COVID por provincia y departamento
#'
#' @param id Módulo Shiny (string) usado como identificador del namespace.
#' @param amba_reducido_names Character vector con los nombres de los partidos
#' que conforman el AMBA.
#' @param area Reactive que indica si el raster corresponde a Buenos Aires provincia o a AMBA.
#' @param partido Reactive que devuelve el nombre del partido de la provincia de Buenos Aires,
#' seleccionado en otro módulo.
#'
#' @return Una lista con tres reactives: 
#' \describe{
#'   \item{casos_covid}{La fecha seleccionada por el usuario en el gráfico de COVID-19.}
#'   \item{grafico_casos_prov}{Reactive con el gráfico de casos por provincia o CABA.}
#'   \item{grafico_casos_dpto}{Reactive con el gráfico de casos por departamento o comuna.}
#' }
#' @export
Dygraph_Server <- function(id,
                           amba_reducido_names,
                           area,
                           partido) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      # Convierto los datos al formato que necesita dygraph
      data_xts <- reactive({
        req(partido())
       
        
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

        if (partido() %in% amba_reducido_names &
          stringr::str_detect(partido(),
            pattern = "^Comuna"
          ) | # si quiero visualizar la comuna
          dim(dplyr::filter(
            data_sisa,
            .data$residencia_departamento_nombre == partido()
          ))[1] == 0) { # si no hay casos en el partido

          list(
            caba = data_xts_caba,
            bsas = data_xts_bsas,
            cabaybsas = data_xts_combinado
          )
        } else {
          dpartido <- data_sisa |>
            dplyr::filter(.data$residencia_departamento_nombre == partido()) |>
            dplyr::count(.data$fecha_enfermo) |>
            dplyr::filter(
              .data$fecha_enfermo >= base::min(as.Date(geocovidapp::base_raster$fecha, origin = "1970-01-01")),
              .data$fecha_enfermo <= base::max(as.Date(geocovidapp::base_raster$fecha, origin = "1970-01-01"))
            ) |>
            dplyr::mutate(fecha_enfermo = as.Date(.data$fecha_enfermo))

          # Que en la leyenda aparezca el nombre del partido
          base::colnames(dpartido)[2] <- as.character(partido())

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
        if (partido() %in% amba_reducido_names &
          stringr::str_detect(as.character(partido()),
            pattern = "^Comuna"
          ) | # si quiero ver datos de comuna
          dim(dplyr::filter(
            geocovidapp::data_sisa,
            .data$residencia_departamento_nombre == partido()
          ))[1] == 0) { # si no hay casos en el partido

          data_plot <- data_xts()
          # Si el usuario elige una comuna, no se va a visualizar una tercera linea
          dygraphs::dygraph(data_plot$bsas,
            group = "A"
          ) |>
            dygraphs::dySeries("BsAs", color = "#186E8B") |>
            geocovidapp::dygraphs_events()
        } else {
          data_plot <- data_xts()
          # Si no es un dato de comuna, que muestre tambien una linea para CABA
          dygraphs::dygraph(data_plot$cabaybsas,
            group = "A"
          ) |>
            dygraphs::dySeries("BsAs", color = "#186E8B") |>
            dygraphs::dySeries("CABA", color = "#301A4B") |>
            geocovidapp::dygraphs_events()
        }
      })

      output$casos_prov <- dygraphs::renderDygraph({
        grafico_casos_prov()
      })

      # Segundo grafico: Casos por departamento
      grafico_casos_dpto <- reactive({
        if (as.character(area()) == "amba" &
          stringr::str_detect(as.character(partido()),
            pattern = "^Comuna"
          ) || # si quiero ver datos de comuna
          dim(dplyr::filter(
            data_sisa,
            .data$residencia_departamento_nombre == partido()
          ))[1] == 0) { # si no hay casos en el partido

          dygraphs::dygraph(data_xts()$caba,
            group = "A"
          ) |>
            dygraphs::dySeries("CABA",
              color = "#301A4B"
            ) |>
            geocovidapp::dygraphs_events()
        } else {
          dygraphs::dygraph(data_xts()$partido,
            group = "A"
          ) |>
            dygraphs::dySeries(colnames(data_xts()$partido)[2], # nombre del partido
              color = "#6C9AC6"
            ) |>
            geocovidapp::dygraphs_events()
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

   