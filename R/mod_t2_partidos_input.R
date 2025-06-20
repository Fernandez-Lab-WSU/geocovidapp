#' IU: Seleccion de departamentos de provincia de Buenos Aires con IU
#' dinámica.
#'
#' @param id Module name
#' @param amba_reducido_names String. Vector con los nombres de los partidos
#' que conforman el AMBA.
#' @param base_raster Dataframe que lista todos los rasters y desagrega en
#' sus columnas características de interes, como si son rasters de
#' AMBA o Buenos Aires, si el cambio porcentual es semanal o prepandemia
#' o el momento del día que representan.
#'
#' @return Elementos de la IU para que se seleccione partidos de AMBA o de
#' BsAs de forma condicional.
#' @export
Partidos_UI <- function(id, amba_reducido_names, base_raster) {
  ns <- NS(id)

  shiny::tagList(
    tags$div( 
      fluidRow(
        shiny::column(
          6,
          shiny::radioButtons(ns("area"),
            label = "Selecciona el area",
            choices = c(
              "prov. de Buenos Aires" = "baires",
              "AMBA" = "amba"
            ),
            selected = "amba"
          )
        ),
        shiny::column(
          6,
          shiny::selectInput(ns("partidos"),
            label = "Selecciona el partido",
            choices = amba_reducido_names,
            selected = amba_reducido_names[1],
            width = "75%"
          )
        ),
        shiny::column(
          6,
          shiny::radioButtons(ns("porcentaje"),
                              label = "Cambio porcentual",
                              choices = c(
                                "Prepandemia" = "pc",
                                "Semanal" = "7dpc"
                              ),
                              selected = "7dpc")
        ),
        shiny::column(
          6,
          shiny::sliderInput(ns("opacity"),
                             label = "Transparencia",
                             min = 0,
                             max = 1,
                             value = 0.5,
                             width = "75%",
                             ticks = FALSE)
        )
      )
    )
  )
}

#' Servidor: Seleccion de departamentos de provincia de Buenos Aires con IU
#' dinámica.
#'
#' @param id Module name
#' @param bsas Dataset de clase sf con los partidos de Buenos Aires.
#' @param amba_reducido_names String. Vector con los nombres de los partidos
#' que conforman el AMBA.
#'
#' @return El area y el partido seleccionado.
#' @export
Partidos_Server <- function(id,
                            bsas,
                            amba_reducido_names) {
  moduleServer(
    id,
    function(input, output, session) {
  
      choices <- reactive({
        req(input$area)
        
        if (input$area == "amba") {
          sort(amba_reducido_names)
        } else if (input$area == "baires") {
          prov <- dplyr::filter(
            geocovidapp::bsas,
            !partido %in% amba_reducido_names
          )
          sort(unique(prov$partido))
        }
      })
      

      # Actualiza opciones de partidos en base a la eleccion de area
      shiny::observe({
        shiny::updateSelectInput(session,
          inputId = "partidos",
          choices = choices(),
          selected = choices()[1]
        )
      })


      return(
        list(
          area = reactive({
            input$area
          }),
          partido = reactive({
            input$partidos
          }),
          porcentaje = reactive({
            input$porcentaje
          }),
          opacidad = reactive({
            input$opacity
          })
        )
      )
    }
  )
}