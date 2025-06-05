#' Title
#'
#' @param id 
#'
#' @export
selectormapaUI <- function(id) {
  ns <- NS(id)
  tagList(
    geocovidapp::Partidos_UI( # mod_t2_partidos_input.R
      "seleccion_dinamica",
      amba_reducido_names,
      base_raster
    ),
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::radioButtons("porcentaje2",
                            label = "Cambio porcentual",
                            choices = c(
                              "Prepandemia" = "pc",
                              "Semanal" = "7dpc"
                            ),
                            selected = "7dpc")
      ),
      shiny::column(
        6,
        shiny::sliderInput("opacity2",
                           label = "Transparencia",
                           min = 0,
                           max = 1,
                           value = 0.5,
                           width = "75%",
                           ticks = FALSE)
      ),
      shiny::hr(),
      shiny::actionButton(
        "act_mapas",
        "Actualizar el mapa",
        class = "btn btn-secondary btn-sm"
      )
    )
  )
}

#' Title
#'
#' @param id 
#' @param act_mapas 
#' @param fecha 
#'
#' @export
selectormapaServer <- function(id, amba_reducido_names, area, partido) {
  moduleServer(
    id,
    function(input, output, session) {
      
     partido <- geocovidapp::Partidos_Server(  # mod_t2_partidos_input.R
        "seleccion_dinamica",
        amba_reducido_names = amba_reducido_names
      )
      
      imagen <- shiny::eventReactive(act_mapas(),{
        
        # Valores por defecto
        area_val <- if (is.null(area()) || area() == "") "baires" else area()
        partido_val <- if (is.null(partido()) || partido() == "") "Avellaneda" else partido()
        
        # Si la fecha es nula, cargo una fecha por defecto
        if (is.null(fecha())) {
          f_date <- format("2020-05-03", format = "%Y-%m-%d")
        } else {
          # Convierto a formato "YYYY-MM-DD"
          f_date <- formatted_date(fecha())
        }
        
        # Extraigo el raster que eligio el usuario
        raster_data <- geocovidapp::base_raster |>
          dplyr::filter(
            fecha == as.Date(f_date,
                             origin = "1970-01-01"
            ),
            tipo_de_raster == tipo_de_raster(),
            momento == momento_dia, # es un valor no reactivo
            locacion == area_val
          )
        
        if (nrow(raster_data) == 0) {
          # showNotification("No hay datos disponibles para la fecha seleccionada.", type = "warning")
          return(NULL)
        } else {
          rasterLoader(
            pool = pool,
            raster_data = raster_data,
            area = area_val
          )
        }
      })
      
      return(
        list(
          imagen = reactive({ imagen() }),
          partido = reactive({ partido_val }),
          area = reactive({ area_val })
        )
      )
    }
  )
}