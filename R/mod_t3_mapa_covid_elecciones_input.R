#' IU: Selector de fecha
#'
#' @param id Module name
#' @param amba_reducido_names String. Vector con los nombres de los partidos 
#' que conforman el AMBA.
#' @param base_raster Dataframe que lista todos los rasters y desagrega en 
#' sus columnas características de interes, como si son rasters de 
#' AMBA o Buenos Aires, si el cambio porcentual es semanal o prepandemia 
#' o el momento del día que representan. 
#' 
#'
#' @return Selector de fecha
#' @export
MapaCovidElecciones_UI <- function(id, base_raster, amba_reducido_names) {

  ns <- NS(id)
  shiny::tagList(

    shiny::dateInput(ns("fecha"),
                     label = "Fecha",
                     min = min(base::unique(base_raster$fecha)),
                     max = max(base::unique(base_raster$fecha)),
                     value = base::unique(base_raster$fecha)[1],
                     language = "es",
                     format = "yyyy-mm-dd")

  )

}


#' Servidor: Selector de fecha
#'
#' @param id Module name
#' @param data_sisa Dataframe con los casos diarios reportados de COVID-19. 
#' @param bsas Mapa vectoria de la provincia de Buenos Aires simplifica
#' do. 
#' @param part Partido de la provincia de Buenos Aires, seleccionado en otro 
#' módulo. 
#'
#' @return Fecha seleccionada por el usuario
#' @export
MapaCovidElecciones_Server <- function(id, data_sisa, bsas,
                                          part) {
  moduleServer(
    id,
    function(input, output, session) {


      return(
        list(
          fecha = reactive({ input$fecha })
        )
      )

})}

