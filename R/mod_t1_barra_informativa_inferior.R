#' UI Component for Informational Bottom Bar
#'
#' Creates a UI element that displays dynamic informational text
#' at the bottom of the Shiny app interface.
#'
#' @param id Character string used to namespace the UI elements.
#'
#' @return A UI element (tagList) containing a styled text output div.
#' @export
BarraInferior_UI <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::div(
      id = ns("info_text"),
      class = "info-text",
      style = "position: absolute; bottom: 10px; width: 100%; text-align: center; padding: 5px; background-color: rgba(0,0,0,0.6); color: white; font-size: 16px;",
      shiny::textOutput(ns("info_text_output"))
    )
  )
}

#' Server Logic for Informational Bottom Bar
#'
#' Server-side logic for managing the informational text displayed in the
#' bottom bar. Reacts to user interaction and input changes to update the message.
#'
#' @param id Character string used to identify the module namespace.
#' @param boton Reactive expression for the update button (e.g., `input$update_map`).
#' @param fecha Reactive expression returning the selected date as a string.
#' @param area Reactive expression indicating the selected area (e.g., "amba").
#' @param momento Reactive expression indicating the time of day (e.g., "mañana", "tarde").
#' @param porcentaje Reactive expression indicating the data type (e.g., "pc" for prepandemia).
#'
#' @return No return value. Called for side effects to update UI text dynamically.
#' @export
BarraInferior_Server <- function(id,
                                 boton,
                                 fecha,
                                 area,
                                 momento,
                                 porcentaje) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Inicializa la variable reactive para el texto
      info_text <- reactiveVal("Imagen inicial para Buenos Aires, 14/04/2020 a la tarde, Prepandemia. Cliquea en un partido o haz zoom para visualizar. Cambia elecciones y actualiza el mapa para mostrar otra imagen.")
      
      # Observa el evento de clic en el botón de actualizar
      observeEvent(boton(), {
        # Si hay un raster seleccionado
        info_text(paste(
          "Ubicación:", ifelse(area() == "amba", "Área Metropolitana de Buenos Aires", "Buenos Aires"),
          "| Fecha:", fecha(),
          "| Momento del día:", momento(),
          "| Tipo de raster: ", ifelse(porcentaje() == "pc", "Prepandemia", "Semanal")
        ))
      })
      
      # Actualiza el texto cuando cambia el área 
      observeEvent(area(), ignoreInit = TRUE, {
        info_text("No hay imagen seleccionada. Elige fecha, cambio porcentual y momento del día y cliquea actualizar el mapa.")
      })
      
      
      # Actualiza el texto en la franja de abajo usando renderText
      output$info_text_output <- renderText({
        info_text()
      })
    }
  )
}