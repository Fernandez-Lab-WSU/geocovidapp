#' UI Component for Help Button with Tooltip
#'
#' Creates a help button with an associated tooltip that appears on hover.
#' This module is intended to be used within a Shiny UI.
#'
#' @param id Character string used to namespace the UI elements.
#'
#' @return A UI element (tagList) containing a help button and a hidden tooltip.
#' @export
BotonAyuda_UI <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::actionButton(ns("help_button"),
      label = "",
      icon = shiny::icon("question-circle"),
      style = "font-size: 20px; color: lightseagreen;
      background-color: white; border-color: white;"
    ),
    shinyjs::hidden(shiny::div(
      id = ns("help_tooltip"),
      style = "position: absolute; top: 30px; left: 100%;
                            transform: translateX(-50%); background-color: #f9f9f9;
                            border: 1px solid #ddd; padding: 10px; border-radius: 4px;
                            font-size: 16px; /* Aumenta el tamaño de la fuente */
                            width: 300px; /* Define el ancho del tooltip */
                            box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1);
                            border-radius: 8px; /* Aumenta el redondeo de los bordes */
                            pointer-events: none; /* Evita malfuncionamiento del hover sobre el boton */",
      "Este es un mapa interactivo.
      Puedes seleccionar la ubicación, fecha, y otras opciones 
      para visualizar diferentes capas de información."
    ))
  )
}

#' Server Logic for Help Button with Tooltip
#'
#' Server-side logic for the help button module. Shows and hides
#' the tooltip on mouse hover using `shinyjs`.
#'
#' @param id Character string used to identify the module namespace.
#'
#' @return No return value. This function is called for its side effects (UI behavior).
#' @export
BotonAyuda_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observe({
        shinyjs::onevent("mouseenter", "help_button", 
                         shinyjs::show("help_tooltip"))
        shinyjs::onevent("mouseleave", "help_button", 
                         shinyjs::hide("help_tooltip"))
      })    
   
    }
  )
}