Boton_Ayuda_UI <- function(id) {
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

Boton_Ayuda_Server <- function(id) {
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