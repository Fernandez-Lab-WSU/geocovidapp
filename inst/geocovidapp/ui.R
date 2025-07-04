#' GeoCovid app IU
#'
#' @export
ui <- function() {
  shiny::navbarPage(
    title = "GeoCovid App",
    id = "nav",
    header = tagList(
      shiny::includeCSS(system.file("geocovidapp/www/custom.css", package = "geocovidapp")),
      shinyjs::useShinyjs()),
      position = "fixed-top", # Zoom al mapa del tab1
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "flatly",
      fg = "#114B5F",        # Color principal para textos
      bg = "white",          # Fondo blanco
      primary = "#114B5F",   # Color navbar y botones primarios
      secondary = "#1C7C9C", # Color secundario
      info = "#88A9BF",      # Color info
      heading_font = bslib::font_google("Source Sans Pro",
                                        wght = "600", local = FALSE),  # Poppins #Barlow Semi Condensed
      base_font = bslib::font_google("Public Sans", 
                                     wght = "400", local = FALSE), # Lato # 
      "input-border-color" = "#406177" # Color borde inputs
    ),
    windowTitle = "geocovid app",
    shiny::tabPanel( ## TAB 1
      title = "Movilidad Buenos Aires",
      value = "mov_bsas",
      tags$style(
        type = "text/css",
        "body {margin-top: 50px;}"
      ),
      shiny::absolutePanel(
        id = "controles",
        class = "panel panel-default",
        # me permite que se vean los controles
        # por encima del mapa
        style = "z-index:500;",
        geocovidapp::FechaMomento_UI("tab1_barraflotante"),
        draggable = TRUE,
        width = "350px"
      ),
      shinycssloaders::withSpinner(
        geocovidapp::MapaBaires_UI("tab1_mapa"),
        type = 2,
        color = "lightgrey",
        color.background = "white"
      )
    ), ## TAB 2
    shiny::tabPanel(
      title = "Por partido",
      value = "mov_partido",
      tags$style(
        type = "text/css",
        "body {margin-top: 50px;}"
      ),
      shiny::fluidRow(
        shiny::column(
          10,
          bslib::card(
           # style = "height: 400px;", 
            bslib::layout_sidebar(
              title = "Por partido",
              sidebar = bslib::sidebar(
                id = "tab2-sidebar",
                open = "always",
                width = 320,
                shiny::h6("1. Elige el raster a visualizar:"),
                geocovidapp::Partidos_UI("selector_dinamico",
                                         amba_reducido_names = amba_reducido_names),
                hr(),
                geocovidapp::ReporteUI("desc_reporte")
              ), # cierra sidebar
              bslib::layout_columns(
                col_widths = c(9, 3),
                fill = TRUE,  # Fuerza llenar alto
                bslib::card_body(
                  tags$div(
                    # quitamos display:inline que puede generar espacios raros
                    # solo margin y padding a cero
                    style = "margin: 0; padding: 0;",
                    tags$p(
                      "2. Click en el primer gráfico para seleccionar la fecha de los mapas",
                      style = "font-size: 14px; margin-top: 0; margin-bottom: 4px; padding: 0;"
                    ),
                    tags$h6(
                      "Casos de COVID-19 diarios reportados por el sistema de salud",
                      style = "font-size: 18px; font-weight: bold; margin-top: 0; margin-bottom: 0; padding: 0;"
                    )
                  ),
                  geocovidapp::Dygraph_UI("casos_covid")
                ), # cierra card body
                  tags$div(
                    br(),
                shiny::actionButton(
                  "act_mapas",
                  "3. Actualizar el mapa",
                  class = "btn btn-secondary btn-sm",
                  style = "width: 100%;"
                ),
                hr(),
                uiOutput("vb_fecha"), # value boxes
                uiOutput("vb_casos")
                )
              ) # cierra body
)
          ) # cierra card
        ), # cierra column
        shiny::column(
          2,
          shiny::br(),
          tags$h5("Controles Mapas"),
          shiny::sliderInput("opacity",
                             label = "Transparencia",
                             min = 0,
                             max = 1,
                             value = 0.5,
                             width = "80%",
                             ticks = FALSE),
          tags$h6("Porcentaje de Cambio"),
          shiny::tags$div(
            style = "padding:0px;margin-top:0px",
            includeHTML("www/leyenda_leaflet.html"))
      )),
      tags$h5(textOutput("titulo")),
      shiny::fluidRow(
        shiny::column(
          4,
          # style='padding:5px; margin-left:45px',
          bslib::card(
            bslib::card_header("Mañana (8 am)"),
            bslib::layout_sidebar(
              class = "p-0",
              sidebar = bslib::sidebar(
                style = "padding:1px; margin:0px",
                bg = "#88A9BF",
                shiny::br(),
                shiny::br(),
                shiny::br(),
                shiny::br(),
                geocovidapp::HistogramaRaster_UI("hist"),
                width = 150
              ),
              geocovidapp::MapaPartido_UI("baires_partidos")
            )
          )
        ),
        shiny::column(
          4,
          # style='padding:5px;',
          bslib::card(
            bslib::card_header("Tarde (4 pm)"),
            bslib::layout_sidebar(
              class = "p-0",
              sidebar = bslib::sidebar(
                style = "padding:1px; margin:0px",
                bg = "#88A9BF",
                shiny::br(),
                shiny::br(),
                shiny::br(),
                shiny::br(),
                geocovidapp::HistogramaRaster_UI("hist2"),
                width = 150
              ),
              geocovidapp::MapaPartido_UI("baires_partidos2")
            )
          )
        ),
        shiny::column(
          4,
          bslib::card(
            bslib::card_header("Noche (0 am)"),
            bslib::layout_sidebar(
              class = "p-0",
              sidebar = bslib::sidebar(
                style = "padding:1px; margin:0px",
                bg = "#88A9BF",
                shiny::br(),
                shiny::br(),
                shiny::br(),
                shiny::br(),
                geocovidapp::HistogramaRaster_UI("hist3"),
                width = 150
              ),
              geocovidapp::MapaPartido_UI("baires_partidos3")
            )
          )
        )
      )
    ),
    shiny::tabPanel(title ="Panorama Buenos Aires", ## TAB 3
                    value = "cov_bsas",
                    tags$style(type="text/css", "body {margin-top: 50px;}"),
                    geocovidapp::MapaCovidDepartamentos_UI('casos_covid')
    ),
    shiny::tabPanel(
      title = "Sobre el proyecto",
      tags$style(type = "text/css", "body {margin-top: 50px;}"),
      tags$img(
        src = "geocovid_header.png",
        width = "100%",
        height = "auto"
      ),
      shiny::br(),
      div(
        includeMarkdown("www/sobre_el_proyecto.md"),
        class = "markdown-container",
        style = "width: 80%; height: 800px; overflow: auto;" # Scrolling
      )
    )
  )
}
