#' GeoCovid app IU
#'
#' @export
ui <- function() {
  shiny::navbarPage(
    title = "GeoCovid App",
    id = "nav",
    shiny::includeCSS(system.file("geocovidapp/www/custom.css",
      package = "geocovidapp"
    )),
    shinyjs::useShinyjs(),
    position = "fixed-top", # Zoom al mapa del tab1
    tags$style(type = "text/css", ".navbar{padding-left:30px}"),
    # lang = "es",
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "flatly",
      fg = "#114B5F", # fuentes
      bg = "white",
      primary = "#114B5F", # navbar
      secondary = "#1C7C9C",
      info = "#88A9BF",
      heading_font = "Work Sans, sans-serif",
      "input-border-color" = "#406177",
      base_font = bslib::font_collection(
        bslib::font_google("Work Sans",
          wght = "400",
          local = FALSE
        ),
        "Roboto",
        "sans-serif"
      )
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
          shiny::br(),
          tags$h5(paste(
            "Casos de COVID-19 diarios",
            "reportados por el sistema",
            "de salud"
          )),
          bslib::card(
            bslib::layout_sidebar(
              title = "Por partido",
              sidebar = bslib::sidebar(
                id = "tab2-sidebar",
                width = 350,
                geocovidapp::Partidos_UI("selector_dinamico",
                                         amba_reducido_names = amba_reducido_names,
                                         base_raster = base_raster),
                shiny::actionButton(
                  "act_mapas",
                  "Actualizar el mapa",
                  class = "btn btn-secondary btn-sm"
                ),
                geocovidapp::ReporteUI("desc_reporte")
              ), # cierra sidebar
            shiny::tags$h6("Click en el primer grafico para seleccionar la fecha de los mapas"),
              geocovidapp::Dygraph_UI("casos_covid")))
        ),
        shiny::column(
          2,
          shiny::br(),
          tags$h5("Controles Mapas"),
          shiny::br(),
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
