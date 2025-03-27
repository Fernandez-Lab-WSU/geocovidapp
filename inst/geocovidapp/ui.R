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
    lang = "es",
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
    shiny::tabPanel(
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
        geocovidapp::FechaMomento_UI("tab1_barraflotante",
          base_raster = base_raster
        ),
        draggable = TRUE,
        width = "350px"
      ),
      shinycssloaders::withSpinner(
        geocovidapp::MapaBaires_UI("tab1_mapa"),
        type = 2,
        color = "lightgrey",
        color.background = "white"
      )
    ),
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
                width = 500,
                geocovidapp::Partidos_UI(
                  "seleccion_partido",
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
                      selected = "7dpc"
                    )
                  ),
                  shiny::column(
                    6,
                    shiny::sliderInput("opacity2",
                      label = "Transparencia",
                      min = 0,
                      max = 1,
                      value = 0.5,
                      width = "75%",
                      ticks = FALSE
                    )
                  ),
                  shiny::hr(),
                  geocovidapp::ReporteUI("desc_reporte")
                )
              ), # cierra sidebar

              shiny::tags$h6("Click en el primer grafico para seleccionar la fecha de los mapas"),
              geocovidapp::Dygraph_UI("casos_covid")
            )
          )
        ),
        shiny::column(
          2,
          shiny::br(),
          bslib::card(
            bslib::card_header("Porcentaje de cambio"),
            shiny::tags$div(
              style = "padding:0px;margin-top:0px",
              shiny::HTML(
                "<i class=\"added-legend\" style=\"background:#FF0000;margin-bottom:0px\"></i><font size='-1'> Aumento más de 40</font><br><i class=\"added-legend\" style=\"background:#FF3300;opacity:0.5\"></i> <font size='-1'>40 - 30</font><br><i class=\"added-legend\" style=\"background:#FF6600;opacity:0.5\"></i><font size='-1'> 30 - 20</font><br><i class=\"added-legend\" style=\"background:#FF9900;opacity:0.5\"></i><font size='-1'> 20 - 10</font><br><i class=\"added-legend\" style=\"background:#FFCC00;opacity:0.5\"></i><font size='-1'> 10 - 1</font><br><i class=\"added-legend\" style=\"background:#FFFFFF;opacity:0.5;border:black;border-width:1px; border-style:solid;\"></i><font size='-1'> Sin cambio</font><br><i class=\"added-legend\" style=\"background:#00FFFF;opacity:0.5\"></i><font size='-1'> -1 - -10</font><br><i class=\"added-legend\" style=\"background:#00BFFF;opacity:0.5\"></i><font size='-1'> -10 - -20</font><br><i class=\"added-legend\" style=\"background:#0080FF;opacity:0.5\"></i><font size='-1'> -20 - -30</font><br><i class=\"added-legend\" style=\"background:#0040FF;opacity:0.5\"></i><font size='-1'> -30 - -40</font><br><i class=\"added-legend\" style=\"background:#0000FF;opacity:0.5\"></i><font size='-1'> Dismunuyo bajo -40</font><br>"
              )
            )
          )
        )
      ),
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
              geocovidapp::MapaPartido_UI("baires_partidos"),
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
    shiny::tabPanel(title ="Panorama Buenos Aires",
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
