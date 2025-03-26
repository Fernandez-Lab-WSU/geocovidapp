#' Elementos de interfaz de usuario de la barra flotante del tab de Mapa BsAs
#'
#' Esta función crea los elementos de la interfaz de usuario para la barra flotante
#' en el tab de Mapa Buenos Aires, que permite al usuario seleccionar opciones como el
#' mapa base, área de interés, fecha, momento del día y opciones de transparencia.
#'
#' @param id Nombre del módulo.
#' @param base_raster Dataframe que lista todos los rasters y desagrega en sus columnas características de interés, como si son rasters de AMBA o Buenos Aires, si el cambio porcentual es semanal o prepandemia, o el momento del día que representan.
#'
#' @return Elementos de interfaz de usuario para la barra flotante en el tab Mapa Buenos Aires.
#'
#' @export
#'
#' @examples
FechaMomentoUI <- function(id, base_raster) {
  ns <- NS(id)

  momento_del_dia <- as.list(unique(base::unique(base_raster$hora)))
  names(momento_del_dia) <- unique(base::unique(base_raster$momento))

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        10, 
        h4("Cliquea en el mapa")
      ),
      shiny::column(
        2, 
        shiny::actionButton(ns("help_button"),
          label = "",
          icon = shiny::icon("question-circle"),
          style = "font-size: 20px; color: lightseagreen; background-color: white; border-color: black;"
        )
      )
    ),
    shinyjs::hidden(
      shiny::div(
        id = ns("help_tooltip"),
        style = "position: absolute; top: 30px; left: 100%;
                            transform: translateX(-50%); background-color: #f9f9f9;
                            border: 1px solid #ddd; padding: 10px; border-radius: 4px;
                            font-size: 16px; /* Aumenta el tamaño de la fuente */
                            width: 300px; /* Define el ancho del tooltip */
                            box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1);
                            border-radius: 8px; /* Aumenta el redondeo de los bordes */
                            pointer-events: none; /* Evita malfuncionamiento del hover sobre el boton */",
        "Este es un mapa interactivo. Puedes seleccionar la ubicación, fecha, y otras opciones para visualizar diferentes capas de información."
      )
    ),
    shiny::radioButtons(ns("area"),
      label = paste(
        "Selecciona AMBA para visualizar datos",
        "con mayor resolucion en ese area"
      ),
      choices = c(
        "prov. de Buenos Aires" = "baires",
        "AMBA" = "amba"
      ),
      selected = "baires",
      inline = TRUE
    ),
    # shinyjs::hidden(
    #   div(id = "linea1",
    #   hr(style = "border: 2px solid green;")
    # )
    # ),
    shinyjs::hidden(shiny::dateInput(ns("fechas"),
      label = "Fecha",
      min = min(base::unique(base_raster$fecha)), # ojo que va a permitir elegir dias faltantes
      max = max(base::unique(base_raster$fecha)),
      value = base::unique(base_raster$fecha)[1],
      language = "es",
      format = "yyyy-mm-dd"
    )),
    shiny::fluidRow(
      shiny::column(
        6,
        shinyjs::hidden(
          shiny::radioButtons(ns("momento"),
            label = "Momento del día",
            choices = c(
              "Mañana" = "mañana",
              "Tarde" = "tarde",
              "Noche" = "noche"
            ),
            selected = unique(base_raster$momento)[1]
          )
        )
      ),
      shiny::column(
        6,
        shinyjs::hidden(
          shiny::radioButtons(ns("porcentaje"),
            label = "Cambio porcentual",
            choices = c(
              "Prepandemia" = "pc",
              "Semanal" = "7dpc"
            ),
            selected = "pc"
          )
        ),
        shiny::div(
          style = "text-align: left; margin-top: 10px;",
          shinyjs::hidden(shiny::actionButton(
            ns("actualiza_mapa"),
            "Actualizar Mapa"
          ))
        )
      )
    ),
    tags$hr(id = "linea2", style = "border: 2px solid green;"),
    h5("Opciones de visualización"),
    # Agrupamos basemap y opacity en una misma fila
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::radioButtons(ns("basemap"),
          label = "Mapa Base",
          choices = c(
            "Relieve" = "relieve",
            "Calles" = "calles"
          ),
          selected = "relieve",
          inline = TRUE
        )
      ),
      shiny::column(
        6,
        shinyjs::hidden(
          p(
            id = "barra_transparencia",
            "Opciones de visualización del mapa"
          ),
          shiny::sliderInput(ns("opacity"),
            label = "Transparencia",
            min = 0,
            max = 1,
            value = 0.5,
            ticks = FALSE
          )
        )
      )
    )
  )
}
#' Convierte el archivo en raster en base a las elecciones del usuario
#'
#' @param id Module name
#' @param base_raster Dataframe que lista todos los rasters y desagrega en
#' sus columnas características de interes, como si son rasters de
#' AMBA o Buenos Aires, si el cambio porcentual es semanal o prepandemia
#' o el momento del día que representan.
#' @param mapa_zoom Valor de zoom que retorna leaflet en base al uso del mapa.
#'
#' @return Una serie de variables extraidas del dataframe base_raster para
#' el raster que eligio el usuario y representan el area, la opacidad,
#' el raster mismo (imagen) y basemap
#' @export
FechaMomento_Server <- function(id,
                                pool,
                                base_raster,
                                mapa_zoom) {
  moduleServer(id,
    session = getDefaultReactiveDomain(),
    function(input, output, session) {
      # Reactive to get the filtered date range based on area and porcentaje
      fecha_rango <- shiny::eventReactive(list(input$area, input$porcentaje),
        ignoreNULL = FALSE,
        {
          # Filtramos los datos según la locación y tipo de raster seleccionados
          filtered_data <- base_raster |>
            dplyr::filter(
              locacion == input$area,
              tipo_de_raster == input$porcentaje
            )

          # Obtener el rango de fechas mínimo y máximo
          min_date <- min(filtered_data$fecha, na.rm = TRUE)
          max_date <- max(filtered_data$fecha, na.rm = TRUE)

          list(min = min_date, max = max_date)
        }
      )

      # Actualizar el dateInput con el rango dinámico de fechas
      observe({
        req(fecha_rango())
        range <- fecha_rango()

        shiny::updateDateInput(session, "fechas",
          min = range$min,
          max = range$max
          # value = range$min
        ) # Valor predeterminado es el mínimo
      })

      imagen <- shiny::eventReactive(input$actualiza_mapa, ignoreNULL = FALSE, {
        # Get selected date and moment
        selected_date <- input$fechas
        selected_momento <- input$momento

        is_valid <- any(base_raster$locacion == input$area &
          base_raster$tipo_de_raster == input$porcentaje &
          base_raster$fecha == selected_date &
          base_raster$momento == selected_momento)


        if (!is_valid) {
          showNotification("Combinación inválida de fecha y momento. Intenta con otra combinacion.", type = "error")
          return(terra::rast())
        }


        req(is_valid)

        # Extraigo el raster que eligio el usuario
        raster_data <- base_raster |>
          dplyr::filter(
            fecha == as.Date(input$fechas,
              origin = "1970-01-01"
            ),
            tipo_de_raster == input$porcentaje,
            momento == input$momento, # es un valor no reactivo
            locacion == input$area
          )

        print(raster_data)

        # Los rasters de amba y baires estan en diferentes bases de datos
        if (input$area == "amba") {
          query <- paste0(
            "SELECT ST_AsGDALRaster(rast, 'GTiff') AS rast FROM raster_schema.rasters_geo WHERE filename='",
            raster_data$filename, "';"
          )
        } else {
          query <- paste0(
            "SELECT ST_AsGDALRaster(rast, 'GTiff') AS rast FROM raster_schema.raster_geo_baires WHERE filename='",
            raster_data$filename, "';"
          )
        }

        result <- pool::dbGetQuery(pool, query)
        print(result)
        # Save the binary raster data to a temporary file
        temp_file <- tempfile(fileext = ".tif")
        writeBin(result$rast[[1]], temp_file)

        # Load the raster package and read the temporary file
        terra::rast(temp_file)
      })


      # revelo la barra de transparencia cuando el zoom es mayor a 6
      shiny::observeEvent(mapa_zoom(), {
        if (mapa_zoom() <= 6) {
          shinyjs::hide("opacity")
          shinyjs::hide("porcentaje")
          shinyjs::hide("momento")
          shinyjs::hide("fechas")
          shinyjs::hide("actualiza_mapa")
        } else {
          shinyjs::show("opacity")
          shinyjs::show("porcentaje")
          shinyjs::show("momento")
          shinyjs::show("fechas")
          shinyjs::show("actualiza_mapa")
        }
      })

      # In the server function:
      observe({
        shinyjs::onevent("mouseenter", "help_button", shinyjs::show("help_tooltip"))
        shinyjs::onevent("mouseleave", "help_button", shinyjs::hide("help_tooltip"))
      })

      return(
        list(
          area = reactive({
            input$area
          }),
          fecha = reactive({
            input$fechas
          }),
          porcentaje = reactive({
            input$porcentaje
          }),
          momento = reactive({
            input$momento
          }),
          opacity = reactive({
            input$opacity
          }),
          imagen = reactive({
            imagen()
          }),
          boton = reactive({
            input$actualiza_mapa
          }),
          basemap = reactive({
            input$basemap
          })
        )
      )
    }
  )
}