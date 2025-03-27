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
 
  # Crear lista única de momentos del día y asignar nombres
  momento_del_dia <- as.list(unique(unique(base_raster$hora)))
  names(momento_del_dia) <- unique(unique(base_raster$momento))

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        10, 
        h4("Cliquea en el mapa")
      ),
      shiny::column(
        2, 
Boton_Ayuda_UI(ns('barra-flotante'))
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
    # Agrupamos basemap y opacity en una misma fila
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
      
      # Filtra el rango de fechas según la área y tipo de cambio porcentual
      fecha_rango <- shiny::eventReactive(list(input$area, input$porcentaje),
        ignoreNULL = FALSE,
        {
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
         ) # Valor predeterminado es el mínimo por default
      })

      imagen <- shiny::eventReactive(input$actualiza_mapa,
                                     ignoreNULL = FALSE, {
        
        selected_date <- input$fechas
        selected_momento <- input$momento

        # Si hay algun raster que cumpla todas las elecciones,
        # esto dara TRUE
        is_valid <- any(base_raster$locacion == input$area &
          base_raster$tipo_de_raster == input$porcentaje &
          base_raster$fecha == selected_date &
          base_raster$momento == selected_momento)

        # Si no existe un raster para esta combinacion, 
        # Imprimira un mensaje en la pantalla
        if (!is_valid) {
          showNotification("Combinación inválida de fecha y momento. Intenta con otra combinacion.", type = "error")
          return(terra::rast())
        }

        req(is_valid)

        # Extraer el raster correspondiente
        raster_data <- base_raster |>
          dplyr::filter(
            fecha == as.Date(input$fechas,
              origin = "1970-01-01"
            ),
            tipo_de_raster == input$porcentaje,
            momento == input$momento, # es un valor no reactivo
            locacion == input$area
          )
       
       # Bajo el raster de la base de datos
       rasterLoader(pool = pool,
                    raster_data = raster_data,
                    area = input$area)
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

      Boton_Ayuda_Server('barra-flotante')

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