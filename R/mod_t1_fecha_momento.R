#' Elementos de interfaz de usuario de la barra flotante del tab de Mapa BsAs
#'
#' Esta función crea los elementos de la interfaz de usuario para la barra flotante
#' en el tab de Mapa Buenos Aires, que permite al usuario seleccionar opciones como el
#' mapa base, área de interés, fecha, momento del día y opciones de transparencia.
#'
#' @param id Nombre del módulo.
#'
#' @return Elementos de interfaz de usuario para la barra flotante en el tab Mapa Buenos Aires.
#'
#' @export
FechaMomento_UI <- function(id) {
  ns <- NS(id)
 
  # Crear lista única de momentos del día y asignar nombres
  momento_del_dia <- as.list(unique(unique(geocovidapp::base_raster$hora)))
  names(momento_del_dia) <- unique(unique(geocovidapp::base_raster$momento))

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        10, 
        h4("Mapa de Movilidad Humana")
      ),
      shiny::column(
        2, 
BotonAyuda_UI(ns('tab1_menuflotante'))
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
#' Servidor: Carga y configuración de raster COVID-19 según selección del usuario
#'
#' @param id Identificador del módulo Shiny.
#' @param pool Objeto de conexión a base de datos, usado para cargar los raster desde la base.
#' @param mapa_zoom Reactive con el valor de zoom actual del mapa Leaflet, usado para mostrar u ocultar controles.
#'
#' @return Una lista de objetos `reactive()` que incluyen:
#' \describe{
#'   \item{area}{Área seleccionada (AMBA o provincia).}
#'   \item{fecha}{Fecha seleccionada por el usuario.}
#'   \item{porcentaje}{Tipo de cambio porcentual (p.ej. semanal, prepandemia).}
#'   \item{momento}{Momento del día (mañana, tarde, etc.).}
#'   \item{opacity}{Opacidad seleccionada para el mapa.}
#'   \item{imagen}{Raster generado a partir de las selecciones del usuario.}
#'   \item{boton}{Trigger del botón para actualizar el mapa.}
#'   \item{basemap}{Capa base seleccionada.}
#' }
#' @export
FechaMomento_Server <- function(id,
                                pool,
                                mapa_zoom) {
  moduleServer(id,
    session = getDefaultReactiveDomain(),
    function(input, output, session) {
      
      # Filtra el rango de fechas según la área 
      # Notar que esto incluye rasters pc y 7dpc
      fecha_rango <- shiny::eventReactive(list(input$area),
        ignoreNULL = FALSE,
        {
           filtered_data <- geocovidapp::base_raster |>
            dplyr::filter(
              locacion == input$area
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
        
        es_valido <- raster_valido(base_raster = geocovidapp::base_raster,
                      area = input$area,
                      porcentaje = input$porcentaje,
                      fecha = input$fechas,
                      momento = input$momento)

        # Si no existe un raster para esta combinación, mostrar la notificación
        if (!es_valido$combinacion_valida) {
          
          # Mostrar una notificación de error
          showNotification(
            paste(es_valido$faltan),
            type = "error",
            closeButton = TRUE,  # Cierra el mensaje
            duration = NULL      # Set to NULL to keep the notification open indefinitely
          )
          
          return(terra::rast()) # Devuelve un raster vacío si no es válido
        }
        
        req(es_valido$combinacion_valida)

        # Extraer el raster correspondiente
        raster_data <- geocovidapp::base_raster |>
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


      # revelo opciones cuando el zoom es mayor a 6
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

      BotonAyuda_Server('tab1_menuflotante')

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