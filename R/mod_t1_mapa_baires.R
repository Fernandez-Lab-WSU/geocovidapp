#' IU: Mapa raster de la provincia de Buenos Aires
#'
#' @description
#' Esta función crea la interfaz de usuario (UI) para el mapa interactivo
#' de la provincia de Buenos Aires, que se encuentra en el primer tab de la aplicación.
#' Utiliza el paquete `leaflet` para mostrar un mapa interactivo con un alto grado de interactividad,
#' permitiendo a los usuarios visualizar y explorar los datos geoespaciales de la región.
#'
#' @param id Nombre del módulo. Este parámetro es necesario para la correcta
#' integración del módulo UI dentro de la aplicación Shiny.
#'
#' @return Un objeto UI de Shiny que contiene un mapa interactivo de la provincia de Buenos Aires,
#' renderizado con `leaflet`. El mapa es capaz de manejar eventos de interacción como zoom y desplazamiento.
#'
#' @export
MapaBaires_UI <- function(id) {
  ns <- NS(id)

  shiny::tagList(
    shiny::fillPage(
      leaflet::leafletOutput(ns("inter_mapa"),
        height = "90vh"
      ),
      shiny::div(
        id = ns("info_text"),
        class = "info-text",
        style = "position: absolute; bottom: 10px; width: 100%; text-align: center; padding: 5px; background-color: rgba(0,0,0,0.6); color: white; font-size: 16px;",
        shiny::textOutput(ns("info_text_output"))
      ),
      header = tags$style(HTML("
                                        .container-fluid{
                                          padding: 0px !important;
                                        }

                                        .navbar{
                                          margin-bottom: 0px !important;
                                        }"))
    )
  )
}


#' Servidor: Mapa raster de la provincia de Buenos Aires
#'
#' @description
#' Esta función crea la lógica del servidor para renderizar el mapa interactivo de la provincia de Buenos Aires.
#' El mapa incluye un raster que representa la movilidad ciudadana y permite la visualización interactiva
#' de los cambios en la provincia. También incluye controles de mapa como la escala, medidas, y leyendas.
#'
#' @param id Nombre del módulo. Este parámetro es necesario para la correcta integración del módulo de servidor dentro de la aplicación Shiny.
#' @param bsas Dataset de clase `sf` con los partidos de Buenos Aires, utilizado para agregar las geometrías de los municipios y su visualización en el mapa.
#' @param imagen Imagen raster que representa la movilidad ciudadana. Esta imagen es cargada y visualizada sobre el mapa.
#' @param area Indica el área geográfica de interés: puede ser 'provincia' o 'amba'. Dependiendo de este parámetro, el mapa se ajusta en términos de visualización.
#' @param opacidad Valor de opacidad del raster, que controla la transparencia de la imagen raster sobre el mapa.
#' @param basemap Estilo de mapa base para la visualización, como "calles" o "relieve", determinado por el usuario.
#' @param amba_reducido_names Vector de nombres de los partidos que conforman el área del AMBA (Área Metropolitana de Buenos Aires), usado para filtrar los datos geoespaciales.
#'
#' @return Un objeto `leaflet` renderizado con un mapa interactivo, incluyendo la capacidad de hacer zoom y desplazarse por el área seleccionada.
#' También retorna el valor de zoom cuando cambia durante la navegación en el mapa.
#'
#' @export
MapaBaires_Server <- function(id, bsas,
                              imagen, area,
                              opacidad,
                              fecha,
                              momento,
                              porcentaje,
                              basemap,
                              boton,
                              amba_reducido_names) {
  moduleServer(
    id,
    function(input, output, session) {
      # Paleta de color
      pal <- leaflet::colorBin(
        palette = c(
          "#0000FF", "#0040FF", "#0080FF",
          "#00BFFF", "#00FFFF",
          "#FFFFFF",
          "#FFCC00", "#FF9900",
          "#FF6600", "#FF3300", "#FF0000"
        ),
        bins = c(
          50, 40, 30, 20, 10, 1,
          -1, -10, -20, -30, -40, -50
        ),
        na.color = "transparent"
      )

      labels <- c(
        "Aumento más de 40",
        "40 - 30", "30 - 20", "20 - 10", "10 - 1",
        "Sin cambio",
        "-1 - -10", "-10 - -20", "-20 - -30", "-30 - -40",
        "Disminuyó bajo -40"
      )

      # Renderiza el mapa de la provincia de Buenos Aires
      output$inter_mapa <- leaflet::renderLeaflet({
        base_map <- leaflet::leaflet() |>
          addBasemapTiles() |>
          leaflet.extras::addResetMapButton() |>
          leaflet::addMeasure(
            position = "bottomleft",
            primaryLengthUnit = "kilometers",
            primaryAreaUnit = "hectares",
            localization = "es"
          ) |>
          leaflet.extras::addFullscreenControl() |>
          leaflet::addScaleBar(position = "topright") |>
          leafem::addMouseCoordinates() |>
          addPolygonsLayer(bsas, fillopacity_poly = 0.05) |>
          leaflet::setView(
            lat = ifelse(area() == "baires", -36.94, -34.72),
            lng = ifelse(area() == "baires", -63.94, -59.12),
            zoom = ifelse(area() == "baires", 6, 10)
          )
      })


      # Ahora observo cambios en la opacidad y mapa de base
      shiny::observe({
        leafprox <- leaflet::leafletProxy(
          mapId = "inter_mapa",
          session = session
        )


        if (basemap() == "calles") {
          lm <- leafprox |>
            leaflet::clearTiles() |>
            leaflet::addProviderTiles("OpenStreetMap",
              group = "OpenStreetMap",
              layerId = "open"
            ) |>
            leaflet::removeImage(layerId = "raster") |>
            leaflet::addRasterImage(imagen(),
              colors = pal,
              opacity = opacidad(),
              group = "basic",
              layerId = "raster",
              project = FALSE
            ) |>
            leaflet::removeControl("legend") |> # Evita que se duplique la leyenda
            leaflet::addLegend(
              pal = pal, values = terra::values(imagen()),
              title = "Porcentaje de cambio",
              position = "topright",
              group = "basic",
              layerId = "legend",
              labFormat = function(type, cuts, p) {
                paste0(labels)
              }
            )
        } else if (basemap() == "relieve") {
          lm <- leafprox |>
            leaflet::clearTiles() |>
            addBasemapTiles() |>
            leaflet::removeImage(layerId = "raster") |>
            leaflet::addRasterImage(imagen(),
              colors = pal,
              opacity = opacidad(),
              group = "basic",
              layerId = "raster",
              project = FALSE
            ) |>
            leaflet::removeControl("legend") |> # Evita que se duplique la leyenda
            leaflet::addLegend(
              pal = pal, values = terra::values(imagen()),
              title = "Porcentaje de cambio",
              position = "topright",
              group = "basic",
              layerId = "legend",
              labFormat = function(type, cuts, p) {
                paste0(labels)
              }
            )
        }
      })

      # Click en el mapa de la provincia de BsAs permite acercarme al mapa
      shiny::observeEvent(input$inter_mapa_shape_click, {
        click <- input$inter_mapa_shape_click

        leaflet::leafletProxy("inter_mapa") |>
          leaflet::setView(
            lng = click$lng,
            lat = click$lat,
            zoom = 10
          )
      })


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

      return(list(mapa_zoom = reactive({
        input$inter_mapa_zoom
      })))
    }
  )
}