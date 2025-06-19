#' IU: Mapa prov. de Buenos Aires por departamento
#'
#' @description
#' Este mapa se encuentra en el tab 3 de GeoCovid app.
#'
#' @param id Module name
#'
#' @return Elementos de interfaz de usuario del tab 3
#' @export
MapaCovidDepartamentos_UI <- function(id) {
  bsas_lista <- c(
    "25 de Mayo", "9 de Julio", "Adolfo Alsina", "Adolfo Gonzales Chaves",
    "Alberti", "Almirante Brown", "Arrecifes", "Avellaneda", "Ayacucho",
    "Azul", "Bahía Blanca", "Balcarce", "Baradero", "Benito Juárez",
    "Berazategui", "Berisso", "Bolívar", "Bragado", "Brandsen",
    "Campana", "Cañuelas", "Capital Federal", "Capitán Sarmiento",
    "Carlos Casares", "Carlos Tejedor", "Carmen de Areco", "Castelli",
    "Chacabuco", "Chascomús", "Chivilcoy", "Colón", "Coronel de Marina Leonardo Rosales",
    "Coronel Dorrego", "Coronel Pringles", "Coronel Suárez", "Daireaux",
    "Dolores", "Ensenada", "Escobar", "Esteban Echeverría", "Exaltación de la Cruz",
    "Ezeiza", "Florencio Varela", "Florentino Ameghino", "General Alvarado",
    "General Alvear", "General Arenales", "General Belgrano", "General Guido",
    "General Juan Madariaga", "General La Madrid", "General Las Heras",
    "General Lavalle", "General Paz", "General Pinto", "General Pueyrredón",
    "General Rodríguez", "General San Martín", "General Viamonte",
    "General Villegas", "Guaminí", "Hipólito Yrigoyen", "Hurlingham",
    "Ituzaingó", "José C. Paz", "Junín", "La Costa", "La Matanza",
    "La Plata", "Lanús", "Laprida", "Las Flores", "Leandro N. Alem",
    "Lezama", "Lincoln", "Lobería", "Lobos", "Lomas de Zamora",
    "Luján", "Magdalena", "Maipú", "Malvinas Argentinas", "Mar Chiquita",
    "Marcos Paz", "Mercedes", "Merlo", "Monte", "Monte Hermoso",
    "Moreno", "Morón", "Navarro", "Necochea", "Olavarría", "Patagones",
    "Pehuajó", "Pellegrini", "Pergamino", "Pila", "Pilar", "Pinamar",
    "Presidente Perón", "Puán", "Punta Indio", "Quilmes", "Ramallo",
    "Rauch", "Rivadavia", "Rojas", "Roque Pérez", "Saavedra", "Saladillo",
    "Salliqueló", "Salto", "San Andrés de Giles", "San Antonio de Areco",
    "San Cayetano", "San Fernando", "San Isidro", "San Miguel", "San Nicolás",
    "San Pedro", "San Vicente", "Suipacha", "Tandil", "Tapalqué",
    "Tigre", "Tordillo", "Tornquist", "Trenque Lauquen", "Tres Arroyos",
    "Tres de Febrero", "Tres Lomas", "Vicente López", "Villa Gesell",
    "Villarino", "Zárate"
  )

  ns <- NS(id)

  shiny::tagList(
    bslib::layout_sidebar(
      fluidRow(
        column(
          6,
          tags$h5(textOutput(ns("diaselec"))),
          tags$h6("provincia de Buenos Aires"),
          br(),
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("burbujas_map"),
              height = "600px"
            ),
            type = 2,
            color = "lightgrey",
            color.background = "white"
          )
        ),
        column(
          width = 6,
          tags$h5("Casos de COVID-19"),
          tags$h6("Click en el primer grafico para seleccionar la fecha del mapa"),
          Dygraph_UI(
            ns("casos_covid_interno")
          ),
          br(),
          tags$h5("Promedio de movilidad diurna y nocturna"),
          shinycssloaders::withSpinner(
            dygraphs::dygraphOutput(ns("raster_analisis"),
              width = 600,
              height = 100
            ),
            type = 2,
            color = "lightgrey",
            color.background = "white"
          ),
          br(),
          tags$h5("Vision ampliada por departamento"),
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(
              ns("zoom_map"),
              height = 250
            ),
            type = 2,
            color = "lightgrey",
            color.background = "white"
          )
        )
      ),
      sidebar = bslib::sidebar(
        position = "right",
        shiny::selectInput(ns("partidos"),
          label = "Selecciona el partido",
          choices = bsas_lista,
          selected = bsas_lista[1],
          width = "100%"
        ),
        shiny::radioButtons(ns("tipo_tab"),
          label = "Selecciona el raster",
          choices = c(
            "Prepandemia" = "pc",
            "Semanal" = "7dpc"
          ),
          selected = "pc"
        ),
        shiny::radioButtons(ns("momento"),
          label = "Selecciona el momento del dia",
          choices = c(
            "Mañana-Tarde" = "criterio",
            "Noche" = "criterio_noche"
          ),
          selected = "criterio"
        )
      )
    )
  )
}


#' Servidor: Mapa prov. de Buenos Aires por departamento
#'
#' @description
#' Este mapa se encuentra en el tab 3 de GeoCovid app.
#'
#' @param id Module name
#' @param amba_caba String. Vector con los nombres de los partidos
#' que conforman el AMBA sin las comunas de Provincia de Buenos Aires.
#'
#' @importFrom stats quantile
#' @return Mapas y gráficos del tab 3.
#' @export
MapaCovidDepartamentos_Server <- function(id,
                                          pool,
                                          amba_caba) {
  moduleServer(
    id,
    function(input, output, session) {
      # Filtro los partidos de Buenos Aires
      fechas <- Dygraph_Server("casos_covid_interno",
        amba = amba_caba,
        part = reactive({
          input$partidos
        }),
        area = reactiveVal({
          "baires"
        })
      )

      # Datos de movilidad por partido como media de los pixeles
      px_data <- shiny::reactive({
        partidos_input <- input$partidos
        tipo_tab_input <- input$tipo_tab

        # Filtro las condiciones antes de la query
        # Bajar la base de datos completa tarda mucho tiempo.
        query <- paste0(
          "SELECT * FROM px_promedio.px_promedio WHERE ",
          "partido = '", partidos_input, "' AND ",
          "tipo_de_raster = '", tipo_tab_input, "'"
        )

        # Run the query using st_read
        sf::st_read(pool, query = query)
      })



      output$raster_analisis <- dygraphs::renderDygraph({
        pxdy <- px_data()

        a <- pxdy[, c("fecha", "px_mean_dianoche")]
        b <- pxdy[, c("fecha", "mañana_8")]
        c <- pxdy[, c("fecha", "tarde_16")]
        d <- pxdy[, c("fecha", "noche_0")]

        px_baires_dianoche <- xts::xts(a$px_mean_dianoche,
          order.by = a$fecha
        )
        px_baires_8 <- xts::xts(b$mañana_8,
          order.by = b$fecha
        )
        px_baires_16 <- xts::xts(c$tarde_16,
          order.by = c$fecha
        )
        px_baires_0 <- xts::xts(d$noche_0,
          order.by = d$fecha
        )

        px_baires <- cbind(
          px_baires_dianoche,
          px_baires_0,
          px_baires_8,
          px_baires_16
        )


        base::colnames(px_baires)[1] <- as.character("PromedioDiaYTarde")
        base::colnames(px_baires)[2] <- as.character("Noche")

        dygraphs::dygraph(data = px_baires) |>
          dygraphs::dySeries(c(
            "px_baires_8",
            "PromedioDiaYTarde",
            "px_baires_16"
          )) |>
          dygraphs::dySeries("Noche") |>
          dygraphs::dyOptions(
            labelsUTC = TRUE,
            drawGrid = FALSE
          ) |>
          dygraphs::dyAxis("y",
            label = "% de casos por partido"
          ) |>
          dygraphs::dyHighlight(
            highlightCircleSize = 3,
            highlightSeriesBackgroundAlpha = 0.4,
            hideOnMouseOut = TRUE
          ) |>
          dygraphs::dyEvent("2020-04-13") |>
          dygraphs::dyEvent("2020-04-26") |>
          dygraphs::dyEvent("2020-05-10") |>
          dygraphs::dyLegend(show = "follow", width = 400) |>
          dygraphs::dyCSS(system.file("geocovidapp/www/legend.css",
            package = "geocovidapp"
          ))
      })

      fecha_formato <- shiny::reactive({
        # Agrego un dia por default para que renderice si no hay otras fechas.
        if (is.null(fechas$casos_covid())) {
          format("2020-05-12", format = "%Y-%m-%d")
        } else {
          fecha <- fechas$casos_covid()
          formatted_date(fecha = fecha)
        }
      })
      
      # Texto que se ve en la panatalla por sobre el mapa
      output$diaselec <- renderText({
        paste(
          "Movilidad ciudadana",
          if (input$tipo_tab == "pc") {
            "prepandemia"
          } else {
            "semanal"
          },
          "para", if (input$momento == "criterio") {
            "el promedio mañana y tarde"
          } else {
            "la noche"
          },
          "de", format(fecha_formato(), format = "%d-%m-%Y")
        )
      })

      # Datos de casos de COVID para el bubble map
      sisa <- reactive({
        # centroides_mapa <- st_read("data/shapefiles_baires_amba/centroides_mapa.gpkg") # incluye caba

        comunas <- data_sisa |>
          dplyr::filter(residencia_provincia_nombre == "CABA" &
            fecha_enfermo == fecha_formato()) |>
          dplyr::group_by(residencia_provincia_nombre) |>
          dplyr::summarize(n_casos = dplyr::n()) |>
          dplyr::rename("partido" = residencia_provincia_nombre)

        comunas[1, "partido"] <- "Capital Federal"

        casos_diarios <- data_sisa |>
          dplyr::filter(
            residencia_provincia_nombre == "Buenos Aires" &
              fecha_enfermo == fecha_formato()
          ) |> # combino horarios
          dplyr::group_by(residencia_departamento_nombre) |>
          dplyr::summarize(n_casos = dplyr::n()) |>
          dplyr::rename(partido = residencia_departamento_nombre) |>
          rbind(comunas)

        # 3. Grafico
        # uso un left_join porque ya casos_darios_partido no es un sf data.frame
        sisa <- casos_diarios |>
          dplyr::left_join(centroides_mapa,
            by = c("partido")
          ) |>
          tidyr::drop_na(.data$n_casos) |>
          dplyr::arrange(dplyr::desc(.data$n_casos)) |>
          dplyr::mutate(crit_covid = dplyr::case_when(
            10 >= .data$n_casos &
              .data$n_casos >= 1 ~ "1 - 10",
            100 >= .data$n_casos &
              .data$n_casos > 10 ~ "10 - 100",
            .data$n_casos > 100 ~ "Más de 100"
          )) |>
          dplyr::mutate(crit_covid = forcats::fct_relevel(
            .data$crit_covid,
            c(
              "1 - 10",
              "10 - 100",
              "Más de 100"
            )
          ))
        sisa
      })

     # Extraigo la informacion de SQL
      px_data_mapa <- shiny::reactive({
        fecha_input <- fecha_formato() # Esto es output de dygraph
        tipo_tab_input <- input$tipo_tab

        # Filtro las condiciones antes de la query
        # Bajar la base de datos completa tarda mucho tiempo.
        query <- paste0(
          "SELECT * FROM px_prom.px_prom WHERE ",
          "fecha = '", fecha_input, "' AND ",
          "tipo_de_raster = '", tipo_tab_input, "'"
        )
        sf::st_read(pool, query = query)
      })

       # Uso los datos de covid para estimar los valores
      burbujas_plot <- shiny::reactive({
        tam_burb <- sisa() |>
          dplyr::group_by(.data$crit_covid) |>
          dplyr::arrange(dplyr::desc(.data$n_casos))

        # Rangos para n_casos que se corresponden con los tamaños de las burbujas
        q <- quantile(tam_burb$n_casos,
          probs = c(0.5, 0.9)
        )

        size_small <- as.integer(q[[1]])
        size_medium <- as.integer(q[[2]])

        # Genero dataset separados para n_casos
        tam_burb_small <- subset(
          tam_burb,
          n_casos <= size_small
        )

        tam_burb_medium <- subset(
          tam_burb,
          n_casos > size_small &
            n_casos <= size_medium
        )

        tam_burb_large <- subset(
          tam_burb,
          n_casos > size_medium
        )

        px_data_mapa <- px_data_mapa()

        # fun_mapa_bsas_covid_burbujas.R
        geocovidapp::mapa_burbujas(
          data_sf = px_data_mapa,
          color_var = input$momento,
          data_burb_peq = tam_burb_small,
          data_burb_med = tam_burb_medium,
          data_burb_gra = tam_burb_large,
          cent_x = "X",
          cent_y = "Y",
          n_casos = "n_casos",
          partido = "partido",
          burb_peq = size_small,
          burb_med = size_medium
        )
      })
      
      # Mapa de burbujas.
      # Lo saque de esta seccion para poder reutilizarlo
      # En zoom_map
      output$burbujas_map <- plotly::renderPlotly({
        burbujas_plot()
      })

      # Detalle del mapa que se ve a un costado
      output$zoom_map <- plotly::renderPlotly({
        bsas_part <- bsas |>
          dplyr::filter(.data$partido == input$partidos)
        part_bbox <- sf::st_bbox(bsas_part)

        m <- list(
          l = 0,
          r = 0,
          b = 0,
          t = 0
        )

        # Reuso la base del mapa ya creado
        burbujas_plot() |>
          plotly::add_sf(
            stroke = I("#004643"),
            data = subset(
              px_data_mapa(),
              partido == input$partidos
            ),
            color = I("#ffffff00"),
            line = list(width = 4),
            hoverinfo = "skip"
          ) |>
          plotly::layout(
            showlegend = FALSE,
            margin = m,
            xaxis = list(range = c(
              part_bbox$xmin - 0.5,
              part_bbox$xmax + 0.5
            )),
            yaxis = list(range = c(
              part_bbox$ymin - 0.5,
              part_bbox$ymax + 0.5
            ))
          ) |>
          plotly::config(displayModeBar = FALSE)
      })
    }
  )
}