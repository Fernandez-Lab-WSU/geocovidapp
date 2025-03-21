#' IU: Mapa prov. de Buenos Aires por departamento
#'
#' @description
#' Este mapa se encuentra en el tab 3 de GeoCovid app.
#' 
#' @param id Module name
#'
#' @return Elementos de interfaz de usuario del tab 3
#' @export
MapaCovidDepartamentos_UI <- function(id){

  bsas_lista <-  c("25 de Mayo", "9 de Julio", "Adolfo Alsina", "Adolfo Gonzales Chaves", 
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
                   "Villarino", "Zárate")
  
  ns <- NS(id)

    shiny::tagList(
        bslib::layout_sidebar(
          fluidRow(column(6,
             tags$h5(textOutput(ns('diaselec'))),
             tags$h6('provincia de Buenos Aires'),
             br(),
             shinycssloaders::withSpinner(
               plotly::plotlyOutput(ns("burbujas_map"),
                            height = "600px"
               ),
               type = 2,
               color = 'lightgrey',
               color.background = 'white')
     ),
      column(width = 6,
        tags$h5("Casos de COVID-19"),
        tags$h6('Click en el primer grafico para seleccionar la fecha del mapa'),
        Dygraph_UI(
                   ns('casos_covid_interno')),
               br(),
               tags$h5("Promedio de movilidad diurna y nocturna"),
        shinycssloaders::withSpinner(
          dygraphs::dygraphOutput(ns('raster_analisis'),
                      width = 600,
                      height = 100),
          type = 2,
          color = 'lightgrey',
          color.background = 'white'),
        br(),
        tags$h5("Vision ampliada por departamento"),
        shinycssloaders::withSpinner(
        plotly::plotlyOutput(
                     ns("zoom_map"),
                     height = 250),
        type = 2,
        color = 'lightgrey',
        color.background = 'white')
              )
               ),


  sidebar = bslib::sidebar(
                   position = "right",
                   shiny::selectInput(ns("partidos"),
                                      label = "Selecciona el partido",
                                      choices = bsas_lista,
                                      selected = bsas_lista[1],
                                      width = '100%'),
                   shiny::radioButtons(ns("tipo_tab"),
                                       label = 'Selecciona el raster',
                                       choices = c("Prepandemia" = 'pc',
                                                   "Semanal" = '7dpc'),
                                       selected = 'pc'),
                   shiny::radioButtons(ns("momento"),
                                       label = 'Selecciona el momento del dia',
                                       choices = c("Mañana-Tarde" = 'criterio',
                                                   "Noche" = 'criterio_noche'),
                                       selected = 'criterio')
                 )
  ))


}


#' Servidor: Mapa prov. de Buenos Aires por departamento
#'
#' @description
#' Este mapa se encuentra en el tab 3 de GeoCovid app.
#'
#' @param id Module name
#' @param data_sisa Dataframe con los casos diarios reportados de COVID-19.
#' @param base_raster Dataframe que lista todos los rasters y desagrega en 
#' sus columnas características de interes, como si son rasters de 
#' AMBA o Buenos Aires, si el cambio porcentual es semanal o prepandemia 
#' o el momento del día que representan. 
#' @param bsas Dataset de clase sf con los partidos de Buenos Aires.
#' @param amba_caba String. Vector con los nombres de los partidos 
#' que conforman el AMBA sin las comunas de Provincia de Buenos Aires.
#'
#' @importFrom stats quantile
#' @return Mapas y gráficos del tab 3.
#' @export
MapaCovidDepartamentos_Server <- function(id,
                                          data_sisa,
                                          base_raster,
                                          bsas,
                                          amba_caba){
  moduleServer(
    id,
    function(input, output, session) {



  fechas <- Dygraph_Server('casos_covid_interno',
                                     amba =  amba_caba,
                                     data_sisa = data_sisa,
                                     base_raster = base_raster,
                                     part = reactive({ input$partidos }),
                                     area = reactiveVal({ 'baires'}))

  
  # Datos de movilidad por partido como media de los pixeles
  px_data <-  shiny::reactive({
    
    pool <- pool::dbPool(
      drv = RPostgres::Postgres(),
      dbname = "geocovidapp_db",
      user = Sys.getenv("USER"),
      password = Sys.getenv("PASSWORD"),
      port = "5432",
      host = "44.226.244.60"
    )
    onStop(function() {
      pool::poolClose(pool)
    })
    
    # PRIMERA VERSION
    # Esta version de la app no tiene conectados todos los datos.
    # mini_data_sisa <- data_sisa |> filter(fecha_enfermo >= as.Date(min(base_raster$fecha)),
    #                                       fecha_enfermo <= as.Date(max(base_raster$fecha)))
    #
    # write_csv(mini_data_sisa, "mini_data_sisa_deploy.csv")
    
     sf::st_read(pool, query = 'select * from px_prom.px_prom')

  })
  


  output$raster_analisis <- dygraphs::renderDygraph({

  
    pxdy <- px_data()
    
#     pxd_baires <- pxd_baires |>
#       dplyr::filter(.data$fecha < '2020-05-15' &
#                     .data$fecha > '2020-05-08') # sera replazado con los datos finales
# print(pxd_baires)
#      pxdy <-  pxd_baires |>
#         dplyr::filter(.data$partido == input$partidos,
#                       .data$tipo_de_raster == input$tipo_tab)

     a <- pxdy[, c('fecha', 'px_mean_dianoche')]
     b <- pxdy[, c('fecha', 'mañana_8')]
     c <- pxdy[, c('fecha', 'tarde_16')]
     d <- pxdy[, c('fecha', 'noche_0')]

      px_baires_dianoche <- xts::xts(a$px_mean_dianoche, 
                                     order.by = a$fecha)
      px_baires_8  <- xts::xts(b$mañana_8,
                               order.by = b$fecha)
      px_baires_16  <- xts::xts(c$tarde_16, 
                                order.by = c$fecha)
      px_baires_0  <- xts::xts(d$noche_0,
                               order.by = d$fecha)

      px_baires <- cbind(px_baires_dianoche,
                         px_baires_0,
                         px_baires_8,
                         px_baires_16)


base::colnames(px_baires)[1] <- as.character("PromedioDiaYTarde")
base::colnames(px_baires)[2] <- as.character("Noche")

      dygraphs::dygraph(data = px_baires ) |>
        dygraphs::dySeries(c("px_baires_8",
                             "PromedioDiaYTarde",
                             "px_baires_16")) |>
        dygraphs::dySeries("Noche") |>
        dygraphs::dyOptions(labelsUTC = TRUE,
                  drawGrid = FALSE
        ) |>
        dygraphs::dyAxis("y",
               label = "% de casos por partido") |>
        dygraphs::dyHighlight(highlightCircleSize = 3,
                    highlightSeriesBackgroundAlpha = 0.4,
                    hideOnMouseOut = TRUE) |>
        dygraphs::dyEvent("2020-04-13")|>
        dygraphs::dyEvent("2020-04-26") |>
        dygraphs::dyEvent("2020-05-10") |>
        dygraphs::dyLegend(show = 'follow', width = 400
                 ) |>
        dygraphs::dyCSS(system.file("geocovidapp/www/legend.css", 
                                    package = "geocovidapp"))



  })

  fecha_formato <- shiny::reactive({

  # Agrego un dia por default para que renderice si no hay otras fechas.
    if(is.null(fechas$casos_covid())){

      format('2020-05-12', format = "%Y-%m-%d")


    }else{
      fecha = fechas$casos_covid()
      formatted_date(fecha = fecha) 

    }


  })

  output$diaselec <- renderText({

    paste('Movilidad ciudadana',
          if(input$tipo_tab == 'pc'){ 'prepandemia'}else{ 'semanal'},
          'para', if(input$momento == 'criterio'){ 'el promedio mañana y tarde'}
          else{ 'la noche'},
          'de', format(fecha_formato(), format = "%d-%m-%Y"))
  })

      # Datos de casos de COVID para el bubble map
      sisa <- reactive({

        # centroides_mapa <- st_read("data/shapefiles_baires_amba/centroides_mapa.gpkg") # incluye caba
        
         comunas <- data_sisa |>
          dplyr::filter(residencia_provincia_nombre == 'CABA' &
                        fecha_enfermo == fecha_formato()) |>
          dplyr::group_by(residencia_provincia_nombre) |>
          dplyr::summarize(n_casos = dplyr::n()) |>
          dplyr::rename('partido' = residencia_provincia_nombre)

        comunas[1,'partido'] <- "Capital Federal"

        casos_diarios <- data_sisa |> 
                                     dplyr::filter(
                                       residencia_provincia_nombre == 'Buenos Aires' &
                                       fecha_enfermo == fecha_formato() ) |>  #combino horarios
          dplyr::group_by(residencia_departamento_nombre) |>
          dplyr::summarize(n_casos = dplyr::n()) |>
          dplyr::rename(partido = residencia_departamento_nombre) |>
          rbind(comunas)

# 3. Grafico
# uso un left_join porque ya casos_darios_partido no es un sf data.frame

    
        
        sisa <- casos_diarios |>
          dplyr::left_join(centroides_mapa,
                           by = c('partido')) |>
          tidyr::drop_na(.data$n_casos) |>
          dplyr::arrange(dplyr::desc(.data$n_casos)) |>
          dplyr::mutate(crit_covid = dplyr::case_when(
                                           10 >= .data$n_casos &
                                           .data$n_casos >= 1 ~ "1 - 10",
                                           100 >= .data$n_casos &
                                           .data$n_casos > 10 ~ "10 - 100",
                                           .data$n_casos > 100 ~ "Más de 100")) |>
          dplyr::mutate(crit_covid = forcats::fct_relevel(.data$crit_covid, 
                                                          c("1 - 10",
                                                           "10 - 100",
                                                           "Más de 100")))
        print('sisa')
        print(sisa)
         sisa

      })


burbujas_plot <- shiny::reactive({

  m <- list(
    l = 0,
    r = 0,
    b = 0,
    t = 0
  )

  colors <- c("mas de 40"="#67001F",
              "40 - 30"="#B2182B",
              "30 - 20"="#D6604D",
              "20 - 10"="#F4A582",
              "10 - 1"="#FDDBC7",
              "sin cambios"="#F7F7F7",
              "-1 - -10" ="#D1E5F0",
              "-10 - -20"="#92C5DE",
              "-20 - -30"="#4393C3",
              "-30 - -40"="#2166AC",
              "menor a -40"="#053061")
  
  tam_burb <- sisa() |> 
    dplyr::group_by(.data$crit_covid) |>
    dplyr::arrange(dplyr::desc(.data$n_casos)) 

  
  print(tam_burb)
  print('tam_burb')
  # Rangos para n_casos que se corresponden con los tamaños de las burbujas
  q <- quantile(tam_burb$n_casos,
                probs = c(0.5, 0.9))

  size_small <- as.integer(q[[1]])
  size_medium <- as.integer(q[[2]])
  
  print('tam_burb')
  print(tam_burb)
  
  print('size_small')
  print(size_small)

  # Create three separate data frames based on the ranges of n_casos
  tam_burb_small <- subset(tam_burb,
                           n_casos <= size_small) 
  
 # tam_burb_small$X <- sf::st_coordinates(tam_burb_small)[, 1]  # Extract X coordinates
 # tam_burb_small$Y <- sf::st_coordinates(tam_burb_small)[, 2]  # Extract Y coordinates
  
  tam_burb_medium <- subset(tam_burb,
                            n_casos > size_small &
                            n_casos <= size_medium) 
  
 # tam_burb_medium$X <- sf::st_coordinates(tam_burb_medium)[, 1]  # Extract X coordinates
 # tam_burb_medium$Y <- sf::st_coordinates(tam_burb_medium)[, 2]  # Extract Y coordinates
  
  
  tam_burb_large <- subset(tam_burb,
                           n_casos > size_medium)  
  
  #tam_burb_large$X <- sf::st_coordinates(tam_burb_large)[, 1]  # Extract X coordinates
 # tam_burb_large$Y <- sf::st_coordinates(tam_burb_large)[, 2]  # Extract Y coordinates
  
  print('tar_burb_small')
  print(tam_burb_small)
  print(tam_burb_medium)
  print(tam_burb_large)
 plotly::plot_ly() |>
   plotly::add_sf(stroke = I("#95B2C6"),
          data = px_data(),
          split = ~base::get(input$momento),
          name = ~base::get(input$momento),
          color = ~base::get(input$momento),
          colors = colors,
          stroke = I("transparent"),
          hoveron = "fills",
          hoverinfo = 'name',
          legendgroup = 'criterio',
          legendgrouptitle = list(text = 'Promedio % de cambio',
                                  font = list(size = 15,
                                  family = "Work Sans",
                                  color = "black"))
   ) |>
   plotly::add_markers(
     data = tam_burb_small,
     type = 'scatter',
     mode = 'markers',
     x = ~X,
     y = ~Y,
     name = paste('1 -', size_small),
     legendgroup = ~n_casos,
     legendgrouptitle = list(text = 'Casos COVID-19',
                             font = list(size = 15,
                                         family = "Work Sans, sans-serif",
                                         color = "black")),
     marker = list(
       color = 'rgb(196, 193, 192)',
       size = ~n_casos,
       sizemode = 'area', # Importante!
       sizeref = 0.5,
       opacity = 0.7,
       line = list(
         color = 'rgb(24, 40, 37)',
         width = 2)),
     text = ~partido,
     #  hoverinfo = 'text',
     hovertemplate = paste("<b>%{text}</b><br>",
                           "Nro. de casos: %{marker.size}",
                           "<extra></extra>")
   ) |>
   plotly::add_markers(
     data = tam_burb_medium,
     type = 'scatter',
     mode = 'markers',
     x = ~X, # centroide
     y = ~Y, # centroide
     name = paste(size_small, '-', size_medium),
     legendgroup = ~n_casos,
     legendgrouptitle = list(#text = 'Casos COVID-19',
                             font = list(size = 15,
                                         family = "Work Sans, sans-serif",
                                         color = "black")),
     marker = list(
       color = 'rgb(196, 193, 192)',
       size = ~n_casos,
       sizemode = 'area', # Importante!
       sizeref = 0.5,
       opacity = 0.7,
       line = list(
         color = 'rgb(24, 40, 37)',
         width = 2)),
     text = ~partido,
     #  hoverinfo = 'text',
     hovertemplate = paste("<b>%{text}</b><br>",
                           "Nro. de casos: %{marker.size}",
                           "<extra></extra>")
   ) |>
   plotly::add_markers(
     data = tam_burb_large,
     type = 'scatter',
     mode = 'markers',
     x = ~X,
     y = ~Y,
     name = paste('Más de', size_medium),
     legendgroup = ~n_casos,
     legendgrouptitle = list(#text = 'Casos COVID-19',
                             font = list(size = 15,
                                         family = "Work Sans, sans-serif",
                                         color = "black")),
     marker = list(
       color = 'rgb(196, 193, 192)',
       size = ~n_casos,
       sizemode = 'area', # Importante!
       sizeref = 0.5,
       opacity = 0.7,
       line = list(
         color = 'rgb(24, 40, 37)',
         width = 2)),
     text = ~partido,
     #  hoverinfo = 'text',
     hovertemplate = paste("<b>%{text}</b><br>",
                           "Nro. de casos: %{marker.size}",
                           "<extra></extra>")
   ) |>
   plotly::layout(margin = m,
                  showlegend = TRUE,
          legend = list(font = list(size = 15,
                                    family = "Work Sans",
                                    color = "black"),
                        itemsizing  = 'trace',
                        groupclick = 'toggleitem', # permite que cada trace se seleccione individualmente
                        xref = 'paper',
                        yref = 'paper',
                        x = 1,
                        y = 0))




})
      output$burbujas_map <- plotly::renderPlotly({

burbujas_plot()

      })


output$zoom_map <-  plotly::renderPlotly({

bsas_part <- bsas |>
  dplyr::filter(.data$partido == input$partidos)
part_bbox <- sf::st_bbox(bsas_part)

m <- list(
   l = 0,
   r = 0,
   b = 0,
   t = 0

)

  burbujas_plot() |>
    plotly::add_sf(
      stroke = I("#004643"),
      data = subset(px_data(), 
                    partido == input$partidos),
      color = I("#ffffff00"),
      line = list( width = 4 ),
      hoverinfo='skip'
    ) |>
    plotly::layout(showlegend = FALSE,
           margin = m,
           xaxis = list(range = c(part_bbox$xmin - 0.5,
                                  part_bbox$xmax + 0.5)),
           yaxis = list(range = c(part_bbox$ymin - 0.5,
                                  part_bbox$ymax + 0.5))) |>
    plotly::config(displayModeBar = FALSE)

})


    })  }
