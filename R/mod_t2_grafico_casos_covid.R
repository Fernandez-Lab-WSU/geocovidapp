#' Interfaz de usuario: Gráficos de casos de covid por provincia y departamento
#'
#' @param id Module name
#' @import shiny
#' @return Dos gráficos de casos de COVID-19 en el tiempo.
#' @export
Dygraph_UI <- function(id) {
  
  ns <- NS(id)
  shiny::tagList(

    fluidRow(
      column(9,
      shinycssloaders::withSpinner(
      dygraphs::dygraphOutput(ns("casos_prov"),
                            width = 600,
                            height = 120),
      type = 2,
      color = 'lightgrey',
      color.background = 'white'),
    shinycssloaders::withSpinner(
      dygraphs::dygraphOutput(ns("casos_dpto"),
                              width = 600,
                              height = 100),
      type = 2,
      color = 'lightgrey',
      color.background = 'white')),
    column(3,
        # textOutput(ns('l1')),
        # br(),
        # br(),
        # br(),
        # br(),
        # br(),
        # textOutput(ns('l2'))
        htmltools::tags$div(
          style = "margin-top: 10px; font-size: 14px;",
          htmltools::tags$ol(
            htmltools::tags$li("Aislamiento estricto"),
            htmltools::tags$li("Aislamiento preventivo"),
            htmltools::tags$li("Aislamiento por segmentacion geografica"),
            htmltools::tags$li("Aislamiento con reapertura progresiva"),
            htmltools::tags$li("Nueva normalidad*")
          )
        )
  )))
}

#' Servidor: Gráficos de casos de covid por provincia y departamento
#'
#' @param id Module name
#' @param data_sisa Dataframe con los casos diarios reportados de COVID-19.
#' @param amba_reducido_names String. Vector con los nombres de los partidos 
#' que conforman el AMBA.
#' @param base_raster Dataframe que lista todos los rasters y desagrega en 
#' sus columnas características de interes, como si son rasters de 
#' AMBA o Buenos Aires, si el cambio porcentual es semanal o prepandemia 
#' o el momento del día que representan.
#' @param area El raster puede corresponder a Buenos Aires provincia o a AMBA.
#' @param part Partido de la provincia de Buenos Aires, seleccionado en otro 
#' módulo.
#'
#' @return La fecha seleccionada por el usuario en el grafico de COVID-19
#' @export
Dygraph_Server <- function(id, 
                           data_sisa, 
                           amba_reducido_names,
                           base_raster, 
                           area,
                           part){
  shiny::moduleServer(id,
               function(input, output, session){

grafico_dygraph <- reactive({

  
# Cuento los casos por lugar de residencia para los rasters existentes
  tibble::as_tibble(data_sisa) |>
    dplyr::group_by(.data$fecha_enfermo, 
                    .data$residencia_provincia_nombre) |>
    dplyr::summarize(n = dplyr::n()) |>
    dplyr::filter(.data$fecha_enfermo >= min(as.Date(base_raster$fecha)),
                  .data$fecha_enfermo <= max(as.Date(base_raster$fecha))) |>
    dplyr::mutate(fecha_enfermo = as.Date(.data$fecha_enfermo))

})

data_xts <- reactive({
             # genero las series que despues va a utilizar
   dbsas <-  grafico_dygraph() |>
             dplyr::filter(.data$residencia_provincia_nombre == 'Buenos Aires')|>
             dplyr::select('BsAs' = n)

   dcaba <-  grafico_dygraph() |>
             dplyr::filter(.data$residencia_provincia_nombre == 'CABA') |>
             dplyr::select('CABA' = n)

data_xts_bsas <- xts::xts(dbsas,
                         order.by = dbsas$fecha_enfermo)
data_xts_caba <- xts::xts(dcaba,
                         order.by = dcaba$fecha_enfermo)


# Si el usuario elige una comuna, no se va a visualizar una tercera linea
# Ya que no hay muchos datos de covid reportados por comuna, consideramos CABA en general
# Pero, se va apoder visualizar la comuna en el mapa si se quiere

 if(part() %in% amba_reducido_names &
    stringr::str_detect(part(),
                        pattern = '^Comuna') | # si quiero visualizar la comuna
    dim(dplyr::filter(data_sisa,
                      .data$residencia_departamento_nombre == part()))[1] == 0){ # si no hay casos en el partido

                   data_xts <- cbind(data_xts_caba, data_xts_bsas)
                   data_xts$fecha_enfermo <- NULL
                   data_xts$fecha_enfermo.1 <- NULL

                   list(caba = data_xts_caba,
                        bsas = data_xts_bsas,
                        cabaybsas = data_xts)
 }else{

   dpartido <- data_sisa |>
     dplyr::filter(.data$residencia_departamento_nombre == part()) |>
     dplyr::count(.data$fecha_enfermo) |>
     dplyr::filter(.data$fecha_enfermo >= base::min(as.Date(base_raster$fecha, origin = "1970-01-01")),
                   .data$fecha_enfermo <= base::max(as.Date(base_raster$fecha, origin = "1970-01-01"))) |>
     dplyr::mutate(fecha_enfermo = as.Date(.data$fecha_enfermo))

   # Que en la leyenda aparezca el nombre del partido
   base::colnames(dpartido)[2] <- as.character(part())

   data_xts_partido  <- xts::xts(dpartido,
                                 order.by = dpartido$fecha_enfermo) # serie temporal


   data_xts <- cbind(data_xts_caba, data_xts_bsas)
   data_xts$fecha_enfermo <- NULL
   data_xts$fecha_enfermo.1 <- NULL


   list(partido = data_xts_partido,
        caba = data_xts_caba,
        bsas = data_xts_bsas,
        cabaybsas = data_xts)



 }

 })

grafico_casos_prov <- reactive({ 
  if(part() %in% amba_reducido_names &
     stringr::str_detect(as.character(part()),
                         pattern = '^Comuna') | # si quiero ver datos de comuna
     dim(dplyr::filter(data_sisa,
                       .data$residencia_departamento_nombre == part()))[1] == 0){ # si no hay casos en el partido
    
    data_plot <- data_xts()
    
    dygraphs::dygraph(data_plot$bsas,
                      group = 'A')  |>
      dygraphs::dySeries("BsAs",
                         color = '#186E8B') |>
      dygraphs::dyAxis("y",
                       label = "Nro. de casos") |>
      dygraphs::dyOptions(labelsUTC = TRUE,
                          drawGrid = FALSE
      ) |>
      dygraphs::dyHighlight(highlightCircleSize = 3,
                            highlightSeriesBackgroundAlpha = 0.4,
                            hideOnMouseOut = TRUE) |>
      dygraphs::dyCrosshair(direction = "vertical")  |>
      dygraphs::dyEvent("2020-04-13",
                        "2",
                        labelLoc = "bottom")|>
      dygraphs::dyEvent("2020-04-26",
                        "3",
                        labelLoc = "bottom") |>
      dygraphs::dyEvent("2020-05-10",
                        "4",
                        labelLoc = "bottom") |>
      dygraphs::dyEvent("2020-06-07",
                        "5",
                        labelLoc = "bottom") |>
      dygraphs::dyLegend(show = "follow",
                         width = 400
      ) |>
      dygraphs::dyCrosshair(direction = "vertical") |>
    dygraphs::dyAnnotation("2020-04-13", text = "2", tooltip = "Inicio de medidas X") |> 
    dygraphs::dyAnnotation("2020-04-26", text = "3", tooltip = "Cambios en fase Y") |> 
    dygraphs::dyAnnotation("2020-05-10", text = "4", tooltip = "Ajustes en Z") |> 
    dygraphs::dyAnnotation("2020-06-07", text = "5", tooltip = "Reapertura parcial") |> 
      dygraphs::dyCSS(system.file("geocovidapp/www/legend.css", 
                                  package = "geocovidapp"))
    
  }else{
    
    data_plot <- data_xts()
    
    dygraphs::dygraph(data_plot$cabaybsas,
                      group = 'A')  |>
      dygraphs::dySeries("BsAs",
                         color = '#186E8B') |>
      dygraphs::dySeries("CABA",
                         color = '#301A4B') |>
      dygraphs::dyAxis("y",
                       label = "Nro. de casos") |>
      dygraphs::dyOptions(labelsUTC = TRUE,
                          drawGrid = FALSE
      ) |>
      dygraphs::dyHighlight(highlightCircleSize = 3,
                            highlightSeriesBackgroundAlpha = 0.4,
                            hideOnMouseOut = TRUE) |>
      dygraphs::dyCrosshair(direction = "vertical")  |>
      dygraphs::dyEvent("2020-04-13", 
                        "2",
                        labelLoc = "top")|>
      dygraphs::dyEvent("2020-04-26", 
                        "3",
                        labelLoc = "top") |>
      dygraphs::dyEvent("2020-05-10", 
                        "4",
                        labelLoc = "top") |>
      dygraphs::dyEvent("2020-06-07", 
                        "5",
                        labelLoc = "top") |>
      dygraphs::dyCrosshair(direction = "vertical") |>
      dygraphs::dyAnnotation("2020-04-13", text = "2", tooltip = "Inicio de medidas X") |> 
     dygraphs::dyAnnotation("2020-04-26", text = "3", tooltip = "Cambios en fase Y") |> 
     dygraphs::dyAnnotation("2020-05-10", text = "4", tooltip = "Ajustes en Z") |> 
     dygraphs::dyAnnotation("2020-06-07", text = "5", tooltip = "Reapertura parcial") |> 
      dygraphs::dyLegend(
        show = "follow",
        width = 400
      ) |>
      dygraphs::dyCSS(system.file("geocovidapp/www/legend.css", 
                                  package = "geocovidapp"))
    
  }
  })

  output$casos_prov <- dygraphs::renderDygraph({

   grafico_casos_prov()

})

grafico_casos_dpto <-  reactive({
  data_plot <- data_xts()
  
  if(as.character(area()) == 'amba' &
     stringr::str_detect(as.character(part()),
                         pattern = '^Comuna') || # si quiero ver datos de comuna
     dim(dplyr::filter(data_sisa,
                       .data$residencia_departamento_nombre == part()))[1] == 0){ # si no hay casos en el partido
    
    
    dygraphs::dygraph(data_plot$caba,
                      group = 'A')  |>
      dygraphs::dySeries("CABA",
                         color = '#301A4B') |>
      dygraphs::dyAxis("y",
                       label = "Nro. de casos") |>
      dygraphs::dyOptions(labelsUTC = TRUE,
                          drawGrid = FALSE
      ) |>
      dygraphs::dyHighlight(highlightCircleSize = 3,
                            highlightSeriesBackgroundAlpha = 0.4,
                            hideOnMouseOut = TRUE) |>
      dygraphs::dyEvent("2020-04-13")|>
      dygraphs::dyEvent("2020-04-26") |>
      dygraphs::dyEvent("2020-05-10") |>
      dygraphs::dyLegend(
        show = "follow",
        width = 400
      ) |>
      dygraphs::dyCSS(system.file("www/legend.css", 
                                  package = "geocovidapp"))
    
    
  }else{
    
    
    dygraphs::dygraph(data_plot$partido,
                      group = 'A') |>
      dygraphs::dySeries(colnames(data_plot$partido)[2], # nombre del partido
                         color = '#6C9AC6') |>
      dygraphs::dyAxis("y",
                       label = "Nro. de casos") |>
      dygraphs::dyOptions(labelsUTC = TRUE,
                          drawGrid = FALSE
      ) |>
      dygraphs::dyHighlight(highlightCircleSize = 3,
                            highlightSeriesBackgroundAlpha = 0.4,
                            hideOnMouseOut = TRUE) |>
      dygraphs::dyEvent("2020-04-13")|>
      dygraphs::dyEvent("2020-04-26") |>
      dygraphs::dyEvent("2020-05-10") |>
      dygraphs::dyLegend(
        show = "follow",
        width = 400
      ) |>
      dygraphs::dyCSS(system.file("geocovidapp/www/legend.css", 
                                  package = "geocovidapp"))
  }
               })
  
output$casos_dpto <- dygraphs::renderDygraph({

  grafico_casos_dpto()

  })

return(
  list(
    casos_covid = reactive({ input$casos_prov_click$x }),
    grafico_casos_prov = reactive({ grafico_casos_prov() }),
    grafico_casos_dpto = reactive({ grafico_casos_dpto() })
  )
)

})}

