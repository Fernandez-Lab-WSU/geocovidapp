#' Convertir datos de casos COVID-19 a formato xts
#'
#' Esta función filtra los datos de `geocovidapp::casos_covid_dpto` para una provincia específica,
#' renombra la columna de casos (`n`) con un nombre personalizado, y convierte el resultado a un objeto `xts`.
#'
#' @param serie Nombre de la provincia (por ejemplo, `"Buenos Aires"` o `"CABA"`), usado para filtrar los datos.
#' @param nombre_serie Nombre deseado para la columna de valores (por ejemplo, `"casos_bsas"`).
#'
#' @return Un objeto de clase `xts` con las fechas como índice y una columna con los casos, renombrada como `nombre_serie`.
#'
#' @examples
#' \dontrun{
#'   convertir_xts_serie("CABA", "casos_caba")
#' }
#'
#' @importFrom dplyr filter
#' @importFrom xts xts
#' @export
convertir_xts_serie <- function(serie, nombre_serie){
  
  # Bs As provincia o CABA
  data <-  geocovidapp::casos_covid_dpto |>
    dplyr::filter(.data$residencia_provincia_nombre == serie) 
  
  # Renombra columna 'n' a nombre_serie
  colnames(data)[colnames(data) == "n"] <- nombre_serie

  data_xts <- xts::xts(data,
                           order.by = data$fecha_enfermo)

  return(data_xts)
}
  
  
#' Add Custom Dygraph Events and Styling
#'
#' Enhances a dygraph plot by adding axis labels, interactivity options, events, and custom styling.
#'
#' This function takes a dygraph object and adds several enhancements, including:
#' - Y-axis label
#' - Custom options for grid and labels
#' - Interactive highlighting and crosshair
#' - Three vertical event lines with labels
#' - A custom legend
#' - External CSS styling
#'
#' @param plot A dygraph object created with \code{dygraphs::dygraph()}.
#'
#' @return A modified dygraph object with enhanced interactivity and visual elements.
#'
#' @examples
#' \dontrun{
#' library(dygraphs)
#' ts_data <- ts(rnorm(100), start = c(2020, 1), frequency = 52)
#' plot <- dygraph(ts_data)
#' dygraphs_events(plot)
#' }
#'
#' @importFrom dygraphs dyAxis dyOptions dyHighlight dyCrosshair dyEvent dyLegend dyCSS
#' @export

dygraphs_events <- function(plot){
  
    plot |>
    dygraphs::dyAxis("y",
                     label = "Nro. de casos") |>
    dygraphs::dyOptions(labelsUTC = TRUE,
                        drawGrid = FALSE
    ) |>
    dygraphs::dyHighlight(highlightCircleSize = 3,
                          highlightSeriesBackgroundAlpha = 0.4,
                          hideOnMouseOut = TRUE) |>
    dygraphs::dyCrosshair(direction = "vertical")  |>
    dygraphs::dyEvent("2020-04-26",
                      "a",
                      labelLoc = "bottom") |>
    dygraphs::dyEvent("2020-05-10",
                      "b",
                      labelLoc = "bottom") |>
    dygraphs::dyEvent("2020-06-07",
                      "c",
                      labelLoc = "bottom") |>
    dygraphs::dyLegend(show = "follow",
                       width = 400
    ) |>
    dygraphs::dyCSS(system.file("geocovidapp/www/legend.css", 
                                package = "geocovidapp"))
  
  
}