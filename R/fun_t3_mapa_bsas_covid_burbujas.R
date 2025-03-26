#' Genera un mapa interactivo con burbujas y polígonos coloreados
#'
#' @description Esta función crea un mapa interactivo utilizando `plotly`, 
#' que representa polígonos coloreados según una variable categórica y 
#' burbujas de diferentes tamaños según la cantidad de casos de COVID-19.
#'
#' @param data_sf Un `sf` data frame con los polígonos geográficos.
#' @param color_var Una cadena de texto que indica la columna de `data_sf` 
#' utilizada para definir los colores de los polígonos.
#' @param data_burb_peq Un data frame con las coordenadas y datos de burbujas pequeñas.
#' @param data_burb_med Un data frame con las coordenadas y datos de burbujas medianas.
#' @param data_burb_gra Un data frame con las coordenadas y datos de burbujas grandes.
#' @param cent_x Nombre de la columna con la coordenada X del centroide de las burbujas.
#' @param cent_y Nombre de la columna con la coordenada Y del centroide de las burbujas.
#' @param n_casos Nombre de la columna con la cantidad de casos representados por las burbujas.
#' @param partido Nombre de la columna con el nombre de la región o partido.
#' @param burb_peq Límite superior del rango para las burbujas pequeñas.
#' @param burb_med Límite superior del rango para las burbujas medianas.
#'
#' @return Un objeto `plotly` con el mapa interactivo.
#'
#' @examples
#' \dontrun{
#' mapa_burbujas(data_sf = px_data_mapa,
#'               color_var = "momento",
#'               data_burb_peq = tam_burb_small,
#'               data_burb_med = tam_burb_medium,
#'               data_burb_gra = tam_burb_large,
#'               cent_x = "X",
#'               cent_y = "Y",
#'               n_casos = "n_casos",
#'               partido = "partido",
#'               burb_peq = size_small,
#'               burb_med = size_medium)
#' }
#'
#' @export
mapa_burbujas <- function(data_sf,
                          color_var,
                          data_burb_peq,
                          data_burb_med,
                          data_burb_gra, 
                          cent_x, 
                          cent_y,
                          n_casos,
                          partido,
                          burb_peq,
                          burb_med){

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
  
plotly::plot_ly() |>
  plotly::add_sf(stroke = I("#95B2C6"),
                 data = data_sf,
                 split = as.formula(paste("~", color_var)),
                 name = as.formula(paste("~", color_var)),
                 color = as.formula(paste("~", color_var)),
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
    data = data_burb_peq,
    type = 'scatter',
    mode = 'markers',
    x = ~cent_x,
    y = ~cent_y,
    name = paste('1 -', burb_peq),
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
    data = data_burb_med,
    type = 'scatter',
    mode = 'markers',
    x = ~X, # centroide
    y = ~Y, # centroide
    name = paste(burb_peq, '-', burb_med),
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
    data = data_burb_gra,
    type = 'scatter',
    mode = 'markers',
    x = ~X,
    y = ~Y,
    name = paste('Más de', burb_med),
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

}
