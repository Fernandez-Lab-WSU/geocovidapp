#' Generar Mapa para Reporte
#'
#' Crea un mapa con un raster reclasificado y coloreado por rangos de porcentaje de cambio.
#' El área mostrada corresponde al partido especificado. Se utiliza `ggplot2` y `tidyterra` para la visualización.
#'
#' @param part Nombre del partido a visualizar. Debe coincidir con la columna `partido` del objeto `geocovidapp::bsas_comunas`.
#' @param imagen Objeto raster de clase `SpatRaster` (de la librería `terra`) que contiene los datos a mapear.
#'
#' @return Un objeto `ggplot` que muestra el mapa temático del área seleccionada.
#' @export
mapa_reporte <- function(part, imagen) {
  shape_partido <- geocovidapp::bsas_comunas |> 
    dplyr::filter(partido == part) |>
    dplyr::pull(geometry)
  
  
  
  # 1. Definir cortes y colores
  breaks <- c(50, 40, 30, 20, 10, 1, -1, -10, -20, -30, -40, -50)
  colors <- c("#0000FF", "#0040FF", "#0080FF", "#00BFFF", "#00FFFF", "#FFFFFF", 
              "#FFCC00", "#FF9900", "#FF6600", "#FF3300", "#FF0000")
  
  # Invertir cortes para crear reglas válidas para classify()
  rcl <- cbind(breaks[-length(breaks)], breaks[-1], 1:(length(breaks)-1))
  
  # 2. Reclasificar el raster
  raster_binned <- terra::classify(imagen, rcl = rcl)
  
  # 3. Asignar etiquetas como factor usando un data.frame (evita el warning)
  labels_df <- data.frame(
    value = 1:(length(breaks) - 1),
    label = paste(rcl[,1], "to", rcl[,2])
  )
  
  levels(raster_binned) <- labels_df
  
  # Get bounding box of partido shapefile
  bbox <- sf::st_bbox(shape_partido)
  
  # 4. Visualizar con ggplot2
  ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = raster_binned,
                               ggplot2::aes(fill = label), alpha = 0.6) +
    ggplot2::geom_sf(data = shape_partido, 
                     fill = NA, color = "black", size = 0.6) +
    ggplot2::scale_fill_manual(
      values = setNames(colors, paste(rcl[,1], "to", rcl[,2])),
      name = "porcentaje de cambio", na.value = "transparent", drop = FALSE
    ) +
    ggplot2::coord_sf(
      xlim = c(bbox["xmin"], bbox["xmax"]),
      ylim = c(bbox["ymin"], bbox["ymax"]),
      expand = FALSE
    ) +
    ggplot2::theme_minimal()
}

#' Title
#'
#' @param partido 
#' @param fecha 
#'
#' @export
ggplot_casos_covid <- function(partido, fecha){
  
  # Extraigo los datos que me interesan
  data_acumulada <- geocovidapp::data_sisa |>
    dplyr::group_by(residencia_provincia_nombre, fecha_enfermo) |>
    dplyr::summarise(casos_dia = n(), .groups = "drop_last") |> 
    dplyr::mutate(fecha_enfermo = as.Date(fecha_enfermo)) 
  
  # Muestro distinta informacion dependiendo si el partido es o no na comuna
  if(stringr::str_starts(partido, "Comuna")){
    data_plot1 <- data_acumulada |> 
      dplyr::filter(
        residencia_provincia_nombre == "CABA")
  }else{
    data_plot1 <-  data_acumulada |> 
      dplyr::filter(
        residencia_provincia_nombre %in% c("Buenos Aires", "CABA"))
  }
  # Personalizar colores
  colores_personalizados <- c(
    "Buenos Aires" = "#186E8B",
    "CABA" = "#301A4B"
  )
  
  # Elegir fecha para la línea vertical
  fecha_evento <- as.Date(fecha)
  

  ggplot(data_plot1, aes(x = fecha_enfermo, y = casos_dia, 
                        color = residencia_provincia_nombre)) +
    geom_line(size = 0.5) +
    geom_vline(xintercept = as.numeric(fecha_evento), linetype = "dashed", color = "black") +
    scale_color_manual(values = colores_personalizados) +
    scale_x_date(
      date_breaks = "1 month",               # Etiquetas cada mes
      date_labels = "%b %Y",                 # Formato: "Jun 2021"
      expand = expansion(mult = c(0.01, 0.01))  # Evita márgenes excesivos
    ) +
    labs(
      title = "Casos acumulados por provincia",
      x = "Fecha de inicio de síntomas",
      y = "Casos acumulados",
      color = "Provincia"
    ) +
    theme_minimal()
  
}
