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
  
  # 1. Definir cortes con Inf y -Inf
  breaks <- c(-Inf, -40, -30, -20, -10, -1, 1, 10, 20, 30, 40, Inf)
  
  colors <- c(
    "#0000FF", "#0040FF", "#0080FF", "#00BFFF", "#00FFFF", "#FFFFFF",
    "#FFCC00", "#FF9900", "#FF6600", "#FF3300", "#FF0000"
  )
  
  # 2. Matriz de reclasificación
  rcl <- cbind(breaks[-length(breaks)], breaks[-1], 1:(length(breaks) - 1))
  
  # 3. Reclasificar el raster
  raster_binned <- terra::classify(imagen, rcl = rcl)
  
  # 4. Etiquetas personalizadas
  etiquetas <- paste(breaks[-length(breaks)], "to", breaks[-1])
  etiquetas[1] <- "Aumentó más del 40%"    
  etiquetas[6] <- "Sin cambio"               # -1 a 1
  etiquetas[length(etiquetas)] <- "Disminuyó bajo -40%"  
  
  labels_df <- data.frame(
    value = 1:(length(breaks) - 1),
    label = etiquetas
  )
  
  # 5. Forzar factor con todos los niveles
  raster_binned <- terra::as.factor(raster_binned)
  levels_all <- labels_df
  raster_binned <- terra::setValues(raster_binned, terra::values(raster_binned))
  levels(raster_binned) <- levels_all  # 👈 fuerza que todos los niveles estén definidos
  
  bbox <- sf::st_bbox(shape_partido)
  
  # 6. Visualización
  ggplot2::ggplot() +
    tidyterra::geom_spatraster(
      data = raster_binned,
      ggplot2::aes(fill = label),
      alpha = 0.6
    ) +
    ggplot2::geom_sf(
      data = shape_partido,
      fill = NA, color = "black", size = 0.6
    ) +
    ggplot2::scale_fill_manual(
      values = setNames(colors, etiquetas),
      name = "Porcentaje de cambio",
      na.translate = FALSE,
      drop = FALSE  # 👈 evita que se recorte la leyenda
    ) +
    ggplot2::coord_sf(
      xlim = c(bbox["xmin"], bbox["xmax"]),
      ylim = c(bbox["ymin"], bbox["ymax"]),
      expand = FALSE
    ) +
    ggplot2::theme_minimal()
}


#' Gráfico individual y comparativo de casos COVID
#'
#' @param partido Nombre del partido o comuna (ej. "La Matanza" o "Comuna 1")
#' @param fecha Fecha a marcar con una línea vertical (formato "YYYY-MM-DD")
#' @param partido_comunas Vector de nombres de comunas de CABA (ej. paste("Comuna", 1:15))
#'
#' @return Una lista con dos ggplots: `grafico_partido` y `grafico_comparativo`
#' @export
ggplot_casos_covid_doble <- function(partido, fecha, partido_comunas) {
  
  # 1. Datos base
  data_acumulada <- geocovidapp::data_sisa |>
    dplyr::group_by(residencia_provincia_nombre, fecha_enfermo) |>
    dplyr::summarise(casos_dia = n(), .groups = "drop_last") |> 
    dplyr::mutate(fecha_enfermo = as.Date(fecha_enfermo))
  
  # 2. Identificar si el partido es una comuna
  es_comuna <- stringr::str_starts(partido, "Comuna")
  fecha_evento <- as.Date(fecha)
  
  # 3. Colores
  colores_comparativo <- c("Buenos Aires" = "#186E8B", "CABA" = "#301A4B")
  color_partido <- "#6C9AC6"
  
  # --------------------------
  # GRAFICO 1: Individual por partido
  # --------------------------
  grafico_partido <- data_acumulada |>
    dplyr::filter(residencia_provincia_nombre == partido) |>
    ggplot(aes(x = fecha_enfermo, y = casos_dia)) +
    geom_line(color = color_partido, size = 0.5) +
    geom_vline(xintercept = as.numeric(fecha_evento), linetype = "dashed", color = "black") +
    annotate(
      "text",
      x = fecha_evento,
      y = Inf,
      label = format(fecha_evento, "%d-%m-%Y"),
      angle = 90,
      vjust = -0.5,
      hjust = 1,
      size = 3,
      fontface = "plain"
    ) +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b %Y",
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    scale_y_continuous(
      name = "Nro. de Casos",
      breaks = scales::pretty_breaks(n = 8)
    ) +
    labs(
      title = glue::glue("Casos acumulados en {partido}"),
      x = NULL
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # --------------------------
  # GRAFICO 2: Comparativo
  # --------------------------
  provincias <- if (es_comuna) "CABA" else c("Buenos Aires", "CABA")
  
  grafico_comparativo <- data_acumulada |>
    dplyr::filter(residencia_provincia_nombre %in% provincias) |>
    ggplot(aes(x = fecha_enfermo, y = casos_dia, color = residencia_provincia_nombre)) +
    geom_line(size = 0.5) +
    geom_vline(xintercept = as.numeric(fecha_evento), linetype = "dashed", color = "black") +
    annotate(
      "text",
      x = fecha_evento,
      y = Inf,
      label = format(fecha_evento, "%d-%m-%Y"),
      angle = 90,
      vjust = -0.5,
      hjust = 1,
      size = 3,
      fontface = "plain"
    ) +
    scale_color_manual(
      values = colores_comparativo,
      drop = FALSE
    ) +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b %Y",
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    scale_y_continuous(
      name = "Nro. de Casos",
      breaks = scales::pretty_breaks(n = 8)
    ) +
    labs(
      title = if (es_comuna) "Casos acumulados en CABA" else "Casos acumulados en Buenos Aires y CABA",
      x = NULL,
      color = "Provincia"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Retornar como lista
  list(
    grafico_partido = grafico_partido,
    grafico_comparativo = grafico_comparativo
  )
}
