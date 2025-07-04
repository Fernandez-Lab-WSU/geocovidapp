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
  etiquetas <- paste(breaks[-length(breaks)], "a", breaks[-1])
  etiquetas[1] <- "Disminuyó bajo -40%"      
  etiquetas[6] <- "Sin cambio"               
  etiquetas[length(etiquetas)] <- "Aumentó más del 40%" 
  
  labels_df <- data.frame(
    value = 1:(length(breaks) - 1),
    label = etiquetas
  )
  
  # 5. Forzar factor con todos los niveles
  # Invertir el orden de los niveles para la leyenda (solo la leyenda se invierte)
  # Podria haber solucionado esto de otra forma
  labels_df$label <- factor(labels_df$label, levels = rev(labels_df$label))
  
  # Asignar labels al raster binned con niveles invertidos para la leyenda
  levels(raster_binned) <- labels_df
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
      breaks = rev(etiquetas), # Invertir el orden de los breaks para la leyenda
      name = "Porcentaje de cambio",
      na.translate = FALSE, # Remueve NA de la leyenda
      drop = FALSE  # Evita que se recorte la leyenda
    ) +
    ggplot2::coord_sf(
      xlim = c(bbox["xmin"], bbox["xmax"]),
      ylim = c(bbox["ymin"], bbox["ymax"]),
      expand = FALSE
    ) +
    ggplot2::theme_minimal()  +
    ggplot2::theme( # leyenda
      legend.title = ggplot2::element_text(size = 9),
      legend.text = ggplot2::element_text(size = 8),
      legend.key.height = ggplot2::unit(0.5, "lines"),
      legend.key.width = ggplot2::unit(0.5, "lines")
    )
}


#' Gráfico individual y comparativo de casos COVID
#'
#' @param partido Nombre del partido o comuna (ej. "La Matanza" o "Comuna 1")
#' @param fecha Fecha a marcar con una línea vertical (formato "YYYY-MM-DD")
#'
#' @return Una lista con dos ggplots: `grafico_partido` y `grafico_comparativo`
#' @export
ggplot_casos_covid_doble <- function(partido, fecha) {
  
  # 1. Datos base
  geocovidapp::data_sisa |>
    dplyr::group_by(residencia_provincia_nombre, fecha_enfermo) |>
    dplyr::summarise(casos_dia = dplyr::n(), .groups = "drop_last") |> 
    dplyr::mutate(fecha_enfermo = as.Date(fecha_enfermo))
  
  # 2. Identificar si el partido es una comuna
  es_comuna <- stringr::str_starts(partido, "Comuna")
  fecha_evento <- as.Date(fecha)
  
  # 3. Colores
  colores_comparativo <- c("Buenos Aires" = "#186E8B", "CABA" = "#301A4B")
  color_partido <- "#6C9AC6"
  
  # --------------------------
  # GRAFICO 1: Comparativo
  # --------------------------
  # No estamos mostrando datos por comunas. Si la eleccion es comuna
  # Si el partido es una comuna, mostrar CABA, sino
  provincias <- if (es_comuna){"CABA" } else { c("Buenos Aires", "CABA") }
  
  grafico_comparativo <- geocovidapp::data_sisa |>
    dplyr::group_by(residencia_provincia_nombre, fecha_enfermo) |>
    dplyr::summarise(casos_dia = dplyr::n(), .groups = "drop_last") |> 
    dplyr::mutate(fecha_enfermo = as.Date(fecha_enfermo)) |>  # fin datos
    dplyr::filter(residencia_provincia_nombre %in% provincias) |>
    ggplot2::ggplot(ggplot2::aes(x = fecha_enfermo, y = casos_dia, color = residencia_provincia_nombre)) +
    ggplot2::geom_line(size = 0.5) +
    ggplot2::geom_vline(xintercept = as.numeric(fecha_evento), linetype = "dashed", color = "black") +
    ggplot2::annotate(
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
    ggplot2::scale_color_manual(
      values = colores_comparativo,
      drop = FALSE
    ) +
    ggplot2::scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b %Y",
      expand = ggplot2::expansion(mult = c(0.01, 0.01))
    ) +
    ggplot2::scale_y_continuous(
      name = "Nro. de Casos",
      breaks = scales::pretty_breaks(n = 8)
    ) +
    ggplot2::labs(
      title = if (es_comuna) "Casos acumulados en CABA" else "Casos acumulados en Buenos Aires y CABA",
      x = NULL,
      color = "Provincia"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) 
  
  # --------------------------
  # GRAFICO 1: Individual por partido
  # --------------------------
  if (!es_comuna) {
    
    grafico_partido <- geocovidapp::data_sisa |>
      dplyr::filter(residencia_provincia_nombre %in% c("CABA", "Buenos Aires")) |> 
      dplyr::group_by(residencia_provincia_nombre, residencia_departamento_nombre, fecha_enfermo) |>
      dplyr::summarise(casos_dia = dplyr::n(), .groups = "drop_last") |> 
      dplyr::mutate(fecha_enfermo = as.Date(fecha_enfermo)) |> 
      dplyr::filter(residencia_departamento_nombre == partido) |> 
      ggplot2::ggplot(ggplot2::aes(x = fecha_enfermo, y = casos_dia)) +
      ggplot2::geom_line(color = color_partido, size = 0.5) +
      ggplot2::geom_vline(xintercept = as.numeric(fecha_evento), linetype = "dashed", color = "black") +
      ggplot2::annotate(
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
      ggplot2::scale_x_date(
        date_breaks = "1 month",
        date_labels = "%b %Y",
        expand = ggplot2::expansion(mult = c(0.01, 0.01))
      ) +
      ggplot2::scale_y_continuous(
        name = "Nro. de Casos",
        breaks = scales::pretty_breaks(n = 8)
      ) +
      ggplot2::labs(
        title = glue::glue("Casos acumulados en {partido}"),
        x = NULL
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) 
    
  } else {
    grafico_partido <- NULL
  }
  
  # Retornar como lista
  list(
    grafico_partido = grafico_partido,
    grafico_comparativo = grafico_comparativo
  )
}
