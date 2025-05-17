mapa_reporte <- function(part, imagen) {
  shape_partido <- geocovidapp::bsas_comunas |> 
    dplyr::filter(partido == part) |>
    dplyr::pull(geometry)
  
  # 1. Definir cortes y colores
  breaks <- c(-50, -40, -30, -20, -10, -1, 1, 10, 20, 30, 40, 50)
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
    #tidyterra::geom_spatraster_rgb(data = params$map_path1, alpha = 0.7) +
    tidyterra::geom_spatraster(data = raster_binned, ggplot2::aes(fill = label), alpha = 0.6) +
    ggplot2::geom_sf(data = shape_partido, fill = NA, color = "black", size = 0.6) +
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