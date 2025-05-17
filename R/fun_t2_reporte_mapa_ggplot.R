
# a la fecha de hoy es imposible usar otro metodo
mapa_base_ggplot <- function(part, zoom) {

  shape_partido <- geocovidapp::bsas_comunas |> 
    dplyr::filter(partido == part) |>
    dplyr::pull(geometry)
  
  shape_partido <- terra::vect(shape_partido)
  
  map_ext <- terra::ext(shape_partido)
  
  map_tiles <- maptiles::get_tiles(x = map_ext, zoom = zoom, crop = T)
  
  return(map_tiles)
 #  # Convert raster to data frame for ggplot
 #  r_df <- as.data.frame(raster, xy = TRUE, na.rm = TRUE)
 #  colnames(r_df)[3] <- "value"
 #  
 #  # Get bounding box
 #  bbox <- sf::st_bbox(raster)
 #  
 #  # Plot with OSM tiles using ggspatial
 # mapa <-  ggplot2::ggplot() +
 #    ggspatial::annotation_map_tile(type = "osm", zoom = zoom) +
 #    ggplot2::geom_tile(data = r_df, ggplot2::aes(x = x, y = y, fill = value), 
 #                       alpha = opacidad) +
 #    ggplot2::scale_fill_viridis_c() +
 #    ggplot2::coord_sf(  xlim = c(bbox[[1]], bbox[[2]]),
 #               ylim = c(bbox[[3]], bbox[[4]]),
 #               expand = FALSE,
 #               crs = 4326) +
 #   ggplot2::theme_minimal()
 # 
 # return(mapa)
}
