guarda_imagen_leaflet <- function(mapa, filename) {
  if (is.null(mapa)) {
    warning(paste("The map for", filename, "is NULL. Skipping screenshot."))
    return(NULL)
  }
  
   html_path <- file.path(tempdir(), paste0(filename, ".html"))
   img_path <- file.path(tempdir(), paste0(filename, ".png"))

  # html_path <- file.path("www", paste0(filename, ".html"))
  # img_path <- file.path("www", paste0(filename, ".png"))
  
  # Save the map as an HTML widget
  htmlwidgets::saveWidget(mapa, html_path, selfcontained = TRUE)
  
  # Capture a screenshot of the saved HTML
  webshot2::webshot(html_path, file = img_path)
  
  return(img_path)
}