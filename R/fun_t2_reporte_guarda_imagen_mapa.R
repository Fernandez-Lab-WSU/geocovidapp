guarda_imagen_html <- function(objeto, filename) {
  if (is.null(objeto)) {
    warning(paste("The map for", filename, "is NULL. Skipping screenshot."))
    return(NULL)
  }
  
   html_path <- file.path(tempdir(), paste0(filename, ".html"))
   img_path <- file.path(tempdir(), paste0(filename, ".png"))

   # Check if the objeto is an HTML file or an HTML widget
   if (is.character(objeto) && grepl("\\.html$", objeto)) {
     # If objeto is an HTML file (checks file extension)
     file.copy(objeto, html_path, overwrite = TRUE)
   } else {
     # If objeto is an HTML widget
     # Save the map as an HTML widget
     htmlwidgets::saveWidget(objeto, html_path, selfcontained = TRUE)
   } 

  # Capture a screenshot of the saved HTML
  webshot2::webshot(html_path, file = img_path)
  
  return(img_path)
}
