#' IU: reporte
#' 
#' @description
#' Este módulo se encuentra en el tab 2: 'Por partido' de GeoCovid app
#' 
#' @param id Module name
#' @return Botón de descarga del reporte.
#' @export
ReporteUI <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::downloadButton(ns("reporte"), "Descargar reporte",
                          class = "btn btn-secondary btn-sm")
  )
}

#' Servidor: reporte
#' 
#' @description
#' Este módulo se encuentra en el tab 2: 'Por partido' de GeoCovid app
#'
#' @param id Module name
#' @param partido Partido de la provincia de Buenos Aires, seleccionado en otro 
#' módulo.
#' @param fecha Fecha seleccionada.
#' @param imagen Imagen raster representando la movilidad ciudadana.
#' @param area El raster puede corresponder a Buenos Aires provincia o a AMBA.
#' @param opacidad Double. Valor de opacidad del raster.a 
#' @param tipo_de_raster String. Si el raster corresponde a el cambio porcentual 
#' prepandemia (pc) o semanal (7dpc).
#' @param base_raster Dataframe que lista todos los rasters y desagrega en 
#' sus columnas características de interes, como si son rasters de 
#' AMBA o Buenos Aires, si el cambio porcentual es semanal o prepandemia 
#' o el momento del día que representan. 
#' @param bsas Dataset de clase sf con los partidos de Buenos Aires.
#'
#' @return Devuelve un reporte en base a las selecciones en el tab2.
#' @export
ReporteServer <- function(id,
                          partido, fecha,
                          bsas, area,
                          tipo_de_raster, opacidad,
                          mapa_partido_manana,
                          mapa_partido_tarde,
                          mapa_partido_noche,
                          zoom_mapa_partido_manana,
                          zoom_mapa_partido_tarde,
                          zoom_mapa_partido_noche,
                          grafico_casos_prov,
                          grafico_casos_dpto) {
  moduleServer(
    id,
    function(input, output, session) {
      
     fecha_val <- reactive({
        f <- fecha()

        if (is.null(f)) { # Tiene que elegir un valor en el mapa
          as.Date('2020-05-03') # tiene que ser una fecha que tenga raster para todas las opciones
        } else {
          print(as.Date(f))
          as.Date(f)  # Extraigo la fecha solamente
        }
      })
      
      output$reporte <- downloadHandler(
        # https://community.rstudio.com/t/retain-formatting-on-a-pdf-output-from-shiny-downloadhandler/36410
        filename = function(){
          paste0("GeoCovid_", partido(), "_", as.character(fecha_val()), ".docx")
        },
        content = function(file) {
          
          my_tempdir <- tempdir()
          path_report <- file.path(my_tempdir,
                                   "reporte.Rmd")
          
          
          # Copy the reporte.Rmd from the inst folder of the package to the temporary directory
          reporte_path <- system.file("geocovidapp/reporte.Rmd", package = "geocovidapp")
          if (reporte_path == "") stop("reporte.Rmd not found in package.")
          
          file.copy(reporte_path, path_report, overwrite = TRUE)
          
          params <- list(
            partido = partido(),
            fecha = fecha_val(),
            tipo_de_raster = tipo_de_raster(),
            opacidad = opacidad(),
            area = area(),
            imagen_manana = mapa_partido_manana(),
            imagen_tarde = mapa_partido_tarde(),
            imagen_noche = mapa_partido_noche(),
            pandoc = rmarkdown::pandoc_version()
          )
          
          id <- showNotification(
            "Preparando reporte...",
            closeButton = FALSE
          )
          on.exit(removeNotification(id), add = TRUE)
          
          rmarkdown::render(path_report, # es el path al directorio temporario
                            output_file = file,
                            output_format = rmarkdown::word_document(),#reference_docx = tempTemplate
                            params = params,
                            envir = new.env(parent = globalenv())
          )
        }
      )
    }
  )
}