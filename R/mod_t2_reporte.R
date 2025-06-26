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

#' Servidor: reporte personalizado por partido
#'
#' @description
#' Este módulo corresponde al Tab 2: "Por partido" de la aplicación GeoCovid. Permite generar
#' un reporte descargable en formato Word (`.docx`) basado en la selección de un partido de 
#' la provincia de Buenos Aires, una fecha, y los datos de movilidad ciudadana.
#'
#' @param id Nombre del módulo. Necesario para su uso con `moduleServer`.
#' @param partido Reactive. Nombre del partido seleccionado.
#' @param fecha Reactive. Fecha seleccionada por el usuario.
#' @param area Reactive. Área geográfica del raster: `"baires"` o `"amba"`.
#' @param tipo_de_raster Reactive. Tipo de comparación del raster: `"pc"` (prepandemia) o `"7dpc"` (últimos 7 días).
#' @param opacidad Reactive. Valor numérico entre 0 y 1 que define la opacidad del raster.
#' @param mapa_partido_manana Reactive. Raster correspondiente al momento de la mañana.
#' @param mapa_partido_tarde Reactive. Raster correspondiente al momento de la tarde.
#' @param mapa_partido_noche Reactive. Raster correspondiente al momento de la noche.
#'
#' @return Este módulo no retorna valores al servidor de forma explícita, pero registra un `downloadHandler`
#' que permite descargar un reporte en formato Word (`.docx`) con los parámetros seleccionados.
#'
#' @export
ReporteServer <- function(id,
                          partido, 
                          fecha,
                          area,
                          tipo_de_raster,
                          opacidad,
                          mapa_partido_manana,
                          mapa_partido_tarde,
                          mapa_partido_noche) {
  moduleServer(
    id,
    function(input, output, session) {
      
     
      output$reporte <- downloadHandler(
        # https://community.rstudio.com/t/retain-formatting-on-a-pdf-output-from-shiny-downloadhandler/36410
        filename = function(){
          
          
          print("dddd")
          print(reactive({ tipo_de_raster() }) )
          
          req(fecha(), tipo_de_raster(), partido())
          
          paste0("GeoCovid_", partido(), "_",
                 as.character(fecha()), "_", 
                 #ifelse(tipo_de_raster() == "7dpc","semanal", 
                        "prepandemia", ".docx")
        },
        content = function(file) {
          
          req(fecha(), mapa_partido_manana(),
              mapa_partido_tarde(), mapa_partido_noche())
          
          my_tempdir <- tempdir()
          path_report <- file.path(my_tempdir,
                                   "reporte.Rmd")
          
 
     
          
          # Copy the reporte.Rmd from the inst folder of the package to the temporary directory
          reporte_path <- system.file("geocovidapp/reporte.Rmd", package = "geocovidapp")
          if (reporte_path == "") stop("reporte.Rmd not found in package.")
          
          file.copy(reporte_path, path_report, overwrite = TRUE)
          
          params <- list(
            partido = partido(),
            fecha = fecha(),
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