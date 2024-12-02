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
    shiny::downloadButton(ns("reporte"), "Descargar reporte")
  )
}

#' Servidor: reporte
#' 
#' @description
#' Este módulo se encuentra en el tab 2: 'Por partido' de GeoCovid app
#'
#' @param id Module name
#' @param part Partido de la provincia de Buenos Aires, seleccionado en otro 
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
                          part, fecha,
                          base_raster, bsas, area,
                          tipo_de_raster, imagen, opacidad) {
  moduleServer(
    id,
    function(input, output, session) {

      formatted_date <-  reactive({

        if(is.null(fecha())){
          fecha = as.Date('2020-05-03')
          formatted_date <- format(fecha, format = "%Y-%m-%d")
          formatted_date
        }else{
     
      formatted_date <- formatted_date(fecha()) 
      formatted_date
      }
    })


      output$reporte <- downloadHandler(
        # https://community.rstudio.com/t/retain-formatting-on-a-pdf-output-from-shiny-downloadhandler/36410
        filename = function(){
          paste0("GeoCovid_", part(), "_", as.character(formatted_date()), ".html")
          },
        content = function(file) {


          my_tempdir <- tempdir()
          path_report <- file.path(my_tempdir,
                                   "reporte.Rmd")
          if(!dir.exists(paste0(my_tempdir, "/inst/rasters"))){
          dir.create(paste0(my_tempdir, "/inst/rasters"), 
                     recursive = TRUE)}
          path_rasters <- file.path(my_tempdir, "/inst/rasters")
          file.copy(from = here::here('inst', 'rasters'), 
                    to = path_rasters,
                    recursive = TRUE,
                    overwrite = TRUE)
          file.copy("inst/reporte.Rmd", path_report, overwrite = TRUE)


                    params <- list(partido = part(),
                         fecha = fecha(),
                         tipo_de_raster = tipo_de_raster(),
                         partido = part(),
                         #    mapa_partido = map_path,
                         #    grafico_lineas_sisa = lineas_path,
                         #    grafico_dygraph = dygraph_path,
                         opacidad = opacidad(),
                         base_raster = base_raster,
                         bsas = bsas,
                         area = area(),
                         pandoc = rmarkdown::pandoc_version())


          # temp_folder <- tempfile()
          #
          # onStop(function() {
          #   cat("Removing Temporary Files and Folders\n")
          #   unlink(temp_folder,
          #          recursive=TRUE)
          # })
          #




          #dir.create(temp_folder)

          #tempReport <- file.path(temp_folder,
          #                        "reporte.Rmd")
          # tempTemplate <- file.path(temp_folder,
          #                           "template.docx")
          #file.copy("reporte.Rmd", tempReport,
          #          overwrite = TRUE)
          # file.copy("template.docx",
          #           tempTemplate,
          #           overwrite = TRUE)

          # genero un archivo temporal con el mapa para que luego rmarkdown lo
          # tome
          # map_path <- paste0(tempdir(),
          #                    "/map.png")
          # mapshot(mapa_partido_r(),
          #         file = map_path,
          #         cliprect = "viewport")
          # lineas_path <- paste0(tempdir(),
          #                       "/lineas.png")
          # mapshot(grafico_lineas(),
          #         file = lineas_path,
          #         vwidth = 800, # pasados desde webshot
          #         vheight = 400,
          #         cliprect = "viewport")
          # dygraph_path <- paste0(tempdir(),
          #                        "/dygraph.png")
          # mapshot(grafico_dygraph(),
          #         file = dygraph_path,
          #         vwidth = 800,
          #         vheight = 400,
          #         cliprect = "viewport")




          id <- showNotification(
            "Preparando reporte...",
            duration = 5,
            closeButton = FALSE
          )
          on.exit(removeNotification(id), add = TRUE)

          rmarkdown::render(path_report,
                            output_file = file,
                            #  output_format = word_document(reference_docx = tempTemplate),
                            params = params,
                            envir = new.env(parent = globalenv())
          )
        }
      )
    }
  )
}