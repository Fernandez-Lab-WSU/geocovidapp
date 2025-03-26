#' Elementos de interfaz de usuario de la barra flotante del tab de Mapa BsAs
#'
#' @param id Module name
#' @param base_raster Dataframe que lista todos los rasters y desagrega en 
#' sus columnas características de interes, como si son rasters de 
#' AMBA o Buenos Aires, si el cambio porcentual es semanal o prepandemia 
#' o el momento del día que representan.
#'
#' @param id Module name
#' @return Barra flotante del tab Mapa Buenos Aires.
#' 
#' @export
DBrasterUI <- function(id, base_raster) {
  
  ns <- NS(id)
  
  momento_del_dia <- as.list(unique(base::unique(base_raster$hora)))
  names(momento_del_dia) <- unique(base::unique(base_raster$momento))
  
  shiny::tagList(
    h4("Cliquea en el mapa"),
    shiny::radioButtons(ns("basemap"),
                        label = 'Mapa Base:',
                        choices = c("Relieve" = 'relieve',
                                    "Calles" = 'calles'),
                        selected = 'relieve',
                        inline = TRUE),
    shiny::radioButtons(ns("area"),
                        label = paste("Selecciona AMBA para visualizar datos",
                                      "con mayor resolucion en ese area"),
                        choices = c("prov. de Buenos Aires" = 'baires',
                                    "AMBA" = 'amba'),
                        selected = 'baires',
                        inline = TRUE),
   # shinyjs::hidden(
      fechaUI(ns("fecha")
   #          )
      ),
    # shinyjs::hidden(shiny::dateInput(ns("fechas_ui"),
    #                                  label = "Fecha",
    #                                  language = "es",
    #                                  format = "yyyy-mm-dd")),
    shinyjs::hidden(
      shiny::radioButtons(ns('porcentaje'),
                                        label = 'Cambio porcentual',
                                        choices = c("Prepandemia" = "pc",
                                                    "Semanal" = "7dpc"),
                                        inline = TRUE,
                                        selected = "pc")
 )
  ,
   #  shinyjs::hidden(
       momentoUI(ns("momento")
    #            )
       ),
    #   shiny::uiOutput(ns("momento_ui"),
    #                       label = "Momento del día",
    #                     #  choices = c("mañana", "tarde",  "noche"),
    #                       inline = TRUE,
    #                      # selected = NULL
    #                     )),
    shinyjs::hidden(
      p(id = 'barra_transparencia',
        "Opciones de visualizacion del mapa"),
      shiny::sliderInput(ns("opacity"),
                         label = "Transparencia",
                         min = 0,
                         max = 1,
                         value = 0.5,
                         ticks = FALSE))
    
  )
}

#' Convierte el archivo en raster en base a las elecciones del usuario
#'
#' @param id Module name
#' @param base_raster Dataframe que lista todos los rasters y desagrega en 
#' sus columnas características de interes, como si son rasters de 
#' AMBA o Buenos Aires, si el cambio porcentual es semanal o prepandemia 
#' o el momento del día que representan.
#' @param mapa_zoom Valor de zoom que retorna leaflet en base al uso del mapa.
#'
#' @return Una serie de variables extraidas del dataframe base_raster para
#' el raster que eligio el usuario y representan el area, la opacidad, 
#' el raster mismo (imagen) y basemap
#' @export
DBrasterServer <- function(id,
                                base_raster,
                                mapa_zoom
                                ){
  moduleServer(id,
               session = getDefaultReactiveDomain(),
               function(input, output, session){
                 

                 fecha_modulo <- fechaServer("fecha", 
                                             area = reactive(input$area),
                                             porcentaje = reactive(input$porcentaje)) 
             
                  observe({ print('ff')
                    print(fecha_modulo)

                  } )
                 momento_modulo <-  momentoServer("momento",
                                                  area = reactive(input$area),
                                                  porcentaje = reactive(input$porcentaje),
                                                  fecha_modulo = fecha_modulo)
                 observe({ print('mm')
                   print(momento_modulo)
                   
                 } )

                 # conexion a la base de datos
                 imagen <- shiny::reactive({


                  db <- config::get("database")

                  pool <- pool::dbPool(
                     drv = RPostgres::Postgres(),
                     dbname = db$dbname,
                     user = db$user,
                     password = db$password,
                     port = db$port,
                     host = db$host
                   )
                   onStop(function() {
                     pool::poolClose(pool)
                   })

                   if(is.null(fecha_modulo())){fecha_modulo_e <- as.Date("2020-04-26")}else{fecha_modulo_e <- fecha_modulo()}
                   if(is.null(momento_modulo())){momento_modulo_e <- "tarde"}else{momento_modulo_e <- momento_modulo()}
                   print("MAMADERA")
                  
                   
                   # filtro el raster que el usuario eligio
                   raster_data <-  base_raster |>
                     dplyr::filter(fecha == as.Date(fecha_modulo_e, # fecha_modulo()
                                                    origin = "1970-01-01"),
                                   locacion == input$area,
                                   tipo_de_raster == input$porcentaje,
                                   momento == momento_modulo_e) # es un valor no reactivo # momento_modulo()


                 print('raster_data')
                   print(raster_data)

                   # leo los rasters, tengo dos tablas separadas
                   # una para los rasters de amba y
                   # otra para los rasters de baires

                   if(input$area == 'amba'){
                   query <- paste0("SELECT ST_AsGDALRaster(rast, 'GTiff') AS rast FROM raster_schema.rasters_geo WHERE filename='",
                                   raster_data$filename,"';")
                   }else{
                     query <- paste0("SELECT ST_AsGDALRaster(rast, 'GTiff') AS rast FROM raster_schema.raster_geo_baires WHERE filename='",
                                     raster_data$filename,"';")
                   }

                   result <- pool::dbGetQuery(pool, query)

                   # Guardo el raster binario en un archivo temporario
                   temp_file <- tempfile(fileext = ".tif")
                   writeBin(result$rast[[1]], temp_file)

                   # Genero el raster
                   terra::rast(temp_file)

                 })


                 # Revelo la barra de transparencia cuando el zoom es mayor a 6
                 shiny::observeEvent(mapa_zoom(),{
                   if(mapa_zoom() <= 6){
                     shinyjs::hide("opacity")
                     shinyjs::hide("porcentaje")
                    # shinyjs::hide("momento")
                    # shinyjs::hide("fecha")
                   }else{
                     shinyjs::show("opacity")
                     shinyjs::show("porcentaje")
                    # shinyjs::show("momento")
                    # shinyjs::show("fecha")
                   }
                 })

                 return(
                   list(
                     area = reactive({ input$area }),
                     opacity = reactive({ input$opacity }),
                     imagen = reactive({ imagen() }),
                     basemap = reactive({ input$basemap }) #?
                   )
                 )
                 
               })}