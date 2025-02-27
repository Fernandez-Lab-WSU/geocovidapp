#' IU: Histograma de la movilidad ciudadana por departamento
#'
#' @param id Module name
#' @return Histograma de la movilidad ciudadana por partido, respetando la 
#' escala de colores del raster.
#' @export
HistogramaRaster_UI <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    tags$div(style='padding:0px;margin-top:0px',
      shinycssloaders::withSpinner(
      shiny::plotOutput(ns("histograma"),
                        width = 150,
                        height = 150),
      type = 2,
      color = 'lightgrey',
      color.background = 'white'))
  )
}

#' Servidor: Histograma de la movilidad ciudadana por departamento
#'
#' @param id Module name
#' @param bsas_comunas Mapa vectoria de la provincia de Buenos Aires simplifica
#' do.
#' @param amba_reducido_names String. Vector con los nombres de los partidos 
#' que conforman el AMBA.
#' @param base_raster Dataframe que lista todos los rasters y desagrega en 
#' sus columnas características de interes, como si son rasters de 
#' AMBA o Buenos Aires, si el cambio porcentual es semanal o prepandemia 
#' o el momento del día que representan. 
#' @param fecha Fecha seleccionada.
#' @param tipo_de_raster String. Si el raster corresponde a el cambio porcentual 
#' prepandemia (pc) o semanal (7dpc).
#' @param area El raster puede corresponder a Buenos Aires provincia o a AMBA.
#' @param part Partido de la provincia de Buenos Aires, seleccionado en otro 
#' módulo.
#' @param momento_dia Entero. Hora del dia representada en el raster, puede ser 
#' 0am, 8 am o 4 pm.
#'
#' @return Histograma de la movilidad ciudadana por partido, respetando la 
#' escala de colores del raster.
#' 
#' @importFrom graphics hist par
#' @export
HistogramaRaster_Server <- function(id,
                                    amba_reducido_names,
                                    bsas_comunas, base_raster,
                                    area, fecha,
                                    tipo_de_raster, part,
                                    momento_dia) {
  moduleServer(
    id,
    function(input, output, session) {

      # Este elemento reactivo podria unificarlo con el de MapaPartido
      imagen <- shiny::reactive({



        if(is.null(fecha())){
          f_date <- format("2020-05-03", format = "%Y-%m-%d")
          }else{

        
        # Convierto a formato "YYYY-MM-DD" 
        f_date <- formatted_date(fecha()) 

        }


        # selecciono un solo dia y tiempo, ya que estoy probando
        imagen <-    rasterLoader(base_raster = base_raster, 
                                  fecha_elegida = reactiveVal({ f_date }),
                                  momento_dia = reactiveVal({ momento_dia }), # no reactivo
                                  tipo_de_raster = tipo_de_raster,
                                  area = area )

      })

raster_hist <- reactive({

        if(part() %in% amba_reducido_names){

          # ver Patidos_Input.R
          amba <-  dplyr::filter(bsas_comunas,
                                 partido %in% amba_reducido_names)

          # recorto por poligono
          poli <- sf::st_as_sf(subset(amba,
                                      partido == part())) # |> 
          #  sf::st_transform(3857)

          imagen2 <- imagen() |>
            terra::mask(poli) |>
            terra::crop(poli)

          # no quiero que considere valores por arriba de 50 o debajo de 50
          imagen2[imagen2 > 50] <- 50
          imagen2[imagen2 < -50] <- -50
          imagen2

        }else if(!(part() %in% amba_reducido_names)){

          # ver Patidos_Input.R
          prov <-  dplyr::filter(bsas_comunas,
                                 !partido %in% amba_reducido_names)

        # recorto por poligono
        poli <- sf::st_as_sf(subset(prov,
                                    partido == part())) |> sf::st_transform(3857)

        imagen2 <- imagen() |>
          terra::mask(poli) |>
          terra::crop(poli)

        # no quiero que considere valores por arriba de 50 o debajo de 50
        imagen2[imagen2 > 50] <- 50
        imagen2[imagen2 < -50] <- -50
        imagen2

        }





      })



      output$histograma <- renderPlot({
        par(mar=c(4,1,3,1)) # bottom, left, top, right

        hist(terra::values(raster_hist()),
             breaks = c(50,  40, 30 ,
                        20 , 10, 1, -1,
                        -10, -20, -30,
                        -40, -50),
             main= NULL,
             col= c("#0000FF", "#0040FF",
                    "#0080FF", "#00BFFF",
                    "#00FFFF", "#FFFFFF",
                    "#FFCC00", "#FF9900",
                    "#FF6600", "#FF3300",
                    "#FF0000"),  # changes bin color
             xlim = c(50, -50),
             freq = FALSE,
             yaxs = "i",
             xaxs = "i",
             lty="blank",
             # ylab = paste0("% de pixeles")
             yaxt = "n", ylab = "",
             xlab= paste("Movilidad", momento_dia)

        )
      }, bg = "transparent")

    }
  )
}
