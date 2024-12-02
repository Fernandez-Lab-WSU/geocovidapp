#' GeoCovid app Server
#' 
#' @importFrom rlang .data
#' @param input input 
#' @param output output
#' @param session session
#' @param r r
#' @export

server <- function(input, output, session, r) {
  
  observe({print(imagen$imagen)})
   
  # Cargo mapa panel "Movilidad Buenos Aires"
  elecciones_mapa <- MapaBaires_Server("inter_mapa",
                                       bsas = bsas,
                                       amba_reducido_names,
                                       imagen = imagen$imagen,
                                       basemap = imagen$basemap,
                                       opacidad = imagen$opacity,
                                       area = imagen$area)
  
  # Tab movilidad Buenos Aires -----
  # Cargo imagenes raster para cada dia
  imagen <- FechaMomento_Server("test",
                                base_raster = base_raster,
                                mapa_zoom = elecciones_mapa$mapa_zoom
                            )
  
  
  # 
  # # Tab por partido -----
  # 
  # # el ususario selecciona opciones del mapa
  # elecciones_usuario_partidos <- Partidos_Server('seleccion_partido',
  #                                                bsas = bsas,
  #                                                amba_reducido_names = amba_reducido_names)
  # 
  # # el usuario seleciona del grafico una fecha
  # eleccion_fecha <- Dygraph_Server('casos_covid',
  #                                  amba_reducido_names =  amba_reducido_names,
  #                                  data_sisa = data_sisa,
  #                                  base_raster = base_raster,
  #                                  area = elecciones_usuario_partidos$area,
  #                                  part = elecciones_usuario_partidos$partido)
  # 
  # 
  # 
  # output$titulo <- renderText({
  #   
  #   if(is.null(eleccion_fecha$casos_covid())){
  #     
  #     fecha = as.Date('2020-05-12', origin = "1970-01-01")
  #     paste('Movilidad ciudadana el', format(fecha,
  #                                            format = "%Y-%m-%d"))
  #     
  #   }else{
  #     fecha <- eleccion_fecha$casos_covid()
  #     paste('Movilidad ciudadana el', format(lubridate::ymd_hms(fecha),
  #                                            format = "%Y-%m-%d"))}
  # })
  # 
  # # Histogramas 
  # HistogramaRaster_Server('hist',
  #                         amba_reducido_names =  amba_reducido_names,
  #                         bsas_comunas = bsas_comunas, # Incluye comunas de CABA
  #                         base_raster = base_raster,
  #                         area = elecciones_usuario_partidos$area,
  #                         tipo_de_raster = reactive({ input$porcentaje2 }),
  #                         fecha = eleccion_fecha$casos_covid,
  #                         momento_dia = 'mañana',
  #                         part = elecciones_usuario_partidos$partido)
  # 
  # HistogramaRaster_Server('hist2',
  #                         amba_reducido_names =  amba_reducido_names,
  #                         bsas_comunas = bsas_comunas,
  #                         base_raster = base_raster,
  #                         area = elecciones_usuario_partidos$area,
  #                         tipo_de_raster = reactive({ input$porcentaje2 }),
  #                         fecha = eleccion_fecha$casos_covid,
  #                         momento_dia = 'tarde',
  #                         part = elecciones_usuario_partidos$partido)
  # 
  # HistogramaRaster_Server('hist3',
  #                         amba_reducido_names =  amba_reducido_names,
  #                         bsas_comunas = bsas_comunas,
  #                         base_raster = base_raster,
  #                         area = elecciones_usuario_partidos$area,
  #                         tipo_de_raster = reactive({ input$porcentaje2 }),
  #                         fecha = eleccion_fecha$casos_covid,
  #                         momento_dia = 'noche',
  #                         part = elecciones_usuario_partidos$partido)
  # 
  # # Mapas con detalle del partido
  # MapaPartido_Server("baires_partidos",
  #                    amba_reducido_names =  amba_reducido_names,
  #                    bsas_comunas = bsas_comunas,
  #                    base_raster = base_raster,
  #                    area = elecciones_usuario_partidos$area,
  #                    tipo_de_raster = reactive({ input$porcentaje2 }),
  #                    fecha = eleccion_fecha$casos_covid,
  #                    momento_dia = 'mañana',
  #                    part = elecciones_usuario_partidos$partido, # Partidos_Input.R
  #                    opacidad = reactive({ input$opacity2 }))
  # 
  # MapaPartido_Server("baires_partidos2",
  #                    amba_reducido_names =  amba_reducido_names,
  #                    bsas_comunas = bsas_comunas,
  #                    base_raster = base_raster,
  #                    area = elecciones_usuario_partidos$area,
  #                    tipo_de_raster = reactive({ input$porcentaje2 }),
  #                    fecha = eleccion_fecha$casos_covid,
  #                    momento_dia = 'tarde',
  #                    part = elecciones_usuario_partidos$partido, # Partidos_Input.R
  #                    opacidad = reactive({ input$opacity2 }))
  # 
  # MapaPartido_Server("baires_partidos3",
  #                    amba_reducido_names =  amba_reducido_names,
  #                    bsas_comunas = bsas_comunas,
  #                    base_raster = base_raster,
  #                    area = elecciones_usuario_partidos$area,
  #                    tipo_de_raster = reactive({ input$porcentaje2 }),
  #                    fecha = eleccion_fecha$casos_covid,
  #                    momento_dia = 'noche',
  #                    part = elecciones_usuario_partidos$partido, # Partidos_Input.R
  #                    opacidad = reactive({ input$opacity2 }))
  # 
  # 
  # # Reporte
  # ReporteServer("desc_reporte",
  #               bsas = bsas,
  #               base_raster = base_raster,
  #               part = elecciones_usuario_partidos$partido, # Partidos_Input.R
  #               fecha = eleccion_fecha$casos_covid,
  #               area = elecciones_usuario_partidos$area,
  #               tipo_de_raster = reactive({ input$porcentaje2 }),
  #               imagen = imagen$imagen,
  #               opacidad = reactive({ input$opacity2 }))
  # 
  # # Tab 3: casos covid -----
  # 
  # elecciones_mapa_covid <- MapaCovidElecciones_Server('mapa_covid')
  # 
  # MapaCovidDepartamentos_Server('casos_covid',
  #                               amba_caba = amba_caba,
  #                               data_sisa = data_sisa,
  #                               base_raster = base_raster,
  #                               bsas = bsas)

}