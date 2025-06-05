#' GeoCovid app Server
#' 
#' @importFrom rlang .data
#' @param input input 
#' @param output output
#' @param session session
#' @param r r
#' @export

server <- function(input, output, session, r) {
  
  # Cargo mapa panel "Movilidad Buenos Aires"
  elecciones_mapa <- geocovidapp::MapaBaires_Server("tab1_mapa",
                                       bsas = bsas,
                                       amba_reducido_names,
                                       fecha = imagen$fecha, # para la informacion de la franja de abajo 
                                       porcentaje = imagen$porcentaje, # para la informacion de la franja de abajo 
                                       momento = imagen$momento, # para la informacion de la franja de abajo 
                                       imagen = imagen$imagen,
                                       boton = imagen$boton,
                                       basemap = imagen$basemap,
                                       opacidad = imagen$opacity,
                                       area = imagen$area)
  
  # Tab movilidad Buenos Aires -----
  # Cargo imagenes raster para cada dia
  imagen <- geocovidapp::FechaMomento_Server("tab1_barraflotante",
                                pool = pool,
                                base_raster = base_raster,
                                mapa_zoom = elecciones_mapa$mapa_zoom
                            )
  
  

  # Tab2: Por partido -----

  # el ususario selecciona opciones del mapa
  elecciones_usuario_partidos <- geocovidapp::Partidos_Server('seleccion_partido',
                                                 bsas = bsas,
                                                 amba_reducido_names = amba_reducido_names)

  # el usuario seleciona del grafico una fecha
  eleccion_fecha <- geocovidapp::Dygraph_Server('casos_covid',
                                   amba_reducido_names =  amba_reducido_names,
                                   data_sisa = data_sisa,
                                   base_raster = base_raster,
                                   area = elecciones_usuario_partidos$area,
                                   part = elecciones_usuario_partidos$partido)



  output$titulo <- renderText({

    if(is.null(eleccion_fecha$casos_covid())){

      fecha = as.Date('2020-05-12', origin = "1970-01-01")
      paste('Movilidad ciudadana el', format(fecha,
                                             format = "%Y-%m-%d"))

    }else{
      fecha <- eleccion_fecha$casos_covid()
      paste('Movilidad ciudadana el', format(lubridate::ymd_hms(fecha),
                                             format = "%Y-%m-%d"))}
  })
  
  # Carga rasters tanto para los histogramas como los mapas
  
  imagen <- geocovidapp::selectormapaServer("selector_partido",
                                act_mapas = reactive({input$act_mapas}),
                                fecha = eleccion_fecha$casos_covid)
    

  # Histogramas
  geocovidapp::HistogramaRaster_Server('hist',
                          pool = pool,
                          imagen = imagen$imagen,
                          amba_reducido_names = amba_reducido_names,
                          bsas_comunas = bsas_comunas, # Incluye comunas de CABA
                          base_raster = base_raster,
                          area = elecciones_usuario_partidos$area,
                          tipo_de_raster = reactive({ input$porcentaje2 }),
                          fecha = eleccion_fecha$casos_covid,
                          momento_dia = 'mañana',
                          part = elecciones_usuario_partidos$partido)

  geocovidapp::HistogramaRaster_Server('hist2',
                          pool = pool,
                          imagen = imagen$imagen,
                          amba_reducido_names =  amba_reducido_names,
                          bsas_comunas = bsas_comunas,
                          base_raster = base_raster,
                          area = elecciones_usuario_partidos$area,
                          tipo_de_raster = reactive({ input$porcentaje2 }),
                          fecha = eleccion_fecha$casos_covid,
                          momento_dia = 'tarde',
                          part = elecciones_usuario_partidos$partido)

  geocovidapp::HistogramaRaster_Server('hist3',
                          pool = pool,
                          imagen = imagen$imagen,
                          act_mapas = reactive({input$act_mapas}),
                          amba_reducido_names =  amba_reducido_names,
                          bsas_comunas = bsas_comunas,
                          area = elecciones_usuario_partidos$area,
                          tipo_de_raster = reactive({ input$porcentaje2 }),
                          fecha = eleccion_fecha$casos_covid,
                          momento_dia = 'noche',
                          part = elecciones_usuario_partidos$partido)

  # Mapas con detalle del partido
  mapa_manana <- geocovidapp::MapaPartido_Server("baires_partidos",
                     pool = pool,
                     imagen = imagen$imagen,
                     amba_reducido_names =  amba_reducido_names,
                     bsas_comunas = bsas_comunas,
                     area = elecciones_usuario_partidos$area,
                     tipo_de_raster = reactive({ input$porcentaje2 }),
                     fecha = eleccion_fecha$casos_covid,
                     momento_dia = 'mañana',
                     part = elecciones_usuario_partidos$partido, # Partidos_Input.R
                     opacidad = reactive({ input$opacity2 }))

  mapa_tarde <- geocovidapp::MapaPartido_Server("baires_partidos2",
                     pool = pool,
                     imagen = imagen$imagen,
                     amba_reducido_names =  amba_reducido_names,
                     bsas_comunas = bsas_comunas,
                     area = elecciones_usuario_partidos$area,
                     tipo_de_raster = reactive({ input$porcentaje2 }),
                     fecha = eleccion_fecha$casos_covid,
                     momento_dia = 'tarde',
                     part = elecciones_usuario_partidos$partido, # Partidos_Input.R
                     opacidad = reactive({ input$opacity2 }))

  mapa_noche <-geocovidapp::MapaPartido_Server("baires_partidos3",
                     pool = pool,
                     imagen = imagen$imagen,
                     amba_reducido_names =  amba_reducido_names,
                     bsas_comunas = bsas_comunas,
                     area = elecciones_usuario_partidos$area,
                     tipo_de_raster = reactive({ input$porcentaje2 }),
                     fecha = eleccion_fecha$casos_covid,
                     momento_dia = 'noche',
                     part = elecciones_usuario_partidos$partido, # Partidos_Input.R
                     opacidad = reactive({ input$opacity2 }))

  # Reporte
  geocovidapp::ReporteServer("desc_reporte",
                bsas = bsas,
                area = elecciones_usuario_partidos$area,
                part = elecciones_usuario_partidos$partido, # Partidos_Input.R
                fecha = eleccion_fecha$casos_covid,
                mapa_partido_manana = mapa_manana$mapa_partido,
                mapa_partido_tarde = mapa_tarde$mapa_partido,
                mapa_partido_noche = mapa_noche$mapa_partido,
                zoom_mapa_partido_manana = mapa_manana$zoom_mapa_partido,
                zoom_mapa_partido_tarde = mapa_tarde$zoom_mapa_partido,
                zoom_mapa_partido_noche = mapa_noche$zoom_mapa_partido,
                tipo_de_raster = reactive({ input$porcentaje2 }),
                opacidad = reactive({ input$opacity2 }),
                grafico_casos_prov = eleccion_fecha$grafico_casos_prov,
                grafico_casos_dpto = eleccion_fecha$grafico_casos_dpto
                )

  # Tab 3: casos covid -----

  elecciones_mapa_covid <- geocovidapp::MapaCovidElecciones_Server('mapa_covid')

  geocovidapp::MapaCovidDepartamentos_Server('casos_covid',
                                             pool = pool,
                                amba_caba = amba_caba,
                                data_sisa = data_sisa,
                                base_raster = base_raster,
                                bsas = bsas)

}