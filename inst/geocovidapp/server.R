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
                                mapa_zoom = elecciones_mapa$mapa_zoom
                            )
  
  

  # Tab2: Por partido -----

  # # el ususario selecciona opciones del mapa
  # elecciones_usuario_partidos <- geocovidapp::Partidos_Server('seleccion_partido',
  #                                                bsas = bsas,
  #                                                amba_reducido_names = amba_reducido_names)

  # Carga rasters tanto para los histogramas como los mapas
  # dentro de este modulo esta anidado partidos_input.R
  # la eleccion de la fecha depende de dygraph, por ende defini
  # valores
  
  # elecciones_usuario_partido <- geocovidapp::selectormapaServer("selector_partido",
  #                                           act_mapas = reactive({input$act_mapas}),
  #                                           fecha = eleccion_fecha$casos_covid) 
  # 
  
  elecciones_usuario <- geocovidapp::Partidos_Server(  # mod_t2_partidos_input.R
    "selector_dinamico",
    amba_reducido_names = amba_reducido_names
  )
  
  # el usuario seleciona del grafico una fecha
  eleccion_fecha <- geocovidapp::Dygraph_Server('casos_covid',
                                                amba_reducido_names =  amba_reducido_names,
                                                area = elecciones_usuario$area,
                                                partido = elecciones_usuario$partido)
  
  datos_raster <- reactive({
   
    fecha_final <- if (is.null(eleccion_fecha$casos_covid())) {
      as.Date("2020-05-03")
    } else {
      as.Date(geocovidapp::formatted_date(eleccion_fecha$casos_covid()))
    }
    
    d <- geocovidapp::base_raster |>
      dplyr::filter(
        fecha == fecha_final,
        tipo_de_raster == elecciones_usuario$porcentaje(),
        locacion == elecciones_usuario$area()
      )
    
    d
  })
  
  imagen_partido <- eventReactive(input$act_mapas, ignoreNULL = FALSE, {
    raster_data <- datos_raster()
    area_valor <- isolate(elecciones_usuario$area())

    # Chequear
    raster_outputs <- lapply(
      setNames(split(raster_data, raster_data$momento), 
               raster_data$momento),
      function(r) {
        rasterLoader(
          pool = pool,
          raster_data = r,
          area = area_valor
        )
      }
    )
    raster_outputs
  })
  
  
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
  

  # Histogramas
  geocovidapp::HistogramaRaster_Server('hist',
                          pool = pool,
                          act_mapas = reactive({input$act_mapas}),
                          imagen = imagen_partido,
                          amba_reducido_names = amba_reducido_names,
                          bsas_comunas = bsas_comunas, # Incluye comunas de CABA
                          tipo_de_raster = elecciones_usuario$porcentaje,
                          fecha = eleccion_fecha$casos_covid,
                          momento_dia = 'mañana',
                          partido = elecciones_usuario$partido)

  geocovidapp::HistogramaRaster_Server('hist2',
                          pool = pool,
                          act_mapas = reactive({input$act_mapas}),
                          imagen = imagen_partido,
                          amba_reducido_names =  amba_reducido_names,
                          bsas_comunas = bsas_comunas,
                          tipo_de_raster = elecciones_usuario$porcentaje,
                          fecha = eleccion_fecha$casos_covid,
                          momento_dia = 'tarde',
                          partido = elecciones_usuario$partido)

  geocovidapp::HistogramaRaster_Server('hist3',
                          pool = pool,
                          act_mapas = reactive({input$act_mapas}),
                          imagen = imagen_partido,
                          amba_reducido_names =  amba_reducido_names,
                          bsas_comunas = bsas_comunas,
                          tipo_de_raster = elecciones_usuario$porcentaje,
                          fecha = eleccion_fecha$casos_covid,
                          momento_dia = 'noche',
                          partido = elecciones_usuario$partido)

  # Mapas con detalle del partido
  mapa_manana <- geocovidapp::MapaPartido_Server("baires_partidos",
                     pool = pool,
                     act_mapas = reactive({input$act_mapas}),
                     imagen = imagen_partido,
                     amba_reducido_names =  amba_reducido_names,
                     bsas_comunas = bsas_comunas,
                     area = elecciones_usuario$area, # tengo que aclarar esto porque corresponde al subset de rasters
                     tipo_de_raster = elecciones_usuario$porcentaje,
                     fecha = eleccion_fecha$casos_covid,
                     momento_dia = 'mañana',
                     partido = elecciones_usuario$partido, # Partidos_Input.R
                     opacidad = elecciones_usuario$opacidad)

  mapa_tarde <- geocovidapp::MapaPartido_Server("baires_partidos2",
                     pool = pool,
                     act_mapas = reactive({input$act_mapas}),
                     imagen = imagen_partido,
                     amba_reducido_names =  amba_reducido_names,
                     bsas_comunas = bsas_comunas,
                     area = elecciones_usuario$area,
                     tipo_de_raster = elecciones_usuario$porcentaje,
                     fecha = eleccion_fecha$casos_covid,
                     momento_dia = 'tarde',
                     partido = elecciones_usuario$partido, # Partidos_Input.R
                     opacidad = elecciones_usuario$opacidad)

  mapa_noche <-geocovidapp::MapaPartido_Server("baires_partidos3",
                     pool = pool,
                     act_mapas = reactive({input$act_mapas}),
                     imagen = imagen_partido,
                     amba_reducido_names =  amba_reducido_names,
                     bsas_comunas = bsas_comunas,
                     area = elecciones_usuario$area,
                     tipo_de_raster = elecciones_usuario$porcentaje,
                     fecha = eleccion_fecha$casos_covid,
                     momento_dia = 'noche',
                     partido = elecciones_usuario$partido, # Partidos_Input.R
                     opacidad = elecciones_usuario$opacidad)

  # Reporte
  geocovidapp::ReporteServer("desc_reporte",
                bsas = bsas,
                area = elecciones_usuario$area,
                partido = elecciones_usuario$partido, # Partidos_Input.R
                fecha = eleccion_fecha$casos_covid,
                mapa_partido_manana = mapa_manana$mapa_partido,
                mapa_partido_tarde = mapa_tarde$mapa_partido,
                mapa_partido_noche = mapa_noche$mapa_partido,
                zoom_mapa_partido_manana = mapa_manana$zoom_mapa_partido,
                zoom_mapa_partido_tarde = mapa_tarde$zoom_mapa_partido,
                zoom_mapa_partido_noche = mapa_noche$zoom_mapa_partido,
                tipo_de_raster = elecciones_usuario$porcentaje,
                opacidad = elecciones_usuario$opacidad,
                grafico_casos_prov = eleccion_fecha$grafico_casos_prov,
                grafico_casos_dpto = eleccion_fecha$grafico_casos_dpto
                )

  # Tab 3: casos covid -----

  elecciones_mapa_covid <- geocovidapp::MapaCovidElecciones_Server('mapa_covid')

  geocovidapp::MapaCovidDepartamentos_Server('casos_covid',
                                             pool = pool,
                                amba_caba = amba_caba)

}