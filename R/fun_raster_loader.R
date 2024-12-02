rasterLoader <- function(base_raster, 
                         fecha_elegida,
                         momento_dia,
                         tipo_de_raster,
                         area){
  # no puedo usar pool aca

  # con <- pool::dbPool(
  #   drv = RPostgres::Postgres(),
  #   dbname = "geocovidapp_db",
  #   user = Sys.getenv("USER"),
  #   password = Sys.getenv("PASSWORD"),
  #   port = "5432",
  #   host = "44.226.244.60"
  # )

    db <- config::get("database")
    
   con <- DBI::dbConnect(
      drv = RPostgres::Postgres(),
      dbname = db$dbname,
      user = db$user,
      password = db$password,
      port = db$port,
      host = db$host
    )
 

      raster <- reactive({
        # print('INSIDE FUN')
        # print(fecha_elegida)
        # print(tipo_de_raster)
        # print(momento_dia)
        # print(area)
        # print(fecha_elegida())
        # print(tipo_de_raster())
        # print(momento_dia())
        # print(area())
        raster_data <-  base_raster |>
        dplyr::filter(fecha == as.Date(fecha_elegida(),
                                             origin = "1970-01-01"),
                      tipo_de_raster ==  tipo_de_raster(),
                      momento == momento_dia(), # es un valor no reactivo
                      locacion ==  area()
        ) 
        print("CCC")
print(raster_data$filename)
        ## THIS WORKS
        query <- paste0("SELECT ST_AsGDALRaster(rast, 'GTiff') AS rast FROM raster_schema.rasters_geo WHERE filename='",
                        raster_data$filename,"';")
        result <- pool::dbGetQuery(pool, query)
        print(result)
        # Save the binary raster data to a temporary file
        temp_file <- tempfile(fileext = ".tif")
        writeBin(result$rast[[1]], temp_file)
        print(result$rast[[1]])
        # Load the raster package and read the temporary file
        raster <- terra::rast(temp_file)

      
      # onStop(function() {
      #   pool::poolClose(con)
      # })
      # 
      
    return(raster = list(raster))
      })

      terra::rast(raster()) 
     
    }
