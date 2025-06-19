pixel_loader <- function(partido, tipo_de_raster){

  db <- config::get("database")
  
  pool2 <- pool::dbPool(
    drv = RPostgres::Postgres(),
    dbname = db$dbname,
    user = db$user,
    password = db$password,
    port = db$port,
    host = db$host
  )

  query <- paste(
    "
  select * from px_prom",
  "where partido = ",  partido,
  "and tipo_de_raster = ", tipo_de_raster
  )
  print('query')
  print(query)
  data_px <- sf::st_read(pool2, layer = 'pixeles',
                       DBI::Id(schema="px_prom", table = "px_prom")) 


# use_data(data_px)
# 
# print("PXXXX")
# print(data_px)
return(data_px)
}