# Cargar bases de datos
# Los datos estan en una carpeta interna dentro de inst/
 
# Instalo el paquete con los modulos
 source("install.R")

 load("data/bsas.rda")
 load("data/bsas_comunas.rda")
 load("data/centroides_mapa.rda")
 load("data/base_raster.rda")
 load("data/data_sisa.rda")

# Para chequar la reactividad, descomentar
#library(reactlog)
#reactlog_enable()

# Cargo datasets ------

amba_reducido_names <- c('Almirante Brown',
                         'Avellaneda',
                         'Berazategui',
                         paste('Comuna', 1:15), # CABA
                         'Esteban Echeverría', 'Escobar', 'Ezeiza',
                         'Florencio Varela',
                         'General San Martín',
                         'Hurlingham',
                         'Ituzaingó',
                         'José C. Paz',
                         'La Matanza',  'Lanús', 'Lomas de Zamora',
                         'Malvinas Argentinas', 'Merlo', 'Moreno', 'Morón',
                         'Quilmes', 'Pilar', 'Presidente Perón',
                         'San Fernando', 'San Isidro', 'San Miguel',
                         'Tigre', 'Tres de Febrero',
                         'Vicente López')

amba_caba <- c('Almirante Brown',
               'Avellaneda',
               'Berazategui',
               'Capital Federal', # CABA
               'Esteban Echeverría', 'Escobar', 'Ezeiza',
               'Florencio Varela',
               'General San Martín',
               'Hurlingham',
               'Ituzaingó',
               'José C. Paz',
               'La Matanza',  'Lanús', 'Lomas de Zamora',
               'Malvinas Argentinas', 'Merlo', 'Moreno', 'Morón',
               'Quilmes', 'Pilar', 'Presidente Perón',
               'San Fernando', 'San Isidro', 'San Miguel',
               'Tigre', 'Tres de Febrero',
               'Vicente López')


# Conecto con la base de datos
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

