#' Límites de partidos del AMBA y comunas de la Ciudad Autónoma de Buenos Aires
#'
#' Un conjunto de datos que contiene los límites espaciales de todos los partidos
#' del Área Metropolitana de Buenos Aires (AMBA) junto con las comunas de la Ciudad Autónoma de Buenos Aires,
#' con información de la división por partido o comuna y su geometría correspondiente.
#'
#' Para una explicación más detallada sobre el origen y procesamiento de estos datos,
#' consulte el sitio web del proyecto GeoCovid Buenos Aires:
#' <https://fernandez-lab-wsu.github.io/geocovid_bsas/> y el código en
#' el archivo `tools/helper_datasets.R` incluido en este paquete.
#'
#' @format Un data frame con `n` filas y 2 columnas:
#' \describe{
#'   \item{partido}{Nombre del partido o comuna}
#'   \item{geom}{Columna de geometría (típicamente un objeto de clase `sf` o `sfc_POLYGON`)}
#' }
#' #' @source Los poligonos de las comunas de Ciudad Autónoma de Buenos Aires fueron descargados de BA Data, mientras que los polígonos de los partidos de provincia de Buenos Aires se descargaron del Instituto Geográfico Nacional Argentino.
"bsas_comunas"
