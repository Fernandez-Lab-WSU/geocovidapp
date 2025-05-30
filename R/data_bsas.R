#' Límites de los partidos del Gran Buenos Aires
#'
#' Un conjunto de datos que contiene los límites espaciales de distintos partidos
#' (divisiones municipales), incluyendo coordenadas del bounding box y geometrías.
#'
#' Para una explicación más detallada sobre el origen y procesamiento de estos datos,
#' consulte el sitio web del proyecto GeoCovid Buenos Aires:
#' <https://fernandez-lab-wsu.github.io/geocovid_bsas/> y el código en
#' el archivo `tools/helper_datasets.R` incluido en este paquete.
#'
#' @format Un data frame con `n` filas y 7 columnas:
#' \describe{
#'   \item{partido}{Nombre del partido (división municipal)}
#'   \item{lat1}{Latitud sur (límite inferior del bounding box)}
#'   \item{lat2}{Latitud norte (límite superior del bounding box)}
#'   \item{lng1}{Longitud oeste (límite izquierdo del bounding box)}
#'   \item{lng2}{Longitud este (límite derecho del bounding box)}
#'   \item{geom}{Columna de geometría (típicamente un objeto de clase `sf` o `sfc_POLYGON`)}
#' }
#'#' @source Los polígonos de los partidos de provincia de Buenos Aires 
#' se descargaron del Instituto Geográfico Nacional Argentino.
"bsas"

