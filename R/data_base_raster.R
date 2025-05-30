#' Información de archivos raster usados en el proyecto
#'
#' Conjunto de datos que contiene metadata sobre los archivos raster empleados en el análisis,
#' incluyendo información del archivo, ubicación geográfica, tipo de raster, fecha, hora y momento.
#'
#' Para una explicación más detallada sobre el origen y procesamiento de estos datos,
#' consulte el sitio web del proyecto GeoCovid Buenos Aires:
#' <https://fernandez-lab-wsu.github.io/geocovid_bsas/> y el código en
#' el archivo `tools/helper_datasets.R` incluido en este paquete.
#'
#' @format Un data frame con `n` filas y 7 columnas:
#' \describe{
#'   \item{filename}{Nombre del archivo raster}
#'   \item{file_info}{Información adicional sobre el archivo (puede incluir tamaño, formato, etc.)}
#'   \item{locacion}{Ubicación geográfica o área asociada al raster}
#'   \item{tipo_de_raster}{Tipo de raster (por ejemplo, 'pc', '7dpc', etc.)}
#'   \item{fecha}{Fecha asociada al raster}
#'   \item{hora}{Hora asociada al raster}
#'   \item{momento}{Momento o periodo relacionado con el raster (por ejemplo, pre-pandemia, semanal, etc.)}
#' }
#'
#' @source Datos generados y procesados para la aplicación GeoCovid
"base_raster"
