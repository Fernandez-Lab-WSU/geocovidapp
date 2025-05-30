#' Casos de COVID-19 por departamento
#'
#' Conjunto de datos que contiene información diaria sobre la cantidad de casos de COVID-19
#' agrupados por fecha de diagnóstico y provincia de residencia. 
#' 
#' Datos procesados del archivo historico de casos de COVID-19 registrados 
#' desde el 01/03/2020 hasta el 04/06/2022 por la 
#' Dirección Nacional de Epidemiología y Análisis de Situación de Salud.
#' Estos datos son públicos y fueron consultados en Julio de 2023 en 
#' www.datos.salud.gob.ar.
#'
#' Para una explicación más detallada sobre el origen y procesamiento de estos datos,
#' consulte el sitio web del proyecto GeoCovid Buenos Aires:
#' <https://fernandez-lab-wsu.github.io/geocovid_bsas/> y el código en
#' el archivo `tools/helper_datasets.R` incluido en este paquete.
#'
#' @format Un data frame con `n` filas y 3 columnas:
#' \describe{
#'   \item{fecha_enfermo}{Fecha de diagnóstico o fecha en que se confirmó el caso}
#'   \item{residencia_provincia_nombre}{Nombre de la provincia de residencia del caso}
#'   \item{n}{Número de casos confirmados en esa fecha y provincia}
#' }
#'
#' @source Dirección Nacional de Epidemiología y Análisis de Situación de Salud (www.datos.salud.gob.ar)
"casos_covid_dpto"
