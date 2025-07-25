#' Archivo histórico de casos de COVID-19 en Argentina
#'
#' Este conjunto de datos contiene información de casos de COVID-19 registrados
#' por la Dirección Nacional de Epidemiología y Análisis de Situación de Salud,
#' desde el 1 de marzo de 2020 hasta el 4 de junio de 2022.
#'
#' Cada fila representa un caso individual, con su ID, provincia y departamento
#' de residencia, y la fecha de inicio de síntomas.
#'
#' @format Un tibble con 6 filas y 4 columnas:
#' \describe{
#'   \item{id_evento_caso}{`integer`. Identificador único del caso.}
#'   \item{residencia_provincia_nombre}{`character`. Nombre de la provincia de residencia.}
#'   \item{residencia_departamento_nombre}{`character`. Nombre del departamento o localidad de residencia.}
#'   \item{fecha_enfermo}{`character`. Fecha de inicio de síntomas (AAAA-MM-DD).}
#' }
#'
#' @source Dirección Nacional de Epidemiología y Análisis de Situación de Salud.  
#' Datos consultados en julio de 2023 en [datos.salud.gob.ar](https://www.datos.salud.gob.ar).
#'
#' @details Estos datos son de acceso público y forman parte del archivo
#' histórico de casos de COVID-19 en Argentina.
"data_sisa"

