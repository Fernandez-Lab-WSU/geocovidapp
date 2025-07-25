#' Calcular casos diarios de COVID-19 por partido
#'
#' @description
#' Filtra los registros del dataset `data_sisa` para una provincia y fecha específica, 
#' y devuelve la cantidad de casos por partido (departamento).
#'
#' @param provincia `character`. Nombre de la provincia a filtrar (por ejemplo, `"Buenos Aires"`).
#' @param fecha `Date` o `character`. Fecha de referencia para filtrar los casos (`fecha_enfermo`).
#'
#' @return Un `tibble` con dos columnas: `partido` (nombre del partido) y `n_casos` (número de casos reportados).
#'
#' @export
casos_partido_diarios <- function(provincia, fecha){
  
  geocovidapp::data_sisa |>
  dplyr::filter(
    residencia_provincia_nombre == provincia &
      fecha_enfermo == fecha
  ) |> # combino horarios
  dplyr::group_by(residencia_departamento_nombre) |>
  dplyr::summarize(n_casos = dplyr::n()) |>
  dplyr::rename(partido = residencia_departamento_nombre)
  
  }