#' Title
#'
#' @param provincia 
#' @param fecha 
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