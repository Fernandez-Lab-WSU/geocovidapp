#' Verificar si la combinación de parámetros existe en el dataframe `base_raster`
#'
#' Esta función toma los valores de área, tipo de raster, fecha y momento del día 
#' seleccionados por el usuario y verifica si existe una fila en el dataframe `base_raster`
#' que coincida con todos estos valores.
#'
#' @param area Una cadena de texto que representa el área seleccionada por el usuario (por ejemplo, "baires" o "amba").
#' @param porcentaje Una cadena de texto que representa el tipo de cambio porcentual seleccionado por el usuario (por ejemplo, "pc" o "7dpc").
#' @param fecha Un objeto de fecha que representa la fecha seleccionada por el usuario.
#' @param momento Una cadena de texto que representa el momento del día seleccionado por el usuario (por ejemplo, "mañana", "tarde" o "noche").
#' @param base_raster Un dataframe que contiene los rasters con las características de locación, tipo de raster, fecha y momento del día.
#'
#' @return `TRUE` si existe una fila en `base_raster` que cumpla con todas las condiciones especificadas; de lo contrario, `FALSE`.
#'
#' @export
raster_valido <- function(area, porcentaje, fecha, momento, base_raster) {
  # Verifica si existe una fila en base_raster que cumpla con las condiciones
  combinacion_valida <- any(base_raster[["locacion"]] == area &
                              base_raster[["tipo_de_raster"]] == porcentaje &
                              base_raster[["fecha"]] == fecha &
                              base_raster[["momento"]] == momento)
  
  # Voy a extraer tambien cuales son los rasters faltantes 
  # con respecto al tipo de cambio porcentual
  faltantes <- base_raster |> 
    dplyr::filter(locacion == area,
           fecha == as.Date(fecha, format = "%Y-%m-%d"),
           momento == momento)
  
  if (nrow(faltantes) == 0) {
    faltan <- "No hay rasters disponibles para esta fecha"
  } else if (nrow(faltantes) == 2) {
    faltan <- NULL # Ambos rasters estan disponibles
  } else if (nrow(faltantes) == 1 && dplyr::pull(faltantes, tipo_de_raster) == 'pc') {
    faltan <- "Solo el raster de tipo de cambio PREPANDEMIA está disponible para esta fecha"
  } else if (nrow(faltantes) == 1 && dplyr::pull(faltantes, tipo_de_raster) == '7dpc') {
    faltan <- "Solo el raster de tipo de cambio SEMANAL está disponible para esta fecha"
  } else {
    faltan <- "Error"
  }
  
  # Devuelve TRUE si la combinación es válida, de lo contrario FALSE
  return(list(
    combinacion_valida = combinacion_valida,
    faltan = faltan
  ))
}

