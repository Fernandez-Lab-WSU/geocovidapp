#' Convierte el formato de la fecha de %Y-%m-%dT%H:%M:%S a "%Y-%m-%d"
#'
#' @param fecha Fecha 
#'
#' @return Fecha en formato "%Y-%m-%d"
#' @export
#'
#' @examples
#' 
#' formatted_date("2020-05-10T07:00:00.000Z")
#'  
formatted_date <- function(fecha) {
  if (is.null(fecha) || is.na(fecha)) {
    stop("La fecha es NULL o NA.")
  }
  
  parsed_date <- lubridate::parse_date_time( # mas flexible, adapta el formato que se le de
    fecha,
    orders = c("ymd HMS", "ymd HM", "ymd", "dmy HMS", "dmy", "Ymd"),
    quiet = TRUE
  )
  
  if (is.na(parsed_date)) {
    stop(paste("No se pudo interpretar la fecha:", fecha))
  }
  
  return(format(parsed_date, "%Y-%m-%d"))
}