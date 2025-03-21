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
formatted_date <- function(fecha){

# Conservo solo la fecha
parsed_date <- lubridate::ymd_hms(fecha)

# Format the date to "YYYY-MM-DD" format
formatted_date <- format(parsed_date, format = "%Y-%m-%d") 

formatted_date
}