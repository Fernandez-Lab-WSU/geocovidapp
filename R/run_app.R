#' Launches the app
#'
#' @return GeoCovid app
#' @importFrom shiny shinyApp
#'
#' @export
#'
#' @examples
#' \dontrun{
#' run_app()
#' }
run_app <- function() {
  appDir <- system.file("geocovidapp", package = "geocovidapp")
  if (appDir == "") {
    stop("Could not find the app. Try re-installing `geocovidapp`.", call. = FALSE)
  }
  
  shiny::runApp(appDir)
}