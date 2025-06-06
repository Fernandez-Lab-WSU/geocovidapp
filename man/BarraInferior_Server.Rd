% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_t1_barra_informativa_inferior.R
\name{BarraInferior_Server}
\alias{BarraInferior_Server}
\title{Server Logic for Informational Bottom Bar}
\usage{
BarraInferior_Server(id, boton, fecha, area, momento, porcentaje)
}
\arguments{
\item{id}{Character string used to identify the module namespace.}

\item{boton}{Reactive expression for the update button (e.g., \code{input$update_map}).}

\item{fecha}{Reactive expression returning the selected date as a string.}

\item{area}{Reactive expression indicating the selected area (e.g., "amba").}

\item{momento}{Reactive expression indicating the time of day (e.g., "mañana", "tarde").}

\item{porcentaje}{Reactive expression indicating the data type (e.g., "pc" for prepandemia).}
}
\value{
No return value. Called for side effects to update UI text dynamically.
}
\description{
Server-side logic for managing the informational text displayed in the
bottom bar. Reacts to user interaction and input changes to update the message.
}
