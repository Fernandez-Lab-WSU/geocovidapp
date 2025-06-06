% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_t3_mapa_covid_elecciones_input.R
\name{MapaCovidElecciones_UI}
\alias{MapaCovidElecciones_UI}
\title{IU: Selector de fecha}
\usage{
MapaCovidElecciones_UI(id, base_raster, amba_reducido_names)
}
\arguments{
\item{id}{Module name}

\item{base_raster}{Dataframe que lista todos los rasters y desagrega en
sus columnas características de interes, como si son rasters de
AMBA o Buenos Aires, si el cambio porcentual es semanal o prepandemia
o el momento del día que representan.}

\item{amba_reducido_names}{String. Vector con los nombres de los partidos
que conforman el AMBA.}
}
\value{
Selector de fecha
}
\description{
IU: Selector de fecha
}
