% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_t1_fecha_momento.R
\name{FechaMomento_Server}
\alias{FechaMomento_Server}
\title{Convierte el archivo en raster en base a las elecciones del usuario}
\usage{
FechaMomento_Server(id, pool, base_raster, mapa_zoom)
}
\arguments{
\item{id}{Module name}

\item{base_raster}{Dataframe que lista todos los rasters y desagrega en
sus columnas características de interes, como si son rasters de
AMBA o Buenos Aires, si el cambio porcentual es semanal o prepandemia
o el momento del día que representan.}

\item{mapa_zoom}{Valor de zoom que retorna leaflet en base al uso del mapa.}
}
\value{
Una serie de variables extraidas del dataframe base_raster para
el raster que eligio el usuario y representan el area, la opacidad,
el raster mismo (imagen) y basemap
}
\description{
Convierte el archivo en raster en base a las elecciones del usuario
}
