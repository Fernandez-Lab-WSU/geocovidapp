% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_t2_mapa_partido.R
\name{mapa_partido}
\alias{mapa_partido}
\title{Genera un mapa interactivo de un partido con datos ráster}
\usage{
mapa_partido(partido, raster, opacidad)
}
\arguments{
\item{partido}{Un objeto \code{sf} que representa los límites geográficos del partido.}

\item{raster}{Un objeto ráster que contiene los valores a visualizar en el mapa.}

\item{opacidad}{Un valor numérico entre 0 y 1 que define la opacidad de la capa ráster.}
}
\value{
Un objeto \code{leaflet} con el mapa interactivo.
}
\description{
Esta función crea un mapa interactivo utilizando \code{leaflet},
donde se visualiza un partido (unidad geopolítica) y un ráster superpuesto.
Se utiliza una paleta de colores para representar los valores del ráster.
}
\examples{
\dontrun{
mapa <- mapa_partido(partido = mi_partido_sf, 
                     raster = mi_raster, 
                     opacidad = 0.5)
mapa
}
}
