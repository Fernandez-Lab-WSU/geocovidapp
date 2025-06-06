% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_t1_mapa_raster.R
\name{addRasterLegend}
\alias{addRasterLegend}
\title{Agregar leyenda al mapa raster}
\usage{
addRasterLegend(mapa, imagen, opacidad, paleta, etiquetas)
}
\arguments{
\item{mapa}{Mapa Leaflet al que se le añadirán los polígonos.}

\item{imagen}{Objeto \code{raster} que representa la imagen raster a ser agregada al mapa.}

\item{opacidad}{Valor de opacidad que controla la transparencia de la imagen raster en el mapa.}

\item{pal}{Objeto de tipo \code{colorBin} o \code{colorNumeric} de \code{leaflet} que define la paleta de colores que se usará para la visualización de los valores en el raster.}
}
\value{
Un objeto \code{leaflet} con la imagen raster agregada y una leyenda que muestra el porcentaje de cambio.
}
\description{
Esta función agrega una imagen raster al mapa interactivo utilizando \code{leaflet} y agrega una leyenda que muestra el porcentaje de cambio
en los valores del raster. La leyenda es configurada con colores binarios según los valores del raster y se posiciona en la esquina superior derecha.
}
