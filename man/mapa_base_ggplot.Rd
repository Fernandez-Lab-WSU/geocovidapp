% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_t2_reporte_mapa_ggplot.R
\name{mapa_base_ggplot}
\alias{mapa_base_ggplot}
\title{Obtener Mapa Base con Tiles}
\usage{
mapa_base_ggplot(part, zoom)
}
\arguments{
\item{part}{Nombre del partido a visualizar. Debe coincidir con la columna \code{partido} del objeto \code{geocovidapp::bsas_comunas}.}

\item{zoom}{Nivel de zoom (entero) para los tiles del mapa. Valores más altos generan más detalle.}
}
\value{
Un objeto \code{SpatRaster} que contiene los tiles del mapa base para la región especificada.
}
\description{
Descarga un mapa base en formato raster (tiles) para un partido especificado.
Usa la extensión geográfica del partido como área de recorte. Ideal para usar como capa de fondo en visualizaciones con \code{ggplot2} o \code{terra}.
}
\note{
A la fecha de hoy es imposible usar otro método para obtener un mapa base que respete la extensión precisa del área de interés.
}
