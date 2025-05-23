% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_t3_mapa_bsas_covid_burbujas.R
\name{mapa_burbujas}
\alias{mapa_burbujas}
\title{Genera un mapa interactivo con burbujas y polígonos coloreados}
\usage{
mapa_burbujas(
  data_sf,
  color_var,
  data_burb_peq,
  data_burb_med,
  data_burb_gra,
  cent_x,
  cent_y,
  n_casos,
  partido,
  burb_peq,
  burb_med
)
}
\arguments{
\item{data_sf}{Un \code{sf} data frame con los polígonos geográficos.}

\item{color_var}{Una cadena de texto que indica la columna de \code{data_sf}
utilizada para definir los colores de los polígonos.}

\item{data_burb_peq}{Un data frame con las coordenadas y datos de burbujas pequeñas.}

\item{data_burb_med}{Un data frame con las coordenadas y datos de burbujas medianas.}

\item{data_burb_gra}{Un data frame con las coordenadas y datos de burbujas grandes.}

\item{cent_x}{Nombre de la columna con la coordenada X del centroide de las burbujas.}

\item{cent_y}{Nombre de la columna con la coordenada Y del centroide de las burbujas.}

\item{n_casos}{Nombre de la columna con la cantidad de casos representados por las burbujas.}

\item{partido}{Nombre de la columna con el nombre de la región o partido.}

\item{burb_peq}{Límite superior del rango para las burbujas pequeñas.}

\item{burb_med}{Límite superior del rango para las burbujas medianas.}
}
\value{
Un objeto \code{plotly} con el mapa interactivo.
}
\description{
Esta función crea un mapa interactivo utilizando \code{plotly},
que representa polígonos coloreados según una variable categórica y
burbujas de diferentes tamaños según la cantidad de casos de COVID-19.
}
\examples{
\dontrun{
mapa_burbujas(data_sf = px_data_mapa,
              color_var = "momento",
              data_burb_peq = tam_burb_small,
              data_burb_med = tam_burb_medium,
              data_burb_gra = tam_burb_large,
              cent_x = "X",
              cent_y = "Y",
              n_casos = "n_casos",
              partido = "partido",
              burb_peq = size_small,
              burb_med = size_medium)
}

}
