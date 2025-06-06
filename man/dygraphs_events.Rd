% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_t2t3_dygraph_casos_covid.R
\name{dygraphs_events}
\alias{dygraphs_events}
\title{Add Custom Dygraph Events and Styling}
\usage{
dygraphs_events(plot)
}
\arguments{
\item{plot}{A dygraph object created with \code{dygraphs::dygraph()}.}
}
\value{
A modified dygraph object with enhanced interactivity and visual elements.
}
\description{
Enhances a dygraph plot by adding axis labels, interactivity options, events, and custom styling.
}
\details{
This function takes a dygraph object and adds several enhancements, including:
\itemize{
\item Y-axis label
\item Custom options for grid and labels
\item Interactive highlighting and crosshair
\item Three vertical event lines with labels
\item A custom legend
\item External CSS styling
}
}
\examples{
\dontrun{
library(dygraphs)
ts_data <- ts(rnorm(100), start = c(2020, 1), frequency = 52)
plot <- dygraph(ts_data)
dygraphs_events(plot)
}

}
