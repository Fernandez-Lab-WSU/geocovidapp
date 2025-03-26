
# GeoCovid App <img src="inst/geocovidapp/www/geocovid_logo.png" align="right" height="150" />

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R-CMD-check](https://github.com/Fernandez-Lab-WSU/geocovid_app/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Fernandez-Lab-WSU/geocovid_app/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

ESTE PAQUETE SE ENCUENTRA EN REVISION. ES UNA VERSION PRELIMINAR.

## Instalación

Si queres instalar una versión de GeoCovid App podes hacerlo ejecutando
el siguiente código en R:

``` r
# install.packages("devtools")
devtools::install_github("Fernandez-Lab-WSU/geocovidapp")
geocovidapp::runapp()
```

## Tabs

### Tab 1
```mermaid
flowchart TB

  subgraph server
    FechaMomentoServer
    MapaBuenosAiresServer
  end

  subgraph ui
    FechaMomentoUI
    MapaBuenosAiresUI
  end

  FechaMomentoUI -- "elecciones usuario" --> FechaMomentoServer
  FechaMomentoServer -- "raster" --> MapaBuenosAiresServer
  MapaBuenosAiresServer -- "mapa" --> MapaBuenosAiresUI

```

## Licencias

El código contenido en este repositorio se encuentra bajo una [licencia
MIT](https://github.com/Fernandez-Lab-WSU/geocovidapp/blob/main/LICENSE.md).

## Código de Conducta

El proyecto GeoCovid Buenos Aires, GeoCovid app y el paquete `quadkeyr`
se encuentran bajo un [Código de
Conducta](https://www.contributor-covenant.org/es/version/1/4/code-of-conduct/).
