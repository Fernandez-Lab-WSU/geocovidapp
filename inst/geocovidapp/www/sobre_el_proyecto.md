## GeoCovid App

GeoCovid App permite visualizar rasters de movilidad ciudadana para tres 
horas del dia (mañana, tarde y noche) en provincia de Buenos Aires en el
periodo de marzo a diciembre de 2020.

A su vez, los datos se pueden visualizar por partido de la provincia a la para
de los casos de COVID-19 en el tab 'por partido' y promediados en dia y noche
para el tab 'panorama Buenos Aires'


### Datos

El procesamiento de estos datos se encuentra detallado en [GeoCovid Buenos Aires](https://fernandez-lab-wsu.github.io/geocovid_bsas/)

##### 1. [Mapas vectoriales de Buenos Aires y sus centroides](https://fernandez-lab-wsu.github.io/geocovid_bsas/creacion_mapa_bsas.html)

- Los mapas de las comunas de Ciudad Autónoma de Buenos Aires
fueron descargados de [BA Data](https://data.buenosaires.gob.ar/dataset/comunas/resource/Juqdkmgo-612222-resource), 
mientras que los poligonos correspondientes a los
partidos de provincia de Buenos Aires se descargaron
del [Instituto Geográfico Nacional Argentino](https://www.ign.gob.ar/NuestrasActividades/InformacionGeoespacial/CapasSIG).

##### 2. [Casos de COVID-19](https://fernandez-lab-wsu.github.io/geocovid_bsas/analisis_datos_covid.html) 
- Archivo historico de casos de COVID-19 registrados desde el 01/03/2020 hasta
el 04/06/2022 - [Dirección Nacional de Epidemiología y Análisis de Situación de Salud](https://datos.gob.ar/dataset/salud-covid-19-casos-registrados-republica-argentina/archivo/salud_fd657d02-a33a-498b-a91b-2ef1a68b8d16).  
Estos datos son públicos y fueron consultados en Julio de 2023 en [www.datos.salud.gob.ar](http://datos.salud.gob.ar/dataset/covid-19-casos-registrados-en-la-republica-argentina).

##### 3. [Imágenes raster de movilidad ciudadana](https://fernandez-lab-wsu.github.io/geocovid_bsas/creacion_de_rasters.html)

- Los datos de movilidad ciudadana fueron brindados por
[Data for Good - Meta](https://dataforgood.facebook.com/)

- El procesamiento de los datos y la generacion de los rasters fue realizado
usando el [paquete `quadkeyr`](https://github.com/Fernandez-Lab-WSU/quadkeyr). 

##### 4. [Cálculo de la movilidad porcentual por departamento de prov. de Bs.As.](https://fernandez-lab-wsu.github.io/geocovid_bsas/calculo_movilidad_por_departamento.html)

- Los datos de movilidad ciudadana fueron brindados por
[Data for Good - Meta](https://dataforgood.facebook.com/) y se promediaron para
 los mapas vectoriales de prov. de Buenos Aires mencionados en el punto 1.


### Proyectos relacionados
El paquete `quadkeyr`, GeoCovid Buenos Aires y GeoCovid App son parte del mismo
proyecto.

- [Paquete `quadkeyr`](https://github.com/Fernandez-Lab-WSU/quadkeyr)
Permite el analisis de datos de movilidad ciudadana de Meta y su conversión
a imagenes raster.

- [GeoCovid Buenos Aires](https://github.com/Fernandez-Lab-WSU/geocovid_bsas)
Website con cuatro reportes que recopilan el procesamiento de 
los datos de COVID-19, movilidad ciudadana, mapas vectoriales e imágenes 
raster para provincia de Buenos Aires en el marco de la creación de 
a aplicación GeoCovid app.

### Licencias

- El código contenido en este repositorio se encuentra bajo una [licencia MIT](https://github.com/Fernandez-Lab-WSU/geocovidapp/blob/main/LICENSE.md). 

### Citar

> D'Andrea, F. GeoCovid Buenos Aires [Computer software].
> Dr. Fernandez Lab. Washington State University.
> https://github.com/Fernandez-Lab-WSU/geocovidapp

### Código de Conducta

El proyecto GeoCovid Buenos Aires, GeoCovid app y el paquete `quadkeyr`
se encuentran bajo un [Código de Conducta](https://www.contributor-covenant.org/es/version/1/4/code-of-conduct/). 

### Referencias

- Ramírez ML, Martinez SM, Bessone CDV, Allemandi DA, Quinteros DA. COVID-19: 
[Epidemiological Situation of Argentina and its Neighbor Countries after Three
Months of Pandemic.](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8193186/) 
Disaster Med Public Health Prep. 2022 Oct;16(5):1935-1941.
doi: 10.1017/dmp.2021.90. Epub 2021 Mar 25. PMID: 33762042; PMCID: PMC8193186.

