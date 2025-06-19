# Este archivo contiene un promedio de valores de movilidad ciudadana 
# por departamento de Buenos Aires

# Un analisis mas pormenorizado de esto se puede ver en 
# https://fernandez-lab-wsu.github.io/geocovid_bsas/calculo_movilidad_por_departamento.html



base_raster_baires <- base_raster |>
  dplyr::filter(locacion == 'baires') |> #elimina valores de amba que coinciden
  dplyr::cross_join(bsas) |>
  dplyr::mutate(file = paste0('data/inicial/', value)) |>
  sf::st_as_sf()

px_baires <- c()

for(i in 1:nrow(base_raster_baires)){
  
  
  # cada fila corresponde a un partido diferente
  poli <- sf::st_as_sf(base_raster_baires[i, c('partido','geom')]) |>
    st_transform(3857)
  
  # si bien el raster va a ser el mismo en muchos casos
  raster <- terra::rast(base_raster_baires$file[i])
  crs(raster) <-  "epsg:3857"
  
  
  int_data2 <- terra::extract( raster,
                               sf::st_as_sf(poli),
                               fun = mean,
                               na.rm = TRUE)
  
  
  px_data <- data.frame(base_raster_baires[i,],
                        'px_mean' = int_data2[[2]] 
  )
  
  px_baires <- rbind(px_data, px_baires)
}

px_baires_w <- px_baires |>
  select(fecha, locacion, tipo_de_raster, 
         momento, hora, partido, px_mean, geom) |>
  pivot_wider(
    names_from = c(momento, hora),
    values_from = px_mean) |> # esto habria que
  mutate(px_mean_dianoche = ((mañana_8 + tarde_16)/2))


px_baires_w 

px_bsas <-  px_baires_w  |>
  dplyr::filter(fecha == '2020-05-06',
                tipo_de_raster == 'pc'
  ) |>
  dplyr::mutate(criterio = case_when(px_mean_dianoche > 40 ~ "mas de 40",
                                     40 > px_mean_dianoche &
                                       px_mean_dianoche > 30 ~ "40 - 30",
                                     30 > px_mean_dianoche &
                                       px_mean_dianoche > 20 ~ "30 - 20",
                                     20 > px_mean_dianoche &
                                       px_mean_dianoche > 10 ~ "20 - 10",
                                     10 > px_mean_dianoche & 
                                       px_mean_dianoche > 1 ~ "10 - 1",
                                     1 > px_mean_dianoche &
                                       px_mean_dianoche> -1 ~ "sin cambios",
                                     -1 > px_mean_dianoche& 
                                       px_mean_dianoche > -10 ~ "-1 - -10",
                                     -10 > px_mean_dianoche& 
                                       px_mean_dianoche > -20 ~ "-10 - -20",
                                     -20 > px_mean_dianoche& 
                                       px_mean_dianoche > -30 ~ "-20 - -30",
                                     -30 > px_mean_dianoche& 
                                       px_mean_dianoche > -40 ~ "-30 - -40",
                                     -40 > px_mean_dianoche  ~ "menor a -40"),
                criterio_noche = case_when(noche_0 > 40 ~ "mas de 40",
                                           40 > noche_0 &
                                             noche_0 > 30 ~ "40 - 30",
                                           30 > noche_0 &
                                             noche_0 > 20 ~ "30 - 20",
                                           20 > noche_0 &
                                             noche_0 > 10 ~ "20 - 10",
                                           10 > noche_0 &
                                             noche_0 > 1 ~ "10 - 1",
                                           1 > noche_0 &
                                             noche_0> -1 ~ "sin cambios",
                                           -1 > noche_0 &
                                             noche_0 > -10 ~ "-1 - -10",
                                           -10 > noche_0 &
                                             noche_0 > -20 ~ "-10 - -20",
                                           -20 > noche_0 &
                                             noche_0 > -30 ~ "-20 - -30",
                                           -30 > noche_0 &
                                             noche_0 > -40 ~ "-30 - -40",
                                           -40 > noche_0  ~ "menor a -40")) |>
  dplyr::mutate(criterio = fct_relevel(criterio, c("mas de 40", 
                                                   "40 - 30",
                                                   "30 - 20",
                                                   "20 - 10","10 - 1",
                                                   "sin cambios",
                                                   "-1 - -10", 
                                                   "-10 - -20",
                                                   "-20 - -30", 
                                                   "-30 - -40",
                                                   "menor a -40")),
                criterio_noche = fct_relevel(criterio_noche, 
                                             c("mas de 40", 
                                               "40 - 30",
                                               "30 - 20",
                                               "20 - 10",
                                               "10 - 1",
                                               "sin cambios",
                                               "-1 - -10", 
                                               "-10 - -20",
                                               "-20 - -30", 
                                               "-30 - -40",
                                               "menor a -40"))) |>
  st_as_sf()