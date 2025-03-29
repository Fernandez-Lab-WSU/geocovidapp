# POR QUE ESTE ARCHIVO?

# Notar que la app NO esta leyendo el DESCRIPTION file del paquete
# Durante el deployment con shinyapps.io solo se carga la carpeta inst/
# Esto quiere decir que los paquetes que shinyapps.io no detecta de por si
# mejor instalarlos explicitamente en el ambiente de deployment.

if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

if (!requireNamespace("geocovidapp", quietly = TRUE)) {
  devtools::install_github("Fernandez-Lab-WSU/geocovidapp")
}

if (!requireNamespace("markdown", quietly = TRUE)) {
  install.packages("markdown")
}

if (!requireNamespace("config", quietly = TRUE)) {
  install.packages("config")
}

# # Ensure chromote is installed for headless browser
# if (!requireNamespace("chromote", quietly = TRUE)) {
#   install.packages("chromote")
# }
# 
# 
# # Specify the path to Chromium on shinyapps.io
# options(webshot2.chrome = "/usr/bin/chromium")
# Sys.setenv(CHROMOTE_CHROME = "/usr/bin/chromium-browser")
