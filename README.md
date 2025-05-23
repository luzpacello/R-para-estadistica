# R-para-estadistica
codigo de R para estadistica de la untdf

ver el directorio actual
getwd()


Metodos para configurar el directorio(Windows)
- Cambiar el directorio manualmente
setwd("D:/mi_carpeta/mis_datos")      # Usá / o \\
setwd("D:\\mi_carpeta\\mis_datos")    # También funciona

- Seleccionar la carpeta con una ventana
setwd(choose.dir())   # Solo funciona si R tiene acceso a GUI (RGui o RStudio en Windows)

- haciendo click derecho sobre la pestaña del archivo con el que estamos trabajando

- Desde RStudio
Session > Set Working Directory > Choose Directory...

- Usando Paquetes 
install.packages("here")
library(here)

(Te da la raíz del proyecto (muy útil en scripts))
here::here("datos", "archivo.csv")
