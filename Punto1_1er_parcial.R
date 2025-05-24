# UE = objeto o sujeto que pertenece a la muestra o poblacion
# Variable = caracteristica de interes de cada UE
# Poblacion = coleccion o conjunto de individuos, objetos o 
# eventos cuyas propiedades se analizaran
# Muestra = Subconjunto de una poblacion
# Poblacion de undades = Es el conjunto de todas las unidades experimentales posibles que podrían ser observadas en un estudio.
# Muestra de unidades = Un subconjunto de unidades experimentales

#Primero definimos el directorio en el que etamos trabajando
#Nota: Si trabajas con R estudio, le das click derecho al nombre del archivo
#     le das a "Set Working Directory" para setear el directorio en el cual
#     estas trabajando
#Obtenemos el directorio en el que estamos trabajando
getwd()
#Si el directorio es igual que donde esta guardado el archivo, podes continuar
#En caso contrario,mira README.md del repositorio https://github.com/luzpacello/R-para-estadistica.git

# Función para borrar todos los elementos
rm(list=ls())

# Libreria para calcular la asimetria y curtosis
options(scipen = 999)
library(moments)

# Lista de datos
datos<-c(6.3, 6.4, 7.7, 8.4, 8.5, 8.8, 8.9, 9.0,
         9.1,10,10.1,10.2,10.6,10.6,10.7,10.7,10.8,
         10.9,11.1,11.2,11.2,11.4,11.9,11.9,12.2,13.1)

#tamaño de la muestra
n <- length(datos)
cat("tamaño de muestra:", n, "\n")

suma <- sum(datos)
cat("suma:", suma, "\n")
#Medidas de posicion central
media <- round(mean(datos), 4)
cat("media:", media, "\n")
mediana <- round(median(datos),4)
cat("mediana:", mediana, "\n")

moda <- function(x) {
    ux = unique(x)
    tab = tabulate(match(x, ux))
    ux[tab == max(tab)]
}
print("moda")
moda(datos)

# Percentiles, deciles y cuartiles
#como los datos ya estan ordenados no es necesario usar sort

#   ------ PERCENTILES ------
#Funcion
#percentil 50
print("percentil 50")
quantile(datos, probs = 0.50)
#todos los percentilres
#seq es para crear secuencias de datos
print("percentiles")
quantile(datos, probs = seq(0, 1, 0.01))
#   ------ DECILES ------
#decil 50}
print("decil 50")
quantile(datos, probs = 0.50)
#todos los deciles
print("deciles") 
quantile(datos, probs = seq(0, 1, 0.1))
#   ------ CUARTILES ------
#todos los cuartiles
print("cuartiles")
quantile(datos, probs = seq(0, 1, 0.25))

#   ------ Medidas de dispersión ------

#   ------ Medidas de dispersión para media ------
varianza <- round(var(datos), 4)
desviacion <- round(sd(datos), 4)

cat("varianza:", varianza, "\n")
cat("desviacion estandar:", round(desviacion,4), "\n")

#   ------ Medidas de dispersión para la mediana ------
MAD <- round(mad(datos, constant = 1),4)

#   ------ REGLA EMPIRICA ------
#La regla empírica sirve para tener una idea de la simetría del conjunto de datos con
#el cual se está trabajando.
#Para corroborar si el comportamiento de la variable es simétrico se debe verificar que:
cat("Regla empírica para los datos:\n")
cat("68% de los datos están entre:", round(media - desviacion, 2), "y", round(media + desviacion, 2), "\n")
cat("95% de los datos están entre:", round(media - 2*desviacion, 2), "y", round(media + 2*desviacion, 2), "\n")
cat("99.7% de los datos están entre:", round(media - 3*desviacion, 2), "y", round(media + 3*desviacion, 2), "\n")
#¿Cuando usar media o mediana como medida de dispersion?#
#Si se verifica la regla empírica entoences el comportamiento de la variable es
#simétrico y puedes utilizar como medida de posición central la media y el desvío
#estándar.

#Otra forma más extensa para usar la REGLA EMPIRICA
# Intervalos según la regla
intervalo_1 <- c(media - desvio, media + desvio)
intervalo_2 <- c(media - 2*desvio, media + 2*desvio)
intervalo_3 <- c(media - 3*desvio, media + 3*desvio)

# Porcentaje de datos en cada intervalo
porc_1 <- mean(datos >= intervalo_1[1] & datos <= intervalo_1[2]) * 100
porc_2 <- mean(datos >= intervalo_2[1] & datos <= intervalo_2[2]) * 100
porc_3 <- mean(datos >= intervalo_3[1] & datos <= intervalo_3[2]) * 100

#El primero debe ser arriba del 68%
cat("Porcentaje en ±1s:", round(porc_1, 1), "%\n") 
#El el segundo debe ser arriba del 95%
cat("Porcentaje en ±2s:", round(porc_2, 1), "%\n")
#El tercero debe ser arriba del 99.7%
cat("Porcentaje en ±3s:", round(porc_3, 1), "%\n")

#Si se acercan ligeramente los datos a la normalidad pasamos a calcular la simetria para
#terminar de verificar que medida usar

#   ------ Medidas de Asimetria ------
#Asimetria en gallego 
interpretar_sesgo <- function(datos){
    a <- skewness(datos)
    cat("Coeficiente de asimetría:", round(a, 3), "\n")
    if (a > 0.5) {
        cat("→ Distribución asimétrica positiva (sesgo a la derecha)\n")
    } else if (a < -0.5) {
        cat("→ Distribución asimétrica negativa (sesgo a la izquierda)\n")
    } else {
        cat("→ Distribución aproximadamente simétrica\n")
    }
}
interpretar_sesgo(datos)
#en el caso de los datos dados, como es minimo el sesgo a la izq, es bastante simetrica,
#por lo que se puede usar la media como medida de tendencia central


#   ------ Medidas de Curtosis ------
#Es el grado de apuntamiento o curtosis de una distribución
curtosis <- round(kurtosis(datos) - 3 ,4)
#Leptocúrtica: Más apuntada y con colas más anchas que la simétrica.  curtosis > 0
#Platicúrtica: Menos apuntada y con colas menos anchas que la simétrica. curtosis < 0
#Mesocúrtica: Es la distribución simétrica respecto de su media. curtosis = 0


cv <- (media / desviacion) * 100
cat("coeficiente de variacion:", cv, "\n")
#En caso de que la media sea muy próxima a cero no debe usarse ya que el denominador
#es muy pequeño y puede dar un grado erróneo de la dispersión.
#Cuanto menor sea el coeficiente de variación menor será la dispersión en el comportamiento
#del conjunto de datos, y de esa manera, la media será más representativa.




#Diagrama de tallo y hoja
steam(datos)

#Histograma
#tambien breaks podria ser breaks <- seq(6.3, 13.5, by = 1.2) siendo 1.2 elRango intercuartílico alias RI 
breaks <- c(6.3, 7.5, 8.7, 9.9, 11.1, 12.3, 13.5)
hist(datos, breaks = breaks, col = "skyblue", main = "Histograma de Rentabilidad", xlab = "% Rentabilidad")

#Datos del Boxplot
rango <- range(datos)
minimo <- min(datos)
maximo <- max(datos)
Q1 <- quantile(datos, 0.25, type = 2)
Q3 <- quantile(datos, 0.75, type = 2)
RI <- Q1 - Q3

bigote_izq <- Q1 - (1.5 * RI)
bigote_der <- Q3 + (1.5 * RI)

print("Rango")
rango
cat("min:", minimo, "\n")
cat("max:", maximo, "\n")
cat("Q1:", Q1, "\n")
cat("Q3:", Q3, "\n")
print("bigotes izq y der")
bigote_izq
bigote_der

boxplot(datos, horizontal = TRUE, main = "Boxplot de Rentabilidad", col = "lightgreen")

#Tabla de frecuencias
# Clasificación en intervalos definidos
intervalos <- cut(datos, breaks = breaks, right = FALSE, include.lowest = TRUE)

# Calculo de Frecuencias
frec_abs <- table(intervalos)

lim_inf <- breaks[-length(breaks)]
lim_sup <- breaks[-1]
marcas_clase <- (lim_inf + lim_sup) / 2

frec_rel <- prop.table(frec_abs)
frec_porc <- round(frec_rel * 100, 2)
frec_abs_acum <- cumsum(frec_abs)
frec_rel_acum <- cumsum(frec_rel)

# Crear la tabla
tabla_frecuencias <- data.frame(
    Intervalo = names(frec_abs),
    Marca_de_clase = marcas_clase,
    Frecuencia = as.numeric(frec_abs),
    Frec_relativa = round(as.numeric(frec_rel), 3),
    Frec_porcentual = frec_porc,
    Frec_acumulada = as.numeric(frec_abs_acum)
)
# Mostrar tabla
print(tabla_frecuencias, row.names = FALSE)

# la marca de clase es  ((lim inf del intervalo(xi)) * (lim sup del intervalo(xi))) /2
# Para calcular la media es (marca de clase (mi) * frecuencia absoluta (fi)) / la suma de las fi(osea n)
