# hay reemplazo, es hasta primer éxito	
# la probabilidad de que el primer éxito ocurra en el intento x	
# Ej: ¿Cuántas veces lanzo hasta que sale cara?
# Parámetro: p = probabilidad de éxito en cada ensayo.
# Variable aleatoria: X = número de ensayos hasta obtener el primer éxito.

n <- 5        #es ejemplo hay que cambiarlo antes de iniciar
p <- 0.5      #es ejemplo hay que cambiarlo antes de iniciar

# Probabilidad puntual P(X = x)
geo_puntual <- function(p, x){
  if (is.null(x)) stop("Debe ingresar el número de ensayo x.")
  if (x < 1) stop("x debe ser un entero mayor o igual a 1.")
  return(dgeom(x - 1, prob = p)) # dgeom cuenta ensayos hasta primer éxito - 1
}

# Probabilidad acumulada P(X ≤ x)
geo_acum <- function(p, x){
  if (is.null(x)) stop("Debe ingresar el número de ensayo x.")
  if (x < 1) stop("x debe ser un entero mayor o igual a 1.")
  return(pgeom(x - 1, prob = p))
}

# Probabilidad complementaria P(X > x)
geo_mayor <- function(p, x){
  if (is.null(x)) stop("Debe ingresar el número de ensayo x.")
  if (x < 1) stop("x debe ser un entero mayor o igual a 1.")
  return(1 - pgeom(x - 1, prob = p))
}

# Esperanza
geo_esperanza <- function(p){
  return(1 / p)
}

# Varianza
geo_varianza <- function(p){
  return((1 - p) / (p^2))
}


#Ejemplo de forma de uso
#En consola se escribe "print(paste("P(X = 3):", geo_puntual(p, x)))"
#paste solo concatena las cadenas
