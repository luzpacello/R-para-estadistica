#n = número de ensayos
#p = probabilidad de éxito en cada ensayo
#x = número de éxitos deseados


n <- 5        #es ejemplo hay que cambiarlo antes de iniciar
p <- 0.5      #es ejemplo hay que cambiarlo antes de iniciar
x <- 2        #es ejemplo hay que cambiarlo antes de iniciar


# probabilidad puntual P(X = x)
bi_puntual <- function(n_ensayos, p_exito, x){
    if (is.null(x)) stop("Debe ingresar el número de éxitos x.")
    return(dbinom(x, size = n_ensayos, prob = p_exito))
}
# probabilidad acumulada P(X ≤ x)
bi_acum <- function(n_ensayos, p_exito, x){
    if (is.null(x)) stop("Debe ingresar el valor máximo x.")
    return(pbinom(x, size = n_ensayos, prob = p_exito))
}
# probabilidad aumulada complementaria P(X > x)
bi_mayor <- function(n_ensayos, p_exito, x){
    if (is.null(x)) stop("Debe ingresar el valor máximo x.")
    if (x < 0 || x > n) stop("x debe estar entre 0 y n.")
    return(1 - pbinom(x, size = n, prob = p))}

esperanza <- n * p
varianza <- n * p * (1 - p)

print(paste("Esperanza:", esperanza))
print(paste("Varianza:", varianza))

#Ejemplo de forma de uso
#En consola se escribe "print(paste("P(X = 3):", bi_puntual(n, p, x)))"
#paste solo concatena las cadenas
