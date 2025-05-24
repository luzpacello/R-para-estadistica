# N: tamaño total de la población
# K: número total de éxitos en la población
# n: tamaño de la muestra
# x: número de éxitos deseados en la muestra

# P(X = x): probabilidad puntual
hiper_puntual <- function(N, K, n, x) {
  return(dhyper(x, K, N - K, n))
}

# P(X ≤ x): probabilidad acumulada
hiper_acum <- function(N, K, n, x) {
  return(phyper(x, K, N - K, n))
}

# P(X > x): probabilidad complementaria
hiper_mayor <- function(N, K, n, x) {
  return(1 - phyper(x, K, N - K, n))
}

# Esperanza
hiper_esperanza <- function(N, K, n) {
  return(n * (K / N))
}

# Varianza
hiper_varianza <- function(N, K, n) {
  return(n * (K / N) * ((N - K) / N) * ((N - n) / (N - 1)))
}

# P(X = x): probabilidad puntual
hipergeometrica_manual <- function(N, K, n, x) {
  if (x > K || x > n || n > N) {
    stop("Valores no válidos: verificar que x ≤ K, x ≤ n, y n ≤ N")
  }
  
  numerador <- choose(K, x) * choose(N - K, n - x)
  denominador <- choose(N, n)
  return(numerador / denominador)
}
