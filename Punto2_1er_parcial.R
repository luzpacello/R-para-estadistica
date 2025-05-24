# Experiencia (o experimento): Es el proceso mediante el cual se obtiene una observación o medida. Se describe con un verbo (acción) sobre una unidad experimental.

#A: el cliente tiene cuenta de ahorro como producto principal.
#B: el cliente tiene tarjeta de crédito como producto principal.
#AB: tiene paquete cuenta sueldo + tarjeta.
#O: tiene cuenta con solo débito.
#P1: cliente del perfil 1 (tradicional).
#P2: cliente del perfil 2 (tecnológico).
#P3: cliente del perfil 3 (multicanal).

#Matriz donde se guardará la tabla de probabilidades
# Estructura: filas = perfiles (P1, P2, P3), columnas = productos (A, B, AB, O)

matriz <- matrix(
    c(
        0.12, 0.07, 0.03, 0.08,   # Perfil 1
        0.05, 0.15, 0.06, 0.04,   # Perfil 2
        0.13, 0.04, 0.10, 0.13    # Perfil 3
    ),
    nrow = 3, #nro de filas
    byrow = TRUE #la matriz se llena por filas y no por columnas
)
colnames(matriz) <- c("A", "B", "AB", "O") #columnas
rownames(matriz) <- c("P1", "P2", "P3") #filas
# Las columnas son mutuamente excluyentes
# NOTA: ESTE CODIGO PUEDE SERVIR MAS COMO UN AYUDA MEMORIA

#  calcular P(A∩B)
# Si da 0 es porq son mutuamente excluyente los eventos
# Método 1: con P(A) y P(B|A)
prob_interseccion_condicional1 <- function(P_A, P_B_dado_A) {
  return(P_A * P_B_dado_A)
}
# Método 2: con P(B) y P(A|B)
prob_interseccion_condicional2 <- function(P_B, P_A_dado_B) {
  return(P_B * P_A_dado_B)
}

# Calcula P(A ∪ B), considerando si los eventos son excluyentes
prob_union <- function(P_A, P_B, P_AyB = 0, excluyentes = TRUE) {
    if (excluyentes) {
        return(P_A + P_B)
    } else {
        return(P_A + P_B - P_AyB)
    }
}
# Intersección de dos eventos: P(A ∩ B) = P(A) * P(B|A)
prob_interseccion <- function(P_A, P_B_dado_A) {
    return(P_A * P_B_dado_A)
}
#Intersección para eventos independientes, donde P(A∩B)=P(A)×P(B):
prob_interseccion_independientes <- function(P_A, P_B) {
    return(P_A * P_B)
}

# Probabilidad condicional: P(A | B) = P(A ∩ B) / P(B)
prob_condicional <- function(P_AyB, P_B) {
    if (P_B == 0) {
        stop("P(B) no puede ser cero.")
    }
    return(P_AyB / P_B)
}

# Complemento de un evento: P(no A) = 1 - P(A)
prob_complemento <- function(P_A) {
    return(1 - P_A)
}

#Teorema de Bayes
prob_bayes <- function(P_B_dado_A, P_A, P_B) {
    if (P_B == 0) stop("P(B) no puede ser cero.")
        return((P_B_dado_A * P_A) / P_B)
}

#Ejemplo de forma de uso
#En consola se escribe "print(paste("probabilidad de  P(no A)", prob_complemento(P_A)))"
#paste solo concatena las cadenas
