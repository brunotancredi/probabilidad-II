### Ejercicio propuesto: desarrollar el ejemplo 3, generando N= 100, 500, 1000 realizaciones de la sucesión del Error normalizado y construir histogramas/estimación de densidades con los 100, 500, 1000 valores obtenidos cuando M = 500 y 1000. Comparar los histogramas/estimación de densidades así obtenidos con la densidad de una distribución normal centrada con varianza igual a Var(U_1), U_1 ~ Uniforme(0,1). Interpretar los resultados obtenidos.

rm(list=ls())
library(ggplot2)
library(tidyr)

# Establecer semilla global para reproducibilidad
set.seed(123)  # Semilla fija para generar las semillas aleatorias

# Función para generar simulaciones del error normalizado
simula_error_normalizado <- function(seed, M) {
  set.seed(seed)  # Semilla específica para cada simulación
  U <- runif(M)  # Muestras uniformes
  running_mean <- cumsum(U) / (1:M)  # Media muestral acumulada
  norm_error <- sqrt(1:M) * (running_mean - 0.5)  # Error normalizado
  return(norm_error[c(500, 1000, 2000, 5000)])  # Devuelve el error normalizado en M=500, M=1000, M=2000 y M=5000
}

# Parámetros de simulación
N <- 1000 # 500, 1000 Número de simulaciones

# Generar semillas aleatorias reproducibles
semillas <- sample(1:10^6, N, replace = FALSE)

# Generar simulaciones para los valores de M especificados
resultados <- t(sapply(semillas, simula_error_normalizado, M = 5000))
colnames(resultados) <- c("M500", "M1000", "M2000", "M5000")
resultados_df <- data.frame(resultados)

# identificador ficticio 
resultados_df$id <- 1:nrow(resultados_df)

# Transformar a formato largo especificando id.vars
resultados_long <- pivot_longer(resultados_df, 
                        cols = -id,
                        names_to = 'M',
                        values_to = 'Error')

# Convertir la columna 'M' en factor
resultados_long$M <- factor(resultados_long$M, 
          levels = c("M500", "M1000", "M2000", "M5000"),
          labels = c("M = 500", "M = 1000", "M = 2000", "M = 5000"))

# Crear datos para la densidad teórica de la normal con media 0 y varianza 1/12
x_values <- seq(-1.5, 1.5, length.out = 1000)
density_normal <- dnorm(x_values, mean = 0, sd = sqrt(1/12))

# Título dinámico con el valor de N
titulo <- paste("Histograma y estimación de densidad del error normalizado\n", "con densidad teórica N(0, 1/12) - N =", N)

# Gráfico con cuatro paneles
ggplot(resultados_long, aes(x = Error)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.4) +
  geom_density(fill = "blue", alpha = 0.2) +
  geom_line(data = data.frame(x_values, density_normal),
            aes(x = x_values, y = density_normal),
            color = "red", linetype = "dashed", size = 1) +
  facet_wrap(~ M, ncol = 2) + # Organizar en una cuadrícula de dos columnas
  labs(title = titulo,
       x = "Error normalizado",
       y = "Densidad") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))
