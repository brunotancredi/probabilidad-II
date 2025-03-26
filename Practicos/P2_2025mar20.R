### P2 20/3/2025 - Monte Carlo - ejemplo 1 
### Estimación Monte Carlo 
### de \int_{[0,1]^3} exp(x_1 + x_2 + x_3) dx_1 dx_2 dx_3
library(tidyverse)
rm(list=ls())
set.seed(123)  # reproducibilidad
M <- 10000     # número de muestras

# Generar muestras de Uniforme en [0,1]^3
U <- matrix(runif(3*M), ncol = 3)

# valores de la función integranda
f_vals <- exp(rowSums(U))

# media que varía con M 
running_mean <- cumsum(f_vals) / (1:M)

# verdadero valor de la integral
true_value <- (exp(1) - 1)^3

plot(1:M, running_mean, type = "l", lwd = 1.5,
     main = "Convergencia Monte Carlo",
     xlab = "Número de muestras (M)",
     ylab = "Estimación de la integral")
abline(h = true_value, col = "red", lty = 2)
legend("topright", 
       legend = c("Estimación Monte Carlo", "Valor verdadero"),
       col = c("black", "red"), lty = c(1, 2), cex = 0.8)

##########
### P2 20/3/2025 - Monte Carlo - ejemplo 2 
### Simulación de tres sucesiones
library(ggplot2)

set.seed(123)  # Semilla original
M <- 10000
true_value <- (exp(1) - 1)^3

# Función para generar estimaciones Monte Carlo
genera_estimacion <- function(seed, M) {
  set.seed(seed)
  U <- matrix(runif(3*M), ncol = 3)
  f_vals <- exp(rowSums(U))
  data.frame(
    M = 1:M,
    estimacion = cumsum(f_vals)/(1:M),
    semilla = factor(seed)
  )
}

# Generar datos para 3 semillas diferentes
semillas <- c(123, 456, 789)
df <- do.call(rbind, lapply(semillas, genera_estimacion, M = M))

# Gráfico
ggplot(df, aes(x = M, y = estimacion, color = semilla)) +
  geom_line(size = 0.7) +
  geom_hline(yintercept = true_value, 
             linetype = "dashed", 
             color = "red", 
             size = 0.8) +
  scale_color_manual(values = c("123" = "black", 
                                "456" = "blue", 
                                "789" = "green")) +
  labs(title = "Convergencia Monte Carlo con diferentes semillas",
       x = "Número de muestras (M)",
       y = "Estimación de la integral",
       caption = "Línea roja: Valor verdadero (5.073)") +
  theme_minimal() +
  theme(legend.position = "top",
        plot.caption = element_text(hjust = 0.5, face = "italic")) +
  coord_cartesian(ylim = c(4.5, 6))
##########

##########
### P2 20/3/2025 - Monte Carlo - ejemplo 3 
### Estimación Monte Carlo 
### de la esperanza de una v.a. uniforme en (0,1)
rm(list=ls())
library(ggplot2)
library(tidyr)

genera_simulacion <- function(seed, M) {
  set.seed(seed)
  U <- runif(M)  # Muestras uniformes 1D
  m <- 1:M
  
  # media que varía con M 
  running_mean <- cumsum(U)/m
  # error normalizado que varía con M
  norm_error <- sqrt(m) * (running_mean - 0.5)/sqrt(1/12)
  
  data.frame(
    M = m,
    mediamuestral = running_mean,
    normerror = norm_error,
    Semilla = factor(seed)
  )
}

# Generar datos con tres semillas
semillas <- c(123, 456, 789)
M <- 1000
df <- do.call(rbind, lapply(semillas, genera_simulacion, M = M))

# Reformatear datos para facetado
df_long <- pivot_longer(df, 
            cols = c(mediamuestral, normerror),
            names_to = "Metric",
            values_to = "Value") %>%
  mutate(Metric = factor(Metric, 
            levels = c("normerror", "mediamuestral"),   
            labels = c("Error normalizado: √M(1/M ΣU_i - 0.5)",
                       "Media muestral: 1/M ΣU_i")))

# Gráfico con dos facetas
ggplot(df_long, aes(x = M, y = Value, color = Semilla)) +
  geom_line(alpha = 0.8, linewidth = 0.4) +
  facet_wrap(~ Metric, ncol = 1, scales = "free_y") +
  geom_hline(data = data.frame(Metric = levels(df_long$Metric),
                               yint = c(0, 0.5)),
             aes(yintercept = yint),
             linetype = "dashed", color = "red") +
  scale_color_manual(values = c("123" = "black",
                                "456" = "blue",
                                "789" = "green3")) +
  labs(title = "Convergencia de la media muestral a la esperanza \nde la distribución Uniforme (0,1) y \nsimulaciones del error normalizado",
       x = "Número de muestras (M)",
       y = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"))
##########

### Ejercicio propuesto: desarrollar el ejemplo 3, generando N= 100, 500, 1000 realizaciones de la sucesión del Error normalizado y construir histogramas/estimación de densidades con los 100, 500, 1000 valores obtenidos cuando M = 500 y 1000. Comparar los histogramas/estimación de densidades así obtenidos con la densidad de una distribución normal centrada con varianza igual a Var(U_1), U_1 ~ Uniforme(0,1). Interpretar los resultados obtenidos.
semillas <- sample(1:10000,100,replace=F) #Obtenemos 100 semillas
M <- 1000
df <- do.call(rbind, lapply(semillas, genera_simulacion, M = M))
df <- df |> filter(M %in% c(10, 500, 1000))

ggplot(df) +
  geom_function(fun = dnorm, colour = "red") +
  geom_histogram(aes(x = normerror, y=after_stat(density)),bins = 30, fill = "blue") +
  labs(title = paste("Distribución de las medias muestrales"),
       x = "Media Muestral", y = "Frecuencia") +
  xlim(-3.5, 3.5) +
  theme_minimal() +
  facet_wrap(~ M, nrow=2)
