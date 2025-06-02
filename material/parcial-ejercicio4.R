library(tidyverse)

set.seed(1234)

################## EJERCICIO 4.2 ###############################
mu <- 2 #Fijamos mu

varianza <- mu^2 #Varianza de una exponencial
coeficiente_variacion <- varianza/mu^2 # Para el caso de la exponencial coeficiente de variacion es siempre 1
lambda <- 1/mu #Parametro de la exponencials
z <- qnorm(1 - 0.1/2) #Intervalo de confianza (90%) i.e. alpha = 0.1

intervalo <- function(M) {
  #Obtengo M muestras de una exp(\lambda = 1/mu) 

  muestras <- rexp(M, lambda)
  X_techo <- mean(muestras)
  f_X_techo <- log(X_techo)

  limite_inferior <- f_X_techo - z*coeficiente_variacion/sqrt(M)
  limite_superior <- f_X_techo + z*coeficiente_variacion/sqrt(M)
  
  data.frame(M = M, inf = limite_inferior, sup = limite_superior)
}

Ms <- list(M = c(50, 100, 200))

resultados <- pmap(Ms, intervalo) |>
                bind_rows()

resultados |> 
  mutate(tam_intervalo = abs(sup-inf)) |>
  ggplot(aes(x = M, y = tam_intervalo)) +
    geom_point(size = 4) +
    geom_line(color = "#2C3E50", size = 1) +
    labs(
         title = "Intervalos de confianza para log(μ)",
         x = "Tamaño muestra", 
         y = "Tamaño intervalo") +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )  
#A medida que crece M se achica el intervalo

########################################

################## EJERCICIO 4.4 ############################

cv <- function(M, seed){
  
  set.seed(seed)
  
  muestras <- rexp(M, lambda)
  X_sum <- sum(muestras)
  
  X_techo <- mean(muestras)
  S_m <- sqrt( 1/(M-1)*(sum((muestras-X_techo)^2)) )
  
  data.frame(M = M, cv = S_m / X_techo)
}

Ms <- c(10, 25, 50, 100, 200, 500, 1000, 10000)
seeds <- sample(1:100000, 100)

resultados <- pmap(crossing(M = Ms, seed = seeds), cv) |>
  bind_rows()

resultados |> 
  group_by(M) |> 
  summarize(
    mean_cv = mean(cv),
    sd_cv = sd(cv),
    .groups = "drop"
  ) |> 
  ggplot(aes(x = M, y = mean_cv)) +
  geom_line(color = "#2C3E50", size = 1) +
  geom_point(size = 3, color = "#1ABC9C") +
  geom_errorbar(aes(ymin = mean_cv - sd_cv, ymax = mean_cv + sd_cv),
                width = 0.05, color = "#34495E") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 1) +
  scale_x_log10(breaks = unique(resultados$M)) +
  labs(
    title = "Estimación del Coeficiente de Variación (CV)",
    subtitle = "Media y desviación estándar del CV con escala logarítmica en M",
    x = "Tamaño de muestra (M, escala log)",
    y = "CV promedio ± desviación estándar"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
#############################################################
