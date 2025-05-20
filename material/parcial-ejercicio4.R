library(tidyverse)

set.seed(1234)

################## EJERCICIO 4.2 ###############################3
mu <- 2 #Fijamos mu

varianza <- mu^2 #Varianza de una exponencial
coeficiente_variacion <- varianza/mu^2 # Para el caso de la exponencial coeficiente de variacion es siempre 1
lambda <- 1/mu #Parametro de la exponencials
z <- qnorm(1 - 0.1/2) #Intervalo de confianza (90%) i.e. alpha = 0.1

intervalo <- function(M, seed) {
  #Obtengo M muestras de una exp(\lambda = 1/mu) 
  set.seed(seed)
  muestras <- rexp(M, lambda)
  X_techo <- mean(muestras)
  f_X_techo <- log(X_techo)

  limite_inferior <- f_X_techo - z*coeficiente_variacion/sqrt(M)
  limite_superior <- f_X_techo + z*coeficiente_variacion/sqrt(M)
  
  data.frame(M = M, inf = limite_inferior, sup = limite_superior)
}

Ms <- c(50, 100, 200)
repeticion_experimento <- 100
seeds <- sample(1:100, 100)

resultados <- pmap(crossing(M = Ms, seed = seeds), intervalo) |>
                bind_rows()

resultados |> mutate(tam_intervalo = abs(sup-inf))

########################################

################## EJERCICIO 4.4 ############################
#

#############################################################
