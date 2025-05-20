library(dplyr)
library(purrr)

#Definimos la cantidad de X_n que queremos tomar.
M <- 1000

simulacion <- function(x, epsilon, n, eta, tau){

  esperanza  <- epsilon * eta #Parte 2.1
  varianza <- (1-epsilon) + epsilon*(tau^2 + eta^2) - (epsilon*eta)^2 #Parte 2.1
  
  pr <- sapply(1:M, function(m){
    
    #Obtenemos n muestras de la mezcla de normales.
    muestras <- sapply(1:n, function(n){
      p <- rbinom(1 , 1, epsilon)
      if(p == 0){
        rnorm(1)
      }
      else{
        rnorm(1, eta, tau)
      }
    })
    
    X_techo <- sum(muestras)/n 
    
    as.numeric( (X_techo - esperanza) / sqrt(varianza/n) <= x)
  })
  
  list(
      sim = sum(pr)/M, #Prop de cuantos de los m fueron menor igual a x y cuantos mayor (estimacion prob)
      esperanza_teorica = esperanza,
      varianza_teorica = varianza
  )
}

###### ARMAMOS LOS DISTINTOS ESCENARIOS #########
# Parametros base
base <- crossing(
  x = c(0, 1, 2),
  epsilon = c(0.1, 0.2, 0.3),
  n = c(50, 100, 150)
)

# Escenario 1: eta fijo en 0, tau variable
escenario_1 <- base |>
  crossing(
    eta = 0,
    tau = c(1, 2, 3)
  )

# Escenario 2: eta variable, tau fijo en 1
escenario_2 <- base |>
  crossing(
    eta = c(0.5, 1, 2),
    tau = 1
  )

# Escenario 3: eta = tau = c(0.5,1,2)
escenario_3 <- base |>
  crossing(
    tibble(
      eta = c(0.5, 1, 2),
      tau = c(0.5, 1, 2)
    )
  )

# Unir todos los escenarios
escenarios <- bind_rows(escenario_1, escenario_2, escenario_3)
###################################################################

#Obtenemos las simulaciones para los distintos casos.
resultados <- pmap(escenarios, simulacion) |>
                transpose() |>
                map_df(~ unlist(.x)) |>
                as.data.frame()

escenarios |> cbind(resultados)