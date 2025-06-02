# Carga de paquetes
library(ggplot2)
library(tidyr)

# Parámetros 
M <- 5000                     # número de réplicas
mu <- 1/2 
sigma2 <- 1/12
n_vals <- c(1, 10, 100, 500)        # distintos n
eps <- 1                       # ε para la condición de Lindeberg

# Obtenemos las distintas constantes
max_n <- max(n_vals)
C <- sample(1:10000, max_n, replace = TRUE)

# Función que simula las M réplicas de Sn/Vn para tamaño n
muestras <- function(n) {
  sapply(1:M, function(i) {
    X <- runif(n) - mu
    Sn <- sum(X * C[1:n])
    Vn <- sqrt(sum(C[1:n]^2)) * sqrt(sigma2)
    Sn / Vn
  })
}

# Simulaciones para cada n
sim_list <- lapply(n_vals, muestras)
names(sim_list) <- paste0("n_", n_vals)

# Dataframe para graficar
df_sim <- as.data.frame(sim_list) |>
  pivot_longer(everything(), names_to = "n", values_to = "value")


ggplot(df_sim, aes(x = value, color = n)) +
  geom_density(size = 1) +
  stat_function(fun = dnorm,
                args = list(mean = 0, sd = 1),
                color = "black", linetype = "dashed") + #Agregamos la normal
  labs(title = "Densidades de S_n / V_n para distintos n",
       x = expression(S[n]/V[n]),
       color = "n") +
  theme_minimal()

