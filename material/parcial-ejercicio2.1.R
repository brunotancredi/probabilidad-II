## simul_mixture_fx.R
# Simulación en R de P((\u0305X_n - E[\u0305X_n]) / sqrt(Var[\u0305X_n]) <= x) para mezcla de normales

# Definir función para simular una muestra de tamaño n de la mezcla F_X
r_mixture <- function(n, eps, eta, tau) {
  # n vectores de indicadores
  comp <- rbinom(n, 1, eps)
  # para comp==0: N(0,1); comp==1: N(eta, tau^2)
  x <- rnorm(n, mean = 0, sd = 1)
  x[comp == 1] <- rnorm(sum(comp == 1), mean = eta, sd = tau)
  return(x)
}

# Valores de parámetros a explorar
xs <- c(0, 1, 2)
epsilons <- c(0.1, 0.2, 0.3)
sizes <- c(50, 100, 150)

# Escenarios de (eta, tau)
scenarios <- list(
  list(name = "eta0",    eta = 0,  taus = c(1,2,3)),
  list(name = "tau1",    taus = 1, eta = c(0.5,1,2)),
  list(name = "eta_tau", eta = c(0.5,1,2), taus = c(0.5,1,2))
)

# Número de repeticiones Monte Carlo
R <- 10000

# Prealocar lista de resultados
results <- list()

for (eps in epsilons) {
  for (n in sizes) {
    # teóricos de X
    mu_X  <- eps * ifelse(exists('eta_vec'), 0, NA) # overwritten below
    # Usaremos valores de eta y tau por escenario
    for (sc in scenarios) {
      # vectores de eta y tau según escenario
      etas <- if (!is.null(sc$eta) && length(sc$eta)>1) sc$eta else rep(sc$eta, length(sc$taus))
      taus <- if (!is.null(sc$taus) && length(sc$taus)>1) sc$taus else rep(sc$taus, length(sc$eta))
      for (i in seq_along(etas)) {
        eta <- etas[i]; tau <- taus[i]
        # calcular esperanza y varianza teóricas de X
        mu_X  <- eps * eta
        var_X <- (1-eps) + eps*(tau^2 + eta^2) - (eps*eta)^2
        # simular R veces la media muestral estandarizada
        probs <- replicate(R, {
          samp <- r_mixture(n, eps, eta, tau)
          xbar <- mean(samp)
          z    <- (xbar - mu_X) / sqrt(var_X / n)
          as.numeric(z <= xs)  # vector lógico convertido a 0/1
        })
        # probs es un length(xs) x R matrix
        est_p <- rowMeans(probs)
        # armar fila de resultados
        df <- data.frame(
          epsilon = eps,
          n       = n,
          scenario= sc$name,
          eta     = eta,
          tau     = tau,
          x       = xs,
          p_est   = est_p,
          p_norm  = pnorm(xs)
        )
        results[[length(results)+1]] <- df
      }
    }
  }
}

# Combinar en data.frame final
df_results <- do.call(rbind, results)
# Mostrar resultados
print(df_results)

# Guardar a CSV si se desea
# write.csv(df_results, "simul_mixture_fx_results.csv", row.names = FALSE)