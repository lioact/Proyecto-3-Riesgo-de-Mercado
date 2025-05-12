library(quantmod)
library(dplyr)
library(purrr)

# Descargar precios
cartera <- c("AAPL", "MSFT", "NVDA", "AMZN", "META", "GOOGL", "AVGO", "GOOG", "TSLA", "WMT",
             "INTC", "JPM", "F", "NKE", "GS", "NFLX", "CVX", "ADBE", "BAC", "C")
getSymbols(cartera, src = "yahoo", from = "2022-01-01", to = "2025-03-01")

# Precios y rendimientos
precios <- do.call(merge, lapply(cartera, function(x) Cl(get(x))))
colnames(precios) <- paste0("P", seq_along(cartera))
rendimientos <- na.omit(diff(precios) / lag(precios))
precios_actuales <- as.numeric(last(precios))
emisoras <- colnames(precios)

# Parámetros
niveles_conf <- c(0.95, 0.97, 0.99)
horizontes <- c(1, 7, 15, 30, 60, 90, 180)
num_simulaciones <- c(5000, 10000, 20000)

# Función para calcular VaR y ES
calcular_var_es <- function(perdidas, niveles) {
  vars <- quantile(perdidas, probs = 1 - niveles, na.rm = TRUE)
  es <- map_dbl(vars, ~ mean(perdidas[perdidas >= .x], na.rm = TRUE))
  list(VaR = vars, ES = es)
}

# Simulación bootstrap
set.seed(123)
resultados <- list()

for (nsim in num_simulaciones) {
  for (j in seq_along(emisoras)) {
    y <- rendimientos[, j]
    
    for (h in horizontes) {
      sim_rend <- matrix(sample(y, nsim * h, replace = TRUE), ncol = h)
      rend_acum <- rowSums(sim_rend)  # solo sumas de rendimientos
      metrics <- calcular_var_es(-rend_acum, niveles_conf)  # negativo para enfoque conservador
      
      resultados[[length(resultados) + 1]] <- data.frame(
        NumSim = nsim,
        Emisora = emisoras[j],
        Horizonte = h,
        VaR95 = metrics$VaR[1], VaR97 = metrics$VaR[2], VaR99 = metrics$VaR[3],
        ES95 = metrics$ES[1], ES97 = metrics$ES[2], ES99 = metrics$ES[3]
      )
    }
  }
  
  # Portafolio: promedio simple de rendimientos acumulados
  for (h in horizontes) {
    sim_rend_ptf <- replicate(nsim, {
      mean(map_dbl(seq_along(emisoras), function(j) {
        sum(sample(rendimientos[, j], h, replace = TRUE))
      }))
    })
    
    metrics <- calcular_var_es(-sim_rend_ptf, niveles_conf)
    
    resultados[[length(resultados) + 1]] <- data.frame(
      NumSim = nsim,
      Emisora = "PT",
      Horizonte = h,
      VaR95 = metrics$VaR[1], VaR97 = metrics$VaR[2], VaR99 = metrics$VaR[3],
      ES95 = metrics$ES[1], ES97 = metrics$ES[2], ES99 = metrics$ES[3]
    )
  }
}


# Consolidar resultados
tabla_bootstrap2 <- bind_rows(resultados)
