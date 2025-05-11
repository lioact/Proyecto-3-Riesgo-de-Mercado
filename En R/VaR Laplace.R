# Librerías
library(quantmod)
library(fitdistrplus)
library(VGAM)
library(dplyr)
library(tidyr)

# Descargar precios
cartera <- c("AAPL","MSFT","NVDA","AMZN","META","GOOGL","AVGO","GOOG","TSLA","WMT",
             "INTC","JPM","F","NKE","GS","NFLX","CVX","ADBE","BAC","C")
getSymbols(cartera, src = "yahoo", from = "2022-01-01", to = "2025-03-01")

# Armar data frame de precios de cierre
precios <- do.call(merge, lapply(cartera, function(sym) Cl(get(sym))))
colnames(precios) <- paste0("P", 1:ncol(precios))

# Calcular rendimientos simples
rendimientos <- na.omit(diff(precios) / lag(precios))

# Parámetros generales
nc <- c(0.95, 0.97, 0.99)
dias <- c(1, 7, 15, 30, 60, 90, 180)
numsim_vec <- c(5000, 10000, 20000)
emisoras <- colnames(precios)
precio_actuales <- as.numeric(tail(precios, 1))
m <- length(emisoras)

# Estimar parámetros Laplace por emisora
ajustar_laplace <- function(x) {
  mod <- vglm(x ~ 1, laplace, trace = FALSE)
  coef(mod)
}
param_laplace <- apply(rendimientos, 2, ajustar_laplace)
mu_lap <- param_laplace[1, ]
b_lap <- pmax(param_laplace[2, ], 1e-6)

# Inicializar tabla
resultados <- data.frame()
set.seed(123)

# Simulación por emisora
for (s in numsim_vec) {
  for (j in 1:m) {
    mu <- mu_lap[j]
    b <- b_lap[j]
    precio_act <- precio_actuales[j]
    
    for (h in dias) {
      sim_rend <- matrix(rlaplace(s * h, location = mu, scale = b), ncol = h)
      sim_acum <- rowSums(sim_rend)
      # Pérdidas con rendimientos simples acumulados
      sim_precios <- precio_actual * (1 + sim_acum)
      sim_perdidas <- precio_actual - sim_precios  # NO uses pmax(..., 0)
      
      
      for (conf in nc) {
        alpha <- 1 - conf
        var <- quantile(sim_perdidas, probs = 1 - conf)
        es <- mean(sim_perdidas[sim_perdidas <= var])  # pérdidas peores (más negativas) que VaR
        
        
        resultados <- rbind(resultados, data.frame(
          NumSim = s,
          Emisora = emisoras[j],
          Horizonte = h,
          Confianza = conf,
          VaR = var,
          ES = es
        ))
      }
    }
  }
  
  # Simulación del portafolio
  rend_port <- rowMeans(rendimientos)
  mu_pt <- mean(rend_port)
  b_pt <- pmax(sd(rend_port) / sqrt(2), 1e-6)
  precio_pt <- sum(precio_actuales)
  
  for (h in dias) {
    sim_rend <- matrix(rlaplace(s * h, location = mu_pt, scale = b_pt), ncol = h)
    sim_acum <- rowSums(sim_rend)
    sim_precios <- precio_pt * (1 + sim_acum)
    sim_perdidas <- pmax(precio_pt - sim_precios, 0)
    
    for (conf in nc) {
      alpha <- 1 - conf
      var <- quantile(sim_perdidas, probs = alpha)
      es <- mean(sim_perdidas[sim_perdidas >= var])
      
      resultados <- rbind(resultados, data.frame(
        NumSim = s,
        Emisora = "PT",
        Horizonte = h,
        Confianza = conf,
        VaR = var,
        ES = es
      ))
    }
  }
}

#### Tabla final
tabla_Laplace <- resultados %>%
  pivot_wider(
    names_from = Confianza,
    values_from = c(VaR, ES),
    names_glue = "{.value}{round(Confianza * 100)}"
  ) %>%
  arrange(NumSim, Emisora, Horizonte)

# Mostrar tabla
print(tabla_Laplace)
