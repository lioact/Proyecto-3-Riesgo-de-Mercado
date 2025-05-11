# Cargar librerías necesarias
library(quantmod)
library(dplyr)
library(tidyr)
library(stringr)

# Descargar precios
cartera <- c("AAPL","MSFT","NVDA","AMZN","META","GOOGL","AVGO","GOOG","TSLA","WMT",
             "INTC","JPM","F","NKE","GS","NFLX","CVX","ADBE","BAC","C")
getSymbols(cartera, src = "yahoo", from = "2022-01-01", to = "2025-03-01")

# Construir data frame de precios de cierre
precios <- do.call(merge, lapply(cartera, function(t) Cl(get(t))))
colnames(precios) <- paste0("P", 1:20)
precios_df <- data.frame(Fecha = index(precios), coredata(precios))

# Calcular rendimientos porcentuales
rendimientos <- diff(as.matrix(precios_df[,-1])) / head(as.matrix(precios_df[,-1]), -1)
colnames(rendimientos) <- paste0("P", 1:20)

# Agregar columna de rendimiento total de la cartera (suma simple de todos los activos)
rendimientos <- cbind(rendimientos, PT = rowSums(rendimientos))

# Parámetros
niveles_conf <- c(0.95, 0.97, 0.99)
horizontes <- c(1, 7, 15, 30, 60, 90, 180)
alpha <- 0.95

# Función de VaR y ES con alisado exponencial
VaR_ES_alisado <- function(pl_vector, niveles, alpha = 0.95) {
  pl_vector <- na.omit(pl_vector)
  n <- length(pl_vector)
  pl_ord <- sort(pl_vector, decreasing = TRUE)
  pesos <- rev(alpha^(0:(n - 1)) * (1 - alpha))
  pesos <- pesos / sum(pesos)
  Fx <- cumsum(pesos)
  
  sapply(niveles, function(c) {
    idx <- max(which(Fx <= c)) + 1
    VaR <- pl_ord[idx]
    ES <- sum(pl_ord[1:idx] * pesos[1:idx]) / sum(pesos[1:idx])
    c(VaR = VaR, ES = ES)
  })
}

# Calcular y consolidar resultados
resumen <- bind_rows(lapply(colnames(rendimientos), function(nombre) {
  resultado <- VaR_ES_alisado(rendimientos[, nombre], niveles_conf, alpha)
  dias_data <- sapply(horizontes, function(h) resultado * sqrt(h), simplify = "array")
  df <- as.data.frame(aperm(dias_data, c(3, 2, 1)))
  colnames(df) <- c("VaR_95", "VaR_97", "VaR_99", "ES_95", "ES_97", "ES_99")
  df$Horizonte <- rep(horizontes, each = 1)
  df$Emisora <- nombre
  df
}))

# Organizar tabla final
tabla_Alisado <- resumen %>%
  select(Emisora, Horizonte, starts_with("VaR"), starts_with("ES")) %>%
  arrange(Emisora, Horizonte)

library(gtools)  # Para orden natural (P1, P2, ..., P10)

tabla_Alisado <- resumen %>%
  select(Emisora, Horizonte, starts_with("VaR"), starts_with("ES")) %>%
  mutate(Emisora = factor(Emisora, levels = mixedsort(unique(Emisora)))) %>%
  arrange(Emisora, Horizonte)





# Mostrar tabla
print(tabla_Alisado)































