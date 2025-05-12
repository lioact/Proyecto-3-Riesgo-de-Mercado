# Cargar librerías necesarias
library(quantmod)
library(dplyr)
library(tidyr)
library(stringr)

# Descargar datos
cartera <- c("AAPL","MSFT","NVDA","AMZN","META","GOOGL","AVGO","GOOG","TSLA","WMT",
             "INTC","JPM","F","NKE","GS","NFLX","CVX","ADBE","BAC","C") 
getSymbols(cartera, src = "yahoo", from = "2022-01-01", to = "2025-03-01")

# Construir data frame de precios de cierre
precios <- do.call(merge, lapply(cartera, function(t) Cl(get(t))))
colnames(precios) <- paste0("P", 1:20)
d1 <- data.frame(Fecha = index(precios), coredata(precios))

# Calcular rendimientos porcentuales
rendimientos <- diff(as.matrix(d1[,-1])) / head(as.matrix(d1[,-1]), -1)
colnames(rendimientos) <- paste0("P", 1:20)
rendimientos <- as.data.frame(rendimientos)
rendimientos$PT <- rowMeans(rendimientos)  # Portafolio: promedio simple
n <- nrow(rendimientos)

# Parámetros
niveles_confianza <- c(0.95, 0.97, 0.99)
horizontes <- c(1, 7, 15, 30, 60, 90, 180)

# Función Cornish-Fisher
VaR_ES_CornishFisher <- function(x, niveles_confianza) {
  x <- na.omit(x)
  mu <- mean(x)
  sigma <- sd(x)
  S <- mean((x - mu)^3) / sigma^3
  K <- mean((x - mu)^4) / sigma^4
  z <- qnorm(niveles_confianza)
  z_cf <- z + (1/6)*(z^2 - 1)*S + (1/24)*(z^3 - 3*z)*(K - 3) - (1/36)*(2*z^3 - 5*z)*S^2
  VaR <- -(mu + z_cf * sigma)
  ES <- -(mu + sigma * dnorm(z_cf) / (1 - niveles_confianza))
  return(list(VaR = VaR, ES = ES))
}

# Calcular tabla
tabla_CF <- data.frame()
for (emisora in colnames(rendimientos)) {
  res <- VaR_ES_CornishFisher(rendimientos[[emisora]], niveles_confianza)
  for (h in horizontes) {
    VaR_h <- res$VaR * sqrt(h)
    ES_h <- res$ES * sqrt(h)
    fila <- data.frame(
      Emisora = emisora,
      Horizonte = h,
      VaR95 = VaR_h[1], VaR97 = VaR_h[2], VaR99 = VaR_h[3],
      ES95 = ES_h[1], ES97 = ES_h[2], ES99 = ES_h[3]
    )
    tabla_CF <- bind_rows(tabla_CF, fila)
  }
}

# Vista final ordenada
tabla_Cornish <- tabla_CF %>%
  arrange(Emisora, Horizonte)

library(gtools)

tabla_Cornish <- tabla_CF %>%
  mutate(Emisora = factor(Emisora, levels = mixedsort(unique(Emisora)))) %>%
  arrange(Emisora, Horizonte)










# Mostrar tabla
print(tabla_Cornish)






