# Cargar librerías
library(quantmod)
library(dplyr)
library(tidyr)
library(stringr)

# Parámetros
cartera <- c("AAPL","MSFT","NVDA","AMZN","META","GOOGL","AVGO","GOOG","TSLA","WMT",
             "INTC","JPM","F","NKE","GS","NFLX","CVX","ADBE","BAC","C")
niveles_conf <- c(0.95, 0.97, 0.99)
horizontes <- c(1, 7, 15, 30, 60, 90, 180)

# Descargar precios ajustados
getSymbols(cartera, src = "yahoo", from = "2022-01-01", to = "2025-03-01", auto.assign = TRUE)
precios <- do.call(merge, lapply(cartera, function(t) Cl(get(t))))
colnames(precios) <- paste0("P", 1:20)
fechas <- index(precios)

# Calcular rendimientos porcentuales
rendimientos <- diff(as.matrix(precios)) / head(as.matrix(precios), -1)
rendimientos_df <- as.data.frame(rendimientos)
rendimientos_df$PT <- rowSums(rendimientos_df)

# Calcular volatilidades (desviación estándar)
volatilidades <- apply(rendimientos_df, 2, sd)

# Funciones de VaR y ES paramétrico
fVaR <- function(nc, sigma, t) qnorm(nc) * sigma * sqrt(t)
fES  <- function(nc, sigma, t) dnorm(qnorm(nc)) / (1 - nc) * sigma * sqrt(t)

# Crear tabla de resultados
resultados <- expand.grid(
  Emisora = colnames(rendimientos_df),
  Horizonte = horizontes,
  Confianza = niveles_conf
) %>%
  rowwise() %>%
  mutate(
    sigma = volatilidades[Emisora],
    VaR = fVaR(Confianza, sigma, Horizonte),
    ES  = fES(Confianza, sigma, Horizonte)
  ) %>%
  ungroup() %>%
  select(Emisora, Horizonte, Confianza, VaR, ES)

# Formatear tabla final
tabla_Parametrico <- resultados %>%
  mutate(Confianza = paste0(Confianza * 100)) %>%
  pivot_wider(names_from = Confianza, values_from = c(VaR, ES), names_sep = "_") %>%
  rename_with(~ gsub("%", "", .x)) %>%
  select(Emisora, Horizonte, VaR_95, VaR_97, VaR_99, ES_95, ES_97, ES_99) %>%
  mutate(Emisora = factor(Emisora, levels = paste0("P", 1:20), ordered = TRUE)) %>%
  arrange(Emisora, Horizonte)

# Mostrar tabla
print(tabla_Parametrico)
