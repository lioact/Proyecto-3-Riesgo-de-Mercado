
#Delta Normal

# Librerías necesarias
library(quantmod)
library(dplyr)
library(tidyr)
library(knitr)
library(stringr)

# --- 1. Descargar precios ---
cartera <- c("AAPL","MSFT","NVDA","AMZN","META","GOOGL","AVGO","GOOG","TSLA","WMT",
             "INTC","JPM","F","NKE","GS","NFLX","CVX","ADBE","BAC","C") 
getSymbols(cartera, src = "yahoo", from = "2022-01-01", to = "2025-03-01")

# Armar DataFrame de precios
precios <- do.call(merge, lapply(cartera, function(sym) Cl(get(sym))))
colnames(precios) <- paste0("P", 1:ncol(precios))

# --- 2. Parámetros generales ---
nc <- c(0.95, 0.97, 0.99)
z_scores <- qnorm(nc)
dias <- c(1, 7, 15, 30, 60, 90, 180)
acciones <- 1000

# --- 3. Cálculo de rendimientos y portafolio ---
rend <- na.omit(ROC(precios, type = "discrete"))
PL <- as.matrix(rend)
PL <- cbind(PL, PT = rowSums(PL))  # rendimiento total del portafolio
colnames(PL)[ncol(PL)] <- "PT"

# --- 4. VaR diario ---
VaR_dia <- matrix(nrow = ncol(PL), ncol = length(nc))
colnames(VaR_dia) <- paste0("VaR_", nc*100, "%")
rownames(VaR_dia) <- colnames(PL)

for(k in 1:ncol(PL)){
  for(j in seq_along(nc)){
    VaR_dia[k, j] <- quantile(PL[, k], 1 - nc[j], na.rm = TRUE)
  }
}

# --- 5. ES diario ---
ES_dia <- matrix(nrow = ncol(PL), ncol = length(nc))
colnames(ES_dia) <- paste0("ES_", nc*100, "%")
rownames(ES_dia) <- colnames(PL)

for(k in 1:ncol(PL)){
  for(j in seq_along(nc)){
    umbral <- quantile(PL[, k], 1 - nc[j], na.rm = TRUE)
    ES_dia[k, j] <- mean(PL[PL[,k] <= umbral, k], na.rm = TRUE)
  }
}

# --- 6. Ajuste temporal (VaR y ES escalados) ---
VaR_list <- list()
ES_list <- list()

for(k in 1:ncol(PL)){
  VaR_temp <- matrix(nrow = length(dias), ncol = length(nc))
  ES_temp  <- matrix(nrow = length(dias), ncol = length(nc))
  for(i in seq_along(dias)){
    VaR_temp[i, ] <- VaR_dia[k, ] * sqrt(dias[i])
    ES_temp[i, ]  <- ES_dia[k, ]  * sqrt(dias[i])
  }
  colnames(VaR_temp) <- paste0("VaR_", nc*100, "%")
  colnames(ES_temp)  <- paste0("ES_",  nc*100, "%")
  rownames(VaR_temp) <- rownames(ES_temp) <- paste0(dias, " días")
  VaR_list[[colnames(PL)[k]]] <- VaR_temp
  ES_list[[colnames(PL)[k]]] <- ES_temp
}

# --- 7. Tabla final combinada ---
tabla_DN <- do.call(rbind, lapply(names(VaR_list), function(nombre) {
  df <- as.data.frame(VaR_list[[nombre]])
  df$Horizonte <- rownames(df)
  df$Emisora <- nombre
  df$ES_95 <- ES_list[[nombre]][, 1]
  df$ES_97 <- ES_list[[nombre]][, 2]
  df$ES_99 <- ES_list[[nombre]][, 3]
  df
}))

# Ordenar columnas y resultados
tabla_DN <- tabla_DN[, c("Emisora", "Horizonte", 
                         "VaR_95%", "VaR_97%", "VaR_99%", 
                         "ES_95", "ES_97", "ES_99")]
tabla_DN <- tabla_DN[order(as.numeric(str_remove(tabla_DN$Emisora, "P")), 
                           as.numeric(str_remove(tabla_DN$Horizonte, " días"))), ]

# Mostrar tabla final
kable(tabla_DN, digits = 4, caption = "VaR y ES por Delta Normal para cada emisora y portafolio")


















