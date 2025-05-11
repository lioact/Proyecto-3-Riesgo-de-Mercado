# librerias 
library(quantmod) # para descargar datos de Yahoo Finance
library(goftest) # para pruebas de bondad de ajuste
library(corrplot) # para graficar matrices de correlación
library(fitdistrplus) # para ajustar distribuciones
library(dplyr) # para manipulación de datos
library(VGAM) # para ajustar distribuciones
library(tidyr) # para manipulación de datos
library(ggplot2) # para graficar
library(lubridate) # para manipulación de fechas
library(tidyr) # para manipulación de datos
library(knitr) # para crear tablas
library(stringr) # para manipulación de textos

# obtener información
cartera <- c("AAPL","MSFT","NVDA","AMZN","META","GOOGL","AVGO","GOOG","TSLA","WMT","INTC","JPM","F","NKE","GS","NFLX","CVX","ADBE","BAC","C") 
getSymbols(cartera,src = "yahoo",from="2022-01-01", to="2025-03-01")

d1 <- data.frame(P1=AAPL$AAPL.Close,P2=MSFT$MSFT.Close,P3=NVDA$NVDA.Close,P4=AMZN$AMZN.Close,P5=META$META.Close,P6=GOOGL$GOOGL.Close,P7=AVGO$AVGO.Close,P8=GOOG$GOOG.Close,P9=TSLA$TSLA.Close,P10=WMT$WMT.Close,P11=INTC$INTC.Close,P12=JPM$JPM.Close,P13=F$F.Close,P14=NKE$NKE.Close,P15=GS$GS.Close,P16=NFLX$NFLX.Close,P17=CVX$CVX.Close,P18=ADBE$ADBE.Close,P19=BAC$BAC.Close,P20=C$C.Close)


# Parametros generales

colnames(d1) <- c("P1","P2","P3","P4","P5","P6","P7","P8","P9","P10","P11","P12","P13","P14","P15","P16","P17","P18","P19","P20")

n <- length(d1$P1) #número de días 
m <- length(d1[1,])  #número de emisoras
nc <- c(0.95,0.97,0.99) #niveles de confianza
numsim <- c(5000,10000,20000) #número de simulaciones
dias <- c(1, 7, 15, 30, 60, 90, 180)  #horizontes de tiempo



## Rendimientos

# Separar solo las columnas de precios (sin la columna Fecha)
d1_precios <- d1[ , 1:20]  # si las primeras 20 columnas son los precios

n <- nrow(d1_precios)
m <- ncol(d1_precios)

Rend <- matrix(NA, nrow = n - 1, ncol = m)
colnames(Rend) <- paste0("P", 1:m)

for(k in 1:m){
  for(j in 1:(n-1)){
    Rend[j, k] <- d1_precios[j+1, k] / d1_precios[j, k] - 1
  }
}





## Función de perdidas y ganancias
PL <- matrix(data=0,nrow = n-1,ncol = m+1)
colnames(PL) <- c(paste0("P", 1:m), "PT")

PL <- Rend  # directamente los rendimientos ya calculados
PT <- rowSums(Rend, na.rm = TRUE)  # rendimiento total del portafolio
PL <- cbind(PL, PT)  # agregar columna "PT"




## #### Funcion 

PL <- cbind(PL, PT = rowSums(PL, na.rm = TRUE))
m <- ncol(PL)  # esto ya incluye la columna PT al final
VaRInd <- matrix(nrow = m, ncol = length(nc))
colnames(VaRInd) <- c("VaR_95%","VaR_97%","VaR_99%")
rownames(VaRInd) <- colnames(PL)[1:m]

for(k in 1:m){
  for(j in seq_along(nc)){
    VaRInd[k, j] <- quantile(PL[,k], nc[j], na.rm = TRUE)[1]
  }
}

#VaR ajustado a diferentes horizontes de tiempo

VaRIndD <- list()  #lista de emisoras

for (k in 1:m) {
  MVaRD <- matrix(nrow = length(dias), ncol = length(nc))
  for (i in seq_along(dias)) {
    for (j in seq_along(nc)) {
      MVaRD[i, j] <- VaRInd[k, j] * sqrt(dias[i])
    }
  }
  colnames(MVaRD) <- paste0("VaR_", nc * 100, "%")
  rownames(MVaRD) <- paste0(dias, "días")
  VaRIndD[[ colnames(PL)[k] ]] <- MVaRD
}
# ES: Expected Shortfall
VaR_ES <- function(PL, niveles_confianza = c(0.95, 0.97, 0.99)) {
  m <- ncol(PL)  # número de emisoras
  VaR <- matrix(NA, nrow = m, ncol = length(niveles_confianza))
  ES  <- matrix(NA, nrow = m, ncol = length(niveles_confianza))
  
  colnames(VaR) <- paste0("VaR_", niveles_confianza * 100, "%")
  colnames(ES)  <- paste0("ES_", niveles_confianza * 100, "%")
  rownames(VaR) <- rownames(ES) <- colnames(PL)
  
  for (k in 1:m) {
    for (j in seq_along(niveles_confianza)) {
      var_kj <- quantile(PL[, k], niveles_confianza[j], na.rm = TRUE)
      VaR[k, j] <- var_kj
      ES[k, j] <- mean(PL[, k][PL[, k] > var_kj], na.rm = TRUE)
    }
  }
  
  return(list(VaR = VaR, ES = ES))
}

ES.SH <- VaR_ES(PL) # Calcular VaR y ES
# ES ajustado a diferentes horizontes de tiempo
ESIndD <- list()

for (k in 1:m) {
  MESD <- matrix(nrow = length(dias), ncol = length(nc))
  for (i in seq_along(dias)) {
    for (j in seq_along(nc)) {
      MESD[i, j] <- ES.SH$ES[k, j] * sqrt(dias[i])
    }
  }
  colnames(MESD) <- paste0("ES_", nc * 100, "%")
  rownames(MESD) <- paste0(dias, "días")
  ESIndD[[colnames(PL)[k]]] <- MESD
}









#################################TABLA

tabla_Hist <- do.call(rbind, lapply(names(VaRIndD), function(nombre) {
  df <- as.data.frame(VaRIndD[[nombre]])
  df$Horizonte <- rownames(df)
  df$Emisora <- nombre
  df$ES_95 <- ESIndD[[nombre]][, 1]
  df$ES_97 <- ESIndD[[nombre]][, 2]
  df$ES_99 <- ESIndD[[nombre]][, 3]
  
  df
}))

# Reordenar columnas para claridad
tabla_Hist <- tabla_Hist[, c("Emisora", "Horizonte", 
                                         "VaR_95%", "VaR_97%", "VaR_99%", 
                                         "ES_95", "ES_97", "ES_99")]

## Ordenar por el número que sigue a la 'P'
tabla_Hist <- tabla_Hist[order(as.numeric(gsub("P", "", tabla_Hist$Emisora)),
                                           as.numeric(tabla_Hist$Horizonte)), ]
print(tabla_Hist)






























