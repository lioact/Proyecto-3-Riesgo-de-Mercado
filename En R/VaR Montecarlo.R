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


# Agregar columna de fechas
d1$Fecha <- index(AAPL)
d1 <- d1 %>% relocate(Fecha)

# Parámetros generales
colnames(d1)[2:21] <- paste0("P", 1:20)
n <- nrow(d1)
m <- 20
nc <- c(0.95, 0.97, 0.99)
numsim <- c(5000, 10000, 20000)
dias <- c(1, 7, 15, 30, 60, 90, 180)

# Rendimientos
d1_precios <- d1[ , 2:(m+1)]
Rend <- matrix(NA, nrow = n - 1, ncol = m)
colnames(Rend) <- paste0("P", 1:m)

for(k in 1:m){
  for(j in 1:(n-1)){
    Rend[j, k] <- d1_precios[j+1, k] / d1_precios[j, k] - 1
  }
}

# Función de pérdidas y ganancias
PL <- matrix(0, nrow = n - 1, ncol = m + 1)
colnames(PL) <- c(paste0("P", 1:m), "PT")

for(k in 1:m){
  for(j in 1:(n-1)){
    PL[j, k] <- d1[n, k + 1] - d1[n, k + 1] * (1 + Rend[j, k])
    PL[j, m + 1] <- sum(PL[j, 1:m])
  }
}

# VaR Simulación Monte Carlo Normal
# Estimar parámetros mu y sigma
y <- function(x) {
  mod <- fitdist(x[!is.na(x)], "norm", method = "mle")
  return(mod$estimate)
}
p <- apply(Rend, 2, y)
muv <- p[1, ]
sigmav <- p[2, ]

emisoras <- colnames(d1)[2:(m+1)]
precio_actuales <- as.numeric(d1[n, 2:(m+1)])
resultados <- data.frame()
numsim_vec <- c(5000, 10000, 20000)

set.seed(123)

for (s in numsim_vec) {
  # Emisoras individuales
  for (j in seq_along(emisoras)) {
    mu <- muv[j]
    sigma <- sigmav[j]
    precio_actual <- precio_actuales[j]
    
    for (h in dias) {
      sim_rend <- matrix(rnorm(s * h, mu, sigma), ncol = h)
      sim_acum <- rowSums(sim_rend)
      sim_precios <- precio_actual * (1 + sim_acum)
      sim_perdidas <- sim_acum  # rendimientos acumulados directamente
      
      for (conf in nc) {
        var <- quantile(sim_perdidas, probs = 1 - conf, na.rm = TRUE)
        es <- mean(sim_perdidas[sim_perdidas <= var], na.rm = TRUE)
        
        
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
  
  # Portafolio
  mu_port <- mean(rowMeans(Rend, na.rm = TRUE))
  sigma_port <- sd(rowMeans(Rend, na.rm = TRUE))
  precio_port <- sum(precio_actuales)
  
  for (h in dias) {
    sim_rend <- matrix(rnorm(s * h, mu_port, sigma_port), ncol = h)
    sim_acum <- rowSums(sim_rend)
    sim_precios <- precio_port * (1 + sim_acum)
    sim_perdidas <- precio_port - sim_precios
    
    for (conf in nc) {
      var <- quantile(sim_perdidas, probs = conf, na.rm = TRUE)
      es <- mean(sim_perdidas[sim_perdidas > var], na.rm = TRUE)
      
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

# Tabla final
tabla_Montercarlo <- resultados %>%
  pivot_wider(
    names_from = Confianza,
    values_from = c(VaR, ES),
    names_glue = "{.value}{round(100 * Confianza)}"
  ) %>%
  mutate(Emisora_num = ifelse(Emisora == "PT", Inf, as.numeric(gsub("P", "", Emisora)))) %>%
  arrange(NumSim, Emisora_num, Horizonte) %>%
  select(NumSim, Emisora, Horizonte, starts_with("VaR"), starts_with("ES"))

print(tabla_Montercarlo)




















