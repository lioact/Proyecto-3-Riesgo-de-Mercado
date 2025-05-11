# -*- coding: utf-8 -*-
"""
Created on Sun May 11 16:39:09 2025

@author: Domingo
"""

import pandas as pd
import numpy as np
from scipy.stats import norm
from tqdm import tqdm

# Cargar datos
d1 = pd.read_csv("d1.csv")

# Asegurar formato de fecha
if not np.issubdtype(d1.iloc[:, 0].dtype, np.datetime64):
    d1.iloc[:, 0] = pd.to_datetime(d1.iloc[:, 0])

# Renombrar columnas
d1.columns = ['Fecha'] + [f"P{i+1}" for i in range(d1.shape[1] - 1)]

# Calcular rendimientos simples
returns = d1.drop(columns='Fecha').pct_change().dropna()

# Calcular rendimiento del portafolio como promedio simple
returns['PT'] = returns.mean(axis=1)

# Parámetros
emisoras = [f"P{i+1}" for i in range(returns.shape[1] - 1)]
nc = [0.95, 0.97, 0.99]
numsim_vec = [5000, 10000, 20000]
dias = [1, 7, 15, 30, 60, 90, 180]

# Precios actuales
precio_actuales = d1.iloc[-1, 1:].values

# Estimar parámetros de distribución normal
mu = returns.mean()
sigma = returns.std()

# Simulaciones
resultados = []

np.random.seed(123)

for s in tqdm(numsim_vec, desc="Simulaciones"):
    # Emisoras individuales
    for em in emisoras:
        mu_e = mu[em]
        sigma_e = sigma[em]
        precio_actual = d1[em].iloc[-1]

        for h in dias:
            sim_rend = np.random.normal(mu_e, sigma_e, (s, h))
            sim_acum = sim_rend.sum(axis=1)
            sim_perdidas = sim_acum  # rendimientos acumulados

            for conf in nc:
                var = np.quantile(sim_perdidas, 1 - conf)
                es = sim_perdidas[sim_perdidas <= var].mean()

                resultados.append([s, em, h, conf, var, es])

    # Portafolio completo
    mu_port = mu['PT']
    sigma_port = sigma['PT']
    precio_port = d1[emisoras].iloc[-1].sum()

    for h in dias:
        sim_rend = np.random.normal(mu_port, sigma_port, (s, h))
        sim_acum = sim_rend.sum(axis=1)
        sim_precios = precio_port * (1 + sim_acum)
        sim_perdidas = precio_port - sim_precios

        for conf in nc:
            var = np.quantile(sim_perdidas, conf)
            es = sim_perdidas[sim_perdidas > var].mean()

            resultados.append([s, "PT", h, conf, var, es])

# Crear DataFrame con resultados
resultados_df = pd.DataFrame(resultados, columns=["NumSim", "Emisora", "Horizonte", "Confianza", "VaR", "ES"])

# Pivotar tabla para formato final
tabla_Montecarlo = resultados_df.pivot_table(
    index=["NumSim", "Emisora", "Horizonte"],
    columns="Confianza",
    values=["VaR", "ES"]
).reset_index()

# Aplanar nombres de columnas
tabla_Montecarlo.columns = ['NumSim', 'Emisora', 'Horizonte'] + [f"{v}{int(c*100)}" for v, c in tabla_Montecarlo.columns.tolist()[3:]]

# Ordenar tabla
tabla_Montecarlo['Emisora_num'] = tabla_Montecarlo['Emisora'].apply(lambda x: float('inf') if x == 'PT' else int(x[1:]))
tabla_Montecarlo = tabla_Montecarlo.sort_values(by=['NumSim', 'Emisora_num', 'Horizonte']).drop(columns='Emisora_num')

# Mostrar tabla
print(tabla_Montecarlo.head(10))
