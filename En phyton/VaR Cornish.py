# -*- coding: utf-8 -*-
"""
Created on Sun May 11 16:14:12 2025

@author: Domingo
"""

import pandas as pd
import numpy as np
from scipy.stats import norm

# Leer datos desde archivo CSV
d1 = pd.read_csv("d1.csv")

# Asegurar que la primera columna sea datetime
if not np.issubdtype(d1.iloc[:, 0].dtype, np.datetime64):
    d1.iloc[:, 0] = pd.to_datetime(d1.iloc[:, 0])

# Renombrar columnas
d1.columns = ['Fecha'] + [f"P{i+1}" for i in range(d1.shape[1] - 1)]

# Calcular rendimientos porcentuales
rendimientos = d1.drop(columns='Fecha').pct_change().dropna()

# Agregar columna de rendimiento promedio del portafolio
rendimientos['PT'] = rendimientos.mean(axis=1)

# Parámetros
niveles_confianza = [0.95, 0.97, 0.99]
horizontes = [1, 7, 15, 30, 60, 90, 180]

# Función para calcular VaR y ES usando Cornish-Fisher
def cornish_fisher_var_es(series, niveles_confianza):
    x = series.dropna()
    mu = x.mean()
    sigma = x.std()
    S = ((x - mu) ** 3).mean() / sigma ** 3
    K = ((x - mu) ** 4).mean() / sigma ** 4
    z = norm.ppf(niveles_confianza)
    z_cf = z + (1/6)*(z**2 - 1)*S + (1/24)*(z**3 - 3*z)*(K - 3) - (1/36)*(2*z**3 - 5*z)*S**2
    VaR = -(mu + z_cf * sigma)
    ES = -(mu + sigma * norm.pdf(z_cf) / (1 - np.array(niveles_confianza)))
    return VaR, ES

# Construir tabla de resultados
filas = []

for emisora in rendimientos.columns:
    VaR_base, ES_base = cornish_fisher_var_es(rendimientos[emisora], niveles_confianza)
    
    for h in horizontes:
        escala = np.sqrt(h)
        fila = {
            'Emisora': emisora,
            'Horizonte': h,
            'VaR95': VaR_base[0] * escala,
            'VaR97': VaR_base[1] * escala,
            'VaR99': VaR_base[2] * escala,
            'ES95': ES_base[0] * escala,
            'ES97': ES_base[1] * escala,
            'ES99': ES_base[2] * escala,
        }
        filas.append(fila)

# Crear DataFrame final
tabla_Cornish = pd.DataFrame(filas).sort_values(by=['Emisora', 'Horizonte'])

# Mostrar tabla
print(tabla_Cornish)
