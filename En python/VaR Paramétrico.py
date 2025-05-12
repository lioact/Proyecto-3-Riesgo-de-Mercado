#%%
import pandas as pd
import numpy as np
from scipy.stats import norm

# Leer datos
d1 = pd.read_csv("d1.csv")

# Asegurar que la columna de fecha tenga tipo datetime
if not np.issubdtype(d1.iloc[:, 0].dtype, np.datetime64):
    d1.iloc[:, 0] = pd.to_datetime(d1.iloc[:, 0])

# Renombrar columnas: Fecha, P1...P20
d1.columns = ['Fecha'] + [f"P{i+1}" for i in range(d1.shape[1] - 1)]

# Calcular rendimientos simples
returns = d1.drop(columns='Fecha').pct_change().dropna()

# Agregar columna de rendimiento del portafolio (suma simple, igual que en R)
returns['PT'] = returns.sum(axis=1)

# Parámetros
emisoras = returns.columns.tolist()
niveles_conf = [0.95, 0.97, 0.99]
horizontes = [1, 7, 15, 30, 60, 90, 180]

# Calcular desviaciones estándar (volatilidades)
volatilidades = returns.std()

# Funciones de VaR y ES paramétrico (distribución normal)
def fVaR(nc, sigma, t):
    return norm.ppf(nc) * sigma * np.sqrt(t)

def fES(nc, sigma, t):
    return norm.pdf(norm.ppf(nc)) / (1 - nc) * sigma * np.sqrt(t)

# Construir tabla de resultados
resultados = []

for em in emisoras:
    sigma = volatilidades[em]
    for h in horizontes:
        for nc in niveles_conf:
            var = fVaR(nc, sigma, h)
            es = fES(nc, sigma, h)
            resultados.append([em, h, nc, var, es])

# Convertir a DataFrame
resultados_df = pd.DataFrame(resultados, columns=['Emisora', 'Horizonte', 'Confianza', 'VaR', 'ES'])

# Reformatear tabla final tipo pivot
tabla_parametrico = resultados_df.pivot_table(
    index=['Emisora', 'Horizonte'],
    columns='Confianza',
    values=['VaR', 'ES']
).reset_index()

# Aplanar nombres de columnas
tabla_parametrico.columns = ['Emisora', 'Horizonte'] + [f"{v}_{int(c*100)}" for v, c in tabla_parametrico.columns.tolist()[2:]]

# Ordenar por emisora y horizonte
tabla_parametrico['Emisora_num'] = tabla_parametrico['Emisora'].apply(lambda x: float('inf') if x == 'PT' else int(x[1:]))
tabla_parametrico = tabla_parametrico.sort_values(by=['Emisora_num', 'Horizonte']).drop(columns='Emisora_num')

# Mostrar resultado
print(tabla_parametrico.head(10))  
