#%%
import pandas as pd
import numpy as np
from scipy.stats import laplace

# Leer datos
d1 = pd.read_csv("d1.csv")

# Asegurar tipo datetime en la columna de fecha
if not np.issubdtype(d1.iloc[:, 0].dtype, np.datetime64):
    d1.iloc[:, 0] = pd.to_datetime(d1.iloc[:, 0])

# Renombrar columnas
d1.columns = ['Fecha'] + [f"P{i+1}" for i in range(d1.shape[1] - 1)]

# Calcular rendimientos simples
returns = d1.drop(columns='Fecha').pct_change().dropna()

# Agregar columna de rendimiento de portafolio (promedio simple)
returns['PT'] = returns.mean(axis=1)

# Parámetros
nc = [0.95, 0.97, 0.99]
dias = [1, 7, 15, 30, 60, 90, 180]
numsim_vec = [5000, 10000, 20000]

emisoras = returns.columns.drop('PT').tolist()  
m = len(emisoras)
precio_actuales = d1.drop(columns='Fecha').iloc[-1].values
precio_portafolio = precio_actuales.mean() * (m - 1)  # PT excluye la fila PT en el cálculo individual

# Estimar parámetros de Laplace por emisora
mu_lap = returns.mean().values
b_lap = np.maximum(returns.std().values / np.sqrt(2), 1e-6)  # b = std / sqrt(2) para Laplace

# Simulaciones
np.random.seed(123)
filas = []

for nsim in numsim_vec:
    for idx, emisora in enumerate(emisoras):
        mu = mu_lap[idx]
        b = b_lap[idx]
        precio_actual = precio_actuales[idx]

        for h in dias:
            sim_rend = laplace.rvs(loc=mu, scale=b, size=(nsim, h))
            sim_acum = sim_rend.sum(axis=1)
            sim_precios = precio_actual * (1 + sim_acum)
            sim_perdidas = precio_actual - sim_precios

            for conf in nc:
                var = np.quantile(sim_perdidas, 1 - conf)
                es = sim_perdidas[sim_perdidas <= var].mean()

                filas.append({
                    'NumSim': nsim,
                    'Emisora': emisora,
                    'Horizonte': h,
                    'Confianza': conf,
                    'VaR': var,
                    'ES': es
                })

    # Simulación para portafolio
    ret_pt = returns['PT']
    mu_pt = ret_pt.mean()
    b_pt = np.maximum(ret_pt.std() / np.sqrt(2), 1e-6)
    precio_pt = d1.drop(columns='Fecha').iloc[-1].mean() * (m - 1)

    for h in dias:
        sim_rend = laplace.rvs(loc=mu_pt, scale=b_pt, size=(nsim, h))
        sim_acum = sim_rend.sum(axis=1)
        sim_precios = precio_pt * (1 + sim_acum)
        sim_perdidas = precio_pt - sim_precios

        for conf in nc:
            var = np.quantile(sim_perdidas, 1 - conf)
            es = sim_perdidas[sim_perdidas <= var].mean()

            filas.append({
                'NumSim': nsim,
                'Emisora': 'PT',
                'Horizonte': h,
                'Confianza': conf,
                'VaR': var,
                'ES': es
            })

# Convertir resultados a DataFrame
df_resultados = pd.DataFrame(filas)

# Pivotear como en R
tabla_Laplace = df_resultados.pivot_table(
    index=['NumSim', 'Emisora', 'Horizonte'],
    columns='Confianza',
    values=['VaR', 'ES']
)

# Aplanar columnas multinivel
tabla_Laplace.columns = [f"{col[0]}{int(col[1]*100)}" for col in tabla_Laplace.columns]
tabla_Laplace = tabla_Laplace.reset_index().sort_values(by=['NumSim', 'Emisora', 'Horizonte'])

# Mostrar tabla
print(tabla_Laplace)
