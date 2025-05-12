
#%%
import pandas as pd
import numpy as np

# Leer datos desde archivo CSV
d1 = pd.read_csv("d1.csv")

# Asegurar que la primera columna sea de tipo datetime
if not np.issubdtype(d1.iloc[:, 0].dtype, np.datetime64):
    d1.iloc[:, 0] = pd.to_datetime(d1.iloc[:, 0])

# Renombrar columnas: Fecha y precios P1, P2, ...
d1.columns = ['Fecha'] + [f"P{i+1}" for i in range(d1.shape[1] - 1)]

# Calcular rendimientos porcentuales (omitimos la columna de fecha)
rendimientos = d1.drop(columns='Fecha').pct_change().dropna()

# Agregar columna de rendimiento promedio del portafolio
rendimientos['PT'] = rendimientos.mean(axis=1)

# Parámetros
emisoras = rendimientos.columns.tolist()
niveles_conf = [0.95, 0.97, 0.99]
horizontes = [1, 7, 15, 30, 60, 90, 180]
num_simulaciones = [5000, 10000, 20000]

# Función para calcular VaR y ES
def calcular_var_es(perdidas, niveles):
    vars_ = np.quantile(perdidas, 1 - np.array(niveles))
    es_ = [perdidas[perdidas >= v].mean() for v in vars_]
    return {'VaR': vars_, 'ES': es_}

# Simulación bootstrap
np.random.seed(123)
resultados = []

for nsim in num_simulaciones:
    for j, em in enumerate(emisoras):
        y = rendimientos.iloc[:, j].values
        
        for h in horizontes:
            sim_rend = np.random.choice(y, size=(nsim, h), replace=True)
            rend_acum = sim_rend.sum(axis=1)
            metrics = calcular_var_es(-rend_acum, niveles_conf)  # pérdidas negativas
            
            resultados.append({
                "NumSim": nsim,
                "Emisora": em,
                "Horizonte": h,
                "VaR95": metrics['VaR'][0],
                "VaR97": metrics['VaR'][1],
                "VaR99": metrics['VaR'][2],
                "ES95": metrics['ES'][0],
                "ES97": metrics['ES'][1],
                "ES99": metrics['ES'][2],
            })

    # Simulación del portafolio: promedio simple de rendimientos acumulados
    for h in horizontes:
        sim_rend_ptf = np.array([
            np.mean([np.sum(np.random.choice(rendimientos.iloc[:, j].values, size=h, replace=True))
                     for j in range(len(emisoras))])
            for _ in range(nsim)
        ])
        
        metrics = calcular_var_es(-sim_rend_ptf, niveles_conf)
        resultados.append({
            "NumSim": nsim,
            "Emisora": "PT",
            "Horizonte": h,
            "VaR95": metrics['VaR'][0],
            "VaR97": metrics['VaR'][1],
            "VaR99": metrics['VaR'][2],
            "ES95": metrics['ES'][0],
            "ES97": metrics['ES'][1],
            "ES99": metrics['ES'][2],
        })

# Consolidar resultados en un DataFrame
tabla_bootstrap2 = pd.DataFrame(resultados)

# Mostrar resultados
print(tabla_bootstrap2.head())
