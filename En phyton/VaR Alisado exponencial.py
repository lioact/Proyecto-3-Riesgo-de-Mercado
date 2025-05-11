import pandas as pd
import numpy as np

# Leer datos desde archivo CSV
d1 = pd.read_csv("d1.csv")

# Si la columna de fechas no está en formato datetime:
if not np.issubdtype(d1.iloc[:, 0].dtype, np.datetime64):
    d1.iloc[:, 0] = pd.to_datetime(d1.iloc[:, 0])

# Asegurar que la primera columna sea la fecha
d1.columns = ['Fecha'] + [f"P{i+1}" for i in range(d1.shape[1] - 1)]

# Calcular rendimientos porcentuales (omitimos la columna de fecha)
rendimientos = d1.drop(columns='Fecha').pct_change().dropna()

# Agregar columna de rendimiento total de la cartera
rendimientos['PT'] = rendimientos.sum(axis=1)

# Parámetros
niveles_conf = [0.95, 0.97, 0.99]
horizontes = [1, 7, 15, 30, 60, 90, 180]
alpha = 0.95

# Función de VaR y ES con alisado exponencial
def VaR_ES_alisado(pl_vector, niveles, alpha=0.95):
    pl_vector = pl_vector[~np.isnan(pl_vector)]
    n = len(pl_vector)
    pl_ord = np.sort(pl_vector)[::-1]
    pesos = alpha ** np.arange(n-1, -1, -1) * (1 - alpha)
    pesos /= pesos.sum()
    Fx = np.cumsum(pesos)

    resultados = {}
    for c in niveles:
        idx = np.argmax(Fx > c)
        VaR = pl_ord[idx]
        ES = np.sum(pl_ord[:idx+1] * pesos[:idx+1]) / pesos[:idx+1].sum()
        resultados[f"VaR_{int(c*100)}"] = VaR
        resultados[f"ES_{int(c*100)}"] = ES
    return resultados

# Calcular resultados para cada emisora y horizonte
filas = []
for col in rendimientos.columns:
    resultado_base = VaR_ES_alisado(rendimientos[col].values, niveles_conf, alpha)
    for h in horizontes:
        fila = {k: v * np.sqrt(h) for k, v in resultado_base.items()}
        fila['Horizonte'] = h
        fila['Emisora'] = col
        filas.append(fila)

# Crear DataFrame final
tabla_Alisado = pd.DataFrame(filas)
tabla_Alisado = tabla_Alisado[['Emisora', 'Horizonte'] + [col for col in tabla_Alisado.columns if col.startswith("VaR") or col.startswith("ES")]]
tabla_Alisado.sort_values(by=['Emisora', 'Horizonte'], inplace=True)

# Mostrar tabla
print(tabla_Alisado)
