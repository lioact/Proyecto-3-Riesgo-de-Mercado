#%%
import pandas as pd
import numpy as np
from scipy.stats import norm

#Leer datos y preparar DataFrame
d1 = pd.read_csv("d1.csv")

# Asegurar que la primera columna sea datetime
if not np.issubdtype(d1.iloc[:, 0].dtype, np.datetime64):
    d1.iloc[:, 0] = pd.to_datetime(d1.iloc[:, 0])

# Renombrar columnas: Fecha, P1...P20
d1.columns = ['Fecha'] + [f"P{i+1}" for i in range(d1.shape[1] - 1)]

# Calcular rendimientos diarios (porcentuales)
returns = d1.drop(columns='Fecha').pct_change().dropna()

# Agregar rendimiento del portafolio (suma simple)
returns['PT'] = returns.sum(axis=1)

#Parámetros ---
niveles_conf = [0.95, 0.97, 0.99]
dias = [1, 7, 15, 30, 60, 90, 180]
z_scores = norm.ppf(niveles_conf)
columnas = returns.columns.tolist()

#VaR diario histórico ---
VaR_dia = pd.DataFrame(index=columnas, columns=[f"VaR_{int(n*100)}%" for n in niveles_conf])
for col in columnas:
    for nc in niveles_conf:
        VaR_dia.loc[col, f"VaR_{int(nc*100)}%"] = returns[col].quantile(1 - nc)

#ES diario histórico ---
ES_dia = pd.DataFrame(index=columnas, columns=[f"ES_{int(n*100)}%" for n in niveles_conf])
for col in columnas:
    for nc in niveles_conf:
        threshold = returns[col].quantile(1 - nc)
        ES_dia.loc[col, f"ES_{int(nc*100)}%"] = returns[col][returns[col] <= threshold].mean()

#Escalamiento temporal ---
tabla_list = []

for col in columnas:
    for h in dias:
        fila = {'Emisora': col, 'Horizonte': f"{h} días"}
        for nc in niveles_conf:
            var_col = f"VaR_{int(nc*100)}%"
            es_col = f"ES_{int(nc*100)}%"
            fila[var_col] = VaR_dia.loc[col, var_col] * np.sqrt(h)
            fila[es_col]  = ES_dia.loc[col, es_col]  * np.sqrt(h)
        tabla_list.append(fila)

#Convertir a DataFrame y ordenar ---
tabla_DN = pd.DataFrame(tabla_list)

# Ordenar por número de emisora (P1, P2, ..., PT al final)
def emisora_num(e):
    if e == 'PT': return float('inf')
    return int(e[1:])

tabla_DN['Emisora_num'] = tabla_DN['Emisora'].apply(emisora_num)
tabla_DN['Horizonte_num'] = tabla_DN['Horizonte'].str.extract(r'(\d+)').astype(int)
tabla_DN = tabla_DN.sort_values(by=['Emisora_num', 'Horizonte_num']).drop(columns=['Emisora_num', 'Horizonte_num'])

#Mostrar tabla final (opcionalmente exportar o imprimir)
pd.set_option("display.float_format", "{:.4f}".format)
print(tabla_DN.head(10))  


