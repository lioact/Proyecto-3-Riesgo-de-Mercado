
#%%
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
nc = [0.95, 0.97, 0.99]
dias = [1, 7, 15, 30, 60, 90, 180]
m = rendimientos.shape[1]

# Calcular VaR para cada emisora (incluyendo PT)
VaRInd = pd.DataFrame(index=rendimientos.columns, columns=[f"VaR_{int(c*100)}%" for c in nc])
for col in rendimientos.columns:
    for c in nc:
        VaRInd.loc[col, f"VaR_{int(c*100)}%"] = rendimientos[col].quantile(c)

# Ajustar VaR a distintos horizontes
VaRIndD = {}
for col in rendimientos.columns:
    matriz = np.zeros((len(dias), len(nc)))
    for i, d in enumerate(dias):
        for j, c in enumerate(nc):
            matriz[i, j] = VaRInd.loc[col, f"VaR_{int(c*100)}%"] * np.sqrt(d)
    df = pd.DataFrame(matriz, index=[f"{d}días" for d in dias], columns=[f"VaR_{int(c*100)}%" for c in nc])
    VaRIndD[col] = df

# Función para calcular Expected Shortfall
def VaR_ES(rendimientos, niveles_confianza):
    cols = rendimientos.columns
    VaR = pd.DataFrame(index=cols, columns=[f"VaR_{int(c*100)}%" for c in niveles_confianza])
    ES = pd.DataFrame(index=cols, columns=[f"ES_{int(c*100)}%" for c in niveles_confianza])
    
    for col in cols:
        for c in niveles_confianza:
            var_c = rendimientos[col].quantile(c)
            VaR.loc[col, f"VaR_{int(c*100)}%"] = var_c
            ES.loc[col, f"ES_{int(c*100)}%"] = rendimientos[col][rendimientos[col] > var_c].mean()
    
    return {'VaR': VaR, 'ES': ES}

# Calcular ES
ES_SH = VaR_ES(rendimientos, nc)

# Ajustar ES a distintos horizontes
ESIndD = {}
for col in rendimientos.columns:
    matriz = np.zeros((len(dias), len(nc)))
    for i, d in enumerate(dias):
        for j, c in enumerate(nc):
            matriz[i, j] = ES_SH["ES"].loc[col, f"ES_{int(c*100)}%"] * np.sqrt(d)
    df = pd.DataFrame(matriz, index=[f"{d}días" for d in dias], columns=[f"ES_{int(c*100)}%" for c in nc])
    ESIndD[col] = df

# Crear tabla final
tabla_Hist = pd.concat([
    pd.DataFrame({
        "Emisora": nombre,
        "Horizonte": df.index,
        **df,
        "ES_95": ESIndD[nombre].iloc[:, 0].values,
        "ES_97": ESIndD[nombre].iloc[:, 1].values,
        "ES_99": ESIndD[nombre].iloc[:, 2].values
    }) for nombre, df in VaRIndD.items()
], ignore_index=True)

# Extraer número de emisora para ordenación (manejar "PT")
tabla_Hist["Emisora_num"] = tabla_Hist["Emisora"].str.extract("(\d+)").fillna(999).astype(int)
tabla_Hist["Horizonte_num"] = tabla_Hist["Horizonte"].str.extract("(\d+)").astype(int)

# Ordenar
tabla_Hist = tabla_Hist.sort_values(by=["Emisora_num", "Horizonte_num"]).drop(columns=["Emisora_num", "Horizonte_num"])

# Mostrar tabla
print(tabla_Hist)