# -*- coding: utf-8 -*-
"""
Created on Sun May 11 14:53:23 2025

@author: Domingo
"""

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
