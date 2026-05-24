import pandas as pd
import numpy as np
import csv

# Cargar el CSV original
file_path = 'C:/Users/josep/Downloads/es_parte_de.csv'
df = pd.read_csv(file_path, encoding="latin-1")

# Función para separar nombre y áreas si vienen en la columna Nombre
def separar_nombre_areas(row):
    if ',' in row['Nombre']:
        partes = row['Nombre'].split(',', 1)
        nombre = partes[0].strip()
        areas = partes[1].strip()
        return pd.Series([nombre, areas])
    else:
        return pd.Series([row['Nombre'], np.nan])

# Aplicamos la separación a todas las filas
df[['Nombre_extraido', 'Areas_extraido']] = df.apply(separar_nombre_areas, axis=1)

# Si Areas_extraido está vacío, usamos la columna original Areas
df['Areas_final'] = df['Areas_extraido'].combine_first(df['Areas'])

# Limpiar las áreas directamente a string (sin listas)
def normalizar_areas(area):
    if pd.isna(area):
        return ''
    else:
        return ', '.join([a.strip() for a in area.split(',')])

df['Areas_normalizadas'] = df['Areas_final'].apply(normalizar_areas)

# La columna final de nombres la tomamos de Nombre_extraido si existe, sino de Nombre
df['Nombre_final'] = df['Nombre_extraido'].combine_first(df['Nombre'])

# Creamos el DataFrame limpio
resultado = df[['Nombre_final', 'Areas_normalizadas']].rename(columns={'Nombre_final': 'Nombre', 'Areas_normalizadas': 'Areas'})

# Antes de exportar, nos aseguramos de no tener comillas internas
resultado['Nombre'] = resultado['Nombre'].str.replace('"', '', regex=False)
resultado['Areas'] = resultado['Areas'].str.replace('"', '', regex=False)

# Exportamos el CSV con comillas para cada campo
resultado.to_csv(
    'C:/Users/josep/Downloads/es_parte_de_limpio_listas.csv',
    index=False,
    encoding="latin-1"
)
