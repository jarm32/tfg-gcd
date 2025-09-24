import pandas as pd

# Cargar el archivo CSV con la codificación correcta
file_path = 'Sanchis Llopis, Jose Maria.csv'
data = pd.read_csv(file_path, encoding='utf-8')  # Cambia 'utf-8' si el archivo tiene otra codificación

# Separar la última columna del resto
columns_except_last = data.iloc[:, :-1]
last_column = data.iloc[:, -1]

# Guardar los dos nuevos archivos CSV con la misma codificación
file_path_columns = 'ha_publicado_en_SanchisLJM.csv'
file_path_last_column = 'ha_publicado_con_SanchisLJM.csv'
columns_except_last.to_csv(file_path_columns, index=False, encoding='utf-8')
last_column.to_csv(file_path_last_column, index=False, encoding='utf-8')

# Mostrar las rutas de los archivos generados
print(file_path_columns, file_path_last_column)