import pandas as pd
import matplotlib.pyplot as plt

df = pd.read_excel("One_Hot_Palabras_Revistas.xlsx")

col_revista = df.columns[0] 
cols_numericas = df.select_dtypes(include=["number"]).columns 

min_val = 10
suma_columnas = df[cols_numericas].sum()
cols_palabras_filtradas = suma_columnas[suma_columnas > min_val].index.tolist()

cols_a_mantener = [col_revista] + cols_palabras_filtradas
df_filtrado = df[cols_a_mantener]

output_file = "One_Hot_Palabras_Revistas_filtrado.xlsx"
df_filtrado.to_excel(output_file, index=False)
print(f"Archivo '{output_file}' generado con suma > {min_val}.")

plt.figure(figsize=(12, 6))
suma_columnas.loc[cols_palabras_filtradas].sort_values(ascending=False).plot(kind="bar")
plt.title(f"Suma de apariciones (> {min_val})")
plt.xlabel("Palabras")
plt.ylabel("Suma de apariciones")
plt.xticks(rotation=90)
plt.tight_layout()
plt.show()
