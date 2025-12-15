import pandas as pd

primer_csv = "D:/Code/tfg-gcd/_Clusterization/Sin_Locl/Investigadores y keywords/Embeddings_keywords_sinlocl.csv"
segundo_archivo = "D:/Code/tfg-gcd/_Clusterization/Sin_Locl/Investigadores y keywords/Matriz_Investigadores_Keywords.xlsx"
salida_csv = "D:/Code/tfg-gcd/_Clusterization/Sin_Locl/Investigadores y keywords/Matriz_Investigadores_Keywords_sinlocl.xlsx"

df1 = pd.read_csv(primer_csv, header=None)
keywords_embeddings = set(df1.iloc[:, 0].astype(str))

df2 = pd.read_excel(segundo_archivo)

primera_col = df2.columns[0]

columnas_validas = [primera_col] + [
    c for c in df2.columns[1:]
    if str(c) in keywords_embeddings
]

df2_filtrado = df2[columnas_validas]

df2_filtrado.to_excel(salida_csv, index=False)