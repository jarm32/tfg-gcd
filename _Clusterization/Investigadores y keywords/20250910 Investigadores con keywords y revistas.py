import pandas as pd
import glob
import os

df_revistas = pd.read_excel("C:/Code/tfg-gcd/_Clusterization/Investigadores y keywords/One_Hot_Palabras_Revistas_filtrado.xlsx")
df_revistas.set_index(df_revistas.columns[0], inplace=True) # Se accede con df_revistas.index

path = "." 
files = glob.glob(os.path.join(path, "C:/Code/tfg-gcd/_Clusterization/Investigadores y keywords/1ha_publicado_en_*_editoriales.csv")) # Busca todos aquellos archivos con mismo nombre variando la parte de *

investigadores_keywords = {}
relacional = []

for file in files:
    investigador = os.path.basename(file).replace("1ha_publicado_en_", "").replace("_editoriales.csv", "")
    df_pub = pd.read_csv(file, encoding="latin-1")
    revistas = df_pub["journal"].dropna().unique()
    
    keywords_vector = pd.Series(0, index=df_revistas.columns)
    
    for revista in revistas:
        if revista in df_revistas.index:
            kws = df_revistas.loc[revista]
            keywords_vector = keywords_vector | kws
            for kw in kws[kws == 1].index:
                relacional.append([investigador, revista, kw])
    
    investigadores_keywords[investigador] = keywords_vector

df_investigadores = pd.DataFrame.from_dict(investigadores_keywords, orient="index")
df_investigadores.index.name = "Investigador"
df_investigadores.to_excel("C:/Code/tfg-gcd/_Clusterization/Investigadores y keywords/Matriz_Investigadores_Keywords.xlsx")

df_relacional = pd.DataFrame(relacional, columns=["Investigador", "Revista", "Keyword"])
df_relacional.to_excel("C:/Code/tfg-gcd/_Clusterization/Investigadores y keywords/Trazabilidad_Keywords_Revistas.xlsx", index=False)

print("Matrices guardadas")