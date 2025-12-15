import pandas as pd
import matplotlib.pyplot as plt
import re

# --- normalizador de nombres ---
def normalizar_nombre(x):
    if not isinstance(x, str):
        return ""

    x = x.strip()              # quita espacios iniciales/finales
    x = x.replace("\n", "")    # quita saltos de línea
    x = x.replace("\t", "")    # quita tabs
    x = x.strip()

    # quitar caracteres invisibles Unicode
    x = "".join(ch for ch in x if ch.isprintable())

    # convertir a mayúsculas para evitar diferencias
    x = x.upper()

    # colapsar múltiples espacios internos
    x = re.sub(r"\s+", "", x)

    return x


# --- carga de clusters ---
def cargar_clusters_investigadores(path):
    xl = pd.ExcelFile(path)
    clusters = {}

    for sheet in xl.sheet_names:
        if "keyword" in sheet.lower():
            continue

        df = xl.parse(sheet, header=None)

        col = df.iloc[:, 0]

        investigadores = (
            col.astype(str)
               .map(normalizar_nombre)
               .dropna()
               .tolist()
        )

        # quitar títulos, vacíos y basura
        investigadores = [
            x for x in investigadores
            if x not in ["", "INVESTIGADOR", "NAN"]
        ]

        clusters[sheet] = set(investigadores)

    return clusters


# ---------------------------------------------
# Leer tus archivos
# ---------------------------------------------
path_jerMan = "C:/Code/tfg-gcd/_Clusterization/Conclusiones/Clusters_resultado_JerMan.xlsx"
path_umap   = "C:/Code/tfg-gcd/_Clusterization/Conclusiones/Clusters_resultado_JerCosUMAP.xlsx"

clusters_jerMan = cargar_clusters_investigadores(path_jerMan)
clusters_umap   = cargar_clusters_investigadores(path_umap)


# ---------------------------------------------
# Generar matriz de solapamiento
# ---------------------------------------------
index = list(clusters_jerMan.keys())
columns = list(clusters_umap.keys())

overlap = pd.DataFrame(0, index=index, columns=columns)

for c1 in index:
    for c2 in columns:
        comunes = clusters_jerMan[c1].intersection(clusters_umap[c2])
        overlap.loc[c1, c2] = len(comunes)

print("\n=== MATRIZ DE SOLAPAMIENTO ===\n")
print(overlap)


# ---------------------------------------------
# Dibujar heatmap
# ---------------------------------------------
plt.figure(figsize=(10, 7))
plt.imshow(overlap, cmap="viridis", aspect="auto")

plt.xticks(range(len(columns)), columns, rotation=45)
plt.yticks(range(len(index)), index)

plt.colorbar(label="Investigadores en común")
plt.title("Solapamiento entre Clusters (JerMan vs UMAP)")
plt.tight_layout()
plt.show()
