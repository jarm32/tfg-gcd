import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap
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
path_jerMan = "C:/Code/tfg-gcd/_Clusterization/Sin_Locl/Concs/Clusters_resultado_JerCos8.xlsx"
path_umap   = "C:/Code/tfg-gcd/_Clusterization/Sin_Locl/Concs/Clusters_resultado_JerCosUMAP.xlsx"

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
# Dibujar heatmap con paleta personalizada
# ---------------------------------------------
cmap_personalizado = LinearSegmentedColormap.from_list(
    "azul_rojo_solapamiento",
    ["#111184", "#EE6666"]
)

plt.figure(figsize=(9, 6.5))
im = plt.imshow(overlap, cmap=cmap_personalizado, aspect="auto")

plt.xticks(range(len(columns)), columns, rotation=45, ha="right")
plt.yticks(range(len(index)), index)

plt.xlabel("Clusters tras aplicar UMAP + coseno + jerárquico")
plt.ylabel("Clusters sin UMAP (coseno + jerárquico)")

plt.colorbar(im, label="Número de investigadores compartidos")

plt.title(
    "Solapamiento de investigadores entre modelos\n"
    "Coseno + jerárquico vs. Coseno + UMAP + jerárquico"
)

# Añadir valores dentro de cada celda
for i in range(len(index)):
    for j in range(len(columns)):
        valor = overlap.iloc[i, j]
        plt.text(
            j, i, str(valor),
            ha="center",
            va="center",
            fontsize=9,
            color="white"
        )

plt.tight_layout()
plt.savefig(
    "solapamiento_investigadores_jerCos_vs_jerCosUMAP.png",
    dpi=300,
    bbox_inches="tight"
)
plt.show()
