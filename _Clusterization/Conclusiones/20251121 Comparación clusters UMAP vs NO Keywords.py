import pandas as pd
import matplotlib.pyplot as plt

# ------------------------------------------------------------
# 1. Cargar keywords por cluster (tu estructura real)
# ------------------------------------------------------------

def cargar_clusters_keywords(path):
    xl = pd.ExcelFile(path)
    clusters_keywords = {}

    for sheet in xl.sheet_names:
        if "_keywords" in sheet.lower():
            df = xl.parse(sheet)

            # keyword = columna 0, frecuencia = columna 1
            palabras = df.iloc[:, 0].dropna().astype(str).tolist()

            cluster_name = sheet.replace("_keywords", "").strip()
            clusters_keywords[cluster_name] = set(palabras)

    return clusters_keywords


# ------------------------------------------------------------
# 2. Cargar ficheros
# ------------------------------------------------------------

path_jerMan = "C:/Code/tfg-gcd/_Clusterization/Conclusiones/Clusters_resultado_JerMan.xlsx"
path_umap   = "C:/Code/tfg-gcd/_Clusterization/Conclusiones/Clusters_resultado_JerCosUMAP.xlsx"

keywords_jerMan = cargar_clusters_keywords(path_jerMan)
keywords_umap   = cargar_clusters_keywords(path_umap)

clusters_jerMan_names = list(keywords_jerMan.keys())
clusters_umap_names   = list(keywords_umap.keys())


# ------------------------------------------------------------
# 3. MATRIZ DE COINCIDENCIA (CONTADOR)
# ------------------------------------------------------------

overlap_kw_count = pd.DataFrame(0, index=clusters_jerMan_names, columns=clusters_umap_names)

for c1 in clusters_jerMan_names:
    for c2 in clusters_umap_names:
        inter = keywords_jerMan[c1].intersection(keywords_umap[c2])
        overlap_kw_count.loc[c1, c2] = len(inter)

print("\n=== MATRIZ DE COINCIDENCIA DE KEYWORDS (contador) ===\n")
print(overlap_kw_count)


# ------------------------------------------------------------
# 4. HEATMAP DEL CONTADOR
# ------------------------------------------------------------

plt.figure(figsize=(10, 7))
plt.imshow(overlap_kw_count, cmap="viridis", aspect="auto")

plt.xticks(range(len(clusters_umap_names)), clusters_umap_names, rotation=45)
plt.yticks(range(len(clusters_jerMan_names)), clusters_jerMan_names)

plt.colorbar(label="Número de keywords compartidas")
plt.title("Solapamiento de Keywords entre Clusters (JerMan vs UMAP)")
plt.tight_layout()
plt.show()


# ------------------------------------------------------------
# 5. TOP keywords de cada cluster
# ------------------------------------------------------------

def mostrar_top_keywords(path, modelo_name):
    xl = pd.ExcelFile(path)
    print(f"\n\n=== TOP KEYWORDS DE {modelo_name} ===\n")

    for sheet in xl.sheet_names:
        if "_keywords" in sheet.lower():
            df = xl.parse(sheet)

            # keyword – frecuencia
            top = df.iloc[:, :2].dropna().sort_values(df.columns[1], ascending=False)

            print(f"\n--- {sheet.replace('_keywords', '')} ---")
            print(top.head(10).to_string(index=False))  # top 10 keywords


# Mostrar tops
mostrar_top_keywords(path_jerMan, "JerMan (sin UMAP)")
mostrar_top_keywords(path_umap, "UMAP")
