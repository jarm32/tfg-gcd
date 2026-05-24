# Clusterización semántica

Esta carpeta contiene el código y los ficheros principales utilizados para la fase de agrupación semántica de investigadores del TFG.

El objetivo de esta parte del proyecto es construir perfiles temáticos de los investigadores a partir de las revistas en las que han publicado y de las palabras clave asociadas a dichas revistas. A partir de estos perfiles se prueban distintas estrategias de clustering para explorar posibles afinidades temáticas entre investigadores.

## Contenido

- `scripts/`: scripts utilizados para generar las matrices temáticas, calcular embeddings, aplicar clustering y comparar resultados.
- `data/`: matrices y ficheros de entrada necesarios para reproducir la fase final del análisis.
- `results/`: resultados finales obtenidos en las configuraciones seleccionadas, incluyendo asignaciones de clusters, coordenadas UMAP y palabras representativas.
- `experiments/`: scripts utilizados durante la fase exploratoria del análisis semántico. Se conservan como material de trazabilidad metodológica, pero no forman parte del pipeline principal y no se garantiza su ejecución directa sin adaptación, ya que pueden contener rutas de archivos antiguas, nombres de carpetas previos o dependencias asociadas a versiones intermedias del proyecto.

## Flujo general

1. Construcción de la matriz de revistas y palabras clave.
2. Asignación de palabras clave a investigadores según las revistas en las que han publicado.
3. Generación de representaciones semánticas mediante embeddings.
4. Aplicación de métodos de clustering y reducción de dimensionalidad.
5. Comparación e interpretación de los resultados obtenidos.

## Nota sobre los datos

Los ficheros incluidos corresponden a la versión final utilizada en la memoria. No se incluyen pruebas antiguas, versiones intermedias ni ficheros brutos no necesarios para reproducir el análisis descrito.

El flujo reproducible del proyecto se encuentra en `scripts/`, utilizando como entrada los ficheros de `data/` y generando o utilizando las salidas almacenadas en `results/`. La carpeta `experiments/` se incluye únicamente para documentar pruebas descartadas o exploratorias.