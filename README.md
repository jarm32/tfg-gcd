# TFG-GCD: Visualización y agrupación semántica de investigadores del IUMPA

Repositorio asociado al Trabajo Fin de Grado **Diseño de una base de datos para la visualización y agrupación semántica de los investigadores del Instituto Universitario de Matemática Pura y Aplicada**.

El proyecto tiene como objetivo construir una base de datos sobre la actividad investigadora del IUMPA, desarrollar una aplicación interactiva en Shiny para visualizar relaciones mediante grafos y aplicar técnicas de agrupación semántica para explorar afinidades temáticas entre investigadores.

## Estructura del repositorio

```text
.
├── __Interface/
├── __Clusterización/
├── Datasets/
├── General_scripts/
├── .gitignore
└── README.md
```

## Contenido

- `__Interface/`: contiene la aplicación Shiny final y los ficheros mínimos necesarios para ejecutarla. El archivo `Investigadores_internos.csv` incluido en esta carpeta es una versión reducida, con solo las columnas necesarias para la interfaz. La aplicación se ejecuta mediante `app.R`.
- `__Clusterización/`: contiene el código, datos y resultados de la fase de agrupación semántica de investigadores. Incluye scripts del flujo principal y una carpeta `experiments/` con pruebas exploratorias.
- `Datasets/`: contiene la base de datos final organizada en nodos, relaciones y resultados de análisis exploratorio. En esta carpeta, `Nodos/Investigadores_internos.csv` conserva la versión completa del nodo de investigadores.
- `General_scripts/`: contiene scripts generales utilizados durante la construcción, limpieza y preparación de la base de datos, así como versiones separadas de los grafos antes de su integración en la aplicación final.

## Aplicación Shiny

La aplicación final se encuentra en la carpeta `__Interface/`. Para ejecutarla en local, ejecutar el script `app.R`.

La aplicación permite consultar distintos grafos relacionados con publicaciones, proyectos y colaboraciones entre investigadores del IUMPA.

## Clusterización semántica

La carpeta `__Clusterización/` recoge el flujo utilizado para construir perfiles temáticos de investigadores a partir de las revistas en las que han publicado y de las palabras clave asociadas a dichas revistas.

El flujo principal se encuentra en `__Clusterización/scripts/`. La carpeta `experiments/` contiene scripts de pruebas exploratorias; se conservan como trazabilidad metodológica, pero no forman parte del pipeline principal y pueden requerir adaptación para ejecutarse directamente.

## Datos

Los datos incluidos corresponden a la versión final utilizada en la memoria. Se han eliminado ficheros brutos, versiones obsoletas, duplicados y archivos intermedios no necesarios para comprender el flujo principal del proyecto.

La estructura general diferencia entre:

- `Datasets/`: base de datos final completa del proyecto.
- `__Interface/`: datos mínimos necesarios para la ejecución de la aplicación.
- `__Clusterización/data/`: datos necesarios para reproducir la fase final del análisis semántico.

## Nota

El repositorio no pretende recoger todo el histórico de trabajo, sino una versión limpia y organizada del código, los datos finales y los resultados principales desarrollados durante el TFG. El objetivo es facilitar la consulta del proyecto y la reproducción del flujo principal descrito en la memoria, sin incluir versiones obsoletas, pruebas descartadas o ficheros intermedios que no aportan al resultado final.