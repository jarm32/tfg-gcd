import pandas as pd
import unicodedata
import re
import os

#############################
# Funciones auxiliares
#############################

def normalizar(texto):
    texto = ''.join(
        c for c in unicodedata.normalize('NFD', str(texto))
        if unicodedata.category(c) != 'Mn'
    )
    return texto.lower().strip()

def a_ascii(texto):
    if pd.isna(texto):
        return ''
    texto = ''.join(
        c for c in unicodedata.normalize('NFD', str(texto))
        if unicodedata.category(c) != 'Mn'
    )
    return texto.encode('ascii', 'ignore').decode('ascii')

def simplificar_autores(texto):
    if pd.isna(texto):
        return ''
    
    autores = texto.split(';')
    autores_limpios = []
    
    for autor in autores:
        autor = autor.strip()
        autor = re.sub(r'\(Investigador principal \(IP\)\)', '', autor)
        autor = re.sub(r'\(Investigador/a\)', '', autor)
        autor = re.sub(r'\(Becario/a\)', '', autor)
        autor = autor.strip().title()
        autores_limpios.append(autor)
    
    return '; '.join(autores_limpios)

def extraer_ip(autores):
    if pd.isna(autores):
        return None
    patron = r'(?:^|[; ])\s*([^;]*?)\s*\(Investigador principal \(IP\)\)'
    match = re.search(patron, autores)
    if match:
        return match.group(1).strip()
    return None

#############################
# Preparación de rutas y lista de archivos
#############################

# Ruta general donde están los archivos originales
ruta_origen = 'C:/Users/josep/Downloads/ZonaT/Proyectos obsoleto/'

# Ruta de salida
ruta_salida = os.path.join(ruta_origen, 'Limpios')
os.makedirs(ruta_salida, exist_ok=True)

# Lista de archivos según la imagen 3
lista_archivos = [
    "Alfredo Peris Manguillot - Universidad Politécnica de Valencia.xlsx",
    "Álvaro Vargas Moreno - Universidad Politécnica de Valencia.xlsx",
    "Ana Martínez Pastor - Universidad Politécnica de Valencia.xlsx",
    "Andres Roger Arnau Notari - Universidad Politécnica de Valencia.xlsx",
    "Antoni López Martínez - Universidad Politécnica de Valencia.xlsx",
    "Antonia Ferrer Sapena - Universidad Politécnica de Valencia.xlsx",
    "Antonio José Guirao Sánchez - Universidad Politécnica de Valencia.xlsx",
    "Carles Bivià Ausina - Universidad Politécnica de Valencia.xlsx",
    "Carles Milián Enrique - Universidad Politécnica de Valencia.xlsx",
    "Carlos Mas Arabi - Universidad Politécnica de Valencia.xlsx",
    "Christian Cobollo Gómez - Universidad Politécnica de Valencia.xlsx",
    "David Jornet Casanova - Universidad Politécnica de Valencia.xlsx",
    "Enrique Alfonso Sánchez Pérez - Universidad Politécnica de Valencia.xlsx",
    "Enrique Jorda Mora - Universidad Politécnica de Valencia.xlsx",
    "Félix Martínez Jiménez - Universidad Politécnica de Valencia.xlsx",
    "Francisco De Asís Ródenas Escribá - Universidad Politécnica de Valencia.xlsx",
    "Jesús Rodríguez López - Universidad Politécnica de Valencia.xlsx",
    "José Alberto Conejero Casares - Universidad Politécnica de Valencia.xlsx",
    "José Antonio Bonet Solves - Universidad Politécnica de Valencia.xlsx",
    "Jose Manuel Calabuig Rodriguez - Universidad Politécnica de Valencia.xlsx",
    "José María Isidro San Juan - Universidad Politécnica de Valencia.xlsx",
    "José María Sanchís Llopis - Universidad Politécnica de Valencia.xlsx",
    "Luis Miguel García Raffi - Universidad Politécnica de Valencia.xlsx",
    "Luiza Petrosyan  - Universidad Politécnica de Valencia.xlsx",
    "Mª Minerva Báguena Añó - Universidad Politécnica de Valencia.xlsx",
    "Maria Carmen Alegre Gil - Universidad Politécnica de Valencia.xlsx",
    "María Carmen Pedraza Aguilera - Universidad Politécnica de Valencia.xlsx",
    "María Fernanda Peset Mancebo - Universidad Politécnica de Valencia.xlsx",
    "María José Martínez Uso - Universidad Politécnica de Valencia.xlsx",
    "María Josefa Felipe Román - Universidad Politécnica de Valencia.xlsx",
    "Mario Lázaro Navarro - Universidad Politécnica de Valencia.xlsx",
    "Milagros Arroyo Jordá - Universidad Politécnica de Valencia.xlsx",
    "Nuria Ortigosa Araque - Universidad Politécnica de Valencia.xlsx",
    "Pablo Sevilla Peris - Universidad Politécnica de Valencia.xlsx",
    "Paz Arroyo Jordá - Universidad Politécnica de Valencia.xlsx",
    "Pedro José Fernández De Córdoba Castellá - Universidad Politécnica de Valencia.xlsx",
    "Pedro Tirado Peláez - Universidad Politécnica de Valencia.xlsx",
    "Samuel Morillas Gómez - Universidad Politécnica de Valencia.xlsx",
    "Sergio Hoyas Calvo - Universidad Politécnica de Valencia.xlsx",
    "Tatiana Pedraza Aguilera - Universidad Politécnica de Valencia.xlsx",
    "Vicente Asensio López - Universidad Politécnica de Valencia.xlsx",
    "Vicente Romero García - Universidad Politécnica de Valencia.xlsx",
    "VÍCTOR MANUEL ORTIZ SOTOMAYOR - Universidad Politécnica de Valencia.xlsx"
]

print(len(lista_archivos))

output = [
    "PerisMA","VargasMA","MartinezPA","ArnauNAR","LopezMA","FerrerSA","GuiraoSAJ","BiviaC","MilianEC","MasAC","CobolloGC","JornetCD","SanchezPEA","JordaME","MartinezJF",
    "RodenasEF","RodriguezLJ","ConejeroJA","BonetSJ","CalabuigRJM","IsidroSJJM","SanchisLJM","GarciaRLM","PetrosyanL","BaguenaAM","AlegreGC","PedrazaAMC","PesetMF","MartinezUMJ",
    "FelipeRMJ","LazaroNM","ArroyoJM","OrtigosaAN","SevillaPP","ArroyoJP","FernandezCCPJ","TiradoPP","MorillasGS","HoyasCS","PedrazaAT","AsensioLV","RomeroGV","OrtizSVM"
]

print(len(output))

# Archivo de investigadores
archivo_investigadores = 'C:/Users/josep/Downloads/ZonaT/Investigadores_internos_girado.csv'
df_investigadores = pd.read_csv(archivo_investigadores, sep=';', encoding='latin1')
df_investigadores['Nombre_norm'] = df_investigadores['Nombre'].apply(normalizar)
df_investigadores['Girado_norm'] = df_investigadores['Girado'].apply(normalizar)

#############################
# Bucle de procesamiento
#############################

for archivo_nombre,out in zip(lista_archivos,output):
    archivo_completo = os.path.join(ruta_origen, archivo_nombre)
    print(f"Procesando archivo: {archivo_nombre}")
    df = pd.read_excel(archivo_completo, header=1)

    investigador_archivo = archivo_nombre.split(" - ")[0].strip()
    investigador_archivo_norm = normalizar(investigador_archivo)

    df['INVESTIGADOR PRINCIPAL'] = df['AUTORES'].apply(extraer_ip).apply(a_ascii)
    df['AUTORES_ORIGINAL'] = df['AUTORES']  #ELIMINAR SI UNA SOLA NO ES IP
    df['AUTORES'] = df['AUTORES'].apply(a_ascii)

    df['AUTORES SIMPLIFICADO'] = df['AUTORES'].apply(simplificar_autores).apply(a_ascii)

    def es_ip_correcto(ip_extraido_norm):
        if not ip_extraido_norm:
            return False
        coincidencias = df_investigadores[df_investigadores['Girado_norm'] == normalizar(ip_extraido_norm)]
        for _, fila in coincidencias.iterrows():
            if normalizar(fila['Nombre']) == investigador_archivo_norm:
                return True
        return False
    
    def marcar_ip_si_unico_autor(fila): #ELIMINAR SI UNA SOLA NO ES IP
        autores_raw = fila['AUTORES_ORIGINAL']
        investigador = investigador_archivo_norm

        if pd.isna(autores_raw):
            return False

        autores_lista = [a.strip() for a in autores_raw.split(';') if a.strip()]
        
        if len(autores_lista) == 1:
            autor = autores_lista[0]
            autor = re.sub(r'\(.*?\)', '', autor).strip()
            autor_normalizado = normalizar(autor)
            if ',' in autor_normalizado:
                partes = autor_normalizado.split(',')
                autor_normalizado = f"{partes[1].strip()} {partes[0].strip()}"
            if autor_normalizado == investigador:
                return True

        ip_extraido = fila['INVESTIGADOR PRINCIPAL']
        return es_ip_correcto(ip_extraido)

    #df['Es_IP_Principal'] = df['INVESTIGADOR PRINCIPAL'].apply(es_ip_correcto)

    df['Es_IP_Principal'] = df.apply(marcar_ip_si_unico_autor, axis=1) #ELIMINAR SI UNA SOLA NO ES IP

    # Eliminamos columnas no deseadas si existen
    columnas_a_eliminar = [
        'OPENACCESS', 'TIPO', 'TIPO DE PRODUCCIÓN', 'PALABRAS CLAVE',
        'FUENTE', 'IF SJR', 'CITAS EUROPEPMC', 'CITAS INSPIRE',
        'CITAS SCHOLAR', 'Q SJR', 'AUTORES_ORIGINAL'
    ]
    df = df.drop(columns=[col for col in columnas_a_eliminar if col in df.columns])

    # Formateamos el título: ASCII + mayúsculas
    df['TÍTULO'] = df['TÍTULO'].apply(lambda x: a_ascii(x).title())

    # Formatear fecha
    df['FECHA'] = pd.to_datetime(df['FECHA'], errors='coerce').dt.strftime('%Y/%m/%d')

    # Guardar el archivo limpio
    archivo_salida = os.path.join(ruta_salida, f'ha_participado_en_{out}_FINAL.xlsx')
    df.to_excel(archivo_salida, index=False)

    print(f"Archivo procesado y guardado en: {archivo_salida}")

print("Todos los archivos han sido procesados correctamente.")
