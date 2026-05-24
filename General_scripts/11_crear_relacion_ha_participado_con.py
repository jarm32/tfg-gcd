import pandas as pd
import os

contador, recontador = 0,0

try:
    # Lee el archivo principal de investigadores
    df_investigadores = pd.read_csv('C:/Users/josep/Downloads/Zona de trabajo/ha_participado_con/Investigadores_internos.csv', delimiter=';', encoding='latin1')
except FileNotFoundError:
    print("Error: El archivo 'Investigadores_internos.csv' no fue encontrado.")
    exit()

# La ruta donde se encuentran todos los archivos .xlsx
data_folder = 'C:/Users/josep/Downloads/Zona de trabajo/ha_participado_con'

# Lista para almacenar los resultados de cada investigador
all_collaborations = []

for index, row in df_investigadores.iterrows():
    acortacion = row['Acortacion']
    
    if pd.notna(acortacion):
        filename = f"ha_participado_en_{acortacion}_FINAL.xlsx"
        file_path = os.path.join(data_folder, filename)
        
        # Obtener el nombre completo del investigador principal de la fila actual
        primary_inv_name = row['Nombre']
        
        if os.path.exists(file_path):
            print(f"--- Extrayendo colaboradores para: {primary_inv_name} ---")
            contador += 1
            
            try:
                # Lee el archivo .xlsx de colaboraciones
                df_produccion = pd.read_excel(file_path)
                
                # Usamos un conjunto para almacenar nombres de colaboradores únicos
                collaborators = set()
                
                # Iterar sobre cada fila del archivo de producción
                for _, prod_row in df_produccion.iterrows():
                    # Obtener la lista de autores de la columna 'AUTORES SIMPLIFICADO'
                    authors_string = prod_row.get('AUTORES SIMPLIFICADO', '')
                    if pd.notna(authors_string) and authors_string:
                        # Dividir la cadena por el delimitador '; ' y añadir a nuestro conjunto
                        authors_list = [author.strip() for author in authors_string.split(';')]
                        for author in authors_list:
                            if author: # Asegura que no se añaden cadenas vacías
                                collaborators.add(author)

                # Eliminar el nombre del investigador principal de la lista de colaboradores
                # Esto se hace comparando la parte principal del nombre.
                # Por ejemplo, "Carmen Alegre Gil" se eliminará de "Alegre Gil, Maria Carmen"
                # Esta es una heurística necesaria por la diferencia de formato.
                primary_inv_match_found = False
                for collaborator_name in list(collaborators):
                    # Identificar al investigador principal buscando su apellido en el nombre de la colaboración
                    # Se usa una estrategia que busca coincidencias de palabras
                    primary_name_parts = primary_inv_name.split()
                    if len(primary_name_parts) >= 2:
                        last_name = primary_name_parts[-1]
                        second_to_last_name = primary_name_parts[-2]
                        
                        if second_to_last_name.lower() in collaborator_name.lower() and last_name.lower() in collaborator_name.lower():
                            collaborators.remove(collaborator_name)
                            primary_inv_match_found = True
                            break
                            
                # Unir los nombres restantes en una cadena
                other_collaborators_str = "; ".join(sorted(list(collaborators)))
                
                # Añadir la información al listado final
                all_collaborations.append({
                    'nombre': primary_inv_name,
                    'otros inv': other_collaborators_str
                })
                
            except Exception as e:
                print(f"Error al procesar el archivo {filename}: {e}")
        else:
            print(f"Advertencia: El archivo '{filename}' no fue encontrado. Omitiendo.")
            recontador += 1
    else:
        print(f"Advertencia: La fila {index} no tiene un valor en 'Acortacion'. Omitiendo.")
        recontador += 1

# Crear un DataFrame con la lista de resultados
df_final = pd.DataFrame(all_collaborations)

# Guardar el DataFrame en un nuevo archivo CSV
# Se usa 'latin1' para la codificación y ';' como separador para ser consistente
output_path = os.path.join(data_folder, 'ha_participado_con.csv')
df_final.to_csv(output_path, index=False, sep=';', encoding='latin1')

print(f"\n--- Proceso completado. El archivo de colaboraciones ha sido guardado en: {output_path} ---")

print(contador,recontador,contador+recontador)