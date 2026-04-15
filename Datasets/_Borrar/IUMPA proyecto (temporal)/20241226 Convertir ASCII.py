import pandas as pd
import unicodedata

# Ruta del archivo
input_file = 'datos_finales_Scop_SLJM.csv'
output_file = 'Sanchis Llopis, Jose Maria.csv'

def convert_to_ascii(text):
    """Convierte caracteres no ASCII a sus equivalentes ASCII."""
    if isinstance(text, str):
        return ''.join(
            c for c in unicodedata.normalize('NFKD', text)
            if not unicodedata.combining(c)
        )
    return text

def convert_csv_to_ascii(input_path, output_path):
    """Convierte un archivo CSV a formato ASCII y guarda el resultado."""
    try:
        # Leer el archivo CSV
        df = pd.read_csv(input_path)
        
        # Convertir cada columna al formato ASCII
        for col in df.columns:
            df[col] = df[col].apply(convert_to_ascii)

        # Guardar el resultado en un nuevo archivo CSV
        df.to_csv(output_path, index=False, encoding='utf-8')
        print(f"Archivo convertido y guardado en: {output_path}")
    except Exception as e:
        print(f"Ocurrió un error: {e}")

# Ejecutar la función
convert_csv_to_ascii(input_file, output_file)