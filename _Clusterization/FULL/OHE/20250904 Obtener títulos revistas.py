import pandas as pd
import json

df = pd.read_csv("Revistas.csv", encoding="latin1", sep=",")

revistas = df.iloc[:, 0].tolist()

with open('Titulos_revistas.json', 'w') as archivo_json:
    json.dump(revistas, archivo_json, indent=4)