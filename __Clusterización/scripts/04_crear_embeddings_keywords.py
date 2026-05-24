import pandas as pd
from sentence_transformers import SentenceTransformer

# Cargar la matriz binaria investigadores × keywords
df = pd.read_excel("../data/Matriz_Investigadores_Keywords_sinlocl.xlsx", index_col=0).fillna(0)
keywords = df.columns.tolist()

model = SentenceTransformer('paraphrase-multilingual-MiniLM-L12-v2')
embeddings = model.encode(keywords, normalize_embeddings=True)


emb_df = pd.DataFrame(embeddings, index=keywords)
emb_df.to_csv("../data/Embeddings_keywords_sinlocl.csv")
print("Embeddings de keywords guardados en Embeddings_keywords.csv")
