import csv

input_file = "C:/Users/josep/Documents/Zona de trabajo/OHE/Revistas.csv"
output_file = "C:/Users/josep/Documents/Zona de trabajo/OHE/Revistas_arreglado.csv"

with open(input_file, "r", encoding="latin-1") as f:
    lines = f.readlines()

col_counts = [len(line.strip().split(",")) for line in lines]
target_cols = max(set(col_counts), key=col_counts.count)

with open(output_file, "w", encoding="latin-1", newline="") as f_out:
    writer = csv.writer(f_out, quoting=csv.QUOTE_ALL)

    for line in lines:
        parts = [p.strip() for p in line.strip().split(",")]

        if len(parts) > target_cols:
            fixed = parts[:target_cols-1]
            merged = ",".join(parts[target_cols-1:])
            fixed.append(merged)
            writer.writerow(fixed)
        else:
            writer.writerow(parts)

print(f"CSV guardado en {output_file}")

