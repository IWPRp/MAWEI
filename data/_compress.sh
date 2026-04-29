#!/bin/bash

# chmod +x _compress.sh
# ./_compress.sh

# only compress bulky static files (EIA national datasets, Census)
# small/dynamic project-specific CSVs stay uncompressed

UNCOMPRESSED_DIR="all_csvs"

COMPRESS_FILES=(
  "cc-est2024-agesex-all.csv"
  "eia860_3_1_Generator_Y2020_operable.csv"
  "eia860_3_1_Generator_Y2021_operable.csv"
  "eia860_3_1_Generator_Y2022_operable.csv"
  "eia860_3_1_Generator_Y2023_operable.csv"
  "eia860_3_1_Generator_Y2024_operable.csv"
  "eia860_3_3_Solar_Y2024_operable.csv"
  "eia923_Schedule_2_3_4_5_M_12_2020_Final_pg1.csv"
  "eia923_Schedule_2_3_4_5_M_12_2021_Final_pg1.csv"
  "eia923_Schedule_2_3_4_5_M_12_2022_Final_pg1.csv"
  "eia923_Schedule_2_3_4_5_M_12_2023_Final_pg1.csv"
  "eia923_Schedule_2_3_4_5_M_12_2024_Final_pg1.csv"
  "eia_seds_Complete_seds_2024_update.csv"
)

for f in "${COMPRESS_FILES[@]}"; do
  if [ -f "$UNCOMPRESSED_DIR/$f" ]; then
    echo "Compressing $f..."
    gzip -k "$UNCOMPRESSED_DIR/$f"
    mv "$UNCOMPRESSED_DIR/${f}.gz" .
  else
    echo "SKIPPING (not found): $UNCOMPRESSED_DIR/$f"
  fi
done

# copy small/dynamic CSVs directly (no compression)
for f in "$UNCOMPRESSED_DIR"/*.csv; do
  basename="$(basename "$f")"
  # skip if it's one of the compress files
  skip=false
  for cf in "${COMPRESS_FILES[@]}"; do
    if [ "$basename" = "$cf" ]; then skip=true; break; fi
  done
  if [ "$skip" = false ]; then
    echo "Copying $basename..."
    cp "$f" .
  fi
done

echo ""
echo "DONE. Compressed files:"
ls -lh *.csv.gz 2>/dev/null
echo ""
echo "Plain CSV files:"
ls -lh *.csv 2>/dev/null
