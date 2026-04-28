#!/bin/bash

# chmmod +x _compress.sh
# ./_compress.sh

# Compress CSV files in the uncompressed_csvs directory and keep the original files
# Move the compressed ones to the directory where script is executed

# for f in uncompressed_csvs/*.csv; do gzip -k "$f"; done 
for f in uncompressed_csvs/*.csv; do
    echo "Compressing $f..."
    gzip -k "$f"
    mv "${f}.gz" .
done

echo "DONE COMPRESSING. Files and their line counts:" 

# ls *.csv.gz
wc -l *.csv.gz | sort -n