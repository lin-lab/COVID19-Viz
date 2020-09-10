#!/bin/bash

./01_download_rt.sh
Rscript --no-save --no-restore 02_clean_merge.R

# zip data for github
zip clean_data/rt_table_export.csv.zip clean_data/rt_table_export.csv
