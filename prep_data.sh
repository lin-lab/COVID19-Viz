#!/bin/bash

./01_download_rt.sh
Rscript --no-save --no-restore 02_clean_merge.R
