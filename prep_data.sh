#!/bin/bash

set -euxo pipefail

./01_download_rt.sh
Rscript --no-save --no-restore 02_clean_merge.R
./03_upload_to_aws.sh
