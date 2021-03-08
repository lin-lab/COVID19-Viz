#!/bin/bash

set -euxo pipefail

# set Rscript
RSCRIPT=/opt/R/4.0.4/bin/Rscript
if [[ ! -f ${RSCRIPT} ]]
then
    RSCRIPT=/usr/bin/Rscript
fi

./01_download_rt.sh
${RSCRIPT} --no-save --no-restore 02_clean_merge.R
./03_upload_to_aws.sh
