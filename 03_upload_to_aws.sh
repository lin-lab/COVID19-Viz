#!/bin/bash

set -euxo pipefail
s3_dir="s3://hsph-covid-study/website_files_pois/"
clean_dir=clean_data_pois

# upload rt table zipped to aws
zip - ${clean_dir}/rt_table_export.csv > ${clean_dir}/rt_table_export.csv.zip 
aws s3 cp ${clean_dir}/rt_table_export.csv.zip ${s3_dir}

# upload all rds files to aws
find ${clean_dir} -type f -name "*.rds" -exec aws s3 cp {} ${s3_dir} \;
