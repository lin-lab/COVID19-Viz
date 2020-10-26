#!/bin/bash

set -euxo pipefail
s3_dir="s3://hsph-covid-study/website_files/"
clean_dir=clean_data

zip - ${clean_dir}/rt_table_export.csv > ${clean_dir}/rt_table_export.csv.zip 
aws s3 cp ${clean_dir}/rt_table_export.csv.zip ${s3_dir}
aws s3 cp ${clean_dir}/rt_long_all.rds ${s3_dir}
aws s3 cp ${clean_dir}/sf_all.rds ${s3_dir}
aws s3 cp ${clean_dir}/names_list.rds ${s3_dir}
aws s3 cp ${clean_dir}/state_centers.rds ${s3_dir}
