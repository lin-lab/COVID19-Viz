#!/bin/bash

base_url="https://hsph-covid-study.s3.us-east-2.amazonaws.com/Rt-values"
out_dir=raw_data

if [ ! -d ${out_dir} ]
then
  mkdir -v ${out_dir}
fi

for ext in county state global
do
    filename=jhu_${ext}_${file_ending}
    out_zip=${out_dir}/${filename}.zip
    url=${base_url}/${filename}.zip
    wget --output-document=$out_zip $url && unzip -o $out_zip -d ${out_dir} && rm -f $out_zip
done
