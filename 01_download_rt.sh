#!/bin/bash

base_url=https://raw.githubusercontent.com/lin-lab/COVID19-Rt/master/initial_estimates/
out_dir=raw_data

if [ ! -d ${out_dir} ]
then
  mkdir -v ${out_dir}
fi

for ext in county state global
do
    filename=jhu_${ext}_rt.tsv
    out_zip=${out_dir}/${filename}.zip
    url=${base_url}/${filename}.zip
    wget --output-document=$out_zip $url && unzip -o $out_zip -d ${out_dir} && rm -f $out_zip
done
