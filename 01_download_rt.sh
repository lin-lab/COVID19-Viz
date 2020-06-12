#!/bin/bash

base_url=https://raw.githubusercontent.com/lin-lab/COVID19-Rt/master/initial_estimates/
out_dir=raw_data

if [ ! -d ${out_dir} ]
then
  mkdir -v ${out_dir}
fi

wget -O ${out_dir}/jhu_county_rt.tsv ${base_url}/jhu_county_rt.tsv
wget -O ${out_dir}/jhu_state_rt.tsv ${base_url}/jhu_state_rt.tsv
wget -O ${out_dir}/jhu_global_rt.tsv ${base_url}/jhu_global_rt.tsv
