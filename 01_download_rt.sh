#!/bin/bash

base_url="https://raw.githubusercontent.com/lin-lab/COVID19-Rt/pois_glm_rt/pois_reg_estimates"
file_ending="rt_case_death_rate.csv"

out_dir=raw_data

if [ ! -d ${out_dir} ]
then
  mkdir -v ${out_dir}
fi

wget -O ${out_dir}/jhu_county_${file_ending} ${base_url}/jhu_county_${file_ending}
wget -O ${out_dir}/jhu_state_${file_ending} ${base_url}/jhu_state_${file_ending}
wget -O ${out_dir}/jhu_global_${file_ending} ${base_url}/jhu_global_${file_ending}
wget -O ${out_dir}/jhu_subnational_${file_ending} ${base_url}/jhu_subnational_${file_ending}
