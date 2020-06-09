#!/bin/bash

base_url=https://raw.githubusercontent.com/lin-lab/COVID19-Rt/master/initial_estimates/

wget -O jhu_county_rt.tsv ${base_url}/jhu_county_rt.tsv
wget -O jhu_state_rt.tsv ${base_url}/jhu_state_rt.tsv
wget -O jhu_global_rt.tsv ${base_url}/jhu_global_rt.tsv
