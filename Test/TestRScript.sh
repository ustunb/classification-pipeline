#!/bin/bash

#Computer parameters
comp_name="masha"
data_name="breastcancer"
w_pos=1.90
xtra_id="TEST"
fold_id="K05N01"
test_path="/weird path (hello) /file.120.log"

home_dir="/home/user/petukhova/Berk_method/"
data_dir="${home_dir}Data/"
base_run_dir="${home_dir}Run/"
log_dir="${home_dir}Logs/"
test_dir="${home_dir}Test/"
cd "${test_dir}"

#create run name
train_name="${data_name}_F_${fold_id}"
weight_name=$(printf 'pos_%1.9f' ${w_pos})
run_name="${train_name}_L_${hcon_id}_X_${xtra_id}_{weight_name}"

#create run directory
run_dir="${base_run_dir}{train_name}/"
mkdir -p "${run_dir}"

#setup run log
now=$(date +"%m_%d_%Y_%H_%M_%S")
run_log="${run_name}""_""${now}"".log"

Rscript "Test.R" "${comp_name}" "${train_name}" "${run_name}" "${test_path}" "${run_log}"

exit