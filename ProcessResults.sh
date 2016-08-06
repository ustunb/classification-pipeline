##### main parameters
data_name="breastcancer"
fold_id="K05N01"

##### setup file names
now=$(date +"%m_%d_%Y_%H_%M_%S")
train_name="${data_name}_F_${fold_id}"
save_name="${train_name}_processed.RData"
log_name="${train_name}_ProcessResults_${now}.log"

##### run results processing script
source "LocalVariables.sh"
echo "Running ProcessResults in R"
cd "${home_dir}"
Rscript "${R_dir}ProcessResults.R" "${comp_name}" "${train_name}" "${save_name}" "${log_name}"

exit