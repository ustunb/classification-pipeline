##### main parameters
comp_name="berkmac"
data_name="breastcancer"
fold_id="K05N01"

##### setup directories
case ${comp_name} in
    berkmac)
        home_dir="/Users/berk/Desktop/Dropbox (MIT)/Research/ClassificationPipeline/"
        n_cores=1
    ;;
    svante)        
        home_dir="/home/ustunb/ClassificationPipeline/"
    ;;
    umisr)
        home_dir="/home/ustunb/ClassificationPipeline/"
    ;;
    masha)
        home_dir="/home/user/petukhova/Berk_method/"
    ;;
esac
R_dir="${home_dir}R/"
log_dir="${home_dir}Logs/"
mkdir -p "${log_dir}"


##### setup file names
now=$(date +"%m_%d_%Y_%H_%M_%S")
train_name="${data_name}_F_${fold_id}"
save_name="${train_name}_processed.RData"
log_name="${train_name}_ProcessResults_${now}.log"

##### run results processing script
echo "Running ProcessResults in R"
cd "${home_dir}"
Rscript "${R_dir}ProcessResults.R" "${comp_name}" "${train_name}" "${save_name}" "${log_name}"

exit