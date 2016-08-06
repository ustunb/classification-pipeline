comp_name="berkmac"

#set directories
case ${comp_name} in
    berkmac)
        home_dir="/Users/berk/Desktop/Dropbox (MIT)/Research/ClassificationPipeline/"
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
data_dir="${home_dir}Data/"
R_dir="${home_dir}R/"
log_dir="${home_dir}Logs/"
base_run_dir="${home_dir}Run/"
mkdir -p "${log_dir}"
mkdir -p "${base_run_dir}"

#make sure you are in the home directory
cd "${home_dir}"