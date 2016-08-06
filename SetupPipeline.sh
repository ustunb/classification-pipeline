comp_name="berkmac"
case ${comp_name} in
    berkmac)
        home_dir="/Users/berk/Desktop/Dropbox (MIT)/Research/ClassificationPipeline/"
    ;;
    equity)        
        home_dir="/home/ustunb/ClassificationPipeline/"
    ;;
    umisr)
        home_dir="/home/ustunb/ClassificationPipeline/"
    ;;
    masha)
        home_dir="/home/user/petukhova/Berk_method/"
    ;;
    ec2)
        home_dir="/home/ubuntu/ClassificationPipeline/"
    ;;
    *)
        home_dir=`pwd`"/"
    ;;
esac
run_log="${home_dir}Logs/setup.log"
Rscript "${home_dir}Setup.R" "${comp_name}" "${run_log}"
exit