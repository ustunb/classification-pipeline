source "LocalVariables.sh"
run_log="${log_dir}/SetupPipeline.log"
Rscript "${home_dir}SetupPipeline.R" "${comp_name}" "${run_log}"
exit