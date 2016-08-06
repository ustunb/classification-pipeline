#computer parameters
data_name="breastcancer"
fold_id="K05N01"
hcon_id="U001"
xtra_id="TEST"

#all positive weights
all_positive_weights=(0.1 0.2 0.25 0.33 0.5 1 2 3 4 5 10)
#note: w_neg=1.0 always

#method choice
R_run_cart="TRUE"
R_run_c50_rule="TRUE"
R_run_c50_tree="TRUE"
R_run_lars_lasso="TRUE"
R_run_lars_ridge="TRUE"
R_run_lars_elasticnet="TRUE"
R_run_randomforest="TRUE"
R_run_sgb="TRUE"
R_run_svm_linear="TRUE"
R_run_svm_rbf="TRUE"

#high level R flags
R_print_flag="TRUE"
R_standardize_flag="FALSE"
R_check_results_flag="TRUE"

#method-specific settings
R_cart_minsplit=(3 5 10 15 20)
R_cart_minbucket=(5 10)
R_cart_cp=(0.0001 0.001 0.01)
R_cart_save_print_models="TRUE"
R_cart_save_debug_models="FALSE"

R_c50_rule_confidence_factor=(0.25)
R_c50_rule_min_cases=(5 10)
R_c50_rule_no_global_pruning="TRUE"
R_c50_rule_save_print_models="TRUE"
R_c50_rule_save_debug_models="FALSE"

R_c50_tree_confidence_factor=(0.25)
R_c50_tree_min_cases=(5 10)
R_c50_tree_no_global_pruning="TRUE"
R_c50_tree_save_print_models="TRUE"
R_c50_tree_save_debug_models="FALSE"

R_lars_lasso_nlambda=500 #must be scalar
R_lars_lasso_standardize="FALSE" #will override standardize flag
R_lars_lasso_save_print_models="TRUE"
R_lars_lasso_save_debug_models="FALSE"

R_lars_ridge_nlambda=500 #must be scalar
R_lars_ridge_standardize="FALSE" #will override standardize flag
R_lars_ridge_save_print_models="TRUE"
R_lars_ridge_save_debug_models="FALSE"

R_lars_elasticnet_alpha_values=(0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9)
R_lars_elasticnet_nlambda=500 #must be scalar
R_lars_elasticnet_standardize="FALSE" #will override standardize flag
R_lars_elasticnet_save_print_models="TRUE"
R_lars_elasticnet_save_debug_models="FALSE"

R_randomforest_sampsize=(0.632 0.4 0.2)
R_randomforest_nodesize=(1 5 10 20)
R_randomforest_save_print_models="TRUE"
R_randomforest_save_debug_models="FALSE"

R_svm_linear_costs=(0.001 0.002 0.005 0.01 0.02 0.05 0.1 0.2 0.5 1 2 5 10)
R_svm_linear_scale="TRUE"
R_svm_linear_save_print_models="TRUE"
R_svm_linear_save_debug_models="FALSE"

R_svm_rbf_costs=(0.001 0.002 0.005 0.01 0.02 0.05 0.1 0.2 0.5 1 2 5 10)
R_svm_rbf_scale="TRUE"
R_svm_rbf_save_print_models="TRUE"
R_svm_rbf_save_debug_models="FALSE"

R_sgb_rbf_costs=(0.001 0.002 0.005 0.01 0.02 0.05 0.1 0.2 0.5 1 2 5 10)
R_sgb_rbf_scale="TRUE"
R_sgb_save_print_models="TRUE"
R_sgb_save_debug_models="FALSE"

############################################################################
#                      Setup directories and functions                   ############################################################################
source "LocalVariables.sh"
train_name="${data_name}_F_${fold_id}"

varsToFiles() {  

  local outDir=$1 preName=$2 name fname rest isArray

  while IFS='=' read -r name rest; do

    # Determine the output filename - the variable name w/o preName.
    fname=${name#$preName}

    # Determine if the variable is an array.
    [[ $(declare -p "$name" | cut -d' ' -f2) == *a* ]] && isArray=1 || isArray=0
    (( isArray )) && name="$name[@]"

    # Output var. value and send to output file.
    echo "${!name}" > "$outDir""_""$fname.setting"

  done < <(shopt -so posix; set | egrep "^${preName}[^ =]*=")

}

############################################################################
#                      Run R for Each Positive Weight ############################################################################
for w_pos in ${all_positive_weights[*]}; do

    weight_name=$(printf 'pos_%1.9f' ${w_pos})
    run_name="${train_name}_L_${hcon_id}_X_${xtra_id}_${weight_name}"

    #run directory
    run_dir="${base_run_dir}${train_name}/"
    mkdir -p "${run_dir}"

    #run log
    now=$(date +"%m_%d_%Y_%H_%M_%S")
    run_log="${run_name}""_""${now}"".log"
    mkdir -p "${log_dir}"

    #append computer-level parameters with CP
    CP_comp_name=${comp_name}
    CP_n_cores=${n_cores}
    CP_data_name=${data_name}
    CP_w_pos=${w_pos}
    CP_fold_id=${fold_id}
    CP_hcon_id=${hcon_id}
    CP_xtra_id=${xtra_id}
    
    #write settings to disk 
    varsToFiles "${run_dir}${run_name}" "CP_"
    varsToFiles "${run_dir}${run_name}" "R_"

    #run try
    echo "Running ClassificationPipeline in R"
    cd "${home_dir}" 

    #R needs to be run from home_dir
    Rscript "${R_dir}ClassificationPipeline.R" "${comp_name}" "${train_name}" "${run_name}" "${run_log}"

    #clean up settings
    echo "Cleaning up setting files"
    rm "${run_dir}${train_name}"*.setting

done

exit