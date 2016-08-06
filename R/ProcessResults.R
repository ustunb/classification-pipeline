#Results Processing and Aggregration Script
#Berk Ustun | ustunb@mit.edu | 2016
#
#This script will aggregate all results files for a particular train_name
#compute accuracy related metrics and save all processed results as an
#RData file in the directory:${home_dir}/Run/${train_name}/${save_name}
#
#Note: this file must be run from home_dir to work correctly

##### parse command line arguments and setup directories #####
cat(sprintf("Opened ProcessResults.R in directory: %s\n", getwd()))
start_time = proc.time();
print_flag = TRUE;
log_name = NA;
command_line_flag = FALSE;
source("StartUp.R");

args = commandArgs(TRUE);
if (length(args) >= 4){
    command_line_flag = TRUE;
    comp_name = args[1];
    train_name = args[2];
    save_name = args[3];
    log_name = args[4];
} else {
    setwd("/Users/berk/Desktop/Dropbox (MIT)/Research/ClassificationPipeline/")
    comp_name = "berkmac"
    train_name = paste0("breastcancer", "_F_", "K05N01");
    save_name = paste0(train_name, "_processed.RData");
}
rm(args);

#set directories
source("StartUp.R");
fs = get.directories(comp_name);
home_dir = fs$home_dir;
R_dir = fs$R_dir;
results_dir = fs$results_dir;
data_dir = fs$data_dir;
raw_data_dir = fs$raw_data_dir;
log_dir = fs$log_dir;
lib_dir = fs$lib_dir;
rm(fs);

#libraries
set.library(lib_dir, show_flag = FALSE);

#logging
log_file = set.log(log_dir = log_dir, log_name = log_name)

#results
run_dir = paste0(results_dir, train_name, "/")
save_file_name = paste0(run_dir, save_name);

print.to.console(sprintf("comp_name: %s", comp_name))
print.to.console(sprintf("train_name: %s", train_name))
print.to.console(sprintf("log file: %s", log_file))
print.to.console(sprintf("reading all files in: %s", run_dir))
print.to.console(sprintf("saving aggregated results to: %s", save_file_name))

#packages
suppressPackageStartupMessages(library('dplyr', warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE));

##### helper functions ####

merge.results.and.models = function(results_df_list, print_models_list, debug_models_list, print_model_cnt = 0, debug_model_cnt = 0){

    #merge results and renumber all print/model ids sequentially
    results_df = bind_rows(results_df_list)
    results_df = results_df %>% mutate(id = row_number(),
                                       print_model_id = sprintf("M%08d", print_model_cnt + id),
                                       debug_model_id = sprintf("M%08d", debug_model_cnt + id))

    #rename print_models and debug_models sequentially
    for (i in 1:length(results_df_list)){
        names(print_models_list[[i]]) = sprintf("M%08d", print_model_cnt + seq(1, length(print_models_list[[i]])));
        names(debug_models_list[[i]]) = sprintf("M%08d", debug_model_cnt + seq(1, length(debug_models_list[[i]])));
        print_model_cnt = print_model_cnt + length(print_models_list[[i]]);
        debug_model_cnt = debug_model_cnt + length(debug_models_list[[i]]);
    }
    names(print_models_list) = NULL #need to remove names of methods from list before unlist
    names(debug_models_list) = NULL #need to remove names of methods from list before unlist
    print_models = unlist(print_models_list, recursive = FALSE, use.names = TRUE);
    debug_models = unlist(debug_models_list, recursive = FALSE, use.names = TRUE);

    #return list with merged entites
    merged = list()
    merged$results_df = results_df;
    merged$print_models = print_models;
    merged$debug_models = debug_models;
    merged$print_model_cnt = print_model_cnt;
    merged$debug_model_cnt = debug_model_cnt;

    #assert output condition
    return(merged);
}

create.count.table = function(X, Y, X_test, Y_test, folds){

    #check preconditions
    stopifnot(nrow(X) == nrow(Y))
    stopifnot(nrow(X) == length(folds))
    stopifnot(ncol(X) == ncol(X_test))
    stopifnot(nrow(X_test) == nrow(Y_test))

    #create count table
    n_folds = max(folds);
    count_table = data.frame(fold = seq(0, n_folds));
    count_table$P = ncol(X);
    count_table$N = nrow(X);

    count_table$N_pos = sum(Y==1);
    count_table$N_neg = sum(Y!=1);

    count_table$N_test = nrow(X_test);
    count_table$N_test_pos = sum(Y_test==1);
    count_table$N_test_neg = sum(Y_test!=1);

    count_table$N_all = count_table$N + count_table$N_test;
    count_table$N_all_pos = count_table$N_pos + count_table$N_test_pos;
    count_table$N_all_neg = count_table$N_neg + count_table$N_test_neg;

    for (k in seq(0, n_folds)){
        valid_ind = folds == k;
        train_ind = !valid_ind;
        count_table[k+1, "N_train"] = sum(train_ind);
        count_table[k+1, "N_train_pos"] = sum(Y[train_ind]==1);
        count_table[k+1, "N_train_neg"] = sum(Y[train_ind]!=1);

        count_table[k+1, "N_valid"] = sum(valid_ind);
        count_table[k+1, "N_valid_pos"] = sum(Y[valid_ind]==1);
        count_table[k+1, "N_valid_neg"] = sum(Y[valid_ind]!=1);
    }

    #total = positive + negative for all sets
    stopifnot(all(count_table$N==count_table$N_pos+count_table$N_neg));
    stopifnot(all(count_table$N_test==count_table$N_test_pos+count_table$N_test_neg));
    stopifnot(all(count_table$N_all==count_table$N_test_all+count_table$N_test_all));
    stopifnot(all(count_table$N_train==count_table$N_train_pos+count_table$N_train_neg));
    stopifnot(all(count_table$N_valid==count_table$N_valid_pos+count_table$N_valid_neg));

    #no validation for full model
    full_model_idx = count_table$fold == 0;
    stopifnot(count_table$N_valid[full_model_idx]==0);
    stopifnot(count_table$N_valid_pos[full_model_idx]==0);
    stopifnot(count_table$N_valid_neg[full_model_idx]==0);

    #total validation for fold models == total training points for full model
    fold_model_idx = !full_model_idx
    stopifnot(count_table$N_train[full_model_idx]==sum(count_table$N_valid[fold_model_idx]))
    stopifnot(count_table$N_train_pos[full_model_idx]==sum(count_table$N_valid_pos[fold_model_idx]))
    stopifnot(count_table$N_train_neg[full_model_idx]==sum(count_table$N_valid_neg[fold_model_idx]))

    return(count_table);
}

check.results.rep =  function(results_df, count_table){

    #check that the total number of runs = n_parameter_combinations x n_folds
    n_folds = max(results_df$fold)
    n_runs = results_df %>% select(fold, starts_with("parameter_")) %>% distinct() %>% nrow()
    stopifnot((n_runs %% (n_folds+1))==0)

    #check run time metrics
    stopifnot(results_df$runtime >= 0.0)

    #join results_df onto count_table
    results_df = left_join(results_df, count_table, by = "fold")

    #check model size metrics
    chk_df = results_df %>% filter(method_name %in% c("lars_lasso", "lars_ridge", "lars_elasticnet", "svm_linear"))
    stopifnot(all(chk_df$model_size >= 0));
    stopifnot(all(chk_df$model_size <= chk_df$P));

    chk_df = results_df %>% filter(method_name %in% c("randomforest", "sgb", "svm_rbf"))
    stopifnot(all(is.na(chk_df$model_size)));

    #check accuracy metrics for fold models

    #N_train_pos == TP_train + FN_train
    chk =  results_df %>% mutate(chk = N_train_pos == (train_true_positives + train_false_negatives)) %>% select(chk)
    stopifnot(all(chk$chk))

    #N_train_neg == TN_train + FP_train
    chk =  results_df %>% mutate(chk = N_train_neg == (train_true_negatives + train_false_positives)) %>% select(chk)
    stopifnot(all(chk$chk))

    #N_test_pos == TP_test + FN_test
    chk =  results_df %>% mutate(chk = N_test_pos == (test_true_positives + test_false_negatives)) %>% select(chk)
    stopifnot(all(chk$chk))

    #N_test_neg == TN_test + FP_test
    chk =  results_df %>% mutate(chk = N_test_neg == (test_true_negatives + test_false_positives)) %>% select(chk)
    stopifnot(all(chk$chk))

    #N_valid_pos == TP_valid + FN_valid
    chk =  results_df %>% filter(fold>0) %>% mutate(chk = N_valid_pos == (valid_true_positives + valid_false_negatives)) %>% select(chk)
    stopifnot(all(chk$chk))

    #N_valid_neg == TN_valid + FP_valid
    chk =  results_df %>% filter(fold>0) %>% mutate(chk = N_valid_neg == (valid_true_negatives + valid_false_positives)) %>% select(chk)
    stopifnot(all(chk$chk))

    #make sure no accuracy metrics for fold entries are NA
    chk = !(results_df %>% filter(fold > 0) %>% select(starts_with("train"), starts_with("valid"), starts_with("test")) %>% is.na() %>% any())
    stopifnot(chk)

    #make sure accuracy metrics on train/test set for full models are 1) not NA, and 2) are > 0
    chk_table = results_df %>% filter(fold == 0) %>% select(starts_with("train"), starts_with("test"))
    stopifnot(all(!is.na(chk_table)))
    stopifnot(all(chk_table >= 0))

    #make sure accuracy metrics on validation set for full models are NA
    stopifnot(results_df %>% filter(fold == 0) %>% select(starts_with("valid")) %>% is.na() %>% all())

    #true positives <= all positives
    chk = results_df %>% mutate(chk = (train_true_positives <= N_train_pos)) %>% select(chk); stopifnot(all(chk$chk))
    chk = results_df %>% mutate(chk = (test_true_positives <= N_test_pos)) %>% select(chk); stopifnot(all(chk$chk))
    chk = results_df %>% filter(fold > 0) %>% mutate(chk = (valid_true_positives <= N_valid_pos)) %>% select(chk); stopifnot(all(chk$chk))

    #false negatives <= all positives
    chk = results_df %>% mutate(chk = (train_false_negatives <= N_train_pos)) %>% select(chk); stopifnot(all(chk$chk))
    chk = results_df %>% mutate(chk = (test_false_negatives <= N_test_pos)) %>% select(chk); stopifnot(all(chk$chk))
    chk = results_df %>% filter(fold > 0) %>% mutate(chk = (valid_false_negatives <= N_valid_pos)) %>% select(chk); stopifnot(all(chk$chk))

    #true negatives <= all negatives
    chk = results_df %>% mutate(chk = (train_true_negatives <= N_train_neg)) %>% select(chk); stopifnot(all(chk$chk))
    chk = results_df %>% mutate(chk = (test_true_negatives <= N_test_neg)) %>% select(chk); stopifnot(all(chk$chk))
    chk = results_df %>% filter(fold > 0) %>% mutate(chk = (valid_true_negatives <= N_valid_neg)) %>% select(chk); stopifnot(all(chk$chk))

    #false positivies <= all negatives
    chk = results_df %>% mutate(chk = (train_false_positives <= N_train_neg)) %>% select(chk); stopifnot(all(chk$chk))
    chk = results_df %>% mutate(chk = (test_false_positives <= N_test_neg)) %>% select(chk); stopifnot(all(chk$chk))
    chk = results_df %>% filter(fold > 0) %>% mutate(chk = (valid_true_negatives <= N_valid_neg)) %>% select(chk); stopifnot(all(chk$chk))

    return(TRUE)
}

#### aggregate results from all files ####

#identify files
raw_results_files = dir(path=run_dir, pattern="*._results.RData", ignore.case=TRUE)
print.to.console(sprintf("script will aggregate and process the following files in directory %s:", run_dir))
for (results_file in raw_results_files){print.to.console(sprintf("-\t\t%s", results_file))};
raw_results_files = paste0(run_dir, raw_results_files);


print.to.console(sprintf("starting to aggregate results files"))

n_files = length(raw_results_files)
all_results = vector("list", n_files)
all_print_models = vector("list", n_files)
all_debug_models = vector("list", n_files)
all_print_model_cnt = 0;
all_debug_model_cnt = 0;

for (i in 1:n_files){

    print.to.console(sprintf("loading %s", raw_results_files[i]))

    #load saved results
    raw_environment = new.env()
    load(raw_results_files[i], raw_environment)
    raw_info = raw_environment$info
    raw_output = raw_environment$output
    rm(raw_environment)

    #create count table
    data_name = raw_info$data_name;
    fold_id = raw_info$fold_id;

    data_file_name = paste0(data_dir, data_name, "_processed.RData");
    data_environment = new.env()
    load(data_file_name, data_environment)
    data = as.list(data_environment)
    rm(data_environment)
    folds = data$cvindices[[substring(fold_id,1,3)]][,as.double(substr(fold_id,5,6))]
    count_table = create.count.table(X = data$X, Y = data$Y, X_test = data$X_test, Y_test = data$Y_test, folds = folds)
    rm(data)

    #add method_name to results_df
    for (raw_method_name in names(raw_output)){
        raw_output[[raw_method_name]]$results_df = raw_output[[raw_method_name]]$results_df %>% mutate(method_name = raw_method_name)
    }

    #consolidate all of the raw output
    raw_output = merge.results.and.models(results_df_list = lapply(raw_output, function(r) r$results_df),
                                          print_models_list = lapply(raw_output, function(r) r$print_models),
                                          debug_models_list = lapply(raw_output, function(r) r$debug_models),
                                          print_model_cnt = all_print_model_cnt,
                                          debug_model_cnt = all_debug_model_cnt)

    #gather print_models and debug models
    all_print_models[[i]] = raw_output$print_models;
    all_debug_models[[i]] = raw_output$debug_models;
    all_print_model_cnt = raw_output$print_model_cnt;
    all_debug_model_cnt = raw_output$debug_model_cnt;

    #augment results_df with count data and other run-level information
    results_df = raw_output$results_df %>%
        select(-id) %>%
        mutate(data_name = raw_info$data_name[[1]],
               fold_id = raw_info$fold_id[[1]],
               xtra_id = raw_info$xtra_id[[1]],
               hcon_id = raw_info$hcon_id[[1]],
               comp_name = raw_info$comp_name[[1]],
               date = raw_info$date[[1]],
               w_pos = raw_info$w_pos[[1]],
               w_neg = 1.0,
               w_pos_norm = 2.0 * w_pos/(w_pos+w_neg),
               w_neg_norm = 2.0 * w_neg/(w_pos+w_neg))

    all_results[[i]] = left_join(x = results_df, y = count_table, by = "fold");
    rm(results_df, raw_output, raw_info);
}
stopifnot(all_debug_model_cnt == sum(sapply(all_results, function(r) nrow(r))));
stopifnot(all_print_model_cnt == sum(sapply(all_results, function(r) nrow(r))));
results_df = bind_rows(all_results);
print_models = unlist(all_print_models, recursive =  FALSE);
debug_models = unlist(all_debug_models, recursive =  FALSE);
rm(all_results, all_print_models, all_debug_models)
print.to.console(sprintf("finished aggregating"))

#### compute accuracy metrics #####
print.to.console(sprintf("computing accuracy metrics"))
results_df = results_df %>% mutate(
    #
    # ACCURACY METRICS
    #
    #error
    #weighted error
    #true positive rate
    #true negative rate
    #false positive rate
    #false negative rate
    #positive likelihood ratio (TPR/FPR)
    #negative likelihood ratio (FNR/TNR)
    #diagnostic odds ratio (LRPos/LRNeg)
    #predicted positive rate (= (TP+FP)/N)
    #predicted negative rate (= (TN+FN)/N)
    #trivial model flag  (= predicted positive rate == 1 or predicted negative rate == 1)
    #positive predictive value (TP/ Predicted Positive
    #negative predictive value (FP/Predicted Negative)
    #false discovery rate (= FP/Predicted Positive)
    #false omission rate (= FN/Predicted Negative)
    #
    #### TRAINING
    #
    train_error = (train_false_negatives + train_false_positives)/N_train,
    train_weighted_error = (w_pos_norm*train_false_negatives + w_neg_norm*train_false_positives)/N_train,
    train_tpr = train_true_positives/N_train_pos,
    train_fpr = train_false_positives/N_train_neg,
    train_fnr = 1 - train_tpr,
    train_tnr = 1 - train_fpr,
    train_lrp = train_tpr/train_fpr,
    train_lrn = train_fnr/train_tnr,
    train_dor = train_lrp/train_lrn,
    train_ppr = (train_true_positives + train_false_positives) / N_train,
    train_npr = (train_true_negatives + train_false_negatives) / N_train,
    train_trivial_flag = ifelse((train_ppr==1.0)|(train_npr==1.0), 1, 0),
    train_ppv = train_true_positives/(train_true_positives + train_false_positives),
    train_npv = train_false_positives/(train_true_negatives + train_false_negatives),
    train_fdr = train_false_positives/(train_true_positives + train_false_positives),
    train_for = train_false_negatives/(train_true_negatives + train_false_negatives),
    #
    #### TESTING
    #
    test_error = ifelse(N_test==0, NA, (test_false_negatives + test_false_positives)/N_test),
    test_weighted_error = ifelse(N_test==0, NA, (w_pos_norm*test_false_negatives + w_neg_norm*test_false_positives)/N_test),
    test_tpr = ifelse(N_test==0, NA, test_true_positives/N_test_pos),
    test_fpr = ifelse(N_test==0, NA, test_false_positives/N_test_neg),
    test_fnr = ifelse(N_test==0, NA, 1 - test_tpr),
    test_tnr = ifelse(N_test==0, NA, 1 - test_fpr),
    test_lrp = ifelse(N_test==0, NA, test_tpr/test_fpr),
    test_lrn = ifelse(N_test==0, NA, test_fnr/test_tnr),
    test_dor = ifelse(N_test==0, NA, test_lrp/test_lrn),
    test_ppr = ifelse(N_test==0, NA, (test_true_positives + test_false_positives) / N_test),
    test_npr = ifelse(N_test==0, NA, (test_true_negatives + test_false_negatives) / N_test),
    test_trivial_flag = ifelse((test_ppr==1.0)|(test_npr==1.0), 1, 0),
    test_ppv = ifelse(N_test==0, NA, test_true_positives/(test_true_positives + test_false_positives)),
    test_npv = ifelse(N_test==0, NA, test_false_positives/(test_true_negatives + test_false_negatives)),
    test_fdr = ifelse(N_test==0, NA, test_false_positives/(test_true_positives + test_false_positives)),
    test_for = ifelse(N_test==0, NA, test_false_negatives/(test_true_negatives + test_false_negatives)),
    #
    #### VALIDATION
    #
    valid_error = ifelse(N_valid==0, NA, (valid_false_negatives + valid_false_positives)/N_valid),
    valid_weighted_error = ifelse(N_valid==0, NA, (w_pos_norm*valid_false_negatives + w_neg_norm*valid_false_positives)/N_valid),
    valid_tpr = ifelse(N_valid==0, NA, valid_true_positives/N_valid_pos),
    valid_fpr = ifelse(N_valid==0, NA, valid_false_positives/N_valid_neg),
    valid_fnr = ifelse(N_valid==0, NA, 1 - valid_tpr),
    valid_tnr = ifelse(N_valid==0, NA, 1 - valid_fpr),
    valid_lrp = ifelse(N_valid==0, NA, valid_tpr/valid_fpr),
    valid_lrn = ifelse(N_valid==0, NA, valid_fnr/valid_tnr),
    valid_dor = ifelse(N_valid==0, NA, valid_lrp/valid_lrn),
    valid_ppr = ifelse(N_valid==0, NA, (valid_true_positives + valid_false_positives) / N_valid),
    valid_npr = ifelse(N_valid==0, NA, (valid_true_negatives + valid_false_negatives) / N_valid),
    valid_trivial_flag = ifelse((valid_ppr==1.0)|(valid_npr==1.0), 1, 0),
    valid_ppv = ifelse(N_valid==0, NA, valid_true_positives/(valid_true_positives + valid_false_positives)),
    valid_npv = ifelse(N_valid==0, NA, valid_false_positives/(valid_true_negatives + valid_false_negatives)),
    valid_fdr = ifelse(N_valid==0, NA, valid_false_positives/(valid_true_positives + valid_false_positives)),
    valid_for = ifelse(N_valid==0, NA, valid_false_negatives/(valid_true_negatives + valid_false_negatives))
)

#rearrange columns so that everything after "fold" column is specific to a given model
results_df = results_df %>% select(data_name, fold_id, xtra_id, hcon_id,
                                   w_pos, w_neg, w_pos_norm, w_neg_norm,
                                   method_name,
                                   comp_name, date,
                                   P, N,
                                   starts_with("N_"),
                                   starts_with("parameter"),
                                   fold,
                                   print_model_id, debug_model_id,
                                   runtime,
                                   model_size,
                                   starts_with("train"),
                                   starts_with("valid"),
                                   starts_with("test"))

print.to.console(sprintf("accuracy metric computation complete"))

#### compute summary stats #####
print.to.console(sprintf("computing summary stats (warning: this takes a while)"))

#no summary stats for final model (i.e. model trained with ALL training data)
final_df = results_df %>% filter(fold==0) %>% select(-starts_with("N_valid"),
                                                     -starts_with("N_train"),
                                                     -ends_with("_positives"),
                                                     -ends_with("_negatives"),
                                                     -starts_with("valid"));

final_fields = colnames(final_df);
stats_ind = seq(match("fold", final_fields)+1, length(final_fields));
final_fields[stats_ind] = paste(final_fields[stats_ind], "final", sep=".");
colnames(final_df) = final_fields;
final_df = final_df %>% select(-fold);

#compute summary stats for validation (fold-based)
stats_df = results_df %>% filter(fold>0) %>% select(-starts_with("N_valid"),
                                                    -starts_with("N_train"),
                                                    -ends_with("_positives"),
                                                    -ends_with("_negatives"),
                                                    -debug_model_id,
                                                    -print_model_id);

fields = colnames(stats_df);
key_ind = seq(1, match("fold", fields) - 1);
summary_ind = seq(match("fold", fields) + 1 , length(fields));
key_fields = fields[key_ind];
summary_fields = fields[summary_ind];
stats_df = stats_df %>% select(-fold);

mean_plus_sd  = function(x) mean(x) + sd(x);
mean_minus_sd = function(x) mean(x) - sd(x);

stats_df = stats_df %>%
    group_by_(.dots = key_fields) %>%
    summarise_each_(funs(min,median,max,mean,sd,mean_plus_sd,mean_minus_sd), summary_fields) %>%
    ungroup()

field_naming_function   = function(x) paste(x, c("min","median","max","mean","sd","mean_plus_sd","mean_minus_sd"), sep="_")
summarized_field_names  = as.vector(sapply(summary_fields,field_naming_function,USE.NAMES=FALSE));

desired_naming_function   = function(x) paste(x, c("min","med","max","mean","sd","mean_plus_sd","mean_minus_sd"), sep=".")
desired_field_names     = as.vector(sapply(summary_fields,desired_naming_function,USE.NAMES=FALSE));

for (i in 1:length(summarized_field_names)){
    current_fname = summarized_field_names[i];
    desired_fname = desired_field_names[i];
    colnames(stats_df) = gsub(current_fname, desired_fname, colnames(stats_df));
}

#compute validation and final model stats
stats_df = full_join(x=stats_df, y= final_df, by = key_fields);
rm(final_df);

print.to.console(sprintf("summary stats computation complete!"))

##### save and exit #####
save(stats_df, results_df, print_models, debug_models, file = save_file_name);
print.to.console(sprintf("saved results in file %s", save_file_name));

total_runtime = proc.time() - start_time;
print.to.console(sprintf("processing script ran in %1.0f seconds", total_runtime[["elapsed"]]));

#exit
if (command_line_flag){
    print.to.console("quitting R");
    quit(save = "no", status = 0);
}