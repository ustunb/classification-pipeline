#Binary Classification Pipeline
#Berk Ustun | ustunb@mit.edu | 2016
#
#This file will train all
#This file must be run from home_dir to work correctly

##### Setup Directories and Parse Command Line Arguments #####
cat(sprintf("Opened ClassificationPipeline.R in directory: %s\n", getwd()))

print_flag = TRUE;
log_name = NA;
command_line_flag = FALSE
source("StartUp.R");

args = commandArgs(TRUE);
if (length(args) > 0){
    command_line_flag = TRUE;
    comp_name = args[1];
    train_name = args[2];
    run_name = args[3];
    log_name = args[4];
} else {
    comp_name = "berkmac"
    train_name = "breastcancer_F_K10N01"
    run_name = "breastcancer_F_K05N01_pos_1.00000000"
}

#set directories
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
log_file = set.log(log_dir = log_dir, log_name = log_name);

#results directory
run_dir = paste0(results_dir, train_name, "/");
dir.create(run_dir, showWarnings =  FALSE, recursive = FALSE);

#results file
results_file_name = paste0(run_dir, run_name, "_results.RData")

print.to.console(sprintf("comp_name: %s", comp_name));
print.to.console(sprintf("current working directory: %s", paste0(getwd(),"/")));
print.to.console(sprintf("run directory: 'run_dir' = %s", run_dir));
print.to.console(sprintf("run identifier: 'run_name' = %s", run_name));
print.to.console(sprintf("log file: %s",log_file));
print.to.console(sprintf("results file: %s", results_file_name));

###### LOAD SETTINGS ######
#Read run settings from disk
#each setting variable is read from a text file in 'run_dir'
#text file for setting variable 'example_setting' is named:
#paste0(run_name, example_setting, ".txt")
#if this text file is not found, then we set the setting variable to a default value

load.or.set.to.default = function(setting_name, default_value, my_name = run_name, my_dir = run_dir){

    print.to.console(sprintf('loading setting file for: %s',setting_name));
    if (my_name==""){
        setting_file = paste0(my_dir,setting_name,".setting");
        printable_setting_file = paste0(setting_name,".setting");
    } else {
        last_char = substr(my_name,nchar(my_name),nchar(my_name));
        if (last_char!="_"){
            setting_file = paste0(my_dir,my_name,"_",setting_name,".setting");
            printable_setting_file = paste0(my_name,"_",setting_name,".setting");
        } else {
            setting_file = paste0(my_dir,my_name,setting_name,".setting");
            printable_setting_file = paste0(my_name,setting_name,".setting");
        }
    }

    if (file.exists(setting_file)){
        setting_type  = typeof(default_value);
        scanned_value = scan(setting_file,what=setting_type,sep="", quiet = TRUE);
        scanned_type  = typeof(scanned_value);

        if (setting_type=="double" && scanned_type=="character"){
            scanned_value = as.double(scanned_value);
        } else if (setting_type=="logical" && scanned_type=="character"){
            scanned_value = as.logical(scanned_value)
        }
        print.to.console(sprintf('setting file found'));
        if (length(scanned_value)>1){
            print.to.console(sprintf('scanned values: (type: %s) %s', typeof(scanned_value), paste0("(",paste(scanned_value,collapse=" "),")")));
        } else {
            print.to.console(sprintf('scanned value:  (type: %s) %s', typeof(scanned_value), scanned_value))
        }
        if (length(default_value)>1){
            print.to.console(sprintf('default values: (type: %s) %s\n', typeof(default_value), paste0("(",paste(default_value,collapse=" "),")")));
        } else {
            print.to.console(sprintf('default value: (type: %s) %s\n', typeof(default_value), default_value))
        }
        return(scanned_value);
    } else {
        print.to.console(sprintf('setting file %s does not exist', setting_file));
        if (length(default_value)>1){
            print.to.console(sprintf('using default values %s (type: %s)\n', paste0("(",paste(default_value,collapse=" "),")"), typeof(default_value)));
        } else {
            print.to.console(sprintf('using default value %s (type: %s)\n', default_value, typeof(default_value)));
        }
        return(default_value);
    }
}

data_name 				            = load.or.set.to.default("data_name","breastcancer");
fold_id 				            = load.or.set.to.default("fold_id","K05N01");
hcon_id 				            = load.or.set.to.default("hcon_id","NA");
xtra_id 				            = load.or.set.to.default("xtra_id","NA");
w_pos                               = load.or.set.to.default("w_pos", 1.0);
standardize_flag                    = load.or.set.to.default("standardize_flag", FALSE);
check_results_flag 					= load.or.set.to.default("check_results_flag", TRUE);

#choice of methods
settings                            = list();
settings$run_cart                   = load.or.set.to.default("run_cart", FALSE);
settings$run_c50_rule               = load.or.set.to.default("run_c50_rule", TRUE);
settings$run_c50_tree               = load.or.set.to.default("run_c50_tree", TRUE);
settings$run_lars_lasso             = load.or.set.to.default("run_lars_lasso", TRUE);
settings$run_lars_ridge             = load.or.set.to.default("run_lars_ridge", TRUE);
settings$run_lars_elasticnet        = load.or.set.to.default("run_lars_elasticnet", TRUE);
settings$run_randomforest           = load.or.set.to.default("run_randomforest",TRUE);
settings$run_sgb                    = load.or.set.to.default("run_sgb",TRUE); FALSE;
settings$run_svm_linear             = load.or.set.to.default("run_svm_linear",TRUE);
settings$run_svm_rbf                = load.or.set.to.default("run_svm_rbf",TRUE);

#method-specific settings
settings$cart$param$minsplit                = load.or.set.to.default("cart_minsplit", c(3,5,10,15,20));
settings$cart$param$minbucket               = load.or.set.to.default("cart_minbucket", c(3,5));
settings$cart$param$cp                      = load.or.set.to.default("cart_cp", c(0.0001, 0.001, 0.01));
settings$cart$save_print_models             = load.or.set.to.default("cart_save_print_models", TRUE);
settings$cart$save_debug_models             = load.or.set.to.default("cart_save_debug_models", FALSE);

settings$c50_tree$param$CF                  = load.or.set.to.default("c50_tree_confidence_factor", c(0.25));
settings$c50_tree$param$minCases            = load.or.set.to.default("c50_tree_min_cases", c(1, 5, 10));
settings$c50_tree$param$noGlobalPruning     = load.or.set.to.default("c50_tree_no_global_pruning", TRUE);
settings$c50_tree$save_print_models         = load.or.set.to.default("c50_tree_save_print_models", TRUE);
settings$c50_tree$save_debug_models         = load.or.set.to.default("c50_tree_save_debug_models", FALSE);

settings$c50_rule$param$CF                  = load.or.set.to.default("c50_rule_confidence_factor", c(0.25));
settings$c50_rule$param$minCases            = load.or.set.to.default("c50_rule_min_cases", c(1, 5, 10));
settings$c50_rule$param$noGlobalPruning     = load.or.set.to.default("c50_rule_no_global_pruning", TRUE);
settings$c50_rule$save_print_models         = load.or.set.to.default("c50_rule_save_print_models", TRUE);
settings$c50_rule$save_debug_models         = load.or.set.to.default("c50_rule_save_debug_models", FALSE);

settings$lars_lasso$param$nlambda           = load.or.set.to.default("lars_lasso_nlambda", 1000);
settings$lars_lasso$param$standardize       = load.or.set.to.default("lars_lasso_standardize", standardize_flag);
settings$lars_lasso$save_print_models       = load.or.set.to.default("lars_lasso_save_print_models", TRUE);
settings$lars_lasso$save_debug_models       = load.or.set.to.default("lars_lasso_save_debug_models", FALSE);

settings$lars_ridge$param$nlambda           = load.or.set.to.default("lars_ridge_nlambda", 500);
settings$lars_ridge$param$standardize       = load.or.set.to.default("lars_ridge_standardize", standardize_flag);
settings$lars_ridge$save_print_models       = load.or.set.to.default("lars_ridge_save_print_models", TRUE);
settings$lars_ridge$save_debug_models       = load.or.set.to.default("lars_ridge_save_debug_models", FALSE);

settings$lars_elasticnet$alpha_values       = load.or.set.to.default("lars_elasticnet_alpha_values", seq(1,9)/10);
settings$lars_elasticnet$param$nlambda      = load.or.set.to.default("lars_elasticnet_nlambda", 500);
settings$lars_elasticnet$param$standardize  = load.or.set.to.default("lars_elasticnet_standardize", standardize_flag);
settings$lars_elasticnet$save_print_models  = load.or.set.to.default("lars_elasticnet_save_print_models", TRUE);
settings$lars_elasticnet$save_debug_models  = load.or.set.to.default("lars_elasticnet_save_debug_models", FALSE);

settings$randomforest$param$sampsize        = load.or.set.to.default("randomforest_sampprop", c(0.632, 0.4, 0.2));
settings$randomforest$param$nodesize        = load.or.set.to.default("randomforest_nodesize", c(1,5,10,20));
settings$randomforest$save_print_models     = load.or.set.to.default("randomforest_save_print_models", FALSE);
settings$randomforest$save_debug_models     = load.or.set.to.default("randomforest_save_debug_models", FALSE);

settings$svm_linear$param$cost              = load.or.set.to.default("svm_linear_costs", 10^seq(-3,3,by=0.5));
settings$svm_linear$param$scale             = load.or.set.to.default("svm_linear_scale", standardize_flag);
settings$svm_linear$save_print_models       = load.or.set.to.default("svm_linear_save_print_models", TRUE);
settings$svm_linear$save_debug_models       = load.or.set.to.default("svm_linear_save_debug_models", FALSE);

settings$svm_rbf$param$cost                 = load.or.set.to.default("svm_rbf_costs", 10^seq(-3,3,by=0.5));
settings$svm_rbf$param$scale                = load.or.set.to.default("svm_rbf_scale", standardize_flag);
settings$svm_rbf$save_print_models          = load.or.set.to.default("svm_rbf_save_print_models", FALSE);
settings$svm_rbf$save_debug_models          = load.or.set.to.default("svm_rbf_save_debug_models", FALSE);

settings$sgb$param$interaction.depth        = load.or.set.to.default("sgb_interaction_depth", c(2));
settings$sgb$param$shrinkage                = load.or.set.to.default("sgb_shrinkage", c(0.001,0.01,0.1));
settings$sgb$param$n.trees                  = load.or.set.to.default("sgb_n_trees", c(100, 500, 1500, 3000));
settings$sgb$save_print_models              = load.or.set.to.default("sgb_save_print_models", FALSE);
settings$sgb$save_debug_models              = load.or.set.to.default("sgb_save_debug_models", FALSE);

###### PIPELINE VARIABLES ######

#load helper functions
source(paste0(R_dir, "PipelineFunctions.R"))

#load data
data_file_name = paste0(data_dir, data_name, "_processed.RData")
data_environment = new.env()
load(data_file_name, data_environment)
data = as.list(data_environment)

#initialize global variables for helper functions
X 				= data$X
Y               = data$Y
X_test          = data$X_test
Y_test          = data$Y_test
folds     		= data$cvindices[[substring(fold_id,1,3)]][,as.double(substr(fold_id,5,6))]
K 				= max(folds)
ind_pos         = Y == 1
ind_neg         = !ind_pos
ind_pos_test    = Y_test == 1
ind_neg_test    = !ind_pos_test

#drop intercept from feature matrix
to_drop = colnames(X) == "(Intercept)"
X = X[, !to_drop];
X_test = X_test[, !to_drop];

#clear data for memory
rm(data, data_environment);

#create class weights
w_neg = 1.0
class_weights = c(w_neg, w_pos)
class_weights = 2 * class_weights / sum(class_weights)
weighted = !(is.null(class_weights) || class_weights[1]==class_weights[2]);
print.to.console(sprintf("w- = %1.3f w+ = %1.3f", class_weights[1], class_weights[2]))

#create a data
if (check_results_flag){
    count_table = create.count.table(X = X, Y = Y, X_test = X_test, Y_test = Y_test, folds = folds);
    results.are.ok = function(r){return(check.results.rep(results, count_table))};
} else {
    results.are.ok = function(r) return(TRUE);
}

##### MODEL TRAINING #####
output = list();
start_time = proc.time();
random_seed = .Random.seed;

if (settings$run_cart){
    method_name = "cart";
    results = train.cart(settings[[method_name]])
    if (results.are.ok(results)){
        output[[method_name]] =  results;
    }
}

if (settings$run_c50_rule){
    method_name = "c50_rule";
    results = train.c50(settings[[method_name]], type = "rule")
    if (results.are.ok(results)){
        output[[method_name]] =  results;
    }
}

if (settings$run_c50_tree){
    method_name = "c50_tree";
    results = train.c50(settings[[method_name]], type = "tree")
    if (results.are.ok(results)){
        output[[method_name]] =  results;
    }
}

if (settings$run_lars_lasso){
    method_name = "lars_lasso";
    results = train.lars(settings[[method_name]], alpha_value = 0.0)
    if (results.are.ok(results)){
        output[[method_name]] =  results;
    }
}

if (settings$run_lars_ridge){
    method_name = "lars_ridge";
    results = train.lars(settings[[method_name]], alpha_value = 1.0)
    if (results.are.ok(results)){
        output[[method_name]] =  results;
    }
}

if (settings$run_lars_elasticnet){
    method_name = "lars_elasticnet";
    results = train.lars.elasticnet(settings[[method_name]])
    if (results.are.ok(results)){
        output[[method_name]] =  results;
    }
}

if (settings$run_randomforest){
    method_name = "randomforest";
    results = train.randomforest(settings[[method_name]]);
    if (results.are.ok(results)){
        output[[method_name]] =  results;
    }
}

if (settings$run_sgb){
    method_name = "sgb";
    results = train.sgb(settings[[method_name]]);
    if (results.are.ok(results)){
        output[[method_name]] =  results;
    }
}

if (settings$run_svm_linear){
    method_name = "svm_linear"
    results = train.svm(settings[[method_name]], kernel_type = "linear")
    if (results.are.ok(results)){
        output[[method_name]] =  results;
    }
}

if (settings$run_svm_rbf){
    method_name = "svm_rbf"
    results = train.svm(settings[[method_name]], kernel_type = "radial")
    if (results.are.ok(results)){
        output[[method_name]] =  results;
    }
}

###### SAVE RESULTS ######

info = list()
info$data_name = data_name;
info$w_pos = w_pos
info$fold_id = fold_id;
info$hcon_id = hcon_id;
info$xtra_id = xtra_id;
info$comp_name = comp_name;
info$date = format(Sys.time(),"%m/%d/%y");
info$end_time = format(Sys.time());
info$data_file_name = data_file_name;
info$results_file_name = results_file_name;
info$run_name = run_name;
info$run_dir = run_dir;
info$sys_info = Sys.info()
info$random_seed = random_seed;
info$settings = settings;

#save data in R format
save(file=results_file_name, "output", "info")
print.to.console(sprintf("saved results in file %s", results_file_name))

#### exit #####
if (command_line_flag){
    print.to.console("quitting R")
    quit(save = "no", status = 0)
}