#Results Analysis Script
#Berk Ustun | ustunb@mit.edu | 2016
#
#This is an interactive script that shows how to:
#
# - choose models
# - tabulate results
# - plot accuracy
# - produce model reports
#
#This file is meant to be run interactively.
#
#Note: this file must be run from home_dir to work correctly

##### parse command line arguments and setup directories #####
cat(sprintf("Opened CreateReport.R in directory: %s\n", getwd()))
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

######

#load processed results

#pick best model for each value of W+

#create ROC plot using ggplot2

#create AUC table

#create model report





