#Pipeline StartUp Script
#Berk Ustun | ustunb@mit.edu | 2016
#
#This script contains helper functions to set up directories, libraries, and
#logging for all other functions in the pipeline. To setup the pipeline on
#your machine, you must add the following to the get.directories() function:
#
#- comp_name: unique string identifier for your machine
#- home_dir: path of the directory where the pipeline/StartUp.R is located
#- lib_dir: path to R library (NA if default)
#
#This file must be run from home_dir to work correctly

if (!exists("print_flag")){
    print_flag = TRUE;
}

if (!exists("split_flag")){
    split_flag = FALSE;
}

print.to.console = function(print_string, flag = print_flag){
    if (flag){
        cat(sprintf('%s | %s\n', format(Sys.time(),"%X"), print_string))
    }
}

get.directories = function(comp_name, show_directories = print_flag){

    fs = list()
    if (comp_name == "berkmac") {
        fs$home_dir	= "/Users/berk/Desktop/Dropbox (MIT)/Research/ClassificationPipeline/"
        fs$lib_dir = NA;
    } else if (comp_name =="ec2") {
        fs$home_dir	= "/home/ubuntu/ClassificationPipeline/"
        fs$lib_dir = paste0(fs$home_dir, "R/Packages/");
    } else if (comp_name =="svante") {
        fs$home_dir	= "/home/ustunb/SLIM/"
        fs$lib_dir = paste0(R_dir, "Packages/");
    } else if (comp_name == "umisr"){
        fs$home_dir  = "/home/ustunb/ClassificationPipeline/";
        fs$lib_dir = "/home/ustunb/ClassificationPipeline/R/Library/";
    } else if (comp_name == "masha"){
        fs$home_dir  = "/home/user/petukhova/Berk_method/";
        fs$lib_dir = NA;
    } else if (comp_name == "isrmasha"){
        fs$home_dir = "/home/petukhova/ClassificationPipeline/";
        fs$lib_dir = "/home/petukhova/ClassificationPipeline/R/Library";
    # else if (comp_name == "YOURCOMPNAME") {
    #   fs$home_dir = "LOCATION OF THIS FILE"
    #   fs$lib_dir = #LOCATION OF R LIBRARY (or NA if default is OK)
    } else{
        fs$home_dir = paste0(getwd(),"/")
        fs$lib_dir = NA;
    }
    fs$R_dir = paste0(fs$home_dir, "R/");
    fs$results_dir = paste0(fs$home_dir, "Run/");
    fs$log_dir = paste0(fs$home_dir, "Logs/");
    fs$data_dir = paste0(fs$home_dir, "Data/");
    fs$raw_data_dir = paste0(fs$data_dir, "Raw Data Files/");

    if (show_directories){
        print.to.console("pipeline directories set as follows:");
        print.to.console(sprintf("comp_name: %s", comp_name));
        print.to.console(sprintf("home_dir: %s", fs$home_dir));
        print.to.console(sprintf("log_dir: %s", fs$log_dir));
        print.to.console(sprintf("data_dir: %s", fs$data_dir));
        print.to.console(sprintf("raw_data_dir: %s", fs$raw_data_dir));
        if (!is.na(fs$lib_dir)){
            print.to.console(sprintf("lib_dir: %s", fs$lib_dir));
        } else {
            print.to.console(sprintf("lib_dir: %s", .libPaths()));
        }
    }
    return(fs);
}

set.library = function(lib_dir, show_flag = print_flag){
    if (!is.na(lib_dir)){
        if (!dir.exists(lib_dir)){
            print.to.console(sprintf("library directory (lib_dir): %s does not exist", lib_dir));
            print.to.console("will create the directory");
            dir.create(path = lib_dir, showWarnings = FALSE, recursive = TRUE);
        }
        .libPaths(lib_dir)
    }
    if (show_flag){
        print.to.console("libPaths:");
        print.to.console(sprintf("-%s\n", .libPaths()));
    }
}

set.log = function(log_dir, log_name, split_log = split_flag){
    if (!is.na(log_name)){
        log_name = basename(log_name);
        correct_log_file_extension = (substr(log_name, nchar(log_name)-3, nchar(log_name)) == ".log")
        if (correct_log_file_extension){
            log_file = paste0(log_dir, log_name);
        } else {
            log_file = paste0(log_dir, log_name, ".log");
        }
        sink(file = log_file, append = FALSE, split = split_log);
        return(log_file);
    } else {
        return(log_name);
    }
}