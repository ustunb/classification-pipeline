#Data Processing Script
#Berk Ustun | ustunb@mit.edu | 2016
#
#This file shows how to create a .RData file that will be used in model training
#It contains key error-checking to make sure that the pipeline works
#The data file is called data_name"_processed.RData" and is stored under /Data/
#
#Each data file contains the following variables:
#
#- X            P x N matrix of features
#- Y            1 x N vector of outcomes with entries %in% (0,1)
#- X_test       P x N_test matrix of features for testing only
#- Y_test       1 x N_test vector of outcomes for testing only
#- X_names      P x 1 character array with names of the features
#- Y_name       1 x 1 character array with the name of the outcome
#- cvindices    list containing indices used for cross-validation
#- info         list with some information about the dataset
#

##### Setup Directories and Parse Command Line Arguments #####

print_flag = TRUE;
log_name = NA;
command_line_flag = FALSE
source("StartUp.R");
args = commandArgs(TRUE);

if (length(args) >= 2){
    command_line_flag = TRUE
    comp_name = args[1];
    data_name = args[2];
    print_flag = only.if.not.na(print_flag, args[3]);
    log_name = only.if.not.na(log_name, args[4]);
} else {
    comp_name = "berkmac"
    data_name = "breastcancer"
}
fs = get.directories(comp_name);
data_dir = fs$data_dir;
raw_data_dir = fs$raw_data_dir;
log_dir = fs$log_dir;
rm(fs);

#logging
log_file = set.log(log_dir = log_dir, log_name = log_name)

K_max = 10
n_cv_repeats = 10
prop_data_used_for_testing = 0.30           #proportion of data to be saved for testing
max_difference_in_train_test_balance = 0.05 #max difference in P(y=+1) between train/test set
random_seed = 1337
set.seed(random_seed)

#### Read Data from Disk ####
csv_file_name   = paste0(raw_data_dir, data_name, ".csv")
data 			= read.csv(csv_file_name)

#extract X and Y matrices from data
Y_all 			= as.matrix(data[,1]);
X_all 			= as.matrix(data[,2:ncol(data)]);

#check that X and Y are not missing values
stopifnot(is.numeric(X_all)) #entries should be numeric
stopifnot(!is.na(X_all))     #entries should not contain NAs
stopifnot(is.numeric(Y_all)) #entries should be numeric
stopifnot(!is.na(Y_all))     #entries should not contain NAs

#check that all entries of Y are +1 or -1
N_all           = nrow(X_all)
N_pos           = sum(Y_all==1)
N_neg           = sum(Y_all==0)
stopifnot(N_pos+N_neg==N_all) #entries of Y should be in (0,1)

#extract variable names from data
var_names       = colnames(data)
Y_name          = var_names[1]
X_names         = var_names[2:length(var_names)]
stopifnot(!is.null(X_names)) #did not find names for feature variables (X)
stopifnot(!is.null(Y_name))  #did not find names for outcome variable (Y)

# add column of ones to the data to act as an Intercept some methods ####
X_all = cbind(array(1.0, N_all), X_all)
X_names = c("(Intercept)", X_names)
colnames(X_all) = X_names;

#check for uniqueness among variables AND variable names
stopifnot(all(!duplicated(t(X_all))))
stopifnot(!(X_names == length(unique(X_names)))) #feature names should be unique

#### Split Data into Training and Testing with Similar P(Y==+1) #####

N_test = floor(N_all * prop_data_used_for_testing)
N = N_all - N_test;

difference_in_balance = Inf
while (difference_in_balance > max_difference_in_train_test_balance){

    row_perm = sample(nrow(X_all), replace = FALSE)
    test_row_indices = sort(row_perm[1:N_test])
    train_row_indices = sort(row_perm[N_test+1:N_all])

    X = as.matrix(X_all[train_row_indices,])
    Y = as.matrix(Y_all[train_row_indices,])
    X_test = as.matrix(X_all[test_row_indices,])
    Y_test = as.matrix(Y_all[test_row_indices,])

    pos_prop_test = mean(Y_test==1)
    pos_prop_train = mean(Y == 1)
    difference_in_balance = abs(pos_prop_test - pos_prop_train)/(pos_prop_train)

}

#### Create List with folds for repeated K-fold CV for K = 1.. K_max #####

cvindices = list()
for (k in seq(1,K_max)){
    fold_matrix = matrix(nrow = N, ncol = n_cv_repeats)
    if (k == 1){
        fold_matrix[,] = 1.0
    } else {
        for (r in 1:n_cv_repeats){
            fold_matrix[,r] = cut(sample(N), k, labels = FALSE)
        }
    }
    cv_index_name = sprintf("K%02d",k)
    cvindices[[cv_index_name]] = fold_matrix
}

##### Save File #####

data_file_name = paste0(data_dir, data_name, "_processed.RData")

#create an information struct to keep track of who created the data set
info = list()
info$data_name = data_name
info$comp_name = comp_name
info$date = format(Sys.time(),"%m/%d/%y at %H:%M")
info$sys_info = Sys.info()
info$random_seed = random_seed;
info$csv_file_name = csv_file_name;
info$data_file_name = data_file_name;

save(file = data_file_name, "X_names", "Y_name", "X", "X_test", "Y", "Y_test", "cvindices", "info")
cat(sprintf("Saved data variables in file\n%s\n", data_file_name))