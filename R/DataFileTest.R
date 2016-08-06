# This file tests that an .RData file contains all of the necessary elements for
# the pipeline to run
#
# Each data file contains the following variables:
#
#- X            P x N matrix of features; first column = column of 1st for the (Intercept)
#- Y            1 x N vector of outcomes with entries %in% (0,1)
#- X_test       P x N_test matrix of features for testing only
#- Y_test       1 x N_test vector of outcomes for testing only
#- X_names      P x 1 character array with names of the features
#- Y_name       1 x 1 character array with the name of the outcome
#- cvindices    list containing indices used for cross-validation
#- info         list with some information about the dataset
#
##### User Variables ####

comp_name = "berkmac"
data_name = "breastcancer"

#### Set Directories ####

if (comp_name == "berkmac"){
    home_dir = "/Users/berk/Desktop/Dropbox (MIT)/Research/ClassificationPipeline/"
    lib_dir = NA;
} else if (comp_name == "umisr"){
    home_dir = "~/ClassificationPipeline/"
    lib_dir = "~/ClassificationPipeline/R/Library/"
    .libPaths(lib_dir)
} else if (comp_name == "masha"){
    home_dir = "/" #TODO add home directory
    lib_dir = "/" #TODO add library location or NA if using default
}
if (!is.na(lib_dir)){.libPaths(lib_dir)}
data_dir = paste0(home_dir, "Data/")
raw_data_dir = paste0(data_dir, "Raw Data Files/")
R_dir = paste0(home_dir, "R/")
report_files_dir = R_dir
run_dir = paste0(home_dir, "Run/")

#load packages
library(testit)

#load all variables in the data file into a list()
data_file_name = paste0(data_dir, data_name, "_processed.RData")
data_environment = new.env()
load(data_file_name, data_environment)
data = as.list(data_environment)

print(sprintf("loaded file %s", data_file_name))
if ("info" %in% names(data)){
    print(data$info)
}

print(sprintf("running tests for %s", data_file_name))

#check that all elements of data file are present
assert("data file missing X (feature matrix)", "X" %in% names(data))
assert("data file missing Y (outcome vector)", "Y" %in% names(data))
assert("data file missing X_test (feature matrix for testing)", "X_test" %in% names(data))
assert("data file missing Y_test (outcome vector for testing)", "Y_test" %in% names(data))
assert("data file missing X_names (names of features)", "X_names" %in% names(data))
assert("data file missing Y_name (name of outcome)", "Y_name" %in% names(data))
assert("data file missing cvindices (list of cvindices)", "cvindices" %in% names(data))



#check types
assert(class(data$X)=="matrix")
assert(class(data$Y)=="matrix")
assert(class(data$X_test)=="matrix")
assert(class(data$Y_test)=="matrix")
assert(class(data$X_names)=="character")
assert(class(data$Y_name)=="character")

#check sizes
N = nrow(data$X)
P = ncol(data$X)
assert(nrow(data$Y)==N)
assert(ncol(data$Y)==1)

N_test = nrow(data$X_test)
assert(nrow(data$Y_test)==N_test)
assert(ncol(data$Y_test)==1)
assert(ncol(data$X_test)==P)

#check entries of X and Y matrices
assert(!any(is.na(data$X)))
assert(all(is.numeric(data$X)))
assert(!any(is.na(data$Y)))
assert(all(is.numeric(data$Y)))
assert(all(data$Y==1||data$Y==0))

assert(!any(is.na(data$X_test)))
assert(all(is.numeric(data$X_test)))
assert(!any(is.na(data$Y_test)))
assert(all(is.numeric(data$Y_test)))
assert(all(data$Y_test==1||data$Y_test==0))

#check for intercept in X matrices
assert("(Intercept)" %in% data$X_names)
intercept_ind = which("(Intercept)" == data$X_names)
assert("(Intercept) column in X should contain only 1s", all(data$X[,intercept_ind]==1))
assert("(Intercept) column in X_test should contain only 1s", all(data$X_test[,intercept_ind]==1))

#check that columns of X are not duplicated
X_all = rbind(data$X, data$X_test)
assert("X_all contains duplicate features", all(!duplicated(t(X_all))))

#check variable names
assert("X and X_test should have the same column names", colnames(data$X)==colnames(data$X_test))
assert("X should have the same column names as X_names", data$X_names==colnames(data$X))
assert("Each name in X_names should be unique", !any(duplicated(data$X_names)))
assert("Each name in X_names should have at least 1 character", all(sapply(data$X_names, nchar)>0))
assert("Each name in Y_name should have at least 1 character", all(sapply(data$Y_name, nchar)>0))

#check cv indices
assert(class(data$cvindices)=="list")
cvindices = data$cvindices;
K_max = length(names(cvindices))
actual_names = names(cvindices)
expected_names = sprintf("K%02d", seq(1:K_max))
assert(actual_names==sort(expected_names))
n_cv_repeats = ncol(cvindices$K01);

for (K in seq(1,K_max)){
    assert(expected_names[K] %in% actual_names)
    fold_matrix = cvindices[[expected_names[K]]]
    assert(sprintf("cvindices$K%02d  should have %d rows", K, N), nrow(fold_matrix)==N)
    assert(sprintf("cvindices$K%02d should have %d n_cv_repeats", K, n_cv_repeats), ncol(fold_matrix)==n_cv_repeats)
    assert(sprintf("cvindices$K%02d should only contain elements in seq(1,%d)",K, K), all(sort(unique(array(fold_matrix)))==seq(1,K)))
}

print(sprintf("The file: %s passed all tests!", data_file_name))