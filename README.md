## Directory Structure 

- /Data/                  processed RData files of datasets
- /Data/Raw Data Files/   raw csv files of datasets
- /Run/                   RData files containing run results
- /Logs/                  Log files for pipeline runs
- /R/                     R code + Rnw-files to create reports
- /R/Packages/            R package directory (default)

## Pipeline Setup

To setup the pipeline on your machine:

1. Add a unique computer name (`comp_name`) and the path to your home directory (`home_dir`) to the file `LocalVariables.sh`

2. Run `SetupPipeline.sh` in Bash to install the required R packages (note that this requires that R is able to access the internet).

## Dataset Creation

1. Add a raw file to Data/Raw Data Files/. The data file should be named `{data_name}.csv` where `{data_name}` is a unique name for the dataset. The file must adhere to some rough guidelines, namely:

- column names must be at the top row
- no duplicate columns
- no duplicate column names
- outcome variable must be in column 1
- outcome variable must be composed of all 0s and 1s
- all entries must be numeric

2. Run `R/DataProcessingScript.R` to create the `{data_name}_processed.RData` file

3. Run `Test/DataFileTest.R` to check that the dataset passes the tests

## Running the Pipeline

Once the data file is in the proper format, users can run the pipeline to produce classification models. 

Each "call" to the pipeline runs multiple classification methods for a single dataset (`data_name`) and a single weight on the positive examples (`w_pos`). 

Methods that are currently supported include:

- CART (cart)
- C5.0 Tree (c50_tree)
- C5.0 Rule (c50_rule)
- L1-Penalized Logistic Regression (lars_lasso)
- L2-Penalized Logistic Regression (lars_ridge)
- L1+L2-Penalized Logistic Regression (lars_elastic_net)
- Random Forests (randomforest)
- Stochastic Gradient Boosting (sgb)
- SVM Linear Kernel (svm_linear)
- SVM RBF (svm_rbf)

To run the pipeline:

1. Edit `RunPipeline.sh` with the `data_name`, `fold_id`, range of weights, methods and free parameters for each method.

2. Execute `RunPipeline.sh` in Bash

## Analyzing Results

## Coming Soon

**To Do**

- [ ] Report Creation Script
- [ ] Fix ReadMe.txt with full instructions
- [ ] Add Case Weight Support to DataFileTest
- [ ] Check for Packages in StartUp.R
- [ ] Check Results Table in ProcessResults.R

