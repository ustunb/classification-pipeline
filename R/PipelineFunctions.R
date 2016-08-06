# Classification Pipeline Helper Functions
# Berk Ustun | ustunb@mit.edu

#case weight support
#glmnet
#gbm
#C5.0

#no case weight support
#e1071
#randomForest
#Rpart

#Libraries
suppressPackageStartupMessages(library('methods', warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE));
suppressPackageStartupMessages(library('dplyr', warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE));
suppressPackageStartupMessages(library('glmnet',warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE));
suppressPackageStartupMessages(library('rpart', warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE));
suppressPackageStartupMessages(library('C50', warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE));
suppressPackageStartupMessages(library('gbm',  warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE));
suppressPackageStartupMessages(library('randomForest', warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE));
suppressPackageStartupMessages(library('e1071', warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE));

#### logging #####
print.to.console = function(print_string, flag=TRUE){
    if(flag){
        cat(sprintf('%s | %s\n',format(Sys.time(),"%X"), print_string))
    }
}

print.method.is.starting.message = function(method_name, extra_message = NULL) {
    print_message = sprintf("Starting %s on %s with weights w- = %1.3f w+ = %1.3f",
                            method_name,
                            data_name,
                            class_weights[1],
                            class_weights[2]);
    if (!is.null(extra_message)){
        print_message = sprintf("%s%s", print_message, extra_message)
    }
    print.to.console(print_message,print_flag)
    return(print_message)
}

print.method.is.ending.message = function(method_name, extra_message = NULL) {
    print_message = sprintf("Finished %s on %s with weights w- = %1.3f w+ = %1.3f\n",
                            method_name,
                            data_name,
                            class_weights[1],
                            class_weights[2]);
    if (!is.null(extra_message)){
        print_message = sprintf("%s%s", print_message, extra_message)
    }
    print.to.console(print_message,print_flag)
    return(print_message)
}

#### easy parameter tuning and results management #####
sort.instances = function(df){
    #sorts a dataframe by fold, parameter_1_name, parameter_1_value,...
    #every column is sorted in increasing order
    pname_cols = grep("parameter_[0-9]+_name", colnames(df), value = TRUE)
    pvalue_cols = grep("parameter_[0-9]+_value", colnames(df), value = TRUE)
    p_cols = character(0)
    for (n in 1:length(pname_cols)){
        p_cols = c(p_cols, pname_cols[n], pvalue_cols[n])
    }
    arrange_order = c("fold", p_cols)
    df = arrange_(.data=df, .dots = setNames(arrange_order, arrange_order));
    return(df);
}

create.parameter.df = function(parameter_list, K){
    #given a named list of N parameters and a max number of folds (K)
    #returns a data.frame with columns
    #
    #[fold, parameter_1_name, parameter_1_value,... parameter_N_name, parmaeter_N_value]
    #
    #where each row is distinct (fold, parameter) instance that needs to be trained

    if (is.null(parameter_list)){
        return(data.frame(fold=seq(0,K)));
    }

    #get parameter names from list
    parameter_names = names(parameter_list)
    parameter_names[order(parameter_names)]

    #create initial df with headers [fold_column_name, parameter_names]
    fold_values = seq(0, K)
    parameter_list = c(list(fold_values), parameter_list)
    parameter_value_df = expand.grid(parameter_list)
    names(parameter_value_df) = c("fold", parameter_names)
    parameter_value_df = parameter_value_df %>% arrange(fold)
    n_instances = nrow(parameter_value_df)
    n_parameters = length(parameter_names)
    names(parameter_value_df)[seq(2,n_parameters+1)] = sprintf("parameter_%d_value", seq(1,n_parameters))

    #convert df with headers [fold_column_name, parameter_1_name, parameter_1_value...]
    parameter_name_df = as.list(parameter_names)
    names(parameter_name_df) = sprintf("parameter_%d_name", seq(1,n_parameters))

    if (n_parameters == 1){
        parameter_df = parameter_value_df;
        parameter_df$parameter_1_name = parameter_names[1];
    } else {
        parameter_name_df = data.frame(parameter_name_df)
        parameter_name_df = parameter_name_df[rep(1, each=n_instances),]
        parameter_df = bind_cols(parameter_value_df, parameter_name_df)
    }

    #order columns
    parameter_df = parameter_df[order(colnames(parameter_df))]

    #sort rows
    paramater_df = sort.instances(parameter_df)

    return(parameter_df);
}

create.results.df  = function(parameter_list, K){

    results_df = create.parameter.df(parameter_list, K);

    #accuracy metrics
    for (set_name in c("train", "valid", "test")){
        for (stat_name in c("true_positives", "false_positives", "true_negatives", "false_negatives")){
            metric_name = paste0(set_name, "_", stat_name)
            results_df[[metric_name]] = NA
        }
    }

    #model_size, id, print_model_id, debug_model_id
    results_df = results_df %>% mutate(model_size = NA,
                                       id = row_number(),
                                       runtime = NA,
                                       print_model_id = sprintf("M%08d", id),
                                       debug_model_id = sprintf("M%08d", id))

    return(results_df);

}

convert.parameter.df.row.to.list = function(parameter_df_row){
    #given a 1-row data.frame() in the form:
    #
    #parameter_1_name, parameter_1_value ... parameter_N_name, parameter_N_value
    #"alpha", 1.0                       ...  "beta", TRUE
    #
    #will return a named list in the form:
    #list("alpha" = 1.0,...,"beta" = TRUE)

    pname_cols = grep("parameter_[0-9]+_name", colnames(parameter_df_row), value = TRUE)
    pvalue_cols = grep("parameter_[0-9]+_value", colnames(parameter_df_row), value = TRUE)

    #check rep
    stopifnot(nrow(parameter_df_row) == 1)
    stopifnot(nrow(parameter_df_row) == 1)
    stopifnot(length(pname_cols) == length(pvalue_cols))
    n_param = length(pname_cols)
    param_list = list()

    if (n_param > 0){
        for (n in 1:length(pname_cols)){
            pname = (parameter_df_row %>% select(one_of(pname_cols[n])))[[1]]
            pvalue = (parameter_df_row %>% select(one_of(pvalue_cols[n])))[[1]]
            if (is.factor(pvalue)){pvalue = as.vector(pvalue)}
            param_list = c(param_list, setNames(list(pvalue), pname))
        }
    }
    return(param_list);
}

get.filter.string = function(filter_row_df, negate = FALSE){

    filter_string = ""
    fnames = colnames(filter_row_df)
    n_pairs = ncol(filter_row_df)
    for (n in 1:n_pairs){
        fname = fnames[n]
        fvalue = filter_row_df[[fname]]
        if (is.character(fvalue)){
            fvalue = paste0("'", fvalue, "'")
        }
        if (negate){
            if (n < n_pairs){
                filter_string = paste0(filter_string,fname,"!=", fvalue, "|")
            } else {
                filter_string = paste0(filter_string,fname,"!=", fvalue)
            }
        } else {
            if (n < n_pairs){
                filter_string = paste0(filter_string,fname,"==", fvalue, "&")
            } else {
                filter_string = paste0(filter_string,fname,"==", fvalue)
            }
        }
    }
    return(filter_string)
}

merge.results.and.models = function(results_df_list, print_models_list, debug_models_list){

    #merge results and renumber all print/model ids sequentially
    results_df = bind_rows(results_df_list)
    results_df = results_df %>% mutate(id = row_number(),
                                       print_model_id = sprintf("M%08d", id),
                                       debug_model_id = sprintf("M%08d", id))

    #rename print_models and debug_models sequentially
    print_model_cnt = 0;
    debug_model_cnt = 0;
    for (i in 1:length(results_df_list)){
        names(print_models_list[[i]]) = sprintf("M%08d", print_model_cnt + seq(1, length(print_models_list[[i]])));
        names(debug_models_list[[i]]) = sprintf("M%08d", debug_model_cnt + seq(1, length(debug_models_list[[i]])));
        print_model_cnt = print_model_cnt + length(print_models_list[[i]]);
        debug_model_cnt = debug_model_cnt + length(debug_models_list[[i]]);
    }
    print_models = unlist(print_models_list, recursive = FALSE);
    debug_models = unlist(debug_models_list, recursive = FALSE);

    #return list with merged entites
    merged = list()
    merged$results_df = results_df;
    merged$print_models = print_models;
    merged$debug_models = debug_models;
    return(merged)
}

#### classification methods ######

train.lars = function(method_settings, alpha_value){
    #runs L1/L2 penalized logistic regression models using glmnet
    #returns K + 1 models at all effective L1/L2 penalties for fixed values of alpha and class_weights
    #global variable inputs include X, Y, folds, class_weights
    #set alpha = 0.0 for pure L1 penalty (lasso)
    #set alpha = 1.0 for pure L2 penalty (ridge)
    #set alpha in (0.0,1.0) for combined L1/L2 penalty (elasticnet)

    print.method.is.starting.message(method_name);
    training_start_time = proc.time();

    weights = rep(1, nrow(X));
    if (weighted){
        weights[Y!=1] = class_weights[1];
        weights[Y==1] = class_weights[2];
    }

    # run once to figure out lambda values
    initial_param = method_settings$param;
    initial_param$alpha = alpha_value;
    initial_param_df = create.parameter.df(method_settings$param, 0);
    initial_plist = convert.parameter.df.row.to.list(initial_param_df);

    call_inputs = c(initial_plist,
                    list(x = X,
                         y = Y,
                         family = "binomial",
                         weights = weights,
                         foldid = folds,
                         type.measure = "class"))

    initial_model = do.call(cv.glmnet, call_inputs);
    lambda = initial_model$lambda;

    #glmnet output is parametrized by lambda decreasing order, we store results in by lambda in increasing order
    lambda_increasing_idx = sort(lambda, index.return = TRUE);
    lambda_increasing_idx = lambda_increasing_idx$ix;

    #initialize results df (each row contains results from glmnet)
    results_param = method_settings$param;
    results_param$nlambda = NULL
    results_param$lambda = lambda;
    results_param$alpha = alpha_value;
    results_df = create.results.df(results_param, K);
    n_instances = results_df %>% nrow();
    print_models = setNames(vector("list", n_instances), results_df$print_model_id);
    debug_models = setNames(vector("list", n_instances), results_df$debug_model_id);

    #create run_param_df (each row contains parameters for a separate call to glmnet)
    run_param = method_settings$param;
    run_param$nlambda = NULL;
    run_param_df = create.parameter.df(run_param, K)
    n_runs = run_param_df %>% nrow();

    for (ii in 1:n_runs){
        run_param_row = run_param_df[ii,];
        run_param_list = convert.parameter.df.row.to.list(run_param_row);

        #initialize training dataset
        train_ind       = !(folds == run_param_row$fold);
        X_train         = as.matrix(X[train_ind,]);
        Y_train   	    = as.matrix(Y[train_ind]);
        ind_pos_train	= Y_train == 1;
        ind_neg_train	= !ind_pos_train;

        #initialize validation dataset
        has_validation_set = run_param_row$fold > 0;
        valid_ind       = folds == run_param_row$fold;
        X_valid         = as.matrix(X[valid_ind,]);
        Y_valid    		= as.matrix(Y[valid_ind]);
        ind_pos_valid	= Y_valid == 1;
        ind_neg_valid	= !ind_pos_valid;

        #train model
        call_inputs = c(run_param_list,
                        list(x = X_train,
                             y = Y_train,
                             family = "binomial",
                             weights = weights[train_ind],
                             lambda = lambda,
                             maxit = 1000000))

        call_start_time = proc.time();
        model = do.call(glmnet, call_inputs);
        runtime = (proc.time() - call_start_time);

        #get indices of results_df where results should be stored
        row_idx = results_df %>% filter_(get.filter.string(run_param_row)) %>% select(id)
        row_idx = row_idx$id;

        #accuracy metrics on training set
        y_hat = round(predict(model, newx = X_train, type="response"));
        y_hat = y_hat[, lambda_increasing_idx]; #reorder
        results_df[row_idx, "train_true_positives"] 	= apply(y_hat[ind_pos_train,], 2, function(y) sum(y == 1))
        results_df[row_idx, "train_false_negatives"] = apply(y_hat[ind_pos_train,], 2, function(y) sum(y != 1))
        results_df[row_idx, "train_true_negatives"] 	= apply(y_hat[ind_neg_train,], 2, function(y) sum(y != 1))
        results_df[row_idx, "train_false_positives"] = apply(y_hat[ind_neg_train,], 2, function(y) sum(y == 1))

        #accuracy metrics on validation set
        if (has_validation_set){
            y_hat = round(predict(model, newx = X_valid, type="response"));
            y_hat = y_hat[, lambda_increasing_idx]; #reorder
            results_df[row_idx, "valid_true_positives"] 	= apply(y_hat[ind_pos_valid,], 2, function(y) sum(y == 1))
            results_df[row_idx, "valid_false_negatives"] = apply(y_hat[ind_pos_valid,], 2, function(y) sum(y != 1))
            results_df[row_idx, "valid_true_negatives"] 	= apply(y_hat[ind_neg_valid,], 2, function(y) sum(y != 1))
            results_df[row_idx, "valid_false_positives"] = apply(y_hat[ind_neg_valid,], 2, function(y) sum(y == 1))
        }

        #accuracy metrics on testing set
        y_hat  = round(predict(model, newx = X_test, type="response"));
        y_hat = y_hat[, lambda_increasing_idx]; #reorder
        results_df[row_idx, "test_true_positives"]   = apply(y_hat[ind_pos_test,], 2, function(y) sum(y == 1))
        results_df[row_idx, "test_false_negatives"]  = apply(y_hat[ind_pos_test,], 2, function(y) sum(y != 1))
        results_df[row_idx, "test_true_negatives"] 	= apply(y_hat[ind_neg_test,], 2, function(y) sum(y != 1))
        results_df[row_idx, "test_false_positives"]  = apply(y_hat[ind_neg_test,], 2, function(y) sum(y == 1))

        #run time metrics
        results_df[row_idx, "runtime"] = runtime[["elapsed"]]/length(row_idx);
        #divide by total # of instances trained in one call since glmnet trains nlambda models at the same time

        #model size metrics
        coefficients = as.array(coef(model));
        coefficients = coefficients[, lambda_increasing_idx]; #reorder
        variable_names = rownames(coefficients);
        results_df[row_idx, "model_size"] = apply(coefficients[variable_names != "(Intercept)", ], 2, function(b) sum(b != 0.0));

        #store coefficients to print in reports
        if (method_settings$save_print_models){
            models = as.list(as.data.frame(coefficients)); #convert coefficients matrix to a list (1 row = 1 model)
            model_ids = results_df[row_idx,]$print_model_id
            names(models) = model_ids
            for (model_id in model_ids){
                names(models[[model_id]]) = variable_names;
                print_models[[model_id]] = models[[model_id]]
            }
        }

        #full model objects (for debugging or other use)
        if (method_settings$save_debug_models){
            model_ids = results_df[row_idx,]$debug_model_id;
            for (model_id in model_ids){
                debug_models[[model_id]] = model;
            }
        }
    }

    #build output
    results = list();
    results$method_name = method_name;
    results$method_settings = method_settings;
    results$results_df = results_df;
    results$print_models = print_models;
    results$debug_models = debug_models;

    #print completion message
    training_end_time = proc.time() - training_start_time;
    results$total_runtime = training_end_time[["elapsed"]]
    print.to.console(sprintf("Trained %d instances in %1.1f seconds", results$results_df %>% nrow(), results$total_runtime));
    print.method.is.ending.message(method_name);

    return(results)
}

train.lars.elasticnet = function(method_settings){
    #runs L1/L2 penalized logistic regression models using glmnet for many values of alpha
    #returns K + 1 models at each value of alpha and lambda (K-fold CV + one full model)
    #global variable inputs include X, Y, folds, class_weights

    alpha_values = method_settings$alpha_values;
    subresults = vector("list", length(alpha_values));
    start_time = proc.time();
    for (i in 1:length(alpha_values)){
        subresults[[i]] = train.lars(method_settings, alpha_values[i])
    }
    runtime = proc.time() - start_time

    #merge results_df and frame
    results = merge.results.and.models(results_df_list = lapply(subresults, function(r) r$results_df),
                                       print_models_list = lapply(subresults, function(r) r$print_models),
                                       debug_models_list = lapply(subresults, function(r) r$debug_models))

    results$method_name = method_name;
    results$method_settings = method_settings;
    results$total_runtime = runtime[["elapsed"]];
    return(results);
}

train.svm = function(method_settings, kernel_type = "linear"){
    #runs SVM using the e1071 package
    #returns K+1 models at each free parameter instance
    #inputs include: X, Y, folds, class_weights as global variables

    print.method.is.starting.message(method_name);
    training_start_time = proc.time();

    if (!weighted){
        class.weights = c(1,1);
        names(class.weights) = c("0","1");
    } else {
        class.weights = 2*class_weights;
        names(class.weights) = c("0","1");
    }

    #initialize results df (each row contains results from glmnet)
    results_param = method_settings$param;
    results_df = create.results.df(results_param, K);
    n_instances = results_df %>% nrow()
    print_models = setNames(vector("list", n_instances), results_df$print_model_id)
    debug_models = setNames(vector("list", n_instances), results_df$debug_model_id)

    #train instances
    for (row_idx in 1:n_instances){

        run_param_row  = results_df[row_idx,] %>% select(fold, starts_with("parameter"));
        run_param_list = convert.parameter.df.row.to.list(run_param_row);

        #initialize training dataset
        train_ind       = !(folds == run_param_row$fold);
        X_train         = as.matrix(X[train_ind,]);
        Y_train   	    = as.factor(Y[train_ind]);
        ind_pos_train	= Y_train == 1;
        ind_neg_train	= !ind_pos_train;

        #initialize validation dataset
        has_validation_set = run_param_row$fold > 0
        valid_ind       = folds == run_param_row$fold;
        X_valid         = as.matrix(X[valid_ind,]);
        Y_valid    		= as.factor(Y[valid_ind]);
        ind_pos_valid	= Y_valid == 1;
        ind_neg_valid	= !ind_pos_valid;

        #train model
        call_inputs = c(run_param_list,
                        list(x = X_train,
                             y = Y_train,
                             type = "C-classification",
                             class.weights = class.weights,
                             na.action = na.fail,
                             kernel= kernel_type))

        call_start_time = proc.time();
        model = do.call(svm, call_inputs);
        runtime = (proc.time() - call_start_time);

        #accuracy metrics on training set
        y_hat = predict(model, newdata = X_train)
        results_df[row_idx, "train_true_positives"] = sum(y_hat[ind_pos_train]==1);
        results_df[row_idx, "train_false_negatives"] = sum(y_hat[ind_pos_train]!=1);
        results_df[row_idx, "train_true_negatives"] = sum(y_hat[ind_neg_train]!=1);
        results_df[row_idx, "train_false_positives"] = sum(y_hat[ind_neg_train]==1);

        #accuracy metrics on validation set
        if (has_validation_set){
            y_hat = predict(model, newdata = X_valid)
            results_df[row_idx, "valid_true_positives"] = sum(y_hat[ind_pos_valid]==1);
            results_df[row_idx, "valid_false_negatives"] = sum(y_hat[ind_pos_valid]!=1);
            results_df[row_idx, "valid_true_negatives"] = sum(y_hat[ind_neg_valid]!=1);
            results_df[row_idx, "valid_false_positives"] = sum(y_hat[ind_neg_valid]==1);
        }

        #accuracy metrics on testing set
        y_hat = predict(model, newdata = X_test)
        results_df[row_idx, "test_true_positives"] = sum(y_hat[ind_pos_test]==1);
        results_df[row_idx, "test_false_negatives"] = sum(y_hat[ind_pos_test]!=1);
        results_df[row_idx, "test_true_negatives"] = sum(y_hat[ind_neg_test]!=1);
        results_df[row_idx, "test_false_positives"] = sum(y_hat[ind_neg_test]==1);

        #run time metrics
        results_df[row_idx, "runtime"] = runtime[["elapsed"]]

        #model size metrics
        if (kernel_type == "linear"){
            coefficients = rbind("(Intercept)"= -model$rho, t(t(model$coefs) %*% X_train[model$index,]))
            variable_names = rownames(coefficients);
            results_df[row_idx, "model_size"] = sum(coefficients[variable_names != "(Intercept)", ] != 0.0)
        }

        #full model objects (for debugging or other use)
        if (kernel_type == "linear" && method_settings$save_print_models){
            model_id = results_df[row_idx,]$print_model_id
            print_models[[model_id]] = setNames(array(coefficients), variable_names)
        }

        if (method_settings$save_debug_models){
            model_id = results_df[row_idx,]$debug_model_id
            debug_models[[model_id]] = model;
        }
    }

    #build output
    results = list();
    results$method_name = method_name;
    results$method_settings = method_settings;
    results$results_df = results_df;
    results$print_models = print_models;
    results$debug_models = debug_models;

    #print completion message
    training_end_time = proc.time() - training_start_time;
    results$total_runtime = training_end_time[["elapsed"]]
    print.to.console(sprintf("Trained %d instances in %1.1f seconds", results$results_df %>% nrow(), results$total_runtime));
    print.method.is.ending.message(method_name);

    return(results);
}

train.randomforest = function(method_settings){
    #runs randomForest
    #returns K+1 models at each free parameter instance
    #inputs include: X, Y, folds, class_weights as global variables

    print.method.is.starting.message(method_name);
    training_start_time = proc.time();
    weights = 1*class_weights+0;

    #initialize results df (each row contains results from glmnet)
    results_param = method_settings$param;
    results_df = create.results.df(results_param, K);
    n_instances = results_df %>% nrow()
    print_models = setNames(vector("list", n_instances), results_df$print_model_id)
    debug_models = setNames(vector("list", n_instances), results_df$debug_model_id)

    #train instances
    for (row_idx in 1:n_instances){
        run_param_row = results_df[row_idx,] %>% select(fold, starts_with("parameter"))
        run_param_list = convert.parameter.df.row.to.list(run_param_row);

        #initialize training dataset
        train_ind       = !(folds == run_param_row$fold);
        X_train         = as.matrix(X[train_ind,]);
        Y_train   	    = as.factor(Y[train_ind]);
        ind_pos_train	= Y_train == 1;
        ind_neg_train	= !ind_pos_train;

        #initialize validation dataset
        has_validation_set = run_param_row$fold > 0
        valid_ind       = folds == run_param_row$fold;
        X_valid         = as.matrix(X[valid_ind,]);
        Y_valid    		= as.factor(Y[valid_ind]);
        ind_pos_valid	= Y_valid == 1;
        ind_neg_valid	= !ind_pos_valid;

        #update sampesize parameter
        if ("sampsize" %in% names(run_param_list)){
            run_param_list$sampsize = ceiling(run_param_list$sampsize * nrow(X_train))
        }

        #train model
        call_inputs = c(run_param_list,
                        list(x = X_train,
                             y = Y_train,
                             classwt = weights));

        call_start_time = proc.time();
        model = do.call(randomForest, call_inputs);
        runtime = (proc.time() - call_start_time);

        #accuracy metrics on training set
        y_hat = predict(model, newdata = X_train)
        results_df[row_idx, "train_true_positives"] = sum(y_hat[ind_pos_train]==1);
        results_df[row_idx, "train_false_negatives"] = sum(y_hat[ind_pos_train]!=1);
        results_df[row_idx, "train_true_negatives"] = sum(y_hat[ind_neg_train]!=1);
        results_df[row_idx, "train_false_positives"] = sum(y_hat[ind_neg_train]==1);

        #accuracy metrics on validation set
        if (has_validation_set){
            y_hat = predict(model, newdata = X_valid)
            results_df[row_idx, "valid_true_positives"] = sum(y_hat[ind_pos_valid]==1);
            results_df[row_idx, "valid_false_negatives"] = sum(y_hat[ind_pos_valid]!=1);
            results_df[row_idx, "valid_true_negatives"] = sum(y_hat[ind_neg_valid]!=1);
            results_df[row_idx, "valid_false_positives"] = sum(y_hat[ind_neg_valid]==1);
        }

        #accuracy metrics on testing set
        y_hat = predict(model, newdata = X_test)
        results_df[row_idx, "test_true_positives"] = sum(y_hat[ind_pos_test]==1);
        results_df[row_idx, "test_false_negatives"] = sum(y_hat[ind_pos_test]!=1);
        results_df[row_idx, "test_true_negatives"] = sum(y_hat[ind_neg_test]!=1);
        results_df[row_idx, "test_false_positives"] = sum(y_hat[ind_neg_test]==1);

        #run time metrics
        results_df[row_idx, "runtime"] = runtime[["elapsed"]]

        #full model objects (for debugging or other use)
        if (method_settings$save_print_models){
            #cannot do anything
        }

        if (method_settings$save_debug_models){
            model_id = results_df[row_idx,]$debug_model_id
            debug_models[[model_id]] = model;
        }
    }
    #build output
    results = list();
    results$method_name = method_name;
    results$method_settings = method_settings;
    results$results_df = results_df;
    results$print_models = print_models;
    results$debug_models = debug_models;

    #print completion message
    training_end_time = proc.time() - training_start_time;
    results$total_runtime = training_end_time[["elapsed"]]
    print.to.console(sprintf("Trained %d instances in %1.1f seconds", results$results_df %>% nrow(), results$total_runtime));
    print.method.is.ending.message(method_name);
    return(results);
}

train.sgb = function(method_settings){
    #runs boosting using the gbm package
    #returns K+1 models at each free parameter instance
    #inputs include: X, Y, folds, class_weights as global variables

    print.method.is.starting.message(method_name);
    training_start_time = proc.time();
    weights = rep(1, nrow(X));
    if (weighted){
        weights[Y!=1] = class_weights[1];
        weights[Y==1] = class_weights[2];
    }

    #initialize results df (each row contains results from glmnet)
    results_param = method_settings$param;
    results_df = create.results.df(results_param, K);
    n_instances = results_df %>% nrow()
    print_models = setNames(vector("list", n_instances), results_df$print_model_id)
    debug_models = setNames(vector("list", n_instances), results_df$debug_model_id)
    test_data   = data.frame(cbind(X_test,"y"=Y_test));

    #train instances
    for (row_idx in 1:n_instances){

        run_param_row = results_df[row_idx,] %>% select(fold, starts_with("parameter"))
        run_param_list = convert.parameter.df.row.to.list(run_param_row);

        #initialize training dataset
        train_ind       = !(folds == run_param_row$fold);
        train_data      = as.data.frame(cbind(X[train_ind,],"y"= Y[train_ind]));
        ind_pos_train	= Y[train_ind]==1;
        ind_neg_train	= !ind_pos_train;

        #initialize validation dataset
        has_validation_set = run_param_row$fold>0
        valid_ind       = folds == run_param_row$fold;
        valid_data      = as.data.frame(cbind(X[valid_ind,],"y"= Y[valid_ind]))
        ind_pos_valid	= Y[valid_ind] == 1;
        ind_neg_valid	= !ind_pos_valid;

        #train model
        call_inputs = c(run_param_list,
                        list(formula = y~.,
                             data = train_data,
                             weights = weights[train_ind],
                             keep.data = FALSE,
                             distribution = "adaboost",
                             n.minobsinnode = 10,
                             n.cores = 1));

        call_start_time = proc.time();
        model = do.call(gbm, call_inputs);
        runtime = (proc.time() - call_start_time);

        #accuracy metrics on training set
        y_hat = round(predict.gbm(model, newdata = train_data, n.trees = model$n.trees, type='response'))
        results_df[row_idx, "train_true_positives"] = sum(y_hat[ind_pos_train]==1);
        results_df[row_idx, "train_false_negatives"] = sum(y_hat[ind_pos_train]!=1);
        results_df[row_idx, "train_true_negatives"] = sum(y_hat[ind_neg_train]!=1);
        results_df[row_idx, "train_false_positives"] = sum(y_hat[ind_neg_train]==1);

        #accuracy metrics on validation set
        if (has_validation_set){
            y_hat = round(predict.gbm(model, newdata = valid_data, n.trees = model$n.trees, type='response'))
            results_df[row_idx, "valid_true_positives"] = sum(y_hat[ind_pos_valid]==1);
            results_df[row_idx, "valid_false_negatives"] = sum(y_hat[ind_pos_valid]!=1);
            results_df[row_idx, "valid_true_negatives"] = sum(y_hat[ind_neg_valid]!=1);
            results_df[row_idx, "valid_false_positives"] = sum(y_hat[ind_neg_valid]==1);
        }

        #accuracy metrics on testing set
        y_hat = round(predict(model, newdata = test_data, n.trees = model$n.trees, type='response'))
        results_df[row_idx, "test_true_positives"] = sum(y_hat[ind_pos_test]==1);
        results_df[row_idx, "test_false_negatives"] = sum(y_hat[ind_pos_test]!=1);
        results_df[row_idx, "test_true_negatives"] = sum(y_hat[ind_neg_test]!=1);
        results_df[row_idx, "test_false_positives"] = sum(y_hat[ind_neg_test]==1);

        #run time metrics
        results_df[row_idx, "runtime"] = runtime[["elapsed"]]

        #full model objects (for debugging or other use)
        if (method_settings$save_print_models){
            #cannot do anything
        }

        if (method_settings$save_debug_models){
            model_id = results_df[row_idx,]$debug_model_id
            debug_models[[model_id]] = model;
        }
    }

    #build output
    results = list();
    results$method_name = method_name;
    results$method_settings = method_settings;
    results$results_df = results_df;
    results$print_models = print_models;
    results$debug_models = debug_models;

    #print completion message
    training_end_time = proc.time() - training_start_time;
    results$total_runtime = training_end_time[["elapsed"]]
    print.to.console(sprintf("Trained %d instances in %1.1f seconds", results$results_df %>% nrow(), results$total_runtime));
    print.method.is.ending.message(method_name);
    return(results)
}

train.c50 = function(method_settings, type = "tree"){
    #trains c5.0 models using the C5.0 package
    #returns K+1 models at each free parameter instance
    #type = "tree" returns decision trees (c50_tree)
    #type = "rules"returns rule sets (c50_rule)
    #inputs include: X, Y, folds, class_weights as global variables

    print.method.is.starting.message(method_name);
    training_start_time = proc.time();

    cost_matrix 	= matrix(c(0,1.0,1.0,0), byrow=TRUE, nrow=2, dimnames=list(c("0","1"),c("0","1")));
    if (weighted) {
        cost_matrix 	= matrix(c(0,class_weights[1],class_weights[2],0), byrow=TRUE, nrow=2, dimnames=list(c("0","1"),c("0","1")));
    }

    #initialize results df (each row contains results from glmnet)
    results_param = method_settings$param;
    results_df = create.results.df(results_param, K);
    n_instances = results_df %>% nrow()
    print_models = setNames(vector("list", n_instances), results_df$print_model_id)
    debug_models = setNames(vector("list", n_instances), results_df$debug_model_id)

    #train instances
    for (row_idx in 1:n_instances){

        run_param_row  = results_df[row_idx,] %>% select(fold, starts_with("parameter"));
        run_param_list = convert.parameter.df.row.to.list(run_param_row);

        #initialize training dataset
        train_ind       = !(folds == run_param_row$fold);
        X_train         = as.matrix(X[train_ind,]);
        Y_train   	    = as.factor(Y[train_ind]);
        ind_pos_train	= Y_train == 1;
        ind_neg_train	= !ind_pos_train;

        #initialize validation dataset
        has_validation_set = run_param_row$fold > 0
        valid_ind       = folds == run_param_row$fold;
        X_valid         = as.matrix(X[valid_ind,]);
        Y_valid    		= as.factor(Y[valid_ind]);
        ind_pos_valid	= Y_valid == 1;
        ind_neg_valid	= !ind_pos_valid;

        #train model
        call_inputs = list(x = X_train,
                           y = Y_train,
                           rules = (type == "rule"),
                           costs=t(cost_matrix),
                           control = do.call(C5.0Control, run_param_list))

        call_start_time = proc.time();
        model = do.call(C5.0, call_inputs);
        runtime = (proc.time() - call_start_time);

        #accuracy metrics on training set
        y_hat = predict(model, newdata = X_train)
        results_df[row_idx, "train_true_positives"] = sum(y_hat[ind_pos_train]==1);
        results_df[row_idx, "train_false_negatives"] = sum(y_hat[ind_pos_train]!=1);
        results_df[row_idx, "train_true_negatives"] = sum(y_hat[ind_neg_train]!=1);
        results_df[row_idx, "train_false_positives"] = sum(y_hat[ind_neg_train]==1);

        #accuracy metrics on validation set
        if (has_validation_set){
            y_hat = predict(model, newdata = X_valid)
            results_df[row_idx, "valid_true_positives"] = sum(y_hat[ind_pos_valid]==1);
            results_df[row_idx, "valid_false_negatives"] = sum(y_hat[ind_pos_valid]!=1);
            results_df[row_idx, "valid_true_negatives"] = sum(y_hat[ind_neg_valid]!=1);
            results_df[row_idx, "valid_false_positives"] = sum(y_hat[ind_neg_valid]==1);
        }

        #accuracy metrics on testing set
        y_hat = predict(model, newdata = X_test)
        results_df[row_idx, "test_true_positives"] = sum(y_hat[ind_pos_test]==1);
        results_df[row_idx, "test_false_negatives"] = sum(y_hat[ind_pos_test]!=1);
        results_df[row_idx, "test_true_negatives"] = sum(y_hat[ind_neg_test]!=1);
        results_df[row_idx, "test_false_positives"] = sum(y_hat[ind_neg_test]==1);

        #run time metrics
        results_df[row_idx, "runtime"] = runtime[["elapsed"]]

        #model metrics
        results_df[row_idx, "model_size"] = model$size;

        #model objects
        if (method_settings$save_print_models){
            model_id = results_df[row_idx,]$print_model_id
            print_models[[model_id]] = summary(model)
        }

        if (method_settings$save_debug_models){
            model_id = results_df[row_idx,]$debug_model_id
            debug_models[[model_id]] = model;
        }
    }

    #build output
    results = list();
    results$method_name = method_name;
    results$method_settings = method_settings;
    results$results_df = results_df;
    results$print_models = print_models;
    results$debug_models = debug_models;

    #print completion message
    training_end_time = proc.time() - training_start_time;
    results$total_runtime = training_end_time[["elapsed"]]
    print.to.console(sprintf("Trained %d instances in %1.1f seconds", results$results_df %>% nrow(), results$total_runtime));
    print.method.is.ending.message(method_name);
    return(results)
}

train.cart = function(method_settings){
    #trains cart models using the rpart package
    #returns K+1 models at each free parameter instance

    print.method.is.starting.message(method_name);
    training_start_time = proc.time();

    #need to explicitly label the outcome variable for model printing
    ylabels = c("y = 0", "y = 1")
    cost_matrix = matrix(c(0,1.0,1.0,0), byrow=TRUE, nrow=2, dimnames=list(ylabels, ylabels))
    if (weighted) {
        cost_matrix = matrix(c(0,class_weights[1],class_weights[2],0), byrow=TRUE, nrow=2, dimnames=list(ylabels,ylabels));
    }

    #initialize results df (each row contains results from glmnet)
    results_param = method_settings$param;
    results_df = create.results.df(results_param, K);
    n_instances = results_df %>% nrow()
    print_models = setNames(vector("list", n_instances), results_df$print_model_id)
    debug_models = setNames(vector("list", n_instances), results_df$debug_model_id)

    test_data = cbind(as.data.frame(X_test), data.frame("y" = ylabels[Y_test+1]));

    #train instances
    for (row_idx in 1:n_instances){

        run_param_row  = results_df[row_idx,] %>% select(fold, starts_with("parameter"));
        run_param_list = convert.parameter.df.row.to.list(run_param_row);

        #initialize training dataset
        train_ind       = !(folds == run_param_row$fold);
        train_data      = cbind(as.data.frame(X[train_ind,]), data.frame("y" = ylabels[Y[train_ind]+1]));
        ind_pos_train	= Y[train_ind] == 1;
        ind_neg_train	= !ind_pos_train;

        #initialize validation dataset
        valid_ind       = folds == run_param_row$fold;
        valid_data      = cbind(as.data.frame(X[valid_ind,]), data.frame("y" = ylabels[Y[valid_ind]+1]));
        ind_pos_valid	= Y[valid_ind] == 1;
        ind_neg_valid	= !ind_pos_valid;

        has_validation_set = run_param_row$fold > 0

        #train model
        call_inputs = list(formula = y ~ .,
                           method = "class",
                           data = train_data,
                           parms=list(loss=cost_matrix),
                           x = FALSE,
                           y = FALSE,
                           na.action = na.fail,
                           control = do.call(rpart.control, run_param_list))

        failed_run = FALSE;
        call_start_time = proc.time();
        tryCatch({
            model = do.call(rpart, call_inputs)
        }, error = function(e) {
            model = NULL;
            failed_run = TRUE;
            print.to.console(sprintf("error: %s",e));
        });
        runtime = (proc.time() - call_start_time);

        #accuracy metrics on training set
        if (!failed_run){
            y_hat = c(0,1)[predict(model, newdata = train_data, type = "class")]
            results_df[row_idx, "train_true_positives"] = sum(y_hat[ind_pos_train]==1);
            results_df[row_idx, "train_false_negatives"] = sum(y_hat[ind_pos_train]!=1);
            results_df[row_idx, "train_true_negatives"] = sum(y_hat[ind_neg_train]!=1);
            results_df[row_idx, "train_false_positives"] = sum(y_hat[ind_neg_train]==1);

            #accuracy metrics on validation set
            if (has_validation_set){
                y_hat = c(0,1)[predict(model, newdata = valid_data, type = "class")]
                results_df[row_idx, "valid_true_positives"] = sum(y_hat[ind_pos_valid]==1);
                results_df[row_idx, "valid_false_negatives"] = sum(y_hat[ind_pos_valid]!=1);
                results_df[row_idx, "valid_true_negatives"] = sum(y_hat[ind_neg_valid]!=1);
                results_df[row_idx, "valid_false_positives"] = sum(y_hat[ind_neg_valid]==1);
            }

            #accuracy metrics on testing set
            y_hat = c(0,1)[predict(model, newdata = test_data, type = "class")]
            results_df[row_idx, "test_true_positives"] = sum(y_hat[ind_pos_test]==1);
            results_df[row_idx, "test_false_negatives"] = sum(y_hat[ind_pos_test]!=1);
            results_df[row_idx, "test_true_negatives"] = sum(y_hat[ind_neg_test]!=1);
            results_df[row_idx, "test_false_positives"] = sum(y_hat[ind_neg_test]==1);

            #other metrics
            results_df[row_idx, "runtime"] = runtime[["elapsed"]];
            results_df[row_idx, "model_size"] = sum(model$frame$var=="<leaf>");

            #model objects
            if (method_settings$save_print_models){
                model_id = results_df[row_idx,]$print_model_id;
                print_model = list();
                class(print_model) = class(model);
                print_model$frame = model$frame;
                print_model$method = model$method;
                print_models[[model_id]] = print_model;
            }

            if (method_settings$save_debug_models){
                model_id = results_df[row_idx,]$debug_model_id
                debug_models[[model_id]] = model;
            }
        }
    }

    #build output
    results = list();
    results$method_name = method_name;
    results$method_settings = method_settings;
    results$results_df = results_df;
    results$print_models = print_models;
    results$debug_models = debug_models;

    #print completion message
    training_end_time = proc.time() - training_start_time;
    results$total_runtime = training_end_time[["elapsed"]]
    print.to.console(sprintf("Trained %d instances in %1.1f seconds", results$results_df %>% nrow(), results$total_runtime));
    print.method.is.ending.message(method_name);
    return(results);
}

##### error checking #####

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

check.results.rep =  function(results, count_table){

    fnames = names(results);
    stopifnot("method_name" %in% fnames);
    stopifnot("results_df" %in% fnames);
    stopifnot("print_models" %in% fnames);
    stopifnot("debug_models" %in% fnames);
    stopifnot("total_runtime" %in% fnames);
    stopifnot("method_settings" %in% fnames);

    method_name = results$method_name;
    results_df = results$results_df;
    print_models = results$print_models;
    debug_models = results$print_models;
    method_settings = results$method_settings;

    #check that the total number of runs = n_parameter_combinations x n_folds
    n_folds = max(results_df$fold)
    n_parameter_combinations = results_df %>% select(starts_with("parameter_")) %>% distinct() %>% nrow()
    n_runs = results_df %>% select(fold, starts_with("parameter_")) %>% distinct() %>% nrow()
    stopifnot(n_runs == (n_folds+1)*n_parameter_combinations)

    #check that each fold has the same number of runs
    chk_df = results_df %>% group_by(fold) %>% summarize(n_runs=n())
    stopifnot(all(chk_df$n_runs==n_parameter_combinations))

    #check that each parameter combination has K + 1 models
    parameter_colnames = results_df %>% select(starts_with("parameter_")) %>% colnames()
    dots <- lapply(parameter_colnames, as.symbol)
    chk_df = results_df %>% group_by_(.dots = dots) %>% summarize(n=n())
    stopifnot(all(chk_df$n == (K+1)))

    #check run time metrics
    stopifnot(results$total_runtime >= 0.0);
    stopifnot(results_df$runtime >= 0.0)

    #join results_df onto count_table
    results_df = left_join(results_df, count_table, by = "fold")

    #check model size metrics
    linear_methods = c("lars_lasso", "lars_ridge", "lars_elasticnet", "svm_linear")
    blackbox_methods = c("sgb","randomforest","svm_rbf")
    if (method_name %in% linear_methods){
        stopifnot(all(results_df$model_size >= 0));
        stopifnot(all(results_df$model_size <= results_df$P));
    } else if (method_name %in% blackbox_methods) {
        stopifnot(all(is.na(results_df$model_size)));
    };

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
