###### Library Stuff
library(dplyr)
library(xtable)
library(ggplot2)
library(grid)
library(scales)
library(reshape2)
library(stringr)

##### Personal Sanity #####
view.columns = function(df){
  cat(paste0(colnames(df),sep="\n",collapse=NULL))
}

set.default = function(setting_list,setting_name,default_value){
	if(!(setting_name %in% names(setting_list))){
		setting_list[[setting_name]] = default_value;
	}
	return(setting_list)
}

is.field = function(my_list,my_field){
	if (!is.list(my_list)){
		cat(sprintf("user did not pass a list variable\n"))
	} else {
		return(my_field %in% names(my_list))
	}
}

get.field.or.set.default = function(my_list,my_field,default_value){

	if (is.field(my_list,my_field)){
		return(my_list[[my_field]])
	} else {
		return(default_value)
	}
}

print.int.array = function(my_array){
	array_string = sprintf("(%s)",paste(sprintf("%d",my_array),collapse=','));
	return(array_string)
}

find.which = function(full_list,values){

  #get which
  ind = match(full_list,values)

  #drop NA
  ind = found[!is.na(found)]

  return(ind)
}

safe.dir = function(dir_name){
  last_char = substr(dir_name,nchar(dir_name),nchar(dir_name));
  if (last_char != "/") {
    dir_name = paste0(dir_name,"/");
  }
  return(dir_name);
}

##### AUC Calculation ######

get.auc = function(df,type="training"){

  if (type=="training"){
    pts = df[,c("training_fpr","training_tpr")];
  } else if (type=="testing"){
    pts = df[,c("testing_fpr","testing_tpr")];
  }
  colnames(pts) = c("x","y")

  #add endpoints
  pts = rbind(pts,
              c(0.00,0.00),
              c(1.00,1.00));

  #filter to unique rows only
  pts = unique(pts);
  n   = nrow(pts);

  #sort by fpr/tpr pairs
  pts = arrange(pts,x,y)

  x = pts$x;
  y = pts$y;

  auc = sum((x[2:n] - x[1:(n-1)]) * (y[2:n]+y[1:(n-1)]))/2

  return(auc)

}

get.auc.stats = function(stats){

  #get granular information (fold-level+full)
  join_fields  = c("data_name",
                   "method_name",
                   "fold_id",
                   "xtra_id",
                   "hcon_id",
                   "w_pos",
                   "w_neg",
                   "parameter_name_1",
                   "parameter_value_1",
                   "parameter_name_2",
                   "parameter_value_2",
                   "parameter_name_3",
                   "parameter_value_3")

  stats_table = stats[,join_fields];

  auc_table     = join(x=stats_table,y=output$table,by=join_fields,type="left", match="all")

  #calculate AUC for each fold / full
  auc_fields  = c("data_name",
                  "fold_id",
#                  "hcon_id",
                  "method_name",
                  "type",
                  "fold")

  fold_table  = filter(auc_table,type=="fold")
  fold_table  = ddply(.data=fold_table,
                      .variables = auc_fields,
                      function(df) c(training_auc=get.auc(df,"training"),
                                     testing_auc=get.auc(df,"testing")));

  full_table  = filter(auc_table,type=="full");
  full_table  = ddply(.data=full_table,
                      .variables = auc_fields,
                      function(df) c(full_training_auc=get.auc(df,"training")))
  full_table$fold = NULL;
  full_table$type = NULL;

  #obtain K-fold AUC statistics
  summary_fields = c("training_auc","testing_auc")

  counter = 1;
  for (fname in summary_fields){

    summary_function = function(D) data.frame(setNames(list(min(D[[fname]]),
                                                            median(D[[fname]]),
                                                            max(D[[fname]]),
                                                            mean(D[[fname]]),
                                                            sd(D[[fname]]),
                                                            mean(D[[fname]])+sd(D[[fname]]),
                                                            mean(D[[fname]])-sd(D[[fname]])),
                                                       paste(fname,c("min",
                                                                     "med",
                                                                     "max",
                                                                     "mean",
                                                                     "sd",
                                                                     "mean_plus_sd",
                                                                     "mean_minus_sd"),
                                                             sep=".")
    ))

    fstats = ddply(.data = fold_table,
                   .variable = c("data_name",
                                 "fold_id",
                                 #"hcon_id",
                                 "method_name"),
                   function(D) summary_function(D))


    if (counter == 1){
      auc_stats = fstats;
    } else {
      auc_stats = join(x=auc_stats,y=fstats,by=c("data_name",
                                                 "fold_id",
                                                 #"hcon_id",
                                                 "method_name"),
                       type="left", match="all")
    }

    counter = counter + 1;

  }

  #join to produce all stats
  auc_stats = join(x=auc_stats,y=full_table,type="left",match="all")

  return(auc_stats)

}

get.auc.stats.new = function(stats,output){

  #get granular information (fold-level+full)
  join_fields  = c("data_name",
                   "method_name",
                   "fold_id",
                   "xtra_id",
                   "hcon_id",
                   "w_pos",
                   "w_neg",
                   "parameter_name_1",
                   "parameter_value_1",
                   "parameter_name_2",
                   "parameter_value_2",
                   "parameter_name_3",
                   "parameter_value_3")

  stats_table = stats[,join_fields];

  auc_table     = join(x=stats_table,y=output$table,by=join_fields,type="left", match="all")

  #calculate AUC for each fold / full
  auc_fields  = c("data_name",
                  "fold_id",
                  #                  "hcon_id",
                  "method_name",
                  "type",
                  "fold")

  fold_table  = filter(auc_table,type=="fold")
  fold_table  = ddply(.data=fold_table,
                      .variables = auc_fields,
                      function(df) c(training_auc=get.auc(df,"training"),
                                     testing_auc=get.auc(df,"testing")));

  full_table  = filter(auc_table,type=="full");
  full_table  = ddply(.data=full_table,
                      .variables = auc_fields,
                      function(df) c(full_training_auc=get.auc(df,"training")))
  full_table$fold = NULL;
  full_table$type = NULL;

  #obtain K-fold AUC statistics
  summary_fields = c("training_auc","testing_auc")

  counter = 1;
  for (fname in summary_fields){

    summary_function = function(D) data.frame(setNames(list(min(D[[fname]]),
                                                            median(D[[fname]]),
                                                            max(D[[fname]]),
                                                            mean(D[[fname]]),
                                                            sd(D[[fname]]),
                                                            mean(D[[fname]])+sd(D[[fname]]),
                                                            mean(D[[fname]])-sd(D[[fname]])),
                                                       paste(fname,c("min",
                                                                     "med",
                                                                     "max",
                                                                     "mean",
                                                                     "sd",
                                                                     "mean_plus_sd",
                                                                     "mean_minus_sd"),
                                                             sep=".")
    ))

    fstats = ddply(.data = fold_table,
                   .variable = c("data_name",
                                 "fold_id",
                                 #"hcon_id",
                                 "method_name"),
                   function(D) summary_function(D))


    if (counter == 1){
      auc_stats = fstats;
    } else {
      auc_stats = join(x=auc_stats,y=fstats,by=c("data_name",
                                                 "fold_id",
                                                 #"hcon_id",
                                                 "method_name"),
                       type="left", match="all")
    }

    counter = counter + 1;

  }

  #join to produce all stats
  auc_stats = join(x=auc_stats,y=full_table,type="left",match="all")

  return(auc_stats)
}

get.auc.stats.w.fpr.cutoffs = function(stats,fpr_cutoffs=seq(0.05,1.00,by=0.05)){

  #get granular information (fold-level+full)
  key_fields = c("data_name",
                 "fold_id",
                 "xtra_id",
                 "hcon_id",
                 "method_name");

  join_fields  = c("data_name",
                   "method_name",
                   "fold_id",
                   "xtra_id",
                   "hcon_id",
                   "w_pos",
                   "w_neg",
                   "parameter_name_1",
                   "parameter_value_1",
                   "parameter_name_2",
                   "parameter_value_2",
                   "parameter_name_3",
                   "parameter_value_3")

  stats_table = stats[,join_fields];

  auc_table     = join(x=stats_table,y=output$table,by=join_fields,type="left", match="all")

  #calculate AUC for each fold / full
  auc_fields  = c(key_fields,
                  "type",
                  "fold")

  fold_table  = filter(auc_table,type=="fold")
  fold_table  = ddply(.data=fold_table,
                      .variables = auc_fields,
                      function(df) c(training_auc=get.auc(df,"training"),
                                     testing_auc=get.auc(df,"testing")));

  full_table  = filter(auc_table,type=="full");
  full_table  = ddply(.data=full_table,
                      .variables = auc_fields,
                      function(df) c(full_training_auc=get.auc(df,"training")))
  full_table$fold = NULL;
  full_table$type = NULL;

  #obtain K-fold AUC statistics
  summary_fields = c("training_auc","testing_auc")

  counter = 1;
  for (fname in summary_fields){

    summary_function = function(D) data.frame(setNames(list(min(D[[fname]]),
                                                            median(D[[fname]]),
                                                            max(D[[fname]]),
                                                            mean(D[[fname]]),
                                                            sd(D[[fname]]),
                                                            mean(D[[fname]])+sd(D[[fname]]),
                                                            mean(D[[fname]])-sd(D[[fname]])),
                                                       paste(fname,c("min",
                                                                     "med",
                                                                     "max",
                                                                     "mean",
                                                                     "sd",
                                                                     "mean_plus_sd",
                                                                     "mean_minus_sd"),
                                                             sep=".")
    ))

    fstats = ddply(.data = fold_table,
                   .variable = key_fields,
                   function(D) summary_function(D))


    if (counter == 1){
      auc_stats = fstats;
    } else {
      auc_stats = join(x=auc_stats,y=fstats,by=key_fields,type="left", match="all")
    }
    counter = counter + 1;
  }

  #join to produce all stats
  auc_stats = join(x=auc_stats,y=full_table,type="left",match="all")
}

get.roc.stats.w.fpr.cutoffs = function(stats,fpr_cutoffs=seq(0.05,1.00,by=0.05),key_fields=c("data_name","fold_id","method_name"),cutoff_field="training_fpr.max"){

    roc_stats                = stats[0,];
    roc_stats$max_fpr_cutoff = numeric(0);

    for (max_fpr in fpr_cutoffs){

        this_stats = stats[stats[[cutoff_field]]<=max_fpr,]

        if (nrow(this_stats)>0) {
            this_stats = ddply(.data= this_stats,
                               .variables = key_fields,
                               function(df) df[which.max(df$testing_tpr.mean),]);

            this_stats$max_fpr_cutoff = max_fpr;

            roc_stats = rbind(roc_stats,this_stats);
        }
    }

    return(roc_stats)

}

##### Report Creation Function #####
open.pdf  = function(pdf_file){
  system(sprintf("open \"%s\"", pdf_file))
}

merge.pdfs =  function(files_to_merge,merged_file_name="report.pdf",open_after=TRUE){
  system(sprintf("pdftk \"%s\" cat output \"%s\"",paste(files_to_merge,collapse='\" \"'),merged_file_name))
  if (open_after){
    system(sprintf("open \"%s\"", merged_file_name))
  }
}

jam.pdfs =  function(files_to_merge, merged_file_name="report.pdf", rows = NULL, cols = 2, landscape = FALSE, open_after=TRUE, delete_originals=FALSE){

    n_files = length(files_to_merge);
    if (n_files > 0){

        if (is.null(rows)){
            rows = n_files %% cols;
        }
        if (!grepl("*.pdf", merged_file_name)){
            merged_file_name = paste0(merged_file_name,".pdf");
        }
        size_string = sprintf("%dx%d",cols, rows)
        files_string = paste(files_to_merge, collapse='\" \"');

        if (landscape){
            system(sprintf("pdfjam \"%s\" --nup %s --landscape --outfile \"%s\"",files_string, size_string, merged_file_name))
        } else {
            system(sprintf("pdfjam \"%s\" --nup %s --no-landscape --outfile \"%s\"",files_string, size_string, merged_file_name))
        }

        if (delete_originals){
            file.remove(files_to_merge);
        }

        if (open_after){
            system(sprintf("open \"%s\"", merged_file_name))
        }
    }
}

knit.report = function(report_name,report_data,input_report_dir,output_report_dir,output_report_name,remove_pdf=FALSE,remove_tex=FALSE){

  #Doublecheck Dirs
  input_report_dir  = safe.dir(input_report_dir);
  output_report_dir = safe.dir(output_report_dir);

  #input files
  input_Rnw_name     			= paste0(input_report_dir,report_name,".Rnw");

  #output files and directories
  output_pdf_name 				= paste0(output_report_dir,output_report_name,".pdf");
  output_tex_name 				= paste0(output_report_dir,output_report_name,".tex");
  output_fig_dir   				= paste0(output_report_dir,"figure/");

  dir.create(output_report_dir,showWarnings = FALSE);
  dir.create(output_fig_dir,showWarnings = FALSE);

  #compile files
  compile_dir     = "~/knitr_temp/"
  compile_fig_dir = "~/knitr_temp/figure/"

  #Delete Old Director
  unlink(compile_dir,recursive=TRUE,force=TRUE)
  dir.create(compile_dir,showWarnings = FALSE);
  dir.create(compile_fig_dir,showWarnings = FALSE);

  #Setup Compiled Filenames
  compile_Rnw_name   			= paste0(compile_dir,report_name,".Rnw");
  compile_pdf_name     		= paste0(compile_dir,report_name,".pdf");
  compile_tex_name     		= paste0(compile_dir,report_name,".tex");

  #Copy Files from Working Directory to Compilation Directory
  file.copy(from = input_Rnw_name, to=compile_Rnw_name)

  #Knit Report
  current_dir = getwd();

  #tryCatch({

    setwd(compile_dir);

    knit2pdf(input=compile_Rnw_name,quiet=FALSE);

    #Copy Tex/PDF
    file.copy(from=compile_pdf_name,to=output_pdf_name,overwrite=TRUE);
    file.copy(from=compile_tex_name,to=output_tex_name,overwrite=TRUE);

    #Copy Figure Files
    fig_files=list.files(path=compile_fig_dir);
    file.copy(from=paste0(compile_fig_dir,fig_files),to=paste0(output_fig_dir,fig_files));

    #Return to Original Directory
    setwd(current_dir);

    #Delete Compiled Directories
    unlink(compile_dir,recursive=TRUE,force=TRUE);



  #}, error = function(e) {
  #  e
  #}, finally = {
  #})

  if (remove_tex){
    file.remove(output_tex_name)
  }

  if (remove_pdf) {
    file.remove(output_pdf_name)
    return(NULL)
  } else {
    return(output_pdf_name);
  }

}

###### Plotting Functions ######
paste.if.not.na = function(f1,f2){
  if (is.na(f2)){
    return(f1)
  } else if (is.na(f1)){
    return(f2)
  } else {
    return(paste(f1,f2,sep="."))
  }
}

get.point.summary = function(xinfo,yinfo,stats){

  xinfo = set.default(xinfo,"fmin",-Inf);
  xinfo = set.default(xinfo,"fmax",Inf);
  yinfo = set.default(yinfo,"fmin",-Inf);
  yinfo = set.default(yinfo,"fmax",Inf);

  fields = c(paste.if.not.na(xinfo$field,xinfo$mid),
             paste.if.not.na(xinfo$field,xinfo$min),
             paste.if.not.na(xinfo$field,xinfo$max),
             paste.if.not.na(yinfo$field,yinfo$mid),
             paste.if.not.na(yinfo$field,yinfo$min),
             paste.if.not.na(yinfo$field,yinfo$max))

  point_data = data.frame(names   = stats[,"method_name"]
                          ,x 		 = apply(as.array(stats[,fields[1]]),1,function(x) max(min(x,xinfo$fmax),xinfo$fmin))
                          ,xmin   = apply(as.array(stats[,fields[2]]),1,function(x) max(x,xinfo$fmin))
                          ,xmax   = apply(as.array(stats[,fields[3]]),1,function(x) min(x,xinfo$fmax))
                          ,y 		 = apply(as.array(stats[,fields[4]]),1,function(y) max(min(y,yinfo$fmax),yinfo$fmin))
                          ,ymin   = apply(as.array(stats[,fields[5]]),1,function(y) max(y,yinfo$fmin))
                          ,ymax   = apply(as.array(stats[,fields[6]]),1,function(y) min(y,yinfo$fmax))
                          ,stringsAsFactors="false")

  return(point_data);
}

get.path.summary = function (xinfo,yinfo,stats){

  xinfo = set.default(xinfo,"fmin",-Inf);
  xinfo = set.default(xinfo,"fmax",Inf);
  yinfo = set.default(yinfo,"fmin",-Inf);
  yinfo = set.default(yinfo,"fmax",Inf);

  fields = c(paste.if.not.na(xinfo$field,xinfo$mid),
             paste.if.not.na(xinfo$field,xinfo$min),
             paste.if.not.na(xinfo$field,xinfo$max),
             paste.if.not.na(yinfo$field,yinfo$mid),
             paste.if.not.na(yinfo$field,yinfo$min),
             paste.if.not.na(yinfo$field,yinfo$max))


  path_data = data.frame(names 	 = stats[,"method_name"]
                         ,x 		 = apply(as.array(stats[,fields[1]]),1,function(x) max(min(x,xinfo$fmax),xinfo$fmin))
                         ,xmin   = apply(as.array(stats[,fields[2]]),1,function(x) max(x,xinfo$fmin))
                         ,xmax   = apply(as.array(stats[,fields[3]]),1,function(x) min(x,xinfo$fmax))
                         ,y 		 = apply(as.array(stats[,fields[4]]),1,function(y) max(min(y,yinfo$fmax),yinfo$fmin))
                         ,ymin   = apply(as.array(stats[,fields[5]]),1,function(y) max(y,yinfo$fmin))
                         ,ymax   = apply(as.array(stats[,fields[6]]),1,function(y) min(y,yinfo$fmax))
  )

  return(path_data);

}

get.reg.summary = function (xinfo,yinfo,stats){

  xinfo = set.default(xinfo,"fmin",0);
  xinfo = set.default(xinfo,"fmax",1);
  yinfo = set.default(yinfo,"fmin",-Inf);
  yinfo = set.default(yinfo,"fmax",Inf);

  fields = c(paste(xinfo$field),
             paste(xinfo$field),
             paste(xinfo$field),
             paste(yinfo$field,yinfo$mid,sep="."),
             paste(yinfo$field,yinfo$min,sep="."),
             paste(yinfo$field,yinfo$max,sep="."))

  reg_data = data.frame(names 	 = stats[,"method_name"]
                        ,x 		 = apply(as.array(stats[,fields[1]]),1,function(x) max(min(x,xinfo$fmax),xinfo$fmin))
                        ,xmin   = apply(as.array(stats[,fields[2]]),1,function(x) max(x,xinfo$fmin))
                        ,xmax   = apply(as.array(stats[,fields[3]]),1,function(x) min(x,xinfo$fmax))
                        ,y 		 = apply(as.array(stats[,fields[4]]),1,function(y) max(min(y,yinfo$fmax),yinfo$fmin))
                        ,ymin   = apply(as.array(stats[,fields[5]]),1,function(y) max(y,yinfo$fmin))
                        ,ymax   = apply(as.array(stats[,fields[6]]),1,function(y) min(y,yinfo$fmax))
  )

  return(reg_data);

}

get.plot.settings = function(all_method_names = NULL,path_method_names=NULL){

	if (is.null(all_method_names)){
		all_method_names = c("slim",
												 "tilm",
												 "mnrules",
												 "pilm",
												 "lars_lasso",
												 "lars_lasso_hc",
												 "lars_ridge",
												 "lars_elasticnet",
												 "logreg",
												 "C5.0_rule",
												 "C5.0_tree",
												 "cart",
												 "randomforest",
												 "svm_linear",
												 "svm_rbf",
                         "trivial")
	}

	if (is.null(path_method_names)){
		path_method_names = c("slim","lars_lasso","lars_lasso_hc","lars_ridge","lars_elasticnet");
		path_method_names = intersect(path_method_names,all_method_names);
	}

	# Method Names / Labels / Colors
	n_methods 								= length(all_method_names);
	names(all_method_names) 	= all_method_names
	all_method_labels 				= all_method_names;
	all_method_colors 				= all_method_names;
	all_method_sizes    			= all_method_names;
	all_method_shapes   			= all_method_names;

	for (i in 1:n_methods){

		method_name = all_method_names[i]

		if (method_name=="lars_lasso" ) {
			method_label  = "Lasso";
			method_color  = "#E61A33";
      method_size   = 2;
      method_shape  = 15;
		} else if (method_name=="lars_lasso_hc") {
			#method_label = "Lasso (Constrained)";
			method_label = "Lasso";
			method_color = "#E61A33";
			method_size   = 2;
			method_shape  = 15;
		} else if (method_name=="lars_ridge") {
			method_label = "Ridge";
			method_color = "#FF8000";
			method_size   = 2;
			method_shape  = 15;
		} else if (method_name=="lars_elasticnet") {
			method_label = "E. Net";
			method_color = "#FFBF80";
			method_size   = 2;
			method_shape  = 15;
		} else if (method_name=="logreg") {
			method_label = "LR";
			method_color = "#CCBFFF";
			method_size   = 2;
			method_shape  = 15;
		} else if (method_name=="C5.0_rule") {
			method_label = "C5.0R";
			method_color = "#B2FF8C";
			method_size   = 2;
			method_shape  = 15;
		} else if (method_name=="C5.0_tree") {
			method_label = "C5.0T";
			method_color = "#33FF00";
			method_size   = 2;
			method_shape  = 15;
		} else if (method_name=="cart") {
			method_label = "CART";
			method_color = "#197319";
			method_size   = 2;
			method_shape  = 15;
		} else if (method_name=="svm_linear") {
			method_label = "SVM Lin.";
			method_color = "#887050";
			method_size   = 2;
			method_shape  = 15;
		} else if (method_name=="svm_rbf") {
			method_label = "SVM RBF";
			method_color = "#3d004c"
			method_size   = 2;
			method_shape  = 15;
		} else if (method_name=="randomforest"){
			method_label = "RF";
			method_color = "#a6edff";
			method_size   = 2;
			method_shape  = 15;
		} else if (method_name=="slim"){
			method_label = "SLIM";
			method_color = "#0066ff";
			method_size   = 8;
			method_shape  = 15;
		} else if (method_name=="mnrules") {
			method_label = "MN Rules";
			method_color = "#FFFF33";
			method_size   = 8;
			method_shape  = 15;
		} else if (method_name=="trivial") {
		  method_label = "Trivial";
		  method_color = "#000000";
		  method_size   = 8;
		  method_shape  = 15;
		} else {
			method_label = paste(sapply(strsplit(method_name,"_"),toupper),collapse=' ')
			method_color = "#000000"
			method_size   = 8;
			method_shape  = 15;
		}

		all_method_colors[i]  = method_color;
		all_method_labels[i]  = method_label;
		all_method_sizes[i]   = method_size;
		all_method_shapes[i]  = method_color;
	}

	# More colors here: http://www.color-hex.com/color-palette/2698
	# col = c("#FFBF80", "#FF8000", "#FFFF33","#B2FF8C","#33FF00","#A6EDFF","#1AB2FF","#CCBFFF","#664CFF", "#FF99BF","#E61A33", "#197319","#e3c26c","#887050")
	# image(1:length(col),1,matrix(1:length(col), ncol=1),col=col)


# OLD COLORS
# 	for (i in 1:n_methods){
#
# 	  method_name = all_method_names[i]
#
# 	  if (method_name=="lars_lasso" ) {
# 	    method_label = "Lasso";
# 	    method_color = "#E61A33";
# 	  } else if (method_name=="lars_lasso_hc") {
# 	    #method_label = "Lasso (HC)";
# 	    method_label = "Lasso";
# 	    method_color = "#E61A33";
# 	  } else if (method_name=="lars_ridge") {
# 	    method_label = "Ridge";
# 	    method_color = "#FF8000";
# 	  } else if (method_name=="lars_elasticnet") {
# 	    method_label = "E. Net";
# 	    method_color = "#FFBF80";
# 	  } else if (method_name=="logreg") {
# 	    method_label = "LR";
# 	    method_color = "#CCBFFF";
# 	  } else if (method_name=="C5.0_rule") {
# 	    method_label = "C5.0R";
# 	    method_color = "#B2FF8C";
# 	  } else if (method_name=="C5.0_tree") {
# 	    method_label = "C5.0T";
# 	    method_color = "#33FF00";
# 	  } else if (method_name=="cart") {
# 	    method_label = "CART";
# 	    method_color = "#197319";
# 	  } else if (method_name=="svm_linear") {
# 	    method_label = "SVM Lin.";
# 	    method_color = "#887050";
# 	  } else if (method_name=="svm_rbf") {
# 	    method_label = "SVM RBF.";
# 	    method_color = "#e3c26c";
# 	  } else if (method_name=="randomforest"){
# 	    method_label = "RF";
# 	    method_color = "#d2cc9b";
# 	  } else if (method_name=="slim"){
# 	    method_label = "SLIM";
# 	    method_color = "#664CFF";
# 	  } else if (method_name=="mnrules") {
# 	    method_label = "MN Rules";
# 	    method_color = "#FFFF33";
# 	  } else if (method_name=="tilm") {
# 	    method_label = "TILM";
# 	    method_color = "#1AB2FF";
# 	  } else if (method_name=="pilm") {
# 	    method_label = "PILM";
# 	    method_color = "#A6EDFF";
# 	  } else {
# 	    method_label = paste(sapply(strsplit(method_name,"_"),toupper),collapse=' ')
# 	    method_color = "#000000"
# 	  }
#
# 	  all_method_colors[i] = method_color;
# 	  all_method_labels[i] = method_label;
# 	}

	path_method_ind  				= sapply(path_method_names,function(x) which(x==all_method_names));
	path_method_labels 			= all_method_labels[path_method_ind];
	path_method_colors 			= all_method_colors[path_method_ind];


	# Graph Settings
	line_color = "#E9E9E9";

	my_theme = theme_bw() + theme(title 				= element_text(size=18,vjust=1.0)
																,axis.title.x = element_text(size=20,vjust=-1.0)
																,axis.text.x 	= element_text(size=16)
																,axis.ticks.x	= element_line(size=1.0,color=line_color)
																,axis.title.y = element_text(size=20,vjust=1.0)
																,axis.text.y 	= element_text(size=16)
																,axis.ticks.y	= element_line(size=1.0,color=line_color)
																,axis.line    = element_blank() #element_line(size=1.5,color="#E9E9E9")
																#,panel.background = element_rect(color="white")
																,panel.grid.major = element_line(linetype="solid",size=1.0,color=line_color)
																,panel.grid.minor = element_blank()
																,panel.border = element_rect(size=2.0,color=line_color)
																,plot.margin = unit(c(0.25,0,0.75,0.25), "cm")
																,legend.position="right"
																#,legend.margin = unit(10, "mm")
	)

	my_legend = guide_legend(title=NULL
													 ,title.position="top"
													 ,title.theme = element_text(size=14, face="plain", colour = "black",angle=0)
													 ,ncol=1
													 ,keywidth=1
													 ,keyheight=1
	)

	plot_settings 										= list()
	plot_settings$line_color 					= line_color;
	plot_settings$my_theme 						= my_theme;
	plot_settings$my_legend 					= my_legend;
	plot_settings$all_method_names 		= all_method_names;
	plot_settings$all_method_colors 	= all_method_colors;
	plot_settings$all_method_labels 	= all_method_labels;
	plot_settings$all_method_sizes    = all_method_sizes;
	plot_settings$all_method_shapes 	= all_method_shapes;
	plot_settings$path_method_names 	= path_method_names;
	plot_settings$path_method_colors 	= path_method_colors;
	plot_settings$path_method_labels 	= path_method_labels;

	return(plot_settings);
}

create.reg.plot = function(xinfo,yinfo,stats,x_limits=NULL,y_limits=NULL,lab_info=NULL,plot_settings=NULL){


	if (is.null(plot_settings)){
		plot_settings = get.plot.settings();
	}

	my_theme = plot_settings$my_theme;
	my_legend = plot_settings$my_legend;
	line_color 	= plot_settings$line_color;

	all_method_names = plot_settings$all_method_names;
	all_method_colors = plot_settings$all_method_colors;
	all_method_labels = plot_settings$all_method_labels;

	path_method_names = plot_settings$path_method_names;
	path_method_colors = plot_settings$path_method_colors;
	path_method_labels = plot_settings$path_method_labels;

	reg_data 	= get.reg.summary(xinfo,yinfo,stats)

	if (is.null(x_limits)){
		all_xmin = min(reg_data$xmin);
		all_xmax = max(reg_data$xmax);
		x_limits = c(all_xmin,all_xmax);
	}

	if (is.null(y_limits)){
		all_ymin = min(reg_data$ymin);
		all_ymax = max(reg_data$ymax);
		y_limits = c(all_ymin,all_ymax);
	}

	if (is.null(lab_info)){
		lab_info=list()
		lab_info$title = "C_0 Regularization Path"
		lab_info$xname = expression("C_0")
		lab_info$yname = "Test Error"

	}

	reg_plot = ggplot(reg_data, aes(group=names)) +
		geom_smooth(aes(x=x,y=y,ymax=ymax,ymin=ymin,xmax=xmax,xmin=xmin,fill=names),stat="identity",alpha=0.25,size=0) +
		#geom_rect(aes(xmax=xmax, xmin=xmin, ymin=ymin, ymax=ymax,fill=names),size=0.5,alpha=0.25) +
		geom_point(aes(x=x,y=y,color=names,fill=names),size=5,shape=19,alpha=0.5) +
		geom_line(aes(x=x,y=y,color=names),size=1.5,alpha=0.75) +
		#geom_text(aes(x=xmax,y=ymax,label=names),hjust=0,vjust=0) +
		scale_x_continuous(breaks=pretty_breaks(5),limits=x_limits) +
		scale_y_continuous(labels=percent,breaks=pretty_breaks(5),limits=y_limits) +
		scale_fill_manual(name="Method", guide="legend",values=all_method_colors,labels=all_method_labels, drop=TRUE) +
		scale_color_manual(name="Method", guide="legend",values=all_method_colors,labels=all_method_labels, drop=TRUE) +
		labs(x=lab_info$xname,y=lab_info$yname,title=lab_info$title) +
		my_theme + guides(fill = my_legend, color=my_legend)

	if (lab_info$type == "size"){
		reg_plot = reg_plot + scale_y_continuous(breaks=pretty_breaks(5),limits=y_limits)
	} else {
		reg_plot = reg_plot + scale_y_continuous(labels=percent,breaks=pretty_breaks(5),limits=y_limits)
	}

	#scale_shape_manual(guide="legend",values=path_method_colors,labels=path_method_labels,drop=TRUE) +
	#labs(x="Model Size",y="Test Error",title="Methods with Regularization Path") +

	rval = list()
	rval$plot = reg_plot;
	rval$data = reg_data;
	return(rval)

}

create.summary.plot = function(xinfo,yinfo,stats,x_limits=NULL,y_limits=NULL,lab_info = NULL,plot_settings=NULL){

	if (is.null(plot_settings)){
		plot_settings = get.plot.settings();
	}

	my_theme = plot_settings$my_theme;
	my_legend = plot_settings$my_legend;
	line_color 	= plot_settings$line_color;

	all_method_names = plot_settings$all_method_names;
	all_method_colors = plot_settings$all_method_colors;
	all_method_labels = plot_settings$all_method_labels;

	path_method_names = plot_settings$path_method_names;
	path_method_colors = plot_settings$path_method_colors;
	path_method_labels = plot_settings$path_method_labels;

	summary_data 	= get.point.summary(xinfo,yinfo,stats)

	if (is.null(x_limits)){
		all_xmin = min(summary_data$xmin);
		all_xmax = max(summary_data$xmax);
		x_limits = c(all_xmin,all_xmax);
	}

	if (is.null(y_limits)){
		all_ymin = min(summary_data$ymin);
		all_ymax = max(summary_data$ymax);
		y_limits = c(all_ymin,all_ymax);
	}

	if (is.null(lab_info)){
		lab_info=list()
		lab_info$title = "All Methods"
		lab_info$xname = "Model Size"
		lab_info$yname = "Test Error"
	}

	summary_plot = ggplot(summary_data) +
		geom_rect(aes(xmax=xmax, xmin=xmin, ymin=ymin, ymax=ymax,color=names,fill=names),size=1.5,alpha=0.25) +
		geom_point(aes(x=x,y=y,color=names,fill=names),size=5,shape=19) +
		#geom_text(aes(x=xmax,y=ymax,label=names),hjust=0,vjust=0) +
		scale_x_continuous(breaks=pretty_breaks(5),limits=x_limits) +
		scale_y_continuous(labels=percent,breaks=pretty_breaks(5),limits=y_limits) +
		scale_fill_manual(name="Method", guide="legend",values=all_method_colors,labels=all_method_labels, drop=TRUE) +
		scale_color_manual(name="Method", guide="legend",values=all_method_colors,labels=all_method_labels, drop=TRUE) +
		#scale_shape_manual(name="Method", guide="legend",values=all_method_shapes,labels=all_method_labels, drop=TRUE)# +
		labs(x=lab_info$xname,y=lab_info$yname,title=lab_info$title) +
		my_theme  + guides(fill = my_legend, color=my_legend)

	rval = list()
	rval$plot = summary_plot;
	rval$data = summary_data;
	return(rval)

}

create.path.plot = function(xinfo,yinfo,stats,x_limits=NULL,y_limits=NULL,lab_info = NULL,plot_settings=NULL){

	if (is.null(plot_settings)){
		plot_settings = get.plot.settings();
	}

	my_theme = plot_settings$my_theme;
	my_legend = plot_settings$my_legend;
	line_color 	= plot_settings$line_color;

	all_method_names = plot_settings$all_method_names;
	all_method_colors = plot_settings$all_method_colors;
	all_method_labels = plot_settings$all_method_labels;

	path_method_names = plot_settings$path_method_names;
	path_method_colors = plot_settings$path_method_colors;
	path_method_labels = plot_settings$path_method_labels;

	path_data 	= get.path.summary(xinfo,yinfo,stats)

	if (is.null(x_limits)){
		all_xmin = min(path_data$xmin);
		all_xmax = max(path_data$xmax);
		x_limits = c(all_xmin,all_xmax);
	}

	if (is.null(y_limits)){
		all_ymin = min(path_data$ymin);
		all_ymax = max(path_data$ymax);
		y_limits = c(all_ymin,all_ymax);
	}

	if (is.null(lab_info)){
		lab_info=list()
		lab_info$title = "Methods with Regularization Path"
		lab_info$xname = "Model Size"
		lab_info$yname = "Test Error"
	}

	path_plot = ggplot(path_data, aes(group=names)) +
		geom_smooth(aes(x=x,y=y,ymax=ymax,ymin=ymin,xmax=xmax,xmin=xmin,fill=names),stat="identity",alpha=0.25,size=0) +
		#geom_rect(aes(xmax=xmax, xmin=xmin, ymin=ymin, ymax=ymax,fill=names),size=0.5,alpha=0.25) +
		geom_point(aes(x=x,y=y,color=names,fill=names),size=5,shape=19,alpha=0.5) +
		geom_line(aes(x=x,y=y,color=names),size=1.5,alpha=0.75) +
		#geom_text(aes(x=xmax,y=ymax,label=names),hjust=0,vjust=0) +
		scale_x_continuous(breaks=pretty_breaks(5),limits=x_limits) +
		scale_y_continuous(labels=percent,breaks=pretty_breaks(5),limits=y_limits) +
		scale_fill_manual(name="Method", guide="legend",values=all_method_colors,labels=all_method_labels, drop=TRUE) +
		scale_color_manual(name="Method", guide="legend",values=all_method_colors,labels=all_method_labels, drop=TRUE) +
		labs(x=lab_info$xname,y=lab_info$yname,title=lab_info$title) +
		my_theme + guides(fill = my_legend, color=my_legend)

	#scale_shape_manual(guide="legend",values=path_method_colors,labels=path_method_labels,drop=TRUE) +
	#labs(x="Model Size",y="Test Error",title="Methods with Regularization Path") +


	rval = list()
	rval$plot = path_plot;
	rval$data = path_data;
	return(rval)

}

create.legend.plot = function(legend_method_names,plot_settings = NULL){



	if (is.null(plot_settings)){
		plot_settings = get.plot.settings()
	}

	all_method_colors = plot_settings$all_method_colors
	all_method_labels = plot_settings$all_method_labels
	all_method_colors = all_method_colors[legend_method_names];
	all_method_labels = all_method_labels[legend_method_names];

	all_method_labels = sapply(all_method_labels,function (x) paste0(paste0(rep(" ",3),collapse=""),x))
	all_method_labels = str_pad(all_method_labels,width=18,side="right")
	names(all_method_labels) = legend_method_names


	#create sample data
	n_methods = length(legend_method_names)

	sample_data = data.frame(names=legend_method_names,
													 x=rep(5,n_methods),
													 xmin = rep(2.5,n_methods),
													 xmax = rep(7.5,n_methods),
													 y=rep(0.15,n_methods),
													 ymin = rep(0.125,n_methods),
													 ymax = rep(0.205,n_methods))

	my_theme = theme_bw() + theme(title 				= element_blank()
																,axis.title.x = element_blank()
																,axis.text.x 	= element_blank()
																,axis.ticks.x	= element_blank()
																,axis.title.y = element_blank()
																,axis.text.y 	= element_blank()
																,axis.ticks.y	= element_blank()
																,axis.line    = element_blank() #element_line(size=1.5,color="#E9E9E9")
																#,panel.background = element_rect(color="white")
																,panel.grid.major = element_blank()
																,panel.grid.minor = element_blank()
																,panel.border = element_blank()
																,legend.position="bottom"
																,legend.margin = unit(200, "cm"))

	my_legend = guide_legend(title=element_blank()
													 ,title.theme = element_blank()
													 ,label.theme=element_text(face="plain",size=14,angle=0,lineheight=30)
													 ,nrow=2
													 ,keywidth=1.5
													 ,keyheight=1.5)

	legend_plot = ggplot(sample_data) +
		geom_rect(aes(xmax=xmax, xmin=xmin, ymin=ymin, ymax=ymax,color=names,fill=names),size=1.5,alpha=0.25) +
		geom_point(aes(x=x,y=y,color=names,fill=names),size=5,shape=19) +
		#geom_text(aes(x=xmax,y=ymax,label=names),hjust=0,vjust=0) +
		scale_fill_manual(name="Method", guide="legend",values=all_method_colors,labels=all_method_labels, drop=TRUE) +
		scale_color_manual(name="Method", guide="legend",values=all_method_colors,labels=all_method_labels, drop=TRUE) +
		#scale_shape_manual(name="Method", guide="legend",values=all_method_shapes,labels=all_method_labels, drop=TRUE)# +
		my_theme + guides(fill = my_legend, color = my_legend, nrow = 2,byrow=TRUE)

	return(legend_plot)
}

plot.empirical.roc.curve = function(coefs,X,Y,plot_settings=plot_settings,normalize=TRUE){

  X=data_info$X;
  Y=data_info$Y;

  #compute statistics
  if (length(coefs)==dim(X)[2]) {
    #R based methods
    scores = as.matrix(X) %*% coefs
  } else if (length(coefs)==(dim(X)[2]+1)){
    #SLIM
    scores = as.matrix(X) %*% coefs[2:length(coefs)] + coefs[1]
  }

  #reorder
  sort_ind = sort.int(scores, index.return=TRUE)$ix
  scores    = scores[sort_ind];
  Y         = Y[sort_ind];

  #calculate relevant statistics
  pos_ind   = Y==1
  neg_ind   = !(Y==1)
  N         = length(Y);
  N_pos     = sum(pos_ind);
  N_neg     = N - N_pos;

  Yhat  = as.numeric(scores>0);
  blank = as.numeric(rep(0,N));

  TP          = blank;
  TN          = blank;
  FP          = blank;
  FN          = blank;

  TP[pos_ind] = as.numeric(Y[pos_ind]==Yhat[pos_ind]);
  FP[neg_ind] = as.numeric(Y[neg_ind]!=Yhat[neg_ind]);

  if (normalize){
    TPR         = cumsum(TP)/N_pos;
    FPR         = cumsum(FP)/N_neg;
  } else {
    TPR         = cumsum(TP);
    FPR         = cumsum(FP);
  }

  df = data.frame(TPR,FPR);


  new_plot = ggplot(df) +
    geom_step(aes(x=FPR,y=TPR),size=1,alpha=0.75)  +
    geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1),color="snow3",alpha=0.25,size=0.5,linetype="dashed") +
    plot_settings$my_theme

  if (normalize){
    new_plot = new_plot +
      scale_x_continuous(name="FPR",labels=percent,breaks=pretty_breaks(5),limits=c(0,1)) +
      scale_y_continuous(name="TPR",labels=percent,breaks=pretty_breaks(5),limits=c(0,1))
  } else {
    new_plot = new_plot +
      scale_x_continuous(name="# FP",breaks=pretty_breaks(5),limits=c(0,N_pos)) +
      scale_y_continuous(name="# TP",breaks=pretty_breaks(5),limits=c(0,N_neg))
  }


  return(new_plot)
}

###### Table Creation #####

print.mean = function(mean_err,digits=1){
	mean_pm_std = paste(formatC(100*mean_err,format="f",digits=digits),"$\\%$",sep="");
	names(mean_pm_std) = names(mean_err);
	return(mean_pm_std)
}

print.value = function(val,digits=1){
  printed_value = paste(formatC(val,format="f",digits=digits),sep="");
  names(printed_value) = names(val);
  return(printed_value)
}

print.value.range = function(lower_value,upper_value,digits){
  value_range = paste(formatC(lower_value,format="f",digits=digits)," - ",formatC(upper_value,format="f",digits=digits),sep="")
  names(value_range) = names(value_range);
  return(value_range);
}

print.mean.pm.std = function(mean_err,std_err,digits=1){
	mean_err 		= formatC(100*mean_err,format="f",digits=digits);
	std_err 		= formatC(100*std_err,format="f",digits=digits);
	mean_pm_std = sprintf("%s $\\pm$ %s%s",mean_err,std_err,"$\\%$");
	names(mean_pm_std) = names(mean_err);
	return(mean_pm_std)
}

print.mean.pm.std.scalar = function(mean_val,std_val,digits=1){
	mean_val 		= formatC(mean_val,format="f",digits=digits);
	std_val 		= formatC(std_val,format="f",digits=digits);
	mean_pm_std = sprintf("%s $\\pm$ %s",mean_val,std_val);
	names(mean_pm_std) = names(mean_val);
	return(mean_pm_std)
}

print.error.range = function(lower_value,upper_value,digits=1){
	mean_range = paste0(formatC(100*lower_value,format="f",digits=digits)," - ",formatC(100*upper_value,format="f",digits=digits),"$\\%$")
	names(mean_range) = names(lower_value);
	return(mean_range);
}

print.mean.range = function(lower_value,upper_value,digits=1){
  mean_range = paste0(formatC(100*lower_value,format="f",digits=digits)," - ",formatC(100*upper_value,format="f",digits=digits),"$\\%$")
  names(mean_range) = names(lower_value);
  return(mean_range);
}

print.model.range = function(lower_value,upper_value){
	model_range = paste(formatC(lower_value,format="d")," - ",formatC(upper_value,format="d"),sep="")
	names(model_range) = names(lower_value);
	return(model_range);
}

match.method.names = function(method_names,plot_settings=NULL){

	if (is.null(plot_settings)){
		plot_settings = get.plot.settings()
	}

	all_method_names = plot_settings$all_method_names;
	all_method_labels = plot_settings$all_method_labels;

	if (!is.character(method_names)){
		method_names = as.character(method_names)
	}

	new_names = method_names;
	for (m in 1:length(method_names)){
		ind = which(method_names[m]==all_method_names);
		if(length(ind)==0){
			method_name 	= gsub("_","",method_name,fixed=TRUE)
			method_name 	= toupper(method_name)
			new_names[m]  = method_name;
		} else {
			new_names[m] = all_method_labels[ind];
		}
	}
	return(new_names);
}

print.method.names = function(stat){
	method_names = names(stat);
	method_names = match.method.names(method_names);
	names(method_names) = names(stat);
	return(method_names)
}

print.bfcell.header = function(header){
	header = gsub("\\","\\\\",header,fixed=TRUE)
	header = sprintf("\\bfcell{c}{%s}",header);
	return(header)
}

print.tex.header = function(header){
	header = gsub("\\","\\\\",header,fixed=TRUE)
	header = sprintf("\\begin{tabular}{>{\\bf}c}%s\\end{tabular}",header);
	return(header)
}

make.single.tabular = function(cells){

	if (class(cells)=="data.frame"){
		cells = sapply(cells,function(x) as.character(x))
	}

	tab = paste0("\\cell{c}{",paste(cells,collapse="\\\\"),"}")

	return(tab)
}

make.multi.tabulars = function(top_cells,bottom_cells,top_size=NULL,bottom_size=NULL){
	if (!is.null(top_size)){
		top_cells = paste(paste0("\\",top_size,"{"),top_cells,"}",sep="")
	}
	if (!is.null(bottom_size)){
		bottom_cells = paste(paste0("\\",bottom_size,"{"),bottom_cells,"}",sep="")
	}
	cells = rbind(top_cells,bottom_cells);
	cells = apply(cells,2,function(x) make.single.tabular(x));
	return(cells);
}

highlight.min = function(cell_value, method_index, field_name, stat_name) {
	if (method_index == which.min(flat_stats[[field_name]][[stat_name]])){
		cell_value = paste("\\cellcolor[gray]{0.7} ",cell_value,sep="");
	}
	return(cell_value)
}

##### Model Creation######

get.linear.model.xtable = function(coefs,max_model_size=NULL,n_vars_per_row=3,xnames,yname,remove_score=FALSE){

  #remove zero coefficients
  nnz_ind = coefs!=0.0;
  xnames 	= xnames[nnz_ind];
  coefs 	= coefs[nnz_ind];
  n_coefs = length(coefs);

  if (is.null(max_model_size) || (n_coefs <= max_model_size)){

    #group positive and negative coefficients together and sort them by size
    pos_ind  	= coefs>0;
    pos_coefs = coefs[pos_ind];
    pos_names = xnames[pos_ind];

    pos_coefs = sort(pos_coefs,decreasing=TRUE,index.return=TRUE);
    pos_ind 	= pos_coefs$ix;
    pos_coefs = pos_coefs$x;
    pos_names = pos_names[pos_ind];

    neg_ind  	= coefs<0;
    neg_coefs = coefs[neg_ind];
    neg_names = xnames[neg_ind];

    neg_coefs = sort(neg_coefs,decreasing=FALSE,index.return=TRUE);
    neg_ind 	= neg_coefs$ix;
    neg_coefs = neg_coefs$x;
    neg_names = neg_names[neg_ind];

    xnames = c(pos_names,neg_names);
    coefs = c(pos_coefs,neg_coefs);

    #shift intercept to the end of the model
    ind 		= xnames=="(Intercept)";
    xnames 	= c(xnames[!ind],xnames[ind]);
    coefs  	= c(coefs[!ind],coefs[ind]);

    #express model as a score string
    n_coefs 		= length(coefs);
    score_string = array(NA,2*n_coefs);
    integer_coefficients = all(ceiling(coefs)==floor(coefs));

    for (n in 1:n_coefs){

      coef_val 	= abs(coefs[n])
      coef_sign = sign(coefs[n]);
      coef_name = xnames[n];

      #Sign
      if ((n==1)&&(coef_sign==1)) {
        sign_string = ""
      } else if (coef_sign>0) {
        sign_string = "$\\scriptsize{+}$"
      } else if (coef_sign<0) {
        sign_string = "$\\scriptsize{-}$"
      }

      # Variable
      if (integer_coefficients){

        if (coef_name=="(Intercept)"){
          coef_string = sprintf("$%d$",coef_val)
        } else {
          if (coef_val == 1){
            coef_string = sprintf("$\\textfn{%s}$", coef_name);
          } else {
            coef_string = sprintf("$%d ~\\textfn{%s}$", coef_val, coef_name);
          }
        }

      } else {

        if ((abs(coef_val)<0.01)||abs(coef_val)>99999){
          coef_val_string = sprintf("%1.2e",coef_val);
          coef_val_string = sprintf("%s}",gsub("e"," \\\\times 10^{",coef_val_string));
        } else {
          coef_val_string = sprintf("%1.2f",coef_val);
        }

        if (coef_name=="(Intercept)"){
          coef_string = sprintf("$%s$",coef_val_string);
        } else {
          coef_string = sprintf("$%s ~\\textfn{%s}$", coef_val_string, coef_name);
        }

      }

      score_string[2*n-1] = sign_string;
      score_string[2*n]  	= coef_string;
    }

    #extend score string so that it has empty entries in remaining rows
    n_last_col = n_coefs %% n_vars_per_row;
    if (n_last_col < n_vars_per_row){
      n_empty_cols = n_vars_per_row - n_last_col;
      score_string = c(score_string,rep("",n_empty_cols*2))
    }

    #create model table
    if(remove_score){

      #create model table
      n_rows   		= ceiling(n_coefs/n_vars_per_row)
      n_cols  		= 2*n_vars_per_row
      model_table = array("",c(n_rows,n_cols));
      model_table[1:n_rows,1:n_cols] = t(array(score_string,dim=c(n_cols,n_rows)))

      #put in latex form
      model_table = xtable(as.data.frame(model_table))
      align(model_table)  = c("l",rep(c("p{1mm}","l"),n_vars_per_row));
      digits(model_table) = rep(0,n_cols+1);

    } else{

      n_rows 			= ceiling(n_coefs/n_vars_per_row)
      n_cols  		= 1 + 2*n_vars_per_row
      model_table = array("",c(n_rows,n_cols));
      model_table[1,1] = "\\textfn{Score} ="
      model_table[1:n_rows,2:n_cols] = t(array(score_string,dim=c(n_cols-1,n_rows)))

      #put in latex form
      model_table = xtable(as.data.frame(model_table))
      align(model_table)  = c("l","l",rep(c("p{1mm}","l"),n_vars_per_row));
      digits(model_table) = rep(0,n_cols+1);

    }
    return(model_table);
  }
}

get.C5.0.model = function(model,model_size,fold_index=NULL,max_model_size=NULL){

	if (is.null(max_model_size) || (model_size <= max_model_size)) {
		model = model$output;
		if (method_name == "C5.0_tree"){
			model = gsub(".*\n\nDecision tree:\n\n","",model);
		} else if (method_name == "C5.0_rule") {
			model = gsub(".*\n\nRules:\n\n","",model);
		}
		model = gsub("\n\n\nEvaluation on training data .*","",model);
	}
	return(model);
}

get.mnrules.model.xtable = function(coefs,max_model_size=NULL,xnames,yname){

	#Get M
	if (all(coefs==0)){
		model_table = sprintf("PREDICT NOTHING\n")
	} else {

		ind 		= xnames=="(Intercept)";
		M 			= -coefs[ind]+1

		#Remove Intercept from Xnames and Coefs
		xnames 	= xnames[!ind]
		coefs  	= coefs[!ind]

		#Remove Zero Coefficients
		nnz_ind = coefs!=0.0;
		xnames 	= xnames[nnz_ind];
		N 			= sum(nnz_ind)
		n_coefs = length(xnames);

		#create model table
		model_table 							= array("",n_coefs+1);
		model_table[1] 						= sprintf("\\textbf{PREDICT %s IF AT LEAST %d OF THE FOLLOWING %d RULES ARE TRUE}\n", yname,M,N);
		model_table[2:(n_coefs+1)]= xnames;

	}
	#put in latex form
	model_table = xtable(as.data.frame(model_table))
	align(model_table)  = c("l","c");
	digits(model_table) = 0;

	return(model_table);
}

create.scoring.system.xtable = function(coefs,xnames,yname,include_subtotals=FALSE){

  #coefs = model
  #xnames = data_info$xnames
  #yname = data_info$yname
  #include_subtotals = FALSE

	#shift intercept to the end of the model
	ind 		= xnames=="(Intercept)";
	cutoff 	= coefs[ind];
	xnames 	= xnames[!ind]
	coefs  	= coefs[!ind]

	#remove zero coefficients
	nnz_ind = coefs!=0.0;
	xnames 	= xnames[nnz_ind];
	coefs 	= coefs[nnz_ind];
	n_coefs = length(coefs);

	#group positive and negative coefficients together and sort them by size
	pos_ind  	= coefs>0;
	pos_coefs = coefs[pos_ind];
	pos_names = xnames[pos_ind];

	pos_coefs = sort(pos_coefs,decreasing=TRUE,index.return=TRUE);
	pos_ind 	= pos_coefs$ix;
	pos_coefs = pos_coefs$x;
	pos_names = pos_names[pos_ind];

	neg_ind  	= coefs<0;
	neg_coefs = coefs[neg_ind];
	neg_names = xnames[neg_ind];
	neg_coefs = sort(neg_coefs,decreasing=TRUE,index.return=TRUE);
	neg_ind 	= neg_coefs$ix;
	neg_coefs = neg_coefs$x;
	neg_names = neg_names[neg_ind];

	xnames = c(pos_names,neg_names);
	coefs = c(pos_coefs,neg_coefs);

	#create strings
	n_pos = length(pos_coefs)
	n_neg = length(neg_coefs)
	n_int = length(ind)

	if (include_subtotals){

	  pos_nums_string 		= sprintf("%d.",seq(1,n_pos))
	  pos_coefs_string 		= sprintf("$\\textfn{%s}$",pos_names)
	  pos_points_string 	= sprintf("%d points",abs(pos_coefs))
	  pos_points_string[abs(pos_coefs)==1] = "1 point"

	  if (n_pos > 1){
	    pos_user_string 		= c("$\\quad\\cdots\\cdots$",rep("$+\\quad\\cdots\\cdots$",n_pos-1))
	    pos_total_string 		= sprintf("\\small{\\textbf{ADD POINTS FROM ROWS %d-%d}}",1,n_pos)
	  } else if (n_pos ==1) {
	    pos_user_string 		= "$\\quad\\cdots\\cdots$"
	    pos_total_string 		= sprintf("\\small{\\textbf{ADD POINTS FROM ROW %d}}",1)
	  }

	  neg_nums_string 		= sprintf("%d.",seq(n_pos+1,n_pos+n_neg))
	  neg_coefs_string   	= sprintf("$\\textfn{%s}$",neg_names)
	  neg_points_string 	= sprintf("%d points",abs(neg_coefs))
	  neg_points_string[abs(neg_coefs)==1] = "1 point"

	  if (n_neg > 1){
	    neg_user_string 		= c("$\\quad\\cdots\\cdots$",rep("$+\\quad\\cdots\\cdots$",n_neg-1))
	    neg_total_string 		= sprintf("\\small{\\textbf{ADD POINTS FROM ROWS %d-%d}}",n_pos+1,n_pos+n_neg)
	  } else if (n_neg ==1) {
	    neg_user_string 		= "$\\quad\\cdots\\cdots$"
	    neg_total_string 		= sprintf("\\small{\\textbf{ADD POINTS FROM ROWS %d}}",n_pos+1)
	  }

	  if ((n_pos > 0) & (n_neg >0) ){

	    nums_column			= c(pos_nums_string,"",neg_nums_string,"","")
	    coefs_column		= c(pos_coefs_string,pos_total_string,neg_coefs_string,neg_total_string,"\\small{\\textbf{SUBTRACT TOTAL B FROM TOTAL A}}")
	    points_column		= c(pos_points_string,"\\small{\\textbf{TOTAL A}}",neg_points_string,"\\small{\\textbf{TOTAL B}}","\\small{\\textbf{SCORE}}")
	    user_column 		= c(pos_user_string,"$=\\quad\\cdots\\cdots$",neg_user_string,"$=\\quad\\cdots\\cdots$","$=\\quad\\cdots\\cdots$")

	    pred_guide 			= sprintf("\\predcell{c}{PREDICT %s IF SCORE $> %d$}", yname,abs(cutoff))
	    hline_guide = c(0,n_pos,n_pos+1,n_pos+n_neg+1,n_pos+n_neg+2,n_pos+n_neg+3)

	  } else if ((n_pos>0) & (n_neg==0)){

	    nums_column			= c(pos_nums_string,"")
	    coefs_column 		= c(pos_coefs_string,pos_total_string)
	    points_column		= c(pos_points_string,"\\small{\\textbf{SCORE}}")
	    user_column 		= c(pos_user_string,"$=\\quad\\cdots\\cdots$")

	    pred_guide 			= sprintf("\\predcell{c}{PREDICT %s IF SCORE $> %d$}", yname,abs(cutoff))

	    hline_guide = c(0,n_pos,n_pos+1)

	  } else if ((n_pos==0) & (n_neg>0)){

	    nums_column			= c(neg_nums_string,"")
	    coefs_column 		= c(neg_coefs_string,neg_total_string)
	    points_column		= c(neg_points_string,"\\small{\\textbf{SCORE}}")
	    user_column 		= c(neg_user_string,"$=\\quad\\cdots\\cdots$")

	    pred_guide 			= sprintf("\\predcell{c}{PREDICT %s if SCORE $< %d$}", yname,-(abs(cutoff)))

	    hline_guide = c(0,n_neg,n_neg+1)

	  }

	} else {

	  if (n_pos>0){
	    pos_nums_string   	= sprintf("%d.",seq(1,n_pos))
	    pos_coefs_string 		= sprintf("$\\textfn{%s}$",pos_names)
	    pos_points_string 	= sprintf("%d points",abs(pos_coefs))
	    pos_points_string[abs(pos_coefs)==1] = "1 point"
	  } else{
	    pos_nums_string = NULL;
	    pos_coefs_string = NULL;
	    pos_points_string = NULL;
	  }

	  if (n_neg>0){
	    neg_nums_string 		= sprintf("%d.",seq(n_pos+1,n_pos+n_neg))
	    neg_coefs_string   	= sprintf("$\\textfn{%s}$",neg_names)
	    neg_points_string 	= sprintf("-%d points",abs(neg_coefs))
	    neg_points_string[abs(neg_coefs)==1] = "-1 point"
	  } else {
	    neg_nums_string   	= NULL
	    neg_coefs_string   	= NULL
	    neg_points_string 	= NULL
	  }


	  total_string   	= sprintf("\\small{\\textbf{ADD POINTS FROM ROWS %d-%d}}",1,n_pos+n_neg)

	  nums_column  		= c(pos_nums_string,neg_nums_string,"")
	  coefs_column		= c(pos_coefs_string,neg_coefs_string,total_string)
	  points_column		= c(pos_points_string,neg_points_string,"\\small{\\textbf{SCORE}}")
	  user_column 		= c("$\\phantom{+}\\quad\\cdots\\cdots$",rep("$+\\quad\\cdots\\cdots$",n_pos+n_neg-1), "$=\\quad\\cdots\\cdots$")
	  pred_guide 			= sprintf("\\predcell{c}{PREDICT %s IF SCORE $> %d$}\n", yname,abs(cutoff))
	  hline_guide = c(0,n_pos+n_neg,n_pos+n_neg+1)

	}

	#remove weird characters
	pred_guide 		= gsub("_","\\_",pred_guide,fixed=TRUE)
	coefs_column 	= gsub("_","\\_",coefs_column,fixed=TRUE)

	#create table
	model_table = cbind(nums_column,coefs_column,points_column,user_column)
	colnames(model_table) = NULL
	rownames(model_table) = NULL
	model_table = xtable(as.data.frame(model_table))

	align(model_table)  = c("|","l","|","l"," l "," c ","|"," c ","|")
	digits(model_table) = rep(0,5);

	#compile output
	scoring_system = list()
	scoring_system$pred_guide = pred_guide;
	scoring_system$xtable = model_table;
	scoring_system$hline_guide = hline_guide;
	scoring_system$size_command = paste0("\\renewcommand{\\arraystretch}{1.2}\n",pred_guide,"\n\\vspace{0.5em}")

	return(scoring_system)

}

##### Data Mining Functions #####

get.model.for.report = function(raw_model, method_name,data_info,newflag=FALSE){

  if (method_name %in% c("lars_lasso",
                         "lars_lasso_hc",
                         "lars_ridge",
                         "lars_ridge_hc",
                         "lars_elasticnet",
                         "lars_elasticnet_hc",
                         "slim",
                         "svm_linear")){

    model = get.linear.model.xtable(raw_model,NULL,3,data_info$xnames,data_info$yname,remove_score=TRUE)

  } else if (method_name %in% c("C5.0_tree","C5.0_rule")){
    model = model$output;
    if (method_name == "C5.0_tree"){
      model = gsub(".*\n\nDecision tree:\n\n","",model);
    } else if (method_name == "C5.0_rule") {
      model = gsub(".*\n\nRules:\n\n","",model);
    }
    model = gsub("\n\n\nEvaluation on training data .*","",model);
  } else if (method_name %in% c("cart")){
    model = raw_model;
  } else {
    model = NA;
  }

  return(model)
}

get.quick.linear.model = function(coefs,data_info) {

  model_table = get.linear.model.xtable(coefs,NULL,3,data_info$xnames,data_info$ynames);

  print.xtable(model_table,
               type = "latex",
               floating = FALSE,
               table.placement = "ht",
               tabular.environment = "tabularx",
               width = "0.5\\textwidth",
               sanitize.text.function = function(str) gsub("_","\\_",str,fixed=TRUE),
               sanitize.rownames.function = function(str) gsub("_","\\_",str,fixed=TRUE),
               sanitize.colnames.function = function(str) gsub("_","\\_",str,fixed=TRUE),
               NA.string="",
               include.rownames = FALSE,
               include.colnames = FALSE,
               hline.after=NULL)

}

get.table.stats = function(df){

  plot_settings = get.plot.settings()
  table_stats     															= list();
  table_stats$method_name												= match.method.names(df$method_name,plot_settings);

  #testing
  table_stats$testing_error.mean 								= print.mean(df$testing_error.mean);
  table_stats$testing_error.range 							=	print.error.range(df$testing_error.min,df$testing_error.max);
  table_stats$weighted_testing_error.mean 			= print.mean(df$weighted_testing_error.mean);
  table_stats$weighted_testing_error.range 			= print.error.range(df$weighted_testing_error.min,df$weighted_testing_error.max);

  #table_stats$pos_testing_error.mean 						= print.mean(df$pos_testing_error.mean);
  #table_stats$pos_testing_error.range 					= print.error.range(df$pos_testing_error.min,df$pos_testing_error.max);
  #table_stats$neg_testing_error.mean 						= print.mean(df$neg_esting_error.mean);
  #table_stats$neg_testing_error.range 					= print.error.range(df$neg_testing_error.min,df$neg_testing_error.max);

  #training
  table_stats$training_error.mean 							= print.mean(df$training_error.mean);
  table_stats$training_error.range 							=	print.error.range(df$training_error.min,df$training_error.max);
  table_stats$weighted_training_error.mean 			= print.mean(df$weighted_training_error.mean);
  table_stats$weighted_training_error.range 		= print.error.range(df$weighted_training_error.min,df$weighted_training_error.max);

  #table_stats$pos_training_error.mean 					= print.mean(df$pos_training_error.mean);
  #table_stats$pos_training_error.range 					= print.error.range(df$pos_training_error.min,df$pos_training_error.max);
  #table_stats$neg_training_error.mean 					= print.mean(df$neg_training_error.mean);
  #table_stats$neg_training_error.range 					= print.error.range(df$neg_training_error.min,df$neg_training_error.max);

  #validation
  if (is.field(df,"full_validation_error")){
    table_stats$full_validation_error 					  = print.mean(df$full_validation_error);
    table_stats$full_weighted_validation_error		=	print.mean(df$full_weighted_validation_error);
    table_stats$full_validation_tpr 			        = print.mean(df$full_validation_tpr);
    table_stats$full_validation_fpr 			        = print.mean(df$full_validation_fpr);
  } else {
    table_stats$full_validation_error 					  = print.mean(NA)
    table_stats$full_weighted_validation_error		=	print.mean(NA)
    table_stats$full_validation_tpr 			        = print.mean(NA)
    table_stats$full_validation_fpr 			        = print.mean(NA)
  }



  #model size
  table_stats$model_size.med 										= formatC(df$model_size.med,digits=0,format="d");
  table_stats$model_size.range 									= print.model.range(df$model_size.min,df$model_size.max);

  #tpr / fpr
  table_stats$testing_tpr.mean                  = print.mean(df$testing_tpr.mean);
  table_stats$testing_tpr.range                 = print.error.range(df$testing_tpr.min,df$testing_tpr.max);
  table_stats$training_tpr.mean                 = print.mean(df$training_tpr.mean);
  table_stats$training_tpr.range                = print.error.range(df$training_tpr.min,df$training_tpr.max);


  table_stats$testing_fpr.mean                  = print.mean(df$testing_fpr.mean);
  table_stats$testing_fpr.range                 = print.error.range(df$testing_fpr.min,df$testing_fpr.max);
  table_stats$training_fpr.mean                 = print.mean(df$training_fpr.mean);
  table_stats$training_fpr.range                = print.error.range(df$training_fpr.min,df$training_fpr.max);

  #full model
  table_stats$full_training_error 							= print.mean(df$full_training_error);
  table_stats$full_weighted_training_error 			= print.mean(df$full_weighted_training_error);
  table_stats$full_model_size 									= df$full_model_size;

  #tpr / fpr
  table_stats$full_training_tpr                  = print.mean(df$full_training_tpr);
  table_stats$full_training_fpr                  = print.mean(df$full_training_fpr);

  #mipgaps
  table_stats$mipgap.mean                       = print.mean(df$mipgap.mean);
  table_stats$mipgap.range                      = print.error.range(df$mipgap.min,df$mipgap.max);

  #table_stats$full_pos_training_error 					= print.mean(df$full_pos_training_error);
  #table_stats$full_neg_training_error 					= print.mean(df$full_neg_training_error);

  #create cells
  #table_stats$weighted_testing_error 						= make.multi.tabulars(table_stats$weighted_testing_error.mean,table_stats$weighted_testing_error.range,top_size="scriptsize",bottom_size="tiny");

  table_stats$testing_error   									= make.multi.tabulars(table_stats$testing_error.mean,table_stats$testing_error.range,top_size="scriptsize",bottom_size="tiny");
  table_stats$training_error   									= make.multi.tabulars(table_stats$training_error.mean,table_stats$training_error.range,top_size="scriptsize",bottom_size="tiny");

  table_stats$full_training_error   						= make.multi.tabulars(table_stats$full_training_error,rep("-",length(df$full_training_error)),top_size="scriptsize",bottom_size="tiny");

  table_stats$model_size    										= make.multi.tabulars(table_stats$model_size.med,table_stats$model_size.range,top_size="scriptsize",bottom_size="tiny");
  table_stats$full_model_size     							= make.multi.tabulars(table_stats$full_model_size,rep("-",length(df$full_model_size)),top_size="scriptsize",bottom_size="tiny");

  table_stats$weighted_training_error 					= make.multi.tabulars(table_stats$weighted_training_error.mean,table_stats$weighted_training_error.range,top_size="scriptsize",bottom_size="tiny");
  table_stats$full_weighted_training_error     	= make.multi.tabulars(table_stats$full_weighted_training_error,rep("-",length(df$full_weighted_training_error)),top_size="scriptsize",bottom_size="tiny");

  table_stats$testing_tpr                       = make.multi.tabulars(table_stats$testing_tpr.mean,table_stats$testing_tpr.range,top_size="scriptsize",bottom_size="tiny");
  table_stats$testing_fpr                       = make.multi.tabulars(table_stats$testing_fpr.mean,table_stats$testing_fpr.range,top_size="scriptsize",bottom_size="tiny");
  table_stats$training_tpr                      = make.multi.tabulars(table_stats$training_tpr.mean,table_stats$training_tpr.range,top_size="scriptsize",bottom_size="tiny");
  table_stats$training_fpr                      = make.multi.tabulars(table_stats$training_fpr.mean,table_stats$training_fpr.range,top_size="scriptsize",bottom_size="tiny");
  table_stats$full_training_tpr                 = make.multi.tabulars(table_stats$full_training_tpr,rep("-",length(df$full_training_error)),top_size="scriptsize",bottom_size="tiny");
  table_stats$full_training_fpr                 = make.multi.tabulars(table_stats$full_training_fpr,rep("-",length(df$full_training_error)),top_size="scriptsize",bottom_size="tiny");

  table_stats$full_validation_error             = make.multi.tabulars(table_stats$full_validation_error,rep("-",length(df$full_validation_error)),top_size="scriptsize",bottom_size="tiny");
  table_stats$full_validation_tpr               = make.multi.tabulars(table_stats$full_validation_tpr,rep("-",length(df$full_validation_tpr)),top_size="scriptsize",bottom_size="tiny");
  table_stats$full_validation_fpr               = make.multi.tabulars(table_stats$full_validation_fpr,rep("-",length(df$full_validation_fpr)),top_size="scriptsize",bottom_size="tiny");
  table_stats$full_weighted_validation_error    = make.multi.tabulars(table_stats$full_weighted_validation_error,rep("-",length(df$full_weighted_validation_error)),top_size="scriptsize",bottom_size="tiny");

  table_stats$mipgap                            = make.multi.tabulars(table_stats$mipgap.mean,table_stats$mipgap.range,top_size="scriptsize",bottom_size="tiny");


  return(table_stats)
}

get.quick.settings.table  = function(df){

  print.na.as = function(val,print_val){
    if(is.na(val)){
      return(print_val)
    } else {
      return(val)}
  }

  settings_table = data.frame(
    "Dataset"=df$data_name,
    "Method"=match.method.names(df$method_name),
    "$W^+$"=df$w_pos,
    "$W^-$"=df$w_neg,
    "Parameter 1"=df$parameter_name_1,
    "Parameter 1 Value"=sprintf("%1.2e",df$parameter_value_1),
    "Parameter 2" = print.na.as(df$parameter_name_2,'-'),
    "Parameter 2 Value"=sprintf("%1.2e",df$parameter_value_2),
    "Parameter 3" =print.na.as(df$parameter_name_3,'-'),
    "Parameter 3 Value"=sprintf("%1.2e",df$parameter_value_3),
    check.names=FALSE
  )

  #convert to xtable
  settings_table            = as.data.frame(t(settings_table))
  rownames(settings_table)  = print.bfcell.header(rownames(settings_table));
  settings_table   		      = xtable(settings_table);
  align(settings_table)     = "lc"
  digits(settings_table)	  = 0;

  return(settings_table)
}

get.quick.model.table = function(df,with_validation=is.field(df,"full_validation_error")){

  if (with_validation){

    st = rbind(c("TPR",df$testing_tpr,df$full_training_tpr,df$full_validation_tpr),
               c("FPR",df$testing_fpr,df$full_training_fpr,df$full_validation_fpr),
               c("Error",df$testing_error,df$full_training_error,df$full_validation_error),
               c("Weighted Error",df$weighted_training_error,df$full_weighted_training_error,df$full_weighted_validation_error),
               c("Model Size",df$model_size,df$full_model_size,df$full_model_size))

    st = data.frame(st,check.names=FALSE)
    colnames(st) = c("Metric","K-Fold CV","All Data","Validation Set")
    colnames(st) = print.bfcell.header(colnames(st));

    #convert to xtable
    st[,1]=print.bfcell.header(st[,1])
    st     	      = xtable(st);
    align(st)     = "llccc"
    digits(st)	  = 0;

  } else {
    #Stats Table
    st = rbind(c("TPR",df$testing_tpr,df$full_training_tpr),
               c("FPR",df$testing_fpr,df$full_training_fpr),
               c("Error",df$testing_error,df$full_training_error),
               c("Weighted Error",df$weighted_training_error,df$full_weighted_training_error),
               c("Model Size",df$model_size,df$full_model_size))

    st = data.frame(st,check.names=FALSE)
    colnames(st) = c("Metric","K-Fold CV","All Data")
    colnames(st) = print.bfcell.header(colnames(st));

    #convert to xtable
    st[,1]=print.bfcell.header(st[,1])
    st     	      = xtable(st);
    align(st)     = "llcc"
    digits(st)	  = 0;
  }

  return(st)

}

get.quick.comp.table = function(df){

  comp_table = data.frame(
    "Dataset"=df$data_name,
    "Method"=match.method.names(df$method_name),
    "$W^+$"=print.value(df$w_pos,digits=2),
    "$W^-$"=print.value(df$w_neg,digits=2),
    "C"=print.value(df$parameter_value_1,digits=4),
    "XID"=df$xtra_id,
    "HID"=df$hcon_id,
    "W. Training Error" = make.multi.tabulars(print.mean(df$weighted_training_error.mean),
                                             print.error.range(df$weighted_training_error.min,df$weighted_training_error.max),
                                             top_size="scriptsize",
                                             bottom_size="tiny"),
    "Model Size"= make.multi.tabulars(print.value(df$model_size.med),
                                      print.value.range(df$model_size.min,df$model_size.max,digits=1),
                                      top_size="scriptsize",
                                      bottom_size="tiny"),
    "Pos. Training Error" = make.multi.tabulars(print.mean(df$pos_training_error.mean),
                                                print.error.range(df$pos_training_error.min,df$pos_training_error.max),
                                                top_size="scriptsize",
                                                bottom_size="tiny"),
    "Neg. Training Error" = make.multi.tabulars(print.mean(df$neg_training_error.min),
                                                print.error.range(df$neg_training_error.min,df$neg_training_error.max),
                                                top_size="scriptsize",
                                                bottom_size="tiny"),
    "Train Time" = sprintf("%ds",df$train_time),
    "MIP Gap"= make.multi.tabulars(print.mean(df$mipgap.mean),
                                   print.error.range(df$mipgap.min,df$mipgap.max),
                                   top_size="scriptsize",
                                   bottom_size="tiny"),
    "Objective Value" = make.multi.tabulars(print.value(df$objval.mean,digits=2),
                                            print.value.range(df$objval.min,df$objval.max,digits = 2),
                                            top_size="scriptsize",
                                            bottom_size="tiny"),
    "MIP Iterations" = make.multi.tabulars(print.value(df$numiters.mean,digits=0),
                                            print.value.range(df$numiters.min,df$numiters.max,digits=0),
                                            top_size="scriptsize",
                                            bottom_size="tiny"),
    "BB Nodes" = make.multi.tabulars(print.value(df$numnodes.mean,digits=0),
                                         print.value.range(df$numnodes.min,df$numnodes.max,digits=0),
                                         top_size="scriptsize",
                                         bottom_size="tiny"),
    check.names=FALSE
  )

  n_rows = nrow(df)

  if (n_rows == 1) { #print with fields as rows

    comp_table              = as.data.frame(t(comp_table));
    rownames(comp_table)    = print.bfcell.header(rownames(comp_table));
    comp_table              = xtable(comp_table);
    align(comp_table)       = "lc";

  } else { #print with fields as headers

    comp_table              = as.data.frame(comp_table);
    colnames(comp_table)    = print.bfcell.header(colnames(comp_table));
    comp_table              = xtable(comp_table);
    align(comp_table)       = paste(c("l",rep("c",length(colnames(comp_table)))),collapse="")
    digits(comp_table)      = 0;

  }

  return(comp_table)

}