args = commandArgs(TRUE);
comp_name = args[1];
train_name = args[2];
run_name = args[3];
run_dir = args[4];
log_file_name = args[5];

sink(log_file_name)
cat(sprintf("%d arguments passed\n", length(args)));
cat(sprintf("comp_name is %s\n", comp_name));
cat(sprintf("train_name is %s\n", train_name));
cat(sprintf("run_name is %s\n", run_name));
cat(sprintf("run_dir is %s\n", run_dir));
cat(sprintf("log_file_name is %s\n", log_file_name));
quit(save = "no")