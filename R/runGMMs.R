## Run the model on the input and return the output
# n_classes: An integer, which is the number of trajectory class
# is_random_i: A boolean variable, which is whether includes a random intercept.
#               TRUE is including, FALSE is not including
# is_covariates:  A boolean varaibel, which is whether includes a random intercept.
#                 TRUE is including, FALSE is not including
# covariates: A char variable, which is covariates' names
# initial_starts: An integer variable, which is number of initial starts
# final_optimizations: An integer variable, which is number of final optimizations
# output_path_prefix: A char variable, which is output folder
# data_path: A char variable, which is input data location.

runGMMs <- function(n_classes,
                    is_random_i,
                    is_covariates,
                    trend,
                    y_names,
                    time_scores,
                    covariates,
                    initial_starts,
                    final_optimizations,
                    output_path_prefix,
                    data_path,
                    x_names,
                    useObs,
                    auxiliary){

  output_path <- createGMMInput(n_classes = n_classes,
                                is_random_i = is_random_i,
                                is_covariates = is_covariates,
                                trend = trend,
                                y_names = y_names,
                                time_scores = time_scores,
                                covariates = covariates,
                                initial_starts = initial_starts,
                                final_optimizations = final_optimizations,
                                output_path_prefix = output_path_prefix,
                                data_path = data_path,
                                x_names = x_names,
                                useObs = useObs,
                                auxiliary = auxiliary)
  print("output_path is: ")
  print(output_path)
  MplusAutomation::runModels(output_path[[1]],showOutput = TRUE)
  output_path_of_out <- paste(output_path[[1]],output_path[[2]],'.out',sep='')
  outFile <- file(output_path_of_out,"r")
  outFile_read <- readLines(outFile,n=-1)
  close(outFile)

  output_path_of_gh5 <- paste(output_path[[1]],output_path[[2]],'.gh5',sep='')

  trajectory_estimates_means <- mplus.get.estimated_means(output_path_of_gh5,'process1')
  trajectory_estimates_means <- data.frame(trajectory_estimates_means,stringsAsFactors = FALSE)
  names(trajectory_estimates_means) <- unlist(sapply(1:n_classes,FUN = function(x){paste("class",x,sep="")}))
  trajectory_estimates_means$time_score <- time_scores
  output_path_of_trajectory_est <- paste(output_path[[1]],output_path[[2]],'_trajectory_estimates_mean','.csv',sep='')
  write.csv(trajectory_estimates_means,output_path_of_trajectory_est, row.names = FALSE)

  outFile_read <- sapply(outFile_read,stringr::str_trim,side = "both",USE.NAMES = FALSE)
  return(list(output_path[[1]],outFile_read))
}
