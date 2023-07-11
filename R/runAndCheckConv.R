runAndCheckConv <- function(class_range,
                            is_random_i,
                            is_covariates,
                            covariates,
                            trend,
                            y_names,
                            time_scores,
                            initial_starts,
                            final_optimizations,
                            output_path_prefix,
                            data_path,
                            threshold,
                            x_names,
                            useObs ){
  output_dir_out <- ""
  for(n_classes in class_range){
    out_file <- runGMMs(n_classes = n_classes,
                        is_random_i = is_random_i,
                        is_covariates = is_covariates,
                        covariates = covariates,
                        trend = trend,
                        y_names = y_names,
                        time_scores = time_scores,
                        initial_starts = initial_starts,
                        final_optimizations = final_optimizations,
                        output_path_prefix = output_path_prefix,
                        data_path = data_path,
                        x_names = x_names,
                        useObs = useObs)

    class_counts <- classCountsStrToDf(getClassCountsStr(out_file[[2]]))
    if(n_classes < 10){
      output_dir_out <- out_file[[1]]
    }else if(n_classes >= 10 & class_range[1] >= 10){
      output_dir_out <- substr(out_file[[1]],1,nchar(out_file[[1]])-3)
      output_dir_out <- paste(output_dir_out,"9/",sep="")
    }
    print("output_dir_out is")
    print(output_dir_out)


    if(ifAllLarger(data_frame = class_counts,field = 'count',threshold = threshold) == FALSE){
      cat(paste("The model stops on class ",as.character(n_classes),".\n",sep=""))
      cat(paste("Because some class has number of people less than ",as.character(threshold),".\n",sep=""))
      #unlink(out_file[[1]],recursive = TRUE)
      print("list(n_classes-1,output_dir_out) is ")
      print(list(n_classes-1,output_dir_out))
      return(list(n_classes-1,output_dir_out))
      break
    }

    if(ifConvProblem(out_file)){
      cat(paste("The model stops on class ",as.character(n_classes),".\n",sep=""))
      cat("Because there are some convergence problems.")
      #unlink(out_file[[1]],recursive = TRUE)
      return(list(n_classes-1,output_dir_out))
      break
    }
  }
  cat("Models run well for input classes range")
  return(list(n_classes,output_dir_out))
}
