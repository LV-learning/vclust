#' @import mclust
runAndCheckConvMclust <- function(class_range,
                                  y_names,
                                  input_dt,
                                  output_path_prefix,
                                  threshold,
                                  modelNames){
  output_dir_out <- ""
  for(n_classes in class_range){
    if(is.null(modelNames) == TRUE){
      mod <- mclust::Mclust(input_dt[,y_names],G = n_classes)
    }else{
      mod <- mclust::Mclust(input_dt[,y_names],G = n_classes,modelNames = modelNames)
    }
    if(is.null(mod) == TRUE){
      if(n_classes == 2){
        print(summary(mod))
        stop("The Mclust resulted null for the first class.")
      }
      return(list(n_classes-1,output_dir_out))
      break
    }
    summary_mod <- summary(mod)
    print(summary_mod)
    final_result <- as.data.frame(cbind(mod$z,mod$classification))
    output_dir <- paste(output_path_prefix,
                        "cP",
                        n_classes,
                        "/",
                        sep = "")
    if(dir.exists(output_dir) == FALSE){
      dir.create(output_dir)
    }
    aic <- 2*mod$df - 2*mod$loglik
    ##save prob
    output_dir_prob <- paste(output_dir,
                             "cP",
                             n_classes,
                             ".pp",
                             sep = "")
    print(output_dir_prob)
    write.table(final_result,
                output_dir_prob,
                sep = " ",
                col.names = FALSE,row.names = FALSE)
    ##save mean
    output_dir_mean <- paste(output_dir,
                             "cP",
                             n_classes,
                             "_",
                             "trajectory_estimates_mean.csv",
                             sep = "")
    sum_mod_mean <- as.data.frame(summary_mod$mean)
    sum_mod_mean <- cbind("variables"=row.names(sum_mod_mean),sum_mod_mean)

    write.table(sum_mod_mean,
                output_dir_mean,
                sep = ",",
                col.names = TRUE,row.names = FALSE)
    ##save aic,bic
    output_dir_aicbic <- paste(output_dir,
                               "aicbic.csv",
                               sep = "")

    aicbic <- data.frame("bic"=mod$bic,"aic"=aic,"modelName"=mod$modelName)
    write.table(aicbic,
                output_dir_aicbic,
                sep = ",",
                col.names = TRUE,
                row.names = FALSE)
    if(n_classes < 10){
      output_dir_out <- output_dir
    }else if(n_classes >= 10 & class_range[1] >= 10){
      output_dir_out <- substr(output_dir,1,nchar(output_dir)-3)
      output_dir_out <- paste(output_dir_out,"9/",sep="")
    }
    print("output_dir_out is")
    print(output_dir_out)
    class_population <- as.data.frame(table(mod$classification))$Freq
    if(any(sapply(class_population,FUN = function(x){x<threshold}))){
      return(list(n_classes-1,output_dir_out))
      break
    }
  }
  return(list(n_classes,output_dir_out))
}
##output: stop at n
##        folderpath like /Users/zetanli/Desktop/res_frac7/covariates_no_random_intercept/xcP10/

