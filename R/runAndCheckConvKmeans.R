
runAndCheckConvKmeans <- function(class_range,
                                  y_names,
                                  input_dt,
                                  output_path_prefix,
                                  threshold,
                                  kmeans_gap_stats_B,
                                  kmeans_iter,
                                  seed_num){

  output_dir_out <- ""

  for(n_classes in class_range){
    set.seed(seed_num['seed_num_unsupervised_model'])
    mod <- stats::kmeans(input_dt[,y_names],
                  centers = n_classes,
                  iter.max = kmeans_iter,
                  nstart = 1
    )

    final_result <- as.data.frame(mod$cluster)
    names(final_result) <- c("cluster")
    print(mod)
    print(mod$iter)
    output_dir <- paste(output_path_prefix,
                        "cP",
                        n_classes,
                        "/",
                        sep = "")
    print(output_dir)
    if(dir.exists(output_dir) == FALSE){
      dir.create(output_dir)
    }

    mean_silhoutte <- mean(cluster::silhouette(x=mod$cluster,
                                      dist=stats::dist(input_dt[,y_names],
                                                method = "euclidean"))[,3])

    ###save class membership
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
    sum_mod_mean <- as.data.frame(t(mod$centers))
    sum_mod_mean <- cbind("variables"=row.names(sum_mod_mean),sum_mod_mean)

    write.table(sum_mod_mean,
                output_dir_mean,
                sep = ",",
                col.names = TRUE,row.names = FALSE)
    ##save metrics
    output_dir_metrics <- paste(output_dir,
                                "model_metrics.csv",
                                sep = "")

    aicbic <- data.frame("withinss"=mod$tot.withinss,
                         "betweenss"=mod$betweenss,
                         "mean_silhouette"=mean_silhoutte)
    write.table(aicbic,
                output_dir_metrics,
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


    if(any(sapply(mod$size,FUN = function(x){x<threshold}))|mod$ifault == 2){
      if(mod$ifault == 2){
        cat("The model didn't converge in provided number of iterations and stop")
      }
      gap_stats <- cluster::clusGap(input_dt[,y_names],
                           FUNcluster = stats::kmeans,
                           nstart=1,
                           iter.max = kmeans_iter,
                           K.max = (n_classes-1),
                           B=kmeans_gap_stats_B)
      gap_stats <- as.data.frame(gap_stats$Tab)
      gap_stats$n_classes <-rownames(gap_stats)
      gap_stats <- gap_stats[class_range[1]:nrow(gap_stats),]
      write.table(gap_stats,
                  paste(output_path_prefix,
                        "gap_stats.csv",
                        sep = ""),
                  sep = ",",
                  col.names = TRUE,
                  row.names = FALSE)
      return(list(n_classes-1,output_dir_out))
      break
    }
  }
  gap_stats <- cluster::clusGap(input_dt[,y_names],
                       FUNcluster = stats::kmeans,
                       nstart=1,
                       iter.max = kmeans_iter,
                       K.max = n_classes,
                       B=kmeans_gap_stats_B)
  gap_stats <- as.data.frame(gap_stats$Tab)
  gap_stats$n_classes <-rownames(gap_stats)
  gap_stats <- gap_stats[class_range[1]:nrow(gap_stats),]
  write.table(gap_stats,
              paste(output_path_prefix,
                    "gap_stats.csv",
                    sep = ""),
              sep = ",",
              col.names = TRUE,
              row.names = FALSE)
  return(list(n_classes,output_dir_out))
}
