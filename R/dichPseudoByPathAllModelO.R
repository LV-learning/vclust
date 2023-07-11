#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarise if_else arrange desc group_modify ungroup
#' @importFrom utils read.csv write.csv
dichPseudoByPathAllModelO <- function(folder_path,
                                      K_fold,
                                      repeated_folds_R,
                                      input_dt,
                                      x_names,
                                      validators,
                                      seed_num,
                                      output_path_prefix,
                                      validation_data_fraction,
                                      if_listwise_deletion,
                                      y_names,
                                      lr_maxiter,
                                      pcd_dropping_pct,
                                      if_CV,
                                      label_category1 = NULL){
  final_pp_if_validators_res_dir <- paste(output_path_prefix,"pp_and_if_train_validators/",sep = "")
  if(dir.exists(final_pp_if_validators_res_dir) == FALSE){
    dir.create(final_pp_if_validators_res_dir)
  }
  # final_predicted_cluster_res_dir <-
  #   paste(output_path_prefix, "predicted_cluster_results/", sep = "")
  # if (dir.exists(final_predicted_cluster_res_dir) == FALSE) {
  #   dir.create(final_predicted_cluster_res_dir)
  # }
  final_roc_res_dir <- paste(output_path_prefix,"roc_data/",sep = "")
  if(dir.exists(final_roc_res_dir) == FALSE){
    dir.create(final_roc_res_dir)
  }
  ##import input data
  pp_dt <- getAProbFromResultPath(folder_path, 2)
  final_metrics_res <- data.frame()
  ##below are validations
  res_n <- dichPseudoByPathAModelNoPCDCategory(pp_dt = pp_dt,
                                               ##model classes
                                               n = 2,
                                               K_fold = K_fold,
                                               repeated_folds_R = repeated_folds_R,
                                               input_dt = input_dt,
                                               x_names = x_names,
                                               validators = validators,
                                               seed_num = seed_num,
                                               validation_data_fraction = validation_data_fraction,
                                               if_listwise_deletion = if_listwise_deletion,
                                               y_names = y_names,
                                               lr_maxiter = lr_maxiter,
                                               pcd_dropping_pct = pcd_dropping_pct,
                                               if_CV = if_CV,
                                               label_category1 = label_category1)
  # if(!sjmisc::is_empty(res_n[["dt_y_test"]])){
  #   write.csv(
  #     res_n[["dt_y_test"]],
  #     paste(
  #       final_predicted_cluster_res_dir,
  #       n,
  #       "_classes",
  #       ".csv",
  #       sep = ""
  #     ),
  #     row.names = FALSE
  #   )
  # }
  metrics <- res_n[["metrics"]]
  metrics$n_classes <- 2
  final_metrics_res <- rbind(final_metrics_res,metrics)
  pp_dt_and_if_in_validators_train <- cbind(pp_dt,
                                            res_n[["id_df"]])
  write.csv(pp_dt_and_if_in_validators_train,
            paste(final_pp_if_validators_res_dir,
                  2,
                  "_classes",
                  ".csv",
                  sep = ""),
            row.names = FALSE)
  roc_res <- res_n[["rocs"]]
  roc_res <- roc_res[roc_res$whichSplit == "C1No9combP2",]
  write.csv(roc_res,
            paste(final_roc_res_dir,
                  2,
                  "_classes",
                  ".csv",
                  sep = ""),
            row.names = FALSE)


  final_metrics_res$choose_m <- sapply(final_metrics_res$whichSplit,FUN = get_choose_m_from_whichSplit)
  final_metrics_res$number_of_choice <- sapply(final_metrics_res$whichSplit,FUN = get_num_from_whichSplit)
  final_metrics_res$combination_of_class_prob <- sapply(final_metrics_res$whichSplit,FUN = get_comb_from_whichSplit)

  if(validation_data_fraction!=1){
    names(final_metrics_res) <- c("accuracy_mean_cv","accuracy_sd_cv","AUC_mean_cv","AUC_sd_cv","sensitivity_mean_cv","sensitivity_sd_cv",
                                  "specificity_mean_cv","specificity_sd_cv","kappa_mean_cv","kappa_sd_cv",
                                  "accuracy_mean_test","accuracy_sd_test","AUC_mean_test","AUC_sd_test",
                                  "sensitivity_mean_test","sensitivity_sd_test","specificity_mean_test",
                                  "specificity_sd_test","kappa_mean_test","kappa_sd_test","whichSplit","validation_group",
                                  "n_classes","choose_m","number_of_choices","combination_of_class_probabilities")
    # write.csv(final_metrics_res,paste(output_path_prefix,"metrics_results.csv",sep = ""))

  }else{
    names(final_metrics_res) <- c("accuracy_mean","accuracy_sd","AUC_mean","AUC_sd","sensitivity_mean","sensitivity_sd","specificity_mean",
                                  "specificity_sd","kappa_mean","kappa_sd","whichSplit","validation_group",
                                  "n_classes","choose_m","number_of_choices","combination_of_class_probabilities")
    # write.csv(final_metrics_res,paste(output_path_prefix,"metrics_results.csv",sep = ""))
    #print("finished write metrics results")
  }
  return(final_metrics_res)
}
