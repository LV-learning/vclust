validAllModelPCD_Cont <- function(cluster_names,
                             r_pseudo,
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
                             kappa_results_threshold_final_metrics = 0.05,
                             optimize_prob_thresh = 0.5,
                             pcd_dropping_pct,
                             if_CV,
                             label_category1 = NULL) {
  final_predicted_cluster_res_dir <-
    paste(output_path_prefix, "predicted_cluster_results/", sep = "")
  if (dir.exists(final_predicted_cluster_res_dir) == FALSE) {
    dir.create(final_predicted_cluster_res_dir)
  }

  final_pp_if_validators_res_dir <-
    paste(output_path_prefix, "pp_and_if_train_validators/", sep = "")
  if (dir.exists(final_pp_if_validators_res_dir) == FALSE) {
    dir.create(final_pp_if_validators_res_dir)
  }

  final_roc_res_dir <-
    paste(output_path_prefix, "roc_data/", sep = "")
  if (dir.exists(final_roc_res_dir) == FALSE) {
    dir.create(final_roc_res_dir)
  }

  final_roc_res <- data.frame()
  final_metrics_res <- data.frame()
  n <- length(cluster_names)

  pp_dt <- input_dt[,cluster_names]
  names(pp_dt) <- paste("P",1:ncol(pp_dt),sep = "")
  pp_dt$placeholder <- 1
  comb_dt <- as.data.frame(allCombOfAModelOpt(pp_dt, n))
  comb_dt[comb_dt > 1] <- 1
  if(!is.null(label_category1)){
    label_category1u <- c()
    for(label_category1i in label_category1){
      print(label_category1)
      label_category1i <- toupper(stringr::str_extract(gsub(" |,","",label_category1i),'([pP][1234567890]+)+'))
      m <- length(unlist(strsplit(label_category1i,"P"))[unlist(strsplit(label_category1i,"P"))!=""])
      label_category1i <- paste("C",
                                m,
                                "No",
                                9,
                                "comb",
                                label_category1i,
                                sep='')
      print("label_category1i is ")
      print(label_category1i)
      label_category1u <- c(label_category1u, label_category1i)
    }
    comb_dt <- comb_dt[, label_category1u, drop=FALSE]
  }
  PCD_list <- apply(comb_dt, 2, pseudoVec, r_pseudo, seed_num['seed_num_PCD'])
  res_n <- dichPseudoByPathAModel(
    pp_dt = pp_dt,
    ##model classes
    n = n,
    r_pseudo = r_pseudo,
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
    PCD_list = PCD_list,
    pcd_dropping_pct = pcd_dropping_pct,
    if_CV = if_CV
  )
  if(!sjmisc::is_empty(res_n[["dt_y_test"]])){

    write.csv(
      res_n[["dt_y_test"]],
      paste(
        final_predicted_cluster_res_dir,
        n,
        "_classes_show_each_pcd",
        ".csv",
        sep = ""
      ),
      row.names = FALSE
    )

    res_n_tmp <- res_n[["dt_y_test"]]
    res_n_tmp <- res_n_tmp %>%
      group_by(validation_group,whichSplit,original_id) %>%
      summarise(y=mean(y),predicted_y=mean(predicted_y))
    write.csv(
      res_n_tmp,
      paste(
        final_predicted_cluster_res_dir,
        n,
        "_classes_pcd_majority",
        ".csv",
        sep = ""
      ),
      row.names = FALSE
    )
  }
  metrics <- res_n[["metrics"]]
  metrics$n_classes <- n
  final_metrics_res <- rbind(final_metrics_res, metrics)
  pp_dt_and_if_in_validators_train <- cbind(pp_dt,
                                            res_n[["id_df"]])
  write.csv(
    pp_dt_and_if_in_validators_train,
    paste(
      final_pp_if_validators_res_dir,
      n,
      "_classes",
      ".csv",
      sep = ""
    ),
    row.names = FALSE
  )
  print("end model")

  print(final_metrics_res)
  #sapply(res_allModel[[2]]$whichSplit,FUN = get_comb_from_whichSplit )
  final_metrics_res$choose_m <-
    sapply(final_metrics_res$whichSplit, FUN = get_choose_m_from_whichSplit)
  final_metrics_res$number_of_choice <-
    sapply(final_metrics_res$whichSplit, FUN = get_num_from_whichSplit)
  final_metrics_res$combination_of_class_prob <-
    sapply(final_metrics_res$whichSplit, FUN = get_comb_from_whichSplit)
  if (validation_data_fraction != 1) {
    names(final_metrics_res) <-
      c(
        "MSE_cv",
        "MSE_SE_cv",
        "RMSE_cv",
        "AUC_SE_cv",
        "MAE_cv",
        "MAE_SE_cv",
        "r_square_cv",
        "r_square_SE_cv",
        "adj_r_square_cv",
        "adj_r_square_SE_cv",
        'aic_cv',
        'aic_SE_cv',
        "MSE_test",
        "MSE_SE_test",
        "RMSE_test",
        "RMSE_SE_test",
        "MAE_test",
        "MAE_SE_test",
        "r_square_test",
        "r_square_SE_test",
        "adj_r_square_test",
        "adj_r_square_SE_test",
        'aic_test',
        'aic_SE_test',
        "whichSplit",
        "validation_group",
        "n_classes",
        "choose_m",
        "number_of_choices",
        "combination_of_class_probabilities"
      )
    if (!is.null(kappa_results_threshold_final_metrics)) {
      final_metrics_res <- final_metrics_res[final_metrics_res$MSE_cv > kappa_results_threshold_final_metrics |
                                               !final_metrics_res$validation_group %in% c("validators1"), ]

      final_metrics_res <- final_metrics_res[paste(final_metrics_res$n,
                                                   final_metrics_res$whichSplit,
                                                   sep = "") %in%
                                               paste(final_metrics_res[final_metrics_res$validation_group %in% c("validators1"), "n"],
                                                     final_metrics_res[final_metrics_res$validation_group %in% c("validators1"), "whichSplit"],
                                                     sep = ""), ]
    }
  } else{
    names(final_metrics_res) <-
      c(
        "MSE",
        "MSE_SE",
        "RMSE",
        "RMSE_SE",
        "MAE",
        "MAE_SE",
        "R_square",
        "R_square_SE",
        "adj_R_square",
        "adj_R_square_SE",
        "AIC",
        "AIC_SE",
        "whichSplit",
        "validation_group",
        "n_classes",
        "choose_m",
        "number_of_choices",
        "combination_of_class_probabilities"
      )
    if (!is.null(kappa_results_threshold_final_metrics)) {
      final_metrics_res <- final_metrics_res[final_metrics_res$MSE > kappa_results_threshold_final_metrics |
                                               !final_metrics_res$validation_group %in% c("validators1"), ]

      final_metrics_res <- final_metrics_res[paste(final_metrics_res$n,
                                                   final_metrics_res$whichSplit,
                                                   sep = "") %in%
                                               paste(final_metrics_res[final_metrics_res$validation_group %in% c("validators1"), "n"],
                                                     final_metrics_res[final_metrics_res$validation_group %in% c("validators1"), "whichSplit"],
                                                     sep = ""), ]
    }
    # write.csv(final_metrics_res,
    #           paste(output_path_prefix, "metrics_results.csv", sep = ""))
    #print("finished write metrics results")
  }
  #print("finished write metrics results2")


  return(final_metrics_res)
}
