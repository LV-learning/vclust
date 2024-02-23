validAllModel_Cat_Cont <- function(cluster_names,
                              ##model classes
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
                              label_category1 = NULL,
                              kappa_filter_threshold = NULL,
                              kappa_results_threshold = NULL) {
  # final_dich_res_dir <-
  #   paste(output_path_prefix, "dich_without_PCD_results/", sep = "")
  # if (dir.exists(final_dich_res_dir) == FALSE) {
  #   dir.create(final_dich_res_dir)
  # }

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

  final_predicted_cluster_res_dir <-
    paste(output_path_prefix, "predicted_cluster_results/", sep = "")
  if (dir.exists(final_predicted_cluster_res_dir) == FALSE) {
    dir.create(final_predicted_cluster_res_dir)
  }

  final_roc_res <- data.frame()
  ##import input data
  final_metrics_res <- data.frame()
  n <- length(cluster_names)
  pp_dt <- input_dt[,cluster_names]

  if (!is.null(kappa_filter_threshold) |
      !is.null(kappa_results_threshold)) {
    use_combs_all <- data.frame()
    acomb <- dichPseudoBestZAModelCategory_Cont(
      folder_path = NULL,
      ##model classes
      n = n,
      input_dt = input_dt,
      validators = validators,
      seed_num = seed_num,
      kappa_filter_threshold =  kappa_filter_threshold,
      if_listwise_deletion = if_listwise_deletion,
      y_names = y_names,
      lr_maxiter = lr_maxiter,
      optimize_prob_thresh = optimize_prob_thresh,
      pp_dt = pp_dt
    )
    #kappas,comb_name,n
    use_combs_all <- rbind(use_combs_all, acomb)

    if (!is.null(kappa_results_threshold)) {
      use_combs_all <-
        use_combs_all[use_combs_all$kappas > kappa_results_threshold, ]
    }
    use_combs_all <-
      use_combs_all[order(use_combs_all$kappas, decreasing = TRUE), ]

    if (!is.null(kappa_filter_threshold)) {
      if (nrow(use_combs_all) > kappa_filter_threshold) {
        use_combs_all <- use_combs_all[1:kappa_filter_threshold, ]
      }
    }

    print("use_combs_all is:")
    print(use_combs_all)
  }
  use_combs2 = NULL
  if (!is.null(kappa_filter_threshold) |
      !is.null(kappa_results_threshold)) {
    use_combs <- use_combs_all[use_combs_all$n == n, "comb_name"]
    print("use combs is:")
    print(use_combs)
    if(length(use_combs) != 0) use_combs2 = use_combs
  }

  if (!is.null(kappa_filter_threshold) |
      !is.null(kappa_results_threshold)) {
    use_combs_all <- data.frame()
    acomb <- dichPseudoBestZAModelCategory(
      folder_path = NULL,
      ##model classes
      n = n,
      input_dt = input_dt,
      validators = validators,
      seed_num = seed_num,
      kappa_filter_threshold =  kappa_filter_threshold,
      if_listwise_deletion = if_listwise_deletion,
      y_names = y_names,
      lr_maxiter = lr_maxiter,
      optimize_prob_thresh = optimize_prob_thresh,
      pp_dt = pp_dt
    )
    #kappas,comb_name,n
    use_combs_all <- rbind(use_combs_all, acomb)

    if (!is.null(kappa_results_threshold)) {
      use_combs_all <-
        use_combs_all[use_combs_all$kappas > kappa_results_threshold, ]
    }
    use_combs_all <-
      use_combs_all[order(use_combs_all$kappas, decreasing = TRUE), ]

    if (!is.null(kappa_filter_threshold)) {
      if (nrow(use_combs_all) > kappa_filter_threshold) {
        use_combs_all <- use_combs_all[1:kappa_filter_threshold, ]
      }
    }

    print("use_combs_all is:")
    print(use_combs_all)
  }
  use_combs2 = NULL
  if (!is.null(kappa_filter_threshold) |
      !is.null(kappa_results_threshold)) {
    use_combs <- use_combs_all[use_combs_all$n == n, "comb_name"]
    print("use combs is:")
    print(use_combs)
    if(length(use_combs) != 0) use_combs2 = use_combs
  }

  input_dt <- input_dt[,!(names(input_dt) %in% cluster_names)]
  res_n <- dichPseudoByPathAModelNoPCDCategory_syncF(
    pp_dt = pp_dt,
    ##model classes
    n = n,
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
    use_combinations = use_combs2,
    label_category1 = label_category1
  )

  if(!sjmisc::is_empty(res_n[["dt_y_test"]])){
    write.csv(
      res_n[["dt_y_test"]],
      paste(
        final_predicted_cluster_res_dir,
        n,
        "_classes",
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
    # write.csv(final_metrics_res,
    #           paste(output_path_prefix, "metrics_results.csv", sep = ""))

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
    cluster_namesbu <- cluster_names
    final_metrics_res <- final_metrics_res %>%
      transmute(model_type = "-",
                model_spec1 = "-",
                model_spec2 = "-",
                model_spec3 = "-",
                n_clusters = n_classes,
                cluster_names = paste(cluster_namesbu,collapse = ""),
                label_group1 = sapply(final_metrics_res$combination_of_class_probabilities,FUN=function(x){paste(cluster_namesbu[as.numeric(strsplit(x,"P")[[1]][strsplit(x,"P")[[1]]!=""])],collapse="")}),
                validator = validation_group,
                MSE = MSE,
                MSE_SE = MSE_SE,
                RMSE = RMSE,
                RMSE_SE = RMSE_SE,
                MAE = MAE,
                MAE_SE = MAE_SE,
                R_square = R_square,
                R_square_SE = R_square_SE,
                adj_R_square = adj_R_square,
                adj_R_square_SE = adj_R_square_SE,
                AIC = AIC,
                AIC_SE = AIC_SE
      )
  }
  #print("finished write metrics results2")

  return(final_metrics_res)
}
