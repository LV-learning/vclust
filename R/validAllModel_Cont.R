validAllModel_Cont <- function(cluster_names,
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
                          combined_posterior_prob_threshold,
                          optimize_prob_thresh = 0.5,
                          pcd_dropping_pct,
                          if_CV,
                          label_category1 = NULL,
                          kappa_filter_threshold = NULL,
                          kappa_results_threshold = NULL,
                          customized = F,
                          used_clusters = NULL,
                          cohend_SD = NULL){
  # final_dich_res_dir <-
  #   paste(output_path_prefix, "dich_without_PCD_results/", sep = "")
  # if (dir.exists(final_dich_res_dir) == FALSE) {
  #   dir.create(final_dich_res_dir)
  # }
  final_predicted_cluster_res_dir <-
    paste(output_path_prefix, "predicted_cluster_results/", sep = "")
  if (dir.exists(final_predicted_cluster_res_dir) == FALSE) {
    dir.create(final_predicted_cluster_res_dir)
  }

  final_predicted_coeff_res_dir <-
    paste(output_path_prefix, "predicted_coeff_results/", sep = "")
  if (dir.exists(final_predicted_coeff_res_dir) == FALSE) {
    dir.create(final_predicted_coeff_res_dir)
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
  pp_dt <- input_dt[,cluster_names]
  if(customized){
    pp_dt$placeholder <- 1
    n = length(used_clusters)
    pp_dt <- data.frame(trajectory_clusters = apply(pp_dt[,1:(ncol(pp_dt)-1),drop=F], 1, which.max))
    pp_dt <- pp_dt[pp_dt$trajectory_clusters %in% which(cluster_names %in% used_clusters),,drop = FALSE]
    input_dt <- input_dt[rownames(input_dt) %in% rownames(pp_dt),]
  }else{
    n = length(cluster_names)
    names(pp_dt) <- paste("P",1:ncol(pp_dt),sep = "")
    pp_dt$placeholder <- 1
  }

  if (!is.null(kappa_filter_threshold) |
      !is.null(kappa_results_threshold)) {
    use_combs_all <- data.frame()

    acomb <- dichPseudoBestZAModel_Cont(
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
  ##Run n range of no PCD/CV results and save results
  ##save top x combinations' name
  ##run n dichPseudoBypathamodel

  ##out put dich without PCD
  ##below are validations

  if (!is.null(kappa_filter_threshold) |
      !is.null(kappa_results_threshold)) {
    use_combs <- use_combs_all[use_combs_all$n == n, "comb_name"]
    print("use_combs is: ")
    print(use_combs)
    if (length(use_combs) != 0) {
      if(customized){
        input_dt <- input_dt[rownames(input_dt) %in% rownames(pp_dt),]
        res_n <- dichPseudoByPathAModelNoPCDCategoryOpt(
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
          kappa_filter_threshold =  kappa_filter_threshold,
          if_listwise_deletion = if_listwise_deletion,
          y_names = y_names,
          lr_maxiter = lr_maxiter,
          use_combinations = use_combs,
          pcd_dropping_pct = pcd_dropping_pct,
          if_CV = if_CV,
          label_category1 = label_category1
        )
      }else{
        res_n <- dichPseudoByPathAModelNoPCDOpt(
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
          use_combinations = use_combs,
          combined_posterior_prob_threshold =
            combined_posterior_prob_threshold,
          pcd_dropping_pct = pcd_dropping_pct,
          if_CV = if_CV,
          label_category1 = label_category1
        )
      }

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

      if(!sjmisc::is_empty(res_n[["coefficients"]])){
        write.csv(
          res_n[["coefficients"]],
          paste(
            final_predicted_coeff_res_dir,
            n,
            "_classes",
            ".csv",
            sep = ""
          ),
          row.names = FALSE
        )
      }
      metrics <- res_n[["metrics"]]
      mean_sd_dt <- res_n[["mean_sd_dt"]]
      mean_sd_dt$n_classes <- n
      metrics$n_classes <- n
      final_metrics_res <- rbind(final_metrics_res, metrics)
      pp_dt_and_if_in_validators_train <- cbind(pp_dt,
                                                res_n[["id_df"]])
    }
  }else
    {

    if(customized){
      print(used_clusters)
      print(head(pp_dt))
      input_dt <- input_dt[rownames(input_dt) %in% rownames(pp_dt),]
      res_n <- dichPseudoByPathAModelNoPCDCategory(
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
        label_category1 = label_category1
      )
    }else{
      res_n <- dichPseudoByPathAModelNoPCD(
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
        combined_posterior_prob_threshold =
          combined_posterior_prob_threshold,
        pcd_dropping_pct = pcd_dropping_pct,
        if_CV = if_CV,
        label_category1 = label_category1
      )
    }

    metrics <- res_n[["metrics"]]
    mean_sd_dt <- res_n[["mean_sd_dt"]]
    mean_sd_dt$n_classes <- n
    metrics$n_classes <- n
    final_metrics_res <- rbind(final_metrics_res, metrics)
    pp_dt_and_if_in_validators_train <- cbind(pp_dt,
                                              res_n[["id_df"]])
    }
  mean_sd_dt$n1s2 <- (mean_sd_dt$n - 1) * (mean_sd_dt$sd ^ 2)
  mean_sd_dt$combination_of_class_prob <-
    sapply(mean_sd_dt$whichSplit, FUN = get_comb_from_whichSplit)

  if(length(unique(mean_sd_dt$combination_of_class_prob)) > 1){
    dt_cohend <- apply(combn(unique(mean_sd_dt$combination_of_class_prob),2), 2, FUN = function(x){
      tmpdt = merge(mean_sd_dt[mean_sd_dt$combination_of_class_prob == x[1],],
                    mean_sd_dt[mean_sd_dt$combination_of_class_prob == x[2],],
                    by = c("repeated", "kfold", "validation_group", "n_classes", "train_or_test"))
      if(customized==T & !is.null(cohend_SD)){
        tmpdt$cohend = (tmpdt$mean.x - tmpdt$mean.y)/cohend_SD
      }else{
        tmpdt$cohend = (tmpdt$mean.x - tmpdt$mean.y)/sqrt((tmpdt$n1s2.x + tmpdt$n1s2.y)/(tmpdt$n.x + tmpdt$n.y - 2))
      }
      tmpdt$cohend_groups = paste(tmpdt$combination_of_class_prob.x, tmpdt$combination_of_class_prob.y, sep = " VS ")
      tmpdt_kfold = tmpdt[,c("repeated", "kfold", "validation_group", "n_classes", "cohend", "cohend_groups", "train_or_test")] %>%
        group_by(repeated, validation_group, train_or_test, cohend_groups, n_classes) %>%
        summarise(cohend_kmean = mean(cohend, na.rm=T), cohend_var = var(cohend, na.rm=T)/dplyr::n())
      tmpdt_kfold%>%
        group_by(validation_group, train_or_test, cohend_groups, n_classes) %>%
        summarise(cohend = mean(cohend_kmean), cohend_SE = (mean(cohend_var) + if_else(is.na(var(cohend_kmean)), 0, var(cohend_kmean)))^0.5 )
      #auc_d <- (mean(auc_d_k,na.rm = TRUE) + var(auc_m_k,na.rm = TRUE))^0.5
    })
    dt_cohend_final <- data.frame()
    for(i in dt_cohend){
      dt_cohend_final <- rbind(dt_cohend_final, i)
    }
    #print(mean_sd_dt)
    print(dt_cohend_final)

    write.csv(
      dt_cohend_final,
      paste(
        output_path_prefix,
        "/cohen's d.csv",
        sep = ""
      ),
      row.names = FALSE
    )


  }

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
  if(!sjmisc::is_empty(res_n[["coefficients"]])){
    write.csv(
      res_n[["coefficients"]],
      paste(
        final_predicted_coeff_res_dir,
        n,
        "_classes",
        ".csv",
        sep = ""
      ),
      row.names = FALSE
    )
  }

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
        "RMSE_SE_cv",
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

  } else
    {
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
    # write.csv(final_metrics_res,
    #           paste(output_path_prefix, "metrics_results.csv", sep = ""))
    #print("finished write metrics results")
  }

  return(final_metrics_res)
}
