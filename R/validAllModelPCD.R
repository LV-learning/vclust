validAllModelPCD <- function(cluster_names,
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
    label_category1 <- strsplit(label_category1," |,")[[1]]
    m <- length(label_category1)
    label_category1 <- paste(paste("P",which(cluster_names %in% label_category1),sep = ""),collapse="")
    label_category1 <- paste("C",
                             m,
                             "No",
                             9,
                             "comb",
                             label_category1,
                             sep='')
    print("label_category1 is ")
    print(label_category1)
    comb_dt <- comb_dt[,label_category1,drop=FALSE]
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
    res_n_tmp %>%
      group_by(validation_group,whichSplit,original_id) %>%
      summarise(y=if_else(sum(y) >= (r_pseudo/2),1,0),predicted_y=if_else(sum(predicted_y) >= (r_pseudo/2),1,0))
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
  write.csv(res_n[["rocs"]],
            paste(final_roc_res_dir,
                  n,
                  "_classes",
                  ".csv",
                  sep = ""),
            row.names = FALSE)
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
        "accuracy_mean_cv",
        "accuracy_sd_cv",
        "AUC_mean_cv",
        "AUC_sd_cv",
        "sensitivity_mean_cv",
        "sensitivity_sd_cv",
        "specificity_mean_cv",
        "specificity_sd_cv",
        "kappa_mean_cv",
        "kappa_sd_cv",
        "accuracy_mean_test",
        "accuracy_sd_test",
        "AUC_mean_test",
        "AUC_sd_test",
        "sensitivity_mean_test",
        "sensitivity_sd_test",
        "specificity_mean_test",
        "specificity_sd_test",
        "kappa_mean_test",
        "kappa_sd_test",
        "whichSplit",
        "validation_group",
        "n_classes",
        "choose_m",
        "number_of_choices",
        "combination_of_class_probabilities"
      )
    # write.csv(final_metrics_res,
    #           paste(output_path_prefix, "metrics_results.csv", sep = ""))

  } else{
    names(final_metrics_res) <-
      c(
        "accuracy_mean",
        "accuracy_sd",
        "AUC_mean",
        "AUC_sd",
        "sensitivity_mean",
        "sensitivity_sd",
        "specificity_mean",
        "specificity_sd",
        "kappa_mean",
        "kappa_sd",
        "whichSplit",
        "validation_group",
        "n_classes",
        "choose_m",
        "number_of_choices",
        "combination_of_class_probabilities"
      )
    # write.csv(final_metrics_res,
    #           paste(output_path_prefix, "metrics_results.csv", sep = ""))
    #print("finished write metrics results")
  }
  #print("finished write metrics results2")
  final_metrics_res_w_aicbic <-final_metrics_res
  #print("finished aic bic results")
  if (validation_data_fraction != 1) {
    #print(final_metrics_res_w_aicbic)
    final_metrics_res_w_aicbic <-
      final_metrics_res_w_aicbic[, c(
        "accuracy_mean_cv",
        "accuracy_sd_cv",
        "AUC_mean_cv",
        "AUC_sd_cv",
        "sensitivity_mean_cv",
        "sensitivity_sd_cv",
        "specificity_mean_cv",
        "specificity_sd_cv",
        "kappa_mean_cv",
        "kappa_sd_cv",
        "accuracy_mean_test",
        "accuracy_sd_test",
        "AUC_mean_test",
        "AUC_sd_test",
        "sensitivity_mean_test",
        "sensitivity_sd_test",
        "specificity_mean_test",
        "specificity_sd_test",
        "kappa_mean_test",
        "kappa_sd_test",
        "whichSplit",
        "validation_group",
        "n_classes",
        "choose_m",
        "number_of_choices",
        "combination_of_class_probabilities"
      )]

    final_metrics_res_w_aicbic <-
      arrange(final_metrics_res_w_aicbic,
              validation_group,
              desc(kappa_mean_cv))
    if (!is.null(kappa_results_threshold_final_metrics)) {
      final_metrics_res_w_aicbic <- final_metrics_res_w_aicbic[final_metrics_res_w_aicbic$kappa_mean_cv > kappa_results_threshold_final_metrics |
                                                                 !final_metrics_res_w_aicbic$validation_group %in% c("validators1"), ]

      final_metrics_res_w_aicbic <- final_metrics_res_w_aicbic[paste(final_metrics_res_w_aicbic$n,
                                                                     final_metrics_res_w_aicbic$whichSplit,
                                                                     sep = "") %in%
                                                                 paste(final_metrics_res_w_aicbic[final_metrics_res_w_aicbic$validation_group %in% c("validators1"), "n"],
                                                                       final_metrics_res_w_aicbic[final_metrics_res_w_aicbic$validation_group %in% c("validators1"), "whichSplit"],
                                                                       sep = ""), ]
    }

    # write.csv(
    #   final_metrics_res_w_aicbic,
    #   paste(
    #     output_path_prefix,
    #     "metrics_results_w_aic_bic.csv",
    #     sep = ""
    #   )
    # )

  } else{
    final_metrics_res_w_aicbic <-
      final_metrics_res_w_aicbic[, c(
        "accuracy_mean",
        "accuracy_sd",
        "AUC_mean",
        "AUC_sd",
        "sensitivity_mean",
        "sensitivity_sd",
        "specificity_mean",
        "specificity_sd",
        "kappa_mean",
        "kappa_sd",
        "whichSplit",
        "validation_group",
        "n_classes",
        "choose_m",
        "number_of_choices",
        "combination_of_class_probabilities"
      )]
    final_metrics_res_w_aicbic$full_class <-
      sapply(
        final_metrics_res_w_aicbic$n_classes,
        FUN = function(x) {
          nrange <- 1:x
          return(paste("P", paste(nrange, collapse = "P"), sep = ""))
        }
      )
    final_metrics_res_w_aicbic$regular_exp <-
      sapply(
        final_metrics_res_w_aicbic$combination_of_class_probabilities,
        FUN = function(x) {
          regular_exp <- unlist(strsplit(x, "P"))
          regular_exp <- regular_exp[regular_exp != ""]
          regular_exp <-
            sapply(
              regular_exp,
              FUN = function(x) {
                return(paste("(P", x, ")", sep = ""))
              }
            )
          regular_exp <- paste(regular_exp, collapse = "|")
        }
      )
    final_metrics_res_w_aicbic$exclusive_class <-
      apply(
        final_metrics_res_w_aicbic,
        1,
        FUN = function(x) {
          gsub(x['regular_exp'], "", x['full_class'])
        }
      )
    final_metrics_res_w_aicbic$exclusive_class_pointer <- FALSE
    final_metrics_res_w_aicbic <- final_metrics_res_w_aicbic %>%
      group_by(validation_group, n_classes) %>%
      group_modify( ~ replaceExclusiveClass(.x)) %>%
      ungroup()
    final_metrics_res_w_aicbic$exclusive_class <-
      paste(
        final_metrics_res_w_aicbic$n_classes,
        final_metrics_res_w_aicbic$exclusive_class,
        sep = ""
      )

    max_kappa <- final_metrics_res_w_aicbic %>%
      group_by(exclusive_class) %>%
      summarise(max_kappa = max(kappa_mean, na.rm = TRUE)) %>%
      ungroup()

    final_metrics_res_w_aicbic <-
      merge(
        final_metrics_res_w_aicbic,
        max_kappa,
        by.x = 'exclusive_class',
        by.y = 'exclusive_class',
        all.x = TRUE
      )

    final_metrics_res_w_aicbic <-
      arrange(final_metrics_res_w_aicbic,
              validation_group,
              #desc(n_classes),
              desc(max_kappa))

    final_metrics_res_w_aicbic <-
      final_metrics_res_w_aicbic[, c(
        "accuracy_mean",
        "accuracy_sd",
        "AUC_mean",
        "AUC_sd",
        "sensitivity_mean",
        "sensitivity_sd",
        "specificity_mean",
        "specificity_sd",
        "kappa_mean",
        "kappa_sd",
        "whichSplit",
        "validation_group",
        "n_classes",
        "choose_m",
        "number_of_choices",
        "combination_of_class_probabilities"
      )]
    if (!is.null(kappa_results_threshold_final_metrics)) {
      final_metrics_res_w_aicbic <- final_metrics_res_w_aicbic[final_metrics_res_w_aicbic$kappa_mean > kappa_results_threshold_final_metrics |
                                                                 !final_metrics_res_w_aicbic$validation_group %in% c("validators1"), ]

      final_metrics_res_w_aicbic <- final_metrics_res_w_aicbic[paste(final_metrics_res_w_aicbic$n,
                                                                     final_metrics_res_w_aicbic$whichSplit,
                                                                     sep = "") %in%
                                                                 paste(final_metrics_res_w_aicbic[final_metrics_res_w_aicbic$validation_group %in% c("validators1"), "n"],
                                                                       final_metrics_res_w_aicbic[final_metrics_res_w_aicbic$validation_group %in% c("validators1"), "whichSplit"],
                                                                       sep = ""), ]
    }
    # write.csv(
    #   final_metrics_res_w_aicbic,
    #   paste(
    #     output_path_prefix,
    #     "metrics_results_w_aic_bic.csv",
    #     sep = ""
    #   )
    # )

  }


  return(final_metrics_res_w_aicbic)
}
