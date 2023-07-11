dichPseudoByPathAModelNoPCD <- function(pp_dt,
                                        ##model classes
                                        n,
                                        K_fold,
                                        repeated_folds_R,
                                        input_dt,
                                        x_names,
                                        ##variable name like PGBI or SCAREP
                                        validators,
                                        seed_num,
                                        validation_data_fraction,
                                        if_listwise_deletion,
                                        y_names,
                                        lr_maxiter,
                                        combined_posterior_prob_threshold,
                                        pcd_dropping_pct,
                                        if_CV,
                                        label_category1 = NULL) {
  print(
    '########################using dichPseudoByPathAModelNoPCD##############################'
  )
  validators_name <-
    paste("validators", 1:length(validators), sep = "")
  final_metrics_all_validators <- data.frame()
  final_out_list <- list()
  comb_dt <- as.data.frame(allCombOfAModelOpt(pp_dt, n))
  comb_dt[comb_dt > 1] <- 1
  if(!is.null(label_category1)){
    label_category1 <- strsplit(label_category1," |,")[[1]]
    label_category1 <- paste(paste("P",1:length(label_category1),sep = ""),collapse="")
    m <- length(unlist(strsplit(label_category1,"P"))[unlist(strsplit(label_category1,"P"))!=""])
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
  diched_comb_dt <- data.frame(comb_dt)
  diched_comb_dt[diched_comb_dt < combined_posterior_prob_threshold] <-
    0
  diched_comb_dt[diched_comb_dt >= combined_posterior_prob_threshold] <-
    1
  diched_comb_dt_names <- names(diched_comb_dt)
  print("diched_comb_dt is ")
  train_id_list <- list()
  dt_rnames_list <- list()
  dt_rmna_list <- list()
  dt_y_test <- data.frame()
  for (v_id in 1:length(validators)) {
    seed_num <- validators[[v_id]]$seed_num
    if(!sjmisc::is_empty(seed_num['seed_num_split'])){
      set.seed(seed_num['seed_num_split'])
    }
    dt_rmna <- dichPseudoPrepare(
      validator = validators[[v_id]],
      pp_dt = pp_dt,
      ##model classes
      n = n,
      input_dt = input_dt,
      if_listwise_deletion = if_listwise_deletion,
      y_names = y_names
    )
    if (identical(validators[[v_id]]$validator_variables, c("PGBI30","PGBI36","PGBI42","PGBI48")) &
        attr(validators[[v_id]],"class") == "validator_to01") {
      dt_rmna <- dt_rmna[!rownames(dt_rmna) == "391", ]
    }
    dt_rnames <- rownames(dt_rmna)
    train_size <- round(validation_data_fraction * nrow(dt_rmna))
    print(train_size)
    train_ind <- sample(dt_rnames, train_size, replace = FALSE)
    train_id_list[[v_id]] <- train_ind
    dt_rnames_list[[v_id]] <- dt_rnames
    dt_rmna_list[[v_id]] <- dt_rmna
  }
  ##dichpesudoPrepare
  ##impute missing data
  roc_res <- data.frame()
  for (v_id in 1:length(validators)) {
    seed_num <- validators[[v_id]]$seed_num
    dt_rnames <- dt_rnames_list[[v_id]]
    dt_rmna <- dt_rmna_list[[v_id]]
    print("size validators is:")
    print(nrow(dt_rnames))
    if (validation_data_fraction != 1) {
      final_metrics <- data.frame()
      train_ind <- train_id_list[[v_id]]
      for (dich_name in diched_comb_dt_names) {
        if (if_CV) {
          m_res <-
            calculateMetricsForAProbSplitNoPCDCategory(
              validator = validators[[v_id]],
              categoryVec = diched_comb_dt[dt_rnames, dich_name, drop = FALSE],
              K_fold = K_fold,
              repeated_folds_R = repeated_folds_R,
              input_and_pp_and_var_df = dt_rmna,
              seed_num = seed_num,
              validation_data_fraction = validation_data_fraction,
              train_ind = train_ind,
              lr_maxiter = lr_maxiter,
              n = n,
              pcd_dropping_pct = pcd_dropping_pct
            )
        } else{
          m_res <-
            calculateMetricsForAProbSplitNoPCDCategoryNoCV(
              validator = validators[[v_id]],
              categoryVec = diched_comb_dt[dt_rnames, dich_name, drop = FALSE],
              input_and_pp_and_var_df = dt_rmna,
              seed_num = seed_num,
              validation_data_fraction = validation_data_fraction,
              train_ind = train_ind,
              lr_maxiter = lr_maxiter,
              n = n,
              pcd_dropping_pct = pcd_dropping_pct
            )
        }


        if(attr(validators[[v_id]],"class") %in% c("validator_direct","validator_flip")  &
           dich_name %in% validators[[v_id]]$predicted_cluster_combination &
           n %in% validators[[v_id]]$predicted_cluster_n){
          dt_y_test_tmp <- m_res[[3]]
          dt_y_test_tmp$whichSplit <- dich_name
          dt_y_test_tmp$validation_group <- validators_name[v_id]
          dt_y_test <- rbind(dt_y_test, dt_y_test_tmp)
        }
        roc_tmp <- m_res[[2]]
        m_res <- m_res[[1]]
        roc_tmp$whichSplit <- dich_name
        roc_tmp$validation_group <- validators_name[v_id]
        roc_res <- rbind(roc_res, roc_tmp)
        m_res <- as.data.frame(t(m_res))
        m_res$whichSplit <- dich_name
        final_metrics <- rbind(final_metrics, m_res)

      }

      final_metrics$validation_group <- validators_name[v_id]
      train_id_column_name <-
        paste(validators_name[v_id], "_train_ind", sep = "")
      final_out_list[[train_id_column_name]] <-
        rownames(pp_dt) %in% train_ind
    } else{
      final_metrics <- data.frame()
      for (dich_name in diched_comb_dt_names) {
        if (if_CV) {
          m_res <-
            calculateMetricsForAProbNoPCDCategory(
              validator = validators[[v_id]],
              categoryVec =
                diched_comb_dt[dt_rnames, dich_name],
              K_fold = K_fold,
              repeated_folds_R = repeated_folds_R,
              input_and_pp_and_var_df = dt_rmna,
              seed_num = seed_num,
              lr_maxiter = lr_maxiter,
              n = n,
              pcd_dropping_pct = pcd_dropping_pct
            )
        } else{
          m_res <-
            calculateMetricsForAProbNoPCDCategoryNoCV(
              validator = validators[[v_id]],
              categoryVec =
                diched_comb_dt[dt_rnames, dich_name],
              input_and_pp_and_var_df = dt_rmna,
              seed_num = seed_num,
              lr_maxiter = lr_maxiter,
              n = n,
              pcd_dropping_pct = pcd_dropping_pct
            )
        }
        roc_tmp <- m_res[[2]]
        m_res <- m_res[[1]]
        roc_tmp$whichSplit <- dich_name
        roc_tmp$validation_group <- validators_name[v_id]
        roc_res <- rbind(roc_res, roc_tmp)
        m_res <- as.data.frame(t(m_res))
        m_res$whichSplit <- dich_name
        final_metrics <- rbind(final_metrics, m_res)
      }

      final_metrics$validation_group <- validators_name[v_id]
    }

    rname_column_name <-
      paste(validators_name[v_id], "_rnames", sep = "")
    final_out_list[[rname_column_name]] <-
      rownames(pp_dt) %in% dt_rnames
    final_metrics_all_validators <-
      rbind(final_metrics_all_validators, final_metrics)
  }
  names(roc_res)[names(roc_res) == "Group.1"] <- "threshold"
  list(
    id_df = as.data.frame(final_out_list),
    metrics = final_metrics_all_validators,
    rocs = roc_res,
    dt_y_test = dt_y_test
  )
}
