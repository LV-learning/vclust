dichPseudoByPathAModelOpt <- function(pp_dt,
                                      ##model classes
                                      n,
                                      r_pseudo,
                                      K_fold,
                                      repeated_folds_R,
                                      input_dt,
                                      x_names,
                                      ##variable name like PGBI or SCAREP
                                      validators,
                                      seed_num,
                                      validation_data_fraction,
                                      kappa_filter_threshold,
                                      if_listwise_deletion,
                                      y_names,
                                      lr_maxiter,
                                      use_combinations,
                                      PCD_list,
                                      pcd_dropping_pct,
                                      if_CV) {
  PCD_list_name <- names(PCD_list)
  ##assign validator names
  validators_name <-
    paste("validators", 1:length(validators), sep = "")
  final_metrics_all_validators <- data.frame()
  final_out_list <- list()
  train_id_list <- list()
  dt_rnames_list <- list()
  dt_rmna_list <- list()
  dt_y_test <- data.frame()
  #set.seed(seed_num)
  for (v_id in 1:length(validators)) {
    seed_num <- validators[[v_id]]$seed_num
    set.seed(seed_num['seed_num_split'])
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
    #get and save rownames for train and ZQW
    dt_rnames <- rownames(dt_rmna)
    train_size <- round(validation_data_fraction * nrow(dt_rmna))
    print(train_size)
    train_ind <- sample(dt_rnames, train_size, replace = FALSE)
    train_id_list[[v_id]] <- train_ind
    dt_rnames_list[[v_id]] <- dt_rnames
    dt_rmna_list[[v_id]] <- dt_rmna
  }
  roc_res <- data.frame()
  coefficients <- data.frame()
  for (v_id in 1:length(validators)) {
    seed_num <- validators[[v_id]]$seed_num
    #create input dt and remove NA by dichPseudoPrepare()
    dt_rnames <- dt_rnames_list[[v_id]]
    dt_rmna <- dt_rmna_list[[v_id]]
    ##split data to train and test
    ##get train/test id
    if (validation_data_fraction != 1) {
      #split train and test
      ##get train/test id
      final_metrics <- data.frame()
      train_ind <- train_id_list[[v_id]]
      for (PCD_name in PCD_list_name) {
        if (PCD_name %in% use_combinations) {
          if (if_CV) {
            m_res <-
              calculateMetricsForAProbSplit(
                validator = validators[[v_id]],
                pseudo_a_prob = PCD_list[[PCD_name]][dt_rnames, ],
                r_pseudo = r_pseudo,
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
              calculateMetricsForAProbSplitNoCV(
                validator = validators[[v_id]],
                pseudo_a_prob = PCD_list[[PCD_name]][dt_rnames, ],
                r_pseudo = r_pseudo,
                input_and_pp_and_var_df = dt_rmna,
                seed_num = seed_num,
                validation_data_fraction = validation_data_fraction,
                train_ind = train_ind,
                lr_maxiter = lr_maxiter,
                n = n,
                pcd_dropping_pct = pcd_dropping_pct
              )
          }
          if(attr(validators[[v_id]],"class") %in% c("validator_direct","validator_flip","validator_continuous") &
             PCD_name %in% validators[[v_id]]$predicted_cluster_combination &
             n %in% validators[[v_id]]$predicted_cluster_n){
            dt_y_test_tmp <- m_res[[3]]
            dt_y_test_tmp$whichSplit <- PCD_name
            dt_y_test_tmp$validation_group <- validators_name[v_id]
            dt_y_test <- rbind(dt_y_test, dt_y_test_tmp)
          }

          tryCatch({
            roc_tmp <- m_res[[2]]

            roc_tmp$whichSplit <- PCD_name
            roc_tmp$validation_group <- validators_name[v_id]
            roc_res <- rbind(roc_res, roc_tmp)
          },
          error = function(e){
            message("continuous outcome doesn't have roc")
          })

          tryCatch({
            coef_tmp <- m_res[['coefficients']]
            coef_tmp$whichSplit <- dich_name
            coef_tmp$validation_group <- validators_name[v_id]
            coefficients <- rbind(coefficients, coef_tmp)
          },
          error = function(e){
            message("no coefficients outputs")
          })

          m_res <- m_res[[1]]
          m_res <- as.data.frame(t(m_res))
          m_res$whichSplit <- PCD_name
          final_metrics <- rbind(final_metrics, m_res)
        }
      }

      final_metrics$validation_group <- validators_name[v_id]
      train_id_column_name <-
        paste(validators_name[v_id], "_train_ind", sep = "")
      final_out_list[[train_id_column_name]] <-
        rownames(pp_dt) %in% train_ind
    } else{
      final_metrics <- data.frame()
      for (PCD_name in PCD_list_name) {
        if (PCD_name %in% use_combinations) {
          if (if_CV) {
            m_res <- calculateMetricsForAProb(
              validator = validators[[v_id]],
              pseudo_a_prob = PCD_list[[PCD_name]][dt_rnames, ],
              r_pseudo = r_pseudo,
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
              calculateMetricsForAProbNoCV(
                validator = validators[[v_id]],
                pseudo_a_prob = PCD_list[[PCD_name]][dt_rnames, ],
                r_pseudo = r_pseudo,
                input_and_pp_and_var_df = dt_rmna,
                seed_num = seed_num,
                lr_maxiter = lr_maxiter,
                n = n,
                pcd_dropping_pct = pcd_dropping_pct
              )
          }
          tryCatch({
            roc_tmp <- m_res[[2]]

            roc_tmp$whichSplit <- PCD_name
            roc_tmp$validation_group <- validators_name[v_id]
            roc_res <- rbind(roc_res, roc_tmp)
          },
          error = function(e){
            message("continuous outcome doesn't have roc")
          })

          tryCatch({
            coef_tmp <- m_res[['coefficients']]
            coef_tmp$whichSplit <- dich_name
            coef_tmp$validation_group <- validators_name[v_id]
            coefficients <- rbind(coefficients, coef_tmp)
          },
          error = function(e){
            message("no coefficients outputs")
          })

          m_res <- m_res[[1]]
          m_res <- as.data.frame(t(m_res))
          m_res$whichSplit <- PCD_name
          final_metrics <- rbind(final_metrics, m_res)
        }
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
    dt_y_test = dt_y_test,
    coefficients = coefficients
  )
}
