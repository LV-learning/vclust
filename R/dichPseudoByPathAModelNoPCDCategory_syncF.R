dichPseudoByPathAModelNoPCDCategory_syncF <- function(pp_dt,
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
                                                pcd_dropping_pct,
                                                if_CV,
                                                use_combinations,
                                                label_category1 = NULL) {
  validators_name <- paste("validators", 1:length(validators), sep = "")
  final_metrics_all_validators <- data.frame()
  final_out_list <- list()
  train_id_list <- list()
  dt_rnames_list <- list()
  dt_rmna_list <- list()
  dt_y_test <- data.frame()
  mean_sd_dt_all <- data.frame()
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
    print(validators[[v_id]])
    if (identical(validators[[v_id]]$validator_variables, c("PGBI30","PGBI36","PGBI42","PGBI48")) &
        attr(validators[[v_id]],"class") == "validator_to01") {
      dt_rmna <- dt_rmna[!rownames(dt_rmna) == "391", ]
    }
    print("finished deleting 391")
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
  print(length(validators))
  for (v_id in 1:length(validators)) {

    seed_num <- validators[[v_id]]$seed_num
    dt_rnames <- dt_rnames_list[[v_id]]
    dt_rmna <- dt_rmna_list[[v_id]]
    dt_comb <- list()
    j <- 1
    for(i in 1:(n-1)){
      tmp_list <- chooseMClassFromNOpt(class_cols_dt=dt_rmna[,c((ncol(dt_rmna)-n+1):ncol(dt_rmna))],m=i,n=n)
      if(!is.null(label_category1)){
        for(label_category1i in label_category1){
          label_category1i <- toupper(stringr::str_extract(gsub(" |,","",label_category1i),'([pP][1234567890]+)+'))
          m <- length(unlist(strsplit(label_category1i,"P"))[unlist(strsplit(label_category1i,"P"))!=""])
          label_category1i <- paste("C",
                                   m,
                                   "No",
                                   9,
                                   "comb",
                                   label_category1i,
                                   sep='')
          if(label_category1i %in% names(tmp_list)){
            dt_comb[[j]] <- tmp_list[,label_category1i,drop=FALSE]
            j <- j + 1
          }
        }
      }else{
        dt_comb[[i]] <- tmp_list
      }

    }


    mean_sd_dt_tmp <- data.frame()
    if (validation_data_fraction != 1) {
      final_metrics <- data.frame()
      train_ind <- train_id_list[[v_id]]
      ##calculate combination of probabilities
      for (mComb in dt_comb) {
        rownames(mComb) <- dt_rnames
        if(!is.null(use_combinations)){
          mComb <-
            mComb[, names(mComb) %in% use_combinations, drop = FALSE]
        }
        if (ncol(mComb) != 0) {
          for (aChoiceProb in 1:ncol(mComb)) {
            if (if_CV) {
              m_res <-
                calculateMetricsForAProbSplitNoPCDCategory(
                  validator = validators[[v_id]],
                  categoryVec = mComb[, aChoiceProb, drop = FALSE],
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
                  categoryVec = mComb[, aChoiceProb, drop = FALSE],
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
               names(mComb)[aChoiceProb] %in% validators[[v_id]]$predicted_cluster_combination &
               n %in% validators[[v_id]]$predicted_cluster_n){
              dt_y_test_tmp <- m_res[[3]]
              dt_y_test_tmp$whichSplit <- names(mComb)[aChoiceProb]
              dt_y_test_tmp$validation_group <- validators_name[v_id]
              dt_y_test <- rbind(dt_y_test, dt_y_test_tmp)
            }

            tryCatch({
              roc_tmp <- m_res[[2]]
              roc_tmp$whichSplit <- names(mComb)[aChoiceProb]
              roc_tmp$validation_group <- validators_name[v_id]
              roc_res <- rbind(roc_res, roc_tmp)
            },
            error = function(e){
              message("no roc for continuous outcome")
            })
            tryCatch({
              coef_tmp <- m_res[[1]][['coefficients']]
              coef_tmp$whichSplit <- names(mComb)[aChoiceProb]
              coef_tmp$validation_group <- validators_name[v_id]
              coefficients <- rbind(coefficients, coef_tmp)
            },
            error = function(e){

            })
            tryCatch({
              coef_tmp <- m_res[['coefficients']]
              coef_tmp$whichSplit <- names(mComb)[aChoiceProb]
              coef_tmp$validation_group <- validators_name[v_id]
              coefficients <- rbind(coefficients, coef_tmp)
            },
            error = function(e){
              message("no coefficients outputs NoPCD syncF")
            })
            tryCatch({
              mean_sd_dt <- m_res[[1]][["mean_sd_dt"]]
              mean_sd_dt$whichSplit <- names(mComb)[aChoiceProb]
              mean_sd_dt_tmp <- rbind(mean_sd_dt_tmp, mean_sd_dt)
              m_res <- as.data.frame(m_res[[1]][["res_matrix"]])
            },
            error = function(e){
              m_res <<- as.data.frame(t(m_res[[1]]))
              message("m_res for non continuous")
            })
            m_res$whichSplit <- names(mComb)[aChoiceProb]
            final_metrics <- rbind(final_metrics, m_res)
          }
        }

      }
      tryCatch({
        mean_sd_dt_tmp$validation_group <- validators_name[v_id]
      },
      error = function(e){
        message("no mean_sd_dt")
      })
      final_metrics$validation_group <- validators_name[v_id]

      train_id_column_name <-
        paste(validators_name[v_id], "_train_ind", sep = "")
      final_out_list[[train_id_column_name]] <-
        rownames(pp_dt) %in% train_ind
    } else
      {
      final_metrics <- data.frame()
      for (mComb in dt_comb) {
        rownames(mComb) <- dt_rnames
        if(!is.null(use_combinations)){
          mComb <-
            mComb[, names(mComb) %in% use_combinations, drop = FALSE]

        }

        if (ncol(mComb) != 0) {
          for (aChoiceProb in 1:ncol(mComb)) {
            if (if_CV) {
              m_res <-
                calculateMetricsForAProbNoPCDCategory(
                  validator = validators[[v_id]],
                  categoryVec = mComb[, aChoiceProb],
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
                  categoryVec = mComb[, aChoiceProb],
                  input_and_pp_and_var_df = dt_rmna,
                  seed_num = seed_num,
                  lr_maxiter = lr_maxiter,
                  n = n,
                  pcd_dropping_pct = pcd_dropping_pct
                )
            }
            tryCatch({
              roc_tmp <- m_res[[2]]
              roc_tmp$whichSplit <- names(mComb)[aChoiceProb]
              roc_tmp$validation_group <- validators_name[v_id]
              roc_res <- rbind(roc_res, roc_tmp)
            },
            error = function(e){
              message("no roc for continuous outcome")
            })
            tryCatch({
              coef_tmp <- m_res[[1]][['coefficients']]
              coef_tmp$whichSplit <- names(mComb)[aChoiceProb]
              coef_tmp$validation_group <- validators_name[v_id]
              coefficients <- rbind(coefficients, coef_tmp)
            },
            error = function(e){

            })
            tryCatch({
              coef_tmp <- m_res[['coefficients']]
              coef_tmp$whichSplit <- names(mComb)[aChoiceProb]
              coef_tmp$validation_group <- validators_name[v_id]
              coefficients <- rbind(coefficients, coef_tmp)
            },
            error = function(e){
              message("no coefficients outputs NoPCD syncF")
            })
            tryCatch({
              mean_sd_dt <- m_res[[1]][["mean_sd_dt"]]
              mean_sd_dt$whichSplit <- names(mComb)[aChoiceProb]
              mean_sd_dt_tmp <- rbind(mean_sd_dt_tmp, mean_sd_dt)
              m_res <- as.data.frame(m_res[[1]][["res_matrix"]])
            },
            error = function(e){
              m_res <<- as.data.frame(t(m_res[[1]]))
              message("m_res for non continuous")
            })
            m_res$whichSplit <- names(mComb)[aChoiceProb]
            final_metrics <- rbind(final_metrics, m_res)
          }
        }

      }
      tryCatch({
        mean_sd_dt_tmp$validation_group <- validators_name[v_id]
      },
      error = function(e){
        message("no mean_sd_dt")
      })

      final_metrics$validation_group <- validators_name[v_id]
    }

    rname_column_name <-
      paste(validators_name[v_id], "_rnames", sep = "")
    final_out_list[[rname_column_name]] <-
      rownames(pp_dt) %in% dt_rnames
    final_metrics_all_validators <-
      rbind(final_metrics_all_validators, final_metrics)
    tryCatch({
      mean_sd_dt_all <- rbind(mean_sd_dt_all, mean_sd_dt_tmp)
    },
    error = function(e){
      message("no mean_sd_dt")
    })

  }
  names(roc_res)[names(roc_res) == "Group.1"] <- "threshold"
  list(
    id_df = as.data.frame(final_out_list),
    metrics = final_metrics_all_validators,
    rocs = roc_res,
    dt_y_test = dt_y_test,
    mean_sd_dt = mean_sd_dt_all,
    coefficients = coefficients
  )
}
