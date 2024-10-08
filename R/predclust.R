#' Conducts supervised learning treating a validated/selected cluster label as a known input or output variable
#'
#' Conducts supervised learning treating a validated/selected cluster label as a known input or output variable.
#' A label identified as a good outcome from the
#' validation step (validclust) is recommended to be used as a prediction
#' output (Jo et al., in press). A label identified as a good predictor of
#' an outcome is recommended to be used as a prediction input.
#' Note that predclust can be used as a standalone procedure or in
#' conjunction with genclust and/or validclust.
#'
#' @param sync_genclust A Boolean variable indicates whether predclust will use
#'                      the input data and clustering results from genclust.
#' @param sync_validclust A Boolean variable indicates whether predclust will
#' use the input data and validation results from validclust.
#' Our program doesn’t support the case when \code{sync_validclust = T} and
#' \code{sync_genclust = T}.
#' Here are two counterparts for this case,
#'
#' 1.	When used \code{sync_genclust = T} in validclust, \code{sync_validclust = T}
#' and \code{sync_genclust = T} is same to \code{sync_genclust = T} and \code{sync_validclust = F}
#'
#' 2.	When used \code{sync_genclust = F} in validclust, \code{sync_validclust = T}
#' and \code{sync_genclust = T} is same to \code{sync_genclust = F} and
#' \code{sync_validclust = T}.
#'
#' @param output_path_prefix The user needs to specify the folder path that will
#' store supervised learning results.
#' The path should be absolute path (full path) when using
#' Windows operation system.
#' For example, “/Users/username/Desktop” for Mac user or “D:/folder” for Windows user.
#' Use “/” instead of “\\” for the path.
#' @param data_path If \code{sync_genclust = FALSE} and \code{sync_validclust = FALSE},
#' the user is expected to specify the folder path that stores the data
#' that will be used in predclust.
#' The data should be in the csv format.
#' The information provided here will supersede the information from genclust and validclust.
#' @param variable_names When data_path is used, the user needs to
#' specify variable names.
#' For example, \cr
#' \code{variable_names = }\cr
#' \code{c('x', 'e1', 'e2', 'e3', 'f1', 'f2', 'z1', 'q1', 'w1', 'w2', 'w3', 'u1', 'u2')}.
#' These variable names will overwrite the original names when the data file already has variables names (i.e., header).
#' The user can choose to use those original names by specifying \code{variable_names = NULL}.
#' @param naString A string indicates what string is interpreted as NA value
#' in the input data.
#' @param predictors_names A string vector indicates names of variables to be used as predictors (input variables).
#' For example, \code{predictors_names = c("x","w1","w2","w3","u1","u2")}.
#' @param cluster_names When data_path is not used, \code{sync_genclust = TRUE},\cr
#' and \code{sync_validclust = FALSE},
#' the user is expected to use the cluster names from the
#' summary of the genclust procedure provided in genclust_results.csv.
#' For example, \code{cluster_names = c("P1", "P2", "P3")}
#'
#' When data_path is not used and \code{sync_validclust = TRUE},
#' the user is expected to use the cluster names from the summary of the
#' validclust procedure provided in validclust_results.csv.
#'
#' When data_path is used, the user is expected to use the cluster names
#' from the variables listed in variable_names. Note that,
#' when using cluster membership in probabilities (soft clustering),
#' the total should add up to 1.
#' For example, an individual may have \code{e1=0.3, e2=0.1, e3=0.6}, which add up to 1.
#' When using observed or hard cluster membership (one unit or person belongs to one cluster),
#' for a person who belongs to the third cluster, \code{e1=0, e2=0, e3=1}.
#' @param label_category1 The user needs to specify
#' which clusters will be categorized into the first category of
#' the label that will be used in predclust. The rest are automatically
#' categorized into the second category. For example, based on a 5-cluster
#' clustering solution, if \code{cluster_names= c("P1", "P2", "P3", "P4", "P5")} and
#' \code{label_category1= c("P1", "P3")} each unit or person will have the probability
#' of P1+P3 of belonging to the first category and the probability of P2+P4+P5
#' of belonging to the second category of the label.
#' @param cluster_label_position A string indicates the location of the
#' cluster label in prediction. \cr
#' When \code{cluster_label_position="predictor"},
#' the cluster label defined in label_category1 will be used as a predictor.\cr
#' When \code{cluster_label_position="predicted"}, the cluster label
#' will be used as an outcome predicted by provided predictors (input variables).\cr
#' If \code{cluster_label_position="none"}, the cluster label will be omitted in supervised learning.
#' @param outcome_obs When \code{cluster_label_position = "predictor"}
#'  or \code{cluster_label_position = "none"},
#'  the user is expected to specify the outcome variable to be predicted by
#'  the cluster label and other provided predictors.
#'  This argument comes with the following subcomponents.
#'  \itemize{
#'    \item{\code{outcome_type:}}{ In the current version, only a binary variable is
#'    allowed to be used as a prediction (classification) outcome. There are 2 allowed types:
#'    \code{outcome_type="binary"}, when a single outcome variable is already binary (0/1).
#'    \code{outcome_type="cutpoint"}, when a single binary variable will be created based on
#'     a cutpoint (or cutpoints) applied to a single or multiple variables.
#'    }
#'    \item{\code{outcome_source_variables:}}{ The user may specify a single binary
#'    outcome or set of source variables that will be used to create a binary
#'    outcome. For example, outcome_source_variables= c("a","b","c").}
#'    \item{\code{outcome_source_all_missing:}}{ An integer specifies which value to
#'    take when all variables listed in outcome_source_variables are missing.
#'    The three possible options are NA, 1, or 0.
#'    If \code{outcome_source_all_missing = NA},
#'    the outcome of these individuals or units will be treated as missing.
#'    The default is 0.}
#'    \item{\code{outcome_cutpoint:}}{ A numeric value/vector specifies a threshold or
#'     multiple thresholds to create a binary outcome.
#'     For example, \code{outcome_cutpoint=12}, or \code{outcome_cutpoint=c(12,13,14)}.}
#'    \item{\code{outcome_cutpoint_sign:}}{ A character value/vector specifies
#'    comparison operator(s) to be used with thresholds.
#'    Available options include >=, <=, >, <, ==, GE, LE, GT, LT, and EQ.
#'    When using a vector of multiple thresholds, the signs will be
#'    applied to each cutpoint.}
#'    \item{\code{outcome_cutpoint_max_min_mean:}}{ A string specifies a function to use to
#'    summarize multiple variables into a single variable.
#'    The options include max, min, and mean.
#'    For example, \code{outcome_cutpoint_max_min_mean="max"}.
#'    }
#'  }
#'
#' When outcome_cutpoint is a single value, all cutpoint related arguments
#' can be used together. For example, if \code{outcome_source_variables=c("a","b","c")},
#' \code{outcome_cutpoint  = 12}, \code{outcome_cutpoint_sign =">="},\cr
#' and \code{outcome_cutpoint_max_min_mean="max"}, all cases with \deqn{max(a, b, c) >= 12}
#' will be assigned the value of 1, and the rest the value of 0.
#'
#' When outcome_cutpoint has multiple values, outcome_max_min_mean will be ignored.
#' For example, when \code{outcome_source_variables=c("a","b","c")},
#' \code{outcome_cutpoint = c(12, 13, 14)}, \cr
#' \code{outcome_cutpoint_sign = c(">=", "<", ">")},
#'  all cases with \deqn{a>=12 and b<13 and c>14} will be assigned the value of 1,
#'   and the rest the value of 0.
#' @param supervised_method A string indicates the type of supervised learning.
#' In the current version, we allow logistic regression and glmnet.
#' That is, \code{supervised_method="logistic"}, or \code{supervised_method="glmnet"}.
#' @param glmnet_specs When \href{https://cran.r-project.org/web/packages/glmnet/index.html}{glmnet} is used, the user may utilize the same
#' arguments used in glmnet such as family, lambda, alpha, etc.
#' That is,
#'
#' \code{glmnet_specs(family=”binomial”, alpha=1, nlambda=100, lambda = NULL…)}
#'
#' Note that, in the current version of predclust, we only allow \code{family="binomial"}
#' and one pair of lambda/alpha.
#'
#' The user can also employ an external program called superclust
#' (beta version available), which implements various supervised
#' learning methods with cluster labels in probabilities.
#' @param seed_numbers An integer vector includes 4 items with respect
#' to seed numbers of splitting train/test datasets, cross-validation,
#' pseudoclass draws as well as the supervised model.
#' Their names are seed_num_split, seed_num_kfold, seed_num_pcd, and
#' seed_num_supervised_model respectively. For example, \code{seed_numbers =}
#'
#' \code{c(seed_num_split = 4561234,}
#'
#' \code{seed_num_kfold = 4561234,}
#'
#' \code{seed_num_pcd = 4561234,}
#'
#' \code{seed_num_supervised_model = 4561234)}
#' @param useobs The user may specify a text string that indicates observations to use.
#' For example, if we want to exclude observations with \code{x=9} and \code{x=13},
#' we can set \code{useobs="(x ne 9) and (x ne 13)"}.
#' If useobs has been already used under genclust and/or validclust,
#' this argument can be used to specify additional observations to be excluded.
#' @param listwise_deletion_variables The user can specify listwise deletion
#' based on specific variables. For example, \code{listwise_deletion_variables = c("a1","b1")}.
#' This feature is useful when the user wants to conduct listwise
#' deletion with variables that are not being used in the predclust procedure.
#' As a default, the program uses the standard listwise deletion method
#' for the variables included in the predclust procedure.
#' @param train_fraction A single value between 0 and 1 indicating the
#' fraction of the samples for
#' the train/test split. For example, \code{train_fraction = 0.7} means that
#' 70% are used as the train data and 30% are used as the test data.
#' The program uses 0.7 as the default.
#' @param if_CV A Boolean variable indicates whether K-fold cross validation
#' is used in supervised learning.
#' @param K_fold An integer indicates the number of folds in cross-validation.
#' The default is 10. It is applicable when \code{if_CV = TRUE}.
#' @param repeated_CV An integer indicates the number of repeated K-fold CV.
#' It is applicable when \code{if_CV = TRUE}.
#' @param if_PCD A Boolean variable indicates whether pseudo class draws will
#' be used to take into account uncertainties in cluster or latent class
#' assignment (Jo et al., 2017). This argument is relevant when soft clustering methods are used.
#' @param r_PCD When \code{if_PCD = TRUE}, the user needs to specify the number of pseudo class draws. The default is 20.
#' @param lr_maxiter An integer indicates maximum iterations in logistic
#' regression, which is the default supervised learning method in this program. The default is 25.
#'
#' @return The supervised learning results will be provided as a csv file (predclust_results.csv)
#' in the user-specified folder. For each supervised model, Cohen’s Kappa, accuracy,
#' sensitivity, specificity, and AUC estimates are provided
#' (their means and standard errors if K-fold cross validation and/or pseudoclass draws are used).
#' \itemize{
#'   \item{\code{Supervised_method:}}{ The employed supervised learning method.}
#'   \item{\code{Supervised_spec1 to Supervised_spec3:}}{Further details regarding the employed supervised learning method.}
#'   \item{\code{Cluster_n:}}{ The total number of clusters or classes used in creating a cluster label.}
#'   \item{\code{Cluster_names:}}{ The names of all clusters used in creating a cluster label.}
#'   \item{\code{Label_category1:}}{ The clusters categorized in the first category when generating a binary cluster label.}
#'   \item{\code{Label_position:}}{ Whether the cluster label defined in label_category1 is used as a predictor (predictor),
#'    or as an outcome predicted by provided predictors (predicted), or the cluster label is omitted in supervised learning (none).}
#'   \item{\code{Predictors:}}{ The names of the first two variables used as predictors (input variables) in supervised learning.}
#'   \item{\code{Kappa, sensitivity, specificity, accuracy, AUC:}}{ These are the
#'   measures of association between the cluster label and the predicted label.
#'   When \code{if_CV = TRUE} and/or \code{if_PCD = TRUE}, the provided values are the means across K folds and R pseudoclass draws.
#'   These measures are reported separately for the training and test data.}
#'   \item{\code{Kappa_SE, sensitivity_SE, specificity_SE, accuracy_SE, AUC_SE:}}{ When
#'   \code{if_CV = TRUE} and/or \code{if_PCD = TRUE}, these are the standard deviations across
#'   K folds and R pseudoclass draws. These measures are reported separately for the training and test data.}
#'
#' }
#'
#' @references Jo, B., Hastie, T. J., Li, Z., Youngstrom, E. A., Findling, R. L., & Horwitz, S. M. (2023). Reorienting Latent Variable Modeling for Supervised Learning. Multivariate Behavioral Research, 1-15.
#'
#' @export

predclust <- function(sync_genclust,
                      sync_validclust,
                      output_path_prefix, #
                      data_path, #
                      variable_names, #
                      naString = NULL,
                      predictors_names,
                      cluster_names,
                      label_category1, #
                      cluster_label_position, #
                      outcome_obs,
                      supervised_method, #
                      glmnet_specs,
                      seed_numbers, #
                      useobs, #
                      listwise_deletion_variables, #
                      train_fraction, #
                      if_CV, #
                      K_fold, #
                      repeated_CV, #
                      if_PCD, #
                      r_PCD, #
                      lr_maxiter,
                      customized = F,
                      reference = NULL,
                      comparison = NULL,
                      cohend_SD = NULL){ #
  #test for repeated cv branch
  base::suppressWarnings(try(RNGkind(sample.kind = "Rounding"), silent = TRUE))
  if(customized) used_clusters <- unique(c(reference,comparison))
  if(customized & sjmisc::is_empty(reference)) stop("Please specify reference for customized = TRUE")
  if(isTRUE(sync_genclust) & isTRUE(sync_validclust)){
    stop("Please refer the manual to handle the case when both sync_genclust and sync_validclust are true")
  }

  if(customized & sjmisc::is_empty(comparison) & isTRUE(sync_genclust) & !isTRUE(sync_validclust)){
    all_clusters <- cluster_names
    comparisons <- all_clusters[!all_clusters %in% reference]
    comparisons <- unlist(lapply(1:length(comparisons), function(m) combn(comparisons, m, simplify = FALSE)), recursive=FALSE)
    if(dir.exists(global_parameters$output_path_prefix) == FALSE){
      dir.create(global_parameters$output_path_prefix)
    }

    res <- data.frame()
    output_tmp <- global_parameters$output_path_prefix
    cohend_final <- data.frame()
    for(comparison in comparisons){
      comparison_name = paste(comparison, collapse = "")
      global_parameters$output_path_prefix <<- paste(output_tmp, "/", comparison_name, "/", sep = "")
      if(dir.exists(global_parameters$output_path_prefix) == FALSE){
        dir.create(global_parameters$output_path_prefix)
      }
      csv_files <- paste(output_tmp, "/", list.files(output_tmp, ".csv$"), sep = "")
      file.copy(from=csv_files, to=global_parameters$output_path_prefix,
                overwrite = TRUE, recursive = FALSE,
                copy.mode = TRUE)
      tmpRes <- predclust(sync_genclust,
                          sync_validclust,
                          output_path_prefix, #
                          data_path, #
                          variable_names, #
                          naString,
                          predictors_names,
                          cluster_names,
                          label_category1, #
                          cluster_label_position, #
                          outcome_obs,
                          supervised_method, #
                          glmnet_specs,
                          seed_numbers, #
                          useobs, #
                          listwise_deletion_variables, #
                          train_fraction, #
                          if_CV, #
                          K_fold, #
                          repeated_CV, #
                          if_PCD, #
                          r_PCD, #
                          lr_maxiter,
                          customized,
                          reference,
                          comparison = comparison,
                          cohend_SD = cohend_SD)
      base::suppressWarnings(
        try({tmp_cohend = read.csv(
          paste(
            output_path_prefix,
            "/cohen's d.csv",
            sep = ""
          ),header = TRUE)
        cohend_final <- rbind(cohend_final, tmp_cohend)
        }, silent = TRUE)
      )
      global_parameters$output_path_prefix <<- output_tmp
      res <- rbind(res, tmpRes)
    }
    if(!sjmisc::is_empty(cohend_final)){
      print(cohend_final)
      write.csv(
        cohend_final,
        paste(
          output_tmp,
          "cohen's d.csv",
          sep = ""
        ),
        row.names = FALSE
      )
    }
    write.csv(
      res,
      paste(
        output_tmp,
        "predclust_results.csv",
        sep = ""
      )
    )
    return(res)
  }else if(customized & sjmisc::is_empty(comparison) & !isTRUE(sync_genclust) & isTRUE(sync_validclust)){
    stop("Please specify comparison for sync_genclust == FALSE and sync_validclust == TRUE")
  }else if(customized & sjmisc::is_empty(comparison) & !isTRUE(sync_genclust) & !isTRUE(sync_validclust)){
    if(length(cluster_names) == 1){
      input_dt_tmp <- inputDataPrepare(data_path = data_path,
                                   x_names = variable_names,
                                   naString = naString)
      input_dt_tmp <- input_dt_tmp[!apply(is.na(input_dt_tmp[,cluster_names,drop=FALSE]),1,all),]
      all_clusters <- paste("P",1:length(unique(input_dt_tmp[,cluster_names])), sep="")
      remove(input_dt_tmp)

    }else{
      all_clusters <- cluster_names
    }
    comparisons <- all_clusters[!all_clusters %in% reference]
    comparisons <- unlist(lapply(1:length(comparisons), function(m) combn(comparisons, m, simplify = FALSE)), recursive=FALSE)
    if(dir.exists(output_path_prefix) == FALSE){
      dir.create(output_path_prefix)
    }
    res <- data.frame()
    output_tmp <- output_path_prefix
    cohend_final <- data.frame()
    for(comparison in comparisons){
      comparison_name = paste(comparison, collapse = "")
      output_path_prefix <- paste(output_tmp, "/", comparison_name, "/", sep = "")
      tmpRes <- predclust(sync_genclust,
                          sync_validclust,
                          output_path_prefix, #
                          data_path, #
                          variable_names, #
                          naString,
                          predictors_names,
                          cluster_names,
                          label_category1, #
                          cluster_label_position, #
                          outcome_obs,
                          supervised_method, #
                          glmnet_specs,
                          seed_numbers, #
                          useobs, #
                          listwise_deletion_variables, #
                          train_fraction, #
                          if_CV, #
                          K_fold, #
                          repeated_CV, #
                          if_PCD, #
                          r_PCD, #
                          lr_maxiter,
                          customized,
                          reference,
                          comparison = comparison,
                          cohend_SD = cohend_SD)
      base::suppressWarnings(
        try({tmp_cohend = read.csv(
          paste(
            output_path_prefix,
            "/cohen's d.csv",
            sep = ""
          ),header = TRUE)
        cohend_final <- rbind(cohend_final, tmp_cohend)
        }, silent = TRUE)
      )
      output_path_prefix <- output_tmp
      res <- rbind(res, tmpRes)

    }
    if(!sjmisc::is_empty(cohend_final)){
      print(cohend_final)
      write.csv(
        cohend_final,
        paste(
          output_tmp,
          "cohen's d.csv",
          sep = ""
        ),
        row.names = FALSE
      )
    }
    write.csv(
      res,
      paste(
        output_tmp,
        "predclust_results.csv",
        sep = ""
      )
    )
    return(res)
  }


  if_continuous = FALSE
  if(outcome_obs$outcome_type == "continuous"){
    if_continuous = TRUE
  }

  kappa_filter_threshold <- NULL
  kappa_results_threshold <- NULL
  kappa_results_threshold_final_metrics <- NULL
  combined_posterior_prob_threshold <- 0.5
  if_listwise_deletion <- FALSE
  pcd_dropping_pct <- c(0.2,0.2,1)
  seed_num <- seed_numbers
  seed_num['seed_num_PCD'] <- seed_num['seed_num_pcd']

  if(isTRUE(sync_genclust)){

    label_category1 <- paste("P", which(cluster_names %in% label_category1), sep = "")
    if(customized){
      if_PCD <- FALSE
      label_category1 <- reference
      comparison <- paste(comparison, collapse=",")
    }
    label_category1 <- paste(label_category1, collapse=",")
    ####################################################################
    ###########construct validators by provided arguments###############
    ####################################################################
    # predictors_names <- gsub(","," ",predictors_names)
    # predictors_names <- trimws(predictors_names)
    # predictors_names <- strsplit(predictors_names," |\t|\n|\f|\v|\r")[[1]]
    # predictors_names <- predictors_names[predictors_names!=""]
    # print("predictors_names is: ")
    # print(seed_numbers['seed_num_split'])
    # print(predictors_names)

    if(if_continuous == FALSE & tolower(trimws(cluster_label_position)) == "predicted"){
      validators <- list(validator(
        predicted_cluster_combination = if(customized){c(label_category1,comparison)}else{label_category1},
        predicted_cluster_n = cluster_names,
        validator_source_variables = predictors_names,
        listwise_deletion_variables = listwise_deletion_variables,
        validator_type = "direct",
        seed_num = c(seed_num_split = as.integer(seed_numbers['seed_num_split']),
                     seed_num_kfold = as.integer(seed_numbers['seed_num_kfold']),
                     seed_num_supervised_model = as.integer(seed_numbers['seed_num_supervised_model'])),
        supervised_model = supervised_method,
        alpha = glmnet_specs['alpha'],
        lambda = glmnet_specs['lambda'],
        validator_source_all_missing = outcome_obs$outcome_source_all_missing))
      print("validator is")
      print(validators)
    }else if(if_continuous == FALSE & tolower(trimws(cluster_label_position)) %in% c("predictor","none"))
    {
      validators <- list(validator(validator_cutpoint = outcome_obs$outcome_cutpoint,
                                   validator_cutpoint_sign = outcome_obs$outcome_cutpoint_sign,
                                   validator_cutpoint_max_min_mean = outcome_obs$outcome_cutpoint_max_min_mean,
                                   predicted_cluster_combination = if(customized){c(label_category1,comparison)}else{label_category1},
                                   predicted_cluster_n = cluster_names,
                                   validator_source_variables = outcome_obs$outcome_source_variables,
                                   listwise_deletion_variables = listwise_deletion_variables,
                                   validator_type = "flip",
                                   flip_outcome_type = tolower(trimws(outcome_obs$outcome_type)),
                                   flipped_predictors_variables = predictors_names,
                                   flipped_predictors_cluster = ifelse(cluster_label_position == "predictor",TRUE,FALSE),
                                   flipped_predictors_pp = FALSE,
                                   seed_num = c(seed_num_split = as.integer(seed_numbers['seed_num_split']),
                                                seed_num_kfold = as.integer(seed_numbers['seed_num_kfold']),
                                                seed_num_supervised_model = as.integer(seed_numbers['seed_num_supervised_model'])),
                                   supervised_model = supervised_method,
                                   alpha = glmnet_specs['alpha'],
                                   lambda = glmnet_specs['lambda'],
                                   validator_source_all_missing = outcome_obs$outcome_source_all_missing))
    }else if(if_continuous == TRUE & tolower(trimws(cluster_label_position)) %in% c("predictor","none")){
      validators <- list(validator(validator_cutpoint = outcome_obs$outcome_cutpoint,
                                   validator_cutpoint_sign = outcome_obs$outcome_cutpoint_sign,
                                   validator_cutpoint_max_min_mean = outcome_obs$outcome_cutpoint_max_min_mean,
                                   predicted_cluster_combination = if(customized){c(label_category1,comparison)}else{label_category1},
                                   predicted_cluster_n = cluster_names,
                                   validator_source_variables = outcome_obs$outcome_source_variables,
                                   listwise_deletion_variables = listwise_deletion_variables,
                                   validator_type = "continuous",
                                   flipped_predictors_variables = predictors_names,
                                   flipped_predictors_cluster = ifelse(cluster_label_position == "predictor",TRUE,FALSE),
                                   flipped_predictors_pp = FALSE,
                                   seed_num = c(seed_num_split = as.integer(seed_numbers['seed_num_split']),
                                                seed_num_kfold = as.integer(seed_numbers['seed_num_kfold']),
                                                seed_num_regression_model = as.integer(seed_numbers['seed_num_regression_model'])),
                                   supervised_model = supervised_method,
                                   validator_source_all_missing = outcome_obs$outcome_source_all_missing,
                                   contVarName = outcome_obs$outcome_continuous))
    }

    #folder_path = '/Users/zetanli/Desktop/myproject roc max predicted_cluster /testdiffseed_gmm_runif_validclust/gmm/cP3/',
    folder_path <- global_parameters$folder_path
    listwise_deletion_variables <- global_parameters$listwise_deletion_variables
    #data_path="/Users/zetanli/Desktop/stanfordwork/mplus_projects/scarepgb616.csv",
    if(sjmisc::is_empty(data_path)) data_path <-  global_parameters$data_path
    #output_path_prefix = "/Users/zetanli/Desktop/myproject roc max predicted_cluster /testdiffseed_gmm_runif_validclust_nopcd/",
    if(sjmisc::is_empty(output_path_prefix)) output_path_prefix <- global_parameters$output_path_prefix
    useObs <- global_parameters$useobs
    model_type <- global_parameters$model_type
    covariates <- global_parameters$covariates
    is_covariates <- !sjmisc::is_empty(covariates)
    if(sjmisc::is_empty(variable_names)) variable_names <- global_parameters$variable_names
    naString <- global_parameters$naString
    y_names <- global_parameters$y_names

    print(variable_names)
    if(dir.exists(output_path_prefix) == FALSE){
      dir.create(output_path_prefix)
    }
    if(is.character(data_path) & file.exists(data_path)){
      print('########################################')
      print('##########input_dt is a path############')
      print('########################################')

      if(sjmisc::is_empty(variable_names)){
        x_names1 <- variable_names
      }else{
        x_names1 <- gsub(","," ",variable_names)
        x_names1 <- trimws(x_names1)
        x_names1 <- strsplit(x_names1," |\t|\n|\f|\v|\r")[[1]]
        x_names1 <- x_names1[x_names1!=""]
        print("x_names1 is: ")
        print(x_names1)
      }
      input_dt <- inputDataPrepare(data_path = data_path,
                                   x_names = x_names1,
                                   naString = naString)
      input_dt <- input_dt[!apply(is.na(input_dt[,y_names,drop=FALSE]),1,all),]

      if(tolower(model_type) %in% c("gmm","growth mixture model")){
        if(identical(is_covariates,TRUE)){
          covariates_vec <- unlist(strsplit(covariates," |\t|\n|\f|\v|\r"))
          covariates_vec <- covariates_vec[covariates_vec != ""]
          input_dt <- input_dt[stats::complete.cases(input_dt[,covariates_vec]),]
        }
      }

      useObs <- useObsSplitter(useObs)
      if(is.null(useObs)==FALSE){
        text_useobs <- paste("input_dt <- dplyr::filter(input_dt,",useObs,")",sep = "")
        print("text_useobs is:")
        print(text_useobs)
        eval(parse(text = text_useobs))
        input_dt <- as.data.frame(input_dt)
      }
    }else{
      input_dt <- data_path
    }
    if(dir.exists(output_path_prefix) == FALSE){
      dir.create(output_path_prefix)
    }
    useobs <- useObsSplitter(useobs)
    if(is.null(useobs)==FALSE){
      text_useobs <- paste("input_dt <- dplyr::filter(input_dt,",useobs,")",sep = "")
      print("text_useobs is:")
      print(text_useobs)
      eval(parse(text = text_useobs))
      input_dt <- as.data.frame(input_dt)
    }

    input_dt <- input_dt[stats::complete.cases(input_dt[,c(listwise_deletion_variables)]),]
    rownames(input_dt) <- 1:nrow(input_dt)
    print("input_dt row names")
    print(rownames(input_dt))
    final_pseudo_res_dir <- paste(output_path_prefix,"PCD_binary/",sep = "")

    if(dir.exists(final_pseudo_res_dir) == FALSE){
      dir.create(final_pseudo_res_dir)
    }

    final_pp_if_validators_res_dir <- paste(output_path_prefix,"pp_and_if_train_validators/",sep = "")
    if(dir.exists(final_pp_if_validators_res_dir) == FALSE){
      dir.create(final_pp_if_validators_res_dir)
    }
    final_roc_res_dir <- paste(output_path_prefix,"roc_data/",sep = "")
    if(dir.exists(final_roc_res_dir) == FALSE){
      dir.create(final_roc_res_dir)
    }
    if(tolower(model_type) %in% c("k-means","kmeans","k means","k_means","O","o", "ogroups", "o groups", "ogroup", "o group")){
      if_PCD = FALSE
    }
    if(if_PCD == TRUE){
      if(tolower(model_type) %in% c("gmm","growth mixture model")){
        if(if_continuous){
          res <- dichPseudoByPathAllModel_Cont(folder_path,
                                                    n_range = length(cluster_names):length(cluster_names),
                                                    r_pseudo = r_PCD,
                                                    K_fold,
                                                    repeated_folds_R = repeated_CV,
                                                    input_dt,
                                                    x_names,
                                                    validators,
                                                    seed_num,
                                                    output_path_prefix,
                                                    validation_data_fraction = train_fraction,
                                                    kappa_filter_threshold,
                                                    if_listwise_deletion,
                                                    y_names,
                                                    lr_maxiter,
                                                    kappa_results_threshold,
                                                    kappa_results_threshold_final_metrics,
                                                    optimize_prob_thresh = 0.5,
                                                    pcd_dropping_pct,
                                                    if_CV,
                                                    label_category1 = if(customized){c(label_category1,comparison)}else{label_category1})

        }else{
          res <- dichPseudoByPathAllModel(folder_path,
                                          n_range = length(cluster_names):length(cluster_names),
                                          r_pseudo = r_PCD,
                                          K_fold,
                                          repeated_folds_R = repeated_CV,
                                          input_dt,
                                          x_names,
                                          validators,
                                          seed_num,
                                          output_path_prefix,
                                          validation_data_fraction = train_fraction,
                                          kappa_filter_threshold,
                                          if_listwise_deletion,
                                          y_names,
                                          lr_maxiter,
                                          kappa_results_threshold,
                                          kappa_results_threshold_final_metrics,
                                          optimize_prob_thresh = 0.5,
                                          pcd_dropping_pct,
                                          if_CV,
                                          label_category1 = if(customized){c(label_category1,comparison)}else{label_category1})
        }

      }else if(tolower(model_type) %in% c("mclust", "gaussian mixture model","model based clustering", "model-based clustering","mbc")){
        if(if_continuous){
          res <- dichPseudoByPathAllModel_Cont(folder_path,
                                                    ##model classes
                                                    n_range = length(cluster_names):length(cluster_names),
                                                    r_pseudo = r_PCD,
                                                    K_fold,
                                                    repeated_folds_R = repeated_CV,
                                                    input_dt,
                                                    x_names,
                                                    validators,
                                                    seed_num,
                                                    output_path_prefix,
                                                    validation_data_fraction = train_fraction,
                                                    kappa_filter_threshold,
                                                    if_listwise_deletion,
                                                    y_names,
                                                    lr_maxiter,
                                                    kappa_results_threshold,
                                                    kappa_results_threshold_final_metrics,
                                                    optimize_prob_thresh = 0.5,
                                                    pcd_dropping_pct,
                                                    if_CV,
                                                    label_category1 = if(customized){c(label_category1,comparison)}else{label_category1})

        }else{
          res <- dichPseudoByPathAllModelMclust(folder_path,
                                                ##model classes
                                                n_range = length(cluster_names):length(cluster_names),
                                                r_pseudo = r_PCD,
                                                K_fold,
                                                repeated_folds_R = repeated_CV,
                                                input_dt,
                                                x_names,
                                                validators,
                                                seed_num,
                                                output_path_prefix,
                                                validation_data_fraction = train_fraction,
                                                kappa_filter_threshold,
                                                if_listwise_deletion,
                                                y_names,
                                                lr_maxiter,
                                                kappa_results_threshold,
                                                kappa_results_threshold_final_metrics,
                                                optimize_prob_thresh = 0.5,
                                                pcd_dropping_pct,
                                                if_CV,
                                                label_category1 = if(customized){c(label_category1,comparison)}else{label_category1})
        }


      }
    }else
      {
      if(tolower(model_type) %in% c("gmm","growth mixture model")){
        if(if_continuous){
          res <- dichPseudoByPathAllModelNoPCD_Cont(folder_path,
                                               ##model classes
                                               n_range = length(cluster_names):length(cluster_names),
                                               K_fold,
                                               repeated_folds_R = repeated_CV,
                                               input_dt,
                                               x_names,
                                               validators,
                                               seed_num,
                                               output_path_prefix,
                                               validation_data_fraction = train_fraction,
                                               kappa_filter_threshold,
                                               if_listwise_deletion,
                                               y_names,
                                               lr_maxiter,
                                               kappa_results_threshold,
                                               kappa_results_threshold_final_metrics,
                                               combined_posterior_prob_threshold,
                                               optimize_prob_thresh = 0.5,
                                               pcd_dropping_pct,
                                               if_CV,
                                               label_category1 = if(customized){c(label_category1,comparison)}else{label_category1},
                                               customized = customized,
                                               used_clusters = used_clusters)

        }else{
          res <- dichPseudoByPathAllModelNoPCD(folder_path,
                                               ##model classes
                                               n_range = length(cluster_names):length(cluster_names),
                                               K_fold,
                                               repeated_folds_R = repeated_CV,
                                               input_dt,
                                               x_names,
                                               validators,
                                               seed_num,
                                               output_path_prefix,
                                               validation_data_fraction = train_fraction,
                                               kappa_filter_threshold,
                                               if_listwise_deletion,
                                               y_names,
                                               lr_maxiter,
                                               kappa_results_threshold,
                                               kappa_results_threshold_final_metrics,
                                               combined_posterior_prob_threshold,
                                               optimize_prob_thresh = 0.5,
                                               pcd_dropping_pct,
                                               if_CV,
                                               label_category1 = if(customized){c(label_category1,comparison)}else{label_category1},
                                               customized = customized,
                                               used_clusters = used_clusters)
        }

      }else if(tolower(model_type) %in% c("mclust", "gaussian mixture model","model based clustering", "model-based clustering","mbc")){
        if(if_continuous){
          res <- dichPseudoByPathAllModelNoPCD_Cont(folder_path,
                                                    ##model classes
                                                    n_range = length(cluster_names):length(cluster_names),
                                                    K_fold,
                                                    repeated_folds_R = repeated_CV,
                                                    input_dt,
                                                    x_names,
                                                    validators,
                                                    seed_num,
                                                    output_path_prefix,
                                                    validation_data_fraction = train_fraction,
                                                    kappa_filter_threshold,
                                                    if_listwise_deletion,
                                                    y_names,
                                                    lr_maxiter,
                                                    kappa_results_threshold,
                                                    kappa_results_threshold_final_metrics,
                                                    combined_posterior_prob_threshold,
                                                    optimize_prob_thresh = 0.5,
                                                    pcd_dropping_pct,
                                                    if_CV,
                                                    label_category1 = if(customized){c(label_category1,comparison)}else{label_category1},
                                                    customized = customized,
                                                    used_clusters = used_clusters)

        }else{
          res <- dichPseudoByPathAllModelNoPCDMclust(folder_path,
                                                     ##model classes
                                                     n_range = length(cluster_names):length(cluster_names),
                                                     K_fold,
                                                     repeated_folds_R = repeated_CV,
                                                     input_dt,
                                                     x_names,
                                                     validators,
                                                     seed_num,
                                                     output_path_prefix,
                                                     validation_data_fraction = train_fraction,
                                                     kappa_filter_threshold,
                                                     if_listwise_deletion,
                                                     y_names,
                                                     lr_maxiter,
                                                     kappa_results_threshold,
                                                     kappa_results_threshold_final_metrics,
                                                     combined_posterior_prob_threshold,
                                                     optimize_prob_thresh = 0.5,
                                                     pcd_dropping_pct,
                                                     if_CV,
                                                     label_category1 = if(customized){c(label_category1,comparison)}else{label_category1},
                                                     customized = customized,
                                                     used_clusters = used_clusters)
        }


      }else if(tolower(model_type) %in% c("k-means","kmeans","k means","k_means")){
        if(if_continuous){
          res <- dichPseudoByPathAllModelKmeans_Cont(folder_path,
                                                ##model classes
                                                n_range = length(cluster_names):length(cluster_names),
                                                K_fold,
                                                repeated_folds_R = repeated_CV,
                                                input_dt,
                                                x_names = variable_names,
                                                validators,
                                                seed_num,
                                                output_path_prefix,
                                                validation_data_fraction = train_fraction,
                                                kappa_filter_threshold,
                                                if_listwise_deletion,
                                                y_names,
                                                lr_maxiter,
                                                kappa_results_threshold,
                                                kappa_results_threshold_final_metrics,
                                                optimize_prob_thresh,
                                                pcd_dropping_pct,
                                                if_CV,
                                                label_category1 = if(customized){c(label_category1,comparison)}else{label_category1},
                                                customized = customized,
                                                used_clusters = used_clusters)
        }else{
          res <- dichPseudoByPathAllModelKmeans(folder_path,
                                                ##model classes
                                                n_range = length(cluster_names):length(cluster_names),
                                                K_fold,
                                                repeated_folds_R = repeated_CV,
                                                input_dt,
                                                x_names = variable_names,
                                                validators,
                                                seed_num,
                                                output_path_prefix,
                                                validation_data_fraction = train_fraction,
                                                kappa_filter_threshold,
                                                if_listwise_deletion,
                                                y_names,
                                                lr_maxiter,
                                                kappa_results_threshold,
                                                kappa_results_threshold_final_metrics,
                                                optimize_prob_thresh,
                                                pcd_dropping_pct,
                                                if_CV,
                                                label_category1 = if(customized){c(label_category1,comparison)}else{label_category1},
                                                customized = customized,
                                                used_clusters = used_clusters)
        }


      }else if(tolower(model_type) %in% c("O","o", "ogroups", "o groups", "ogroup", "o group")){
        if(if_continuous){
          res <- dichPseudoByPathAllModelO_Cont(folder_path,
                                           K_fold,
                                           repeated_folds_R = repeated_CV,
                                           input_dt,
                                           x_names = variable_names,
                                           validators,
                                           seed_num,
                                           output_path_prefix,
                                           validation_data_fraction = train_fraction,
                                           if_listwise_deletion,
                                           y_names,
                                           lr_maxiter,
                                           pcd_dropping_pct,
                                           if_CV,
                                           label_category1 = if(customized){c(label_category1,comparison)}else{label_category1})

        }else{
          res <- dichPseudoByPathAllModelO(folder_path,
                                           K_fold,
                                           repeated_folds_R = repeated_CV,
                                           input_dt,
                                           x_names = variable_names,
                                           validators,
                                           seed_num,
                                           output_path_prefix,
                                           validation_data_fraction = train_fraction,
                                           if_listwise_deletion,
                                           y_names,
                                           lr_maxiter,
                                           pcd_dropping_pct,
                                           if_CV,
                                           label_category1 = if(customized){c(label_category1,comparison)}else{label_category1})
        }

      }
      }

    if(if_continuous){
      if(train_fraction == 1){
        res <- res %>%
          transmute(Supervised_method = supervised_method,
                    Supervised_spec1 = "-",
                    Supervised_spec2 = "-",
                    Supervised_spec3 = "-",
                    Cluster_n = length(cluster_names),
                    Cluster_names = paste(cluster_names,collapse = ""),
                    Label_category1 = combination_of_class_probabilities,
                    Label_position = cluster_label_position,
                    Predictors = ifelse(length(predictors_names) < 2, predictors_names, paste(predictors_names[1:2],collapse = " ")),
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
      }else
      {
        res <- res %>%
          transmute(Supervised_method = supervised_method,
                    Supervised_spec1 = "-",
                    Supervised_spec2 = "-",
                    Supervised_spec3 = "-",
                    Cluster_n = length(cluster_names),
                    Cluster_names = paste(cluster_names,collapse = ""),
                    Label_category1 = combination_of_class_probabilities,
                    Label_position = cluster_label_position,
                    Predictors = ifelse(length(predictors_names) < 2, predictors_names, paste(predictors_names[1:2],collapse = " ")),
                    MSE_train = MSE_cv,
                    MSE_test = MSE_test,
                    MSE_SE_train = MSE_SE_cv,
                    MSE_SE_test = MSE_SE_test,
                    RMSE_train = RMSE_cv,
                    RMSE_SE_train = RMSE_SE_cv,
                    RMSE_test = RMSE_test,
                    RMSE_SE_test = RMSE_SE_test,
                    MAE_train = MAE_cv,
                    MAE_test = MAE_test,
                    MAE_SE_train = MAE_SE_cv,
                    MAE_SE_test = MAE_SE_test,
                    r_square_train = r_square_cv,
                    r_square_test = r_square_test,
                    r_square_SE_train = r_square_SE_cv,
                    r_square_SE_test = r_square_SE_test,
                    adj_r_square_train = adj_r_square_cv,
                    adj_r_square_test = adj_r_square_test,
                    adj_r_square_SE_train = adj_r_square_SE_cv,
                    adj_r_square_SE_test = adj_r_square_SE_test,
                    aic_train = aic_cv,
                    aic_test = aic_test,
                    aic_SE_train = aic_SE_cv,
                    aic_SE_test = aic_SE_test
          )
      }
      write.csv(
        res,
        paste(
          output_path_prefix,
          "predclust_results.csv",
          sep = ""
        )
      )
      return(res)
    }
    if(train_fraction == 1){
      res <- res %>%
        transmute(Supervised_method = supervised_method,
                  Supervised_spec1 = "-",
                  Supervised_spec2 = "-",
                  Supervised_spec3 = "-",
                  Cluster_n = length(cluster_names),
                  Cluster_names = paste(cluster_names,collapse = ""),
                  Label_category1 = combination_of_class_probabilities,
                  Label_position = cluster_label_position,
                  Predictors = ifelse(length(predictors_names) < 2, predictors_names, paste(predictors_names[1:2],collapse = " ")),
                  kappa = kappa_mean,
                  kappa_SE = kappa_sd,
                  sensitivity = sensitivity_mean,
                  sensitivity_SE = sensitivity_sd,
                  specificity = specificity_mean,
                  specificity_SE = specificity_sd,
                  accuracy = accuracy_mean,
                  accuracy_SE = accuracy_sd,
                  AUC = AUC_mean,
                  AUC_SE = AUC_sd
        )
    }else
      {
      res <- res %>%
        transmute(Supervised_method = supervised_method,
                  Supervised_spec1 = "-",
                  Supervised_spec2 = "-",
                  Supervised_spec3 = "-",
                  Cluster_n = length(cluster_names),
                  Cluster_names = paste(cluster_names,collapse = ""),
                  Label_category1 = combination_of_class_probabilities,
                  Label_position = cluster_label_position,
                  Predictors = ifelse(length(predictors_names) < 2, predictors_names, paste(predictors_names[1:2],collapse = " ")),
                  kappa_train = kappa_mean_cv,
                  kappa_test = kappa_mean_test,
                  kappa_SE_train = kappa_sd_cv,
                  kappa_SE_test = kappa_sd_test,
                  sensitivity_train = sensitivity_mean_cv,
                  sensitivity_test = sensitivity_mean_test,
                  sensitivity_SE_train = sensitivity_sd_cv,
                  sensitivity_SE_test = sensitivity_sd_test,
                  specificity_train = specificity_mean_cv,
                  specificity_test = specificity_mean_test,
                  specificity_SE_train = specificity_sd_cv,
                  specificity_SE_test = specificity_sd_test,
                  accuracy_train = accuracy_mean_cv,
                  accuracy_test = accuracy_mean_test,
                  accuracy_SE_train = accuracy_sd_cv,
                  accuracy_SE_test = accuracy_sd_test,
                  AUC_train = AUC_mean_cv,
                  AUC_test = AUC_mean_test,
                  AUC_SE_train = AUC_sd_cv,
                  AUC_SE_test = AUC_sd_test
        )
    }
    write.csv(
      res,
      paste(
        output_path_prefix,
        "predclust_results.csv",
        sep = ""
      )
    )
    res
  }else
    {
    if(!isTRUE(sync_genclust) && !isTRUE(sync_validclust)){
      output_path_prefix <- output_path_prefix
    }else if(!isTRUE(sync_genclust) && isTRUE(sync_validclust)){
      if(sjmisc::is_empty(output_path_prefix)) output_path_prefix <- global_parameters_valid[['output_path_prefix']]
      if(sjmisc::is_empty(data_path)) data_path <- global_parameters_valid[['data_path']]
      if(sjmisc::is_empty(variable_names)) variable_names <- global_parameters_valid[['variable_names']]
      if(sjmisc::is_empty(naString)) naString <- global_parameters_valid[['naString']]
      if(sjmisc::is_empty(cluster_names)) cluster_names <- global_parameters_valid[['cluster_names']]
    }
    print("start input_dt")
    input_dt <- inputDataPrepare(data_path = data_path,
                                 x_names = variable_names,
                                 naString = naString)
    print("end input_dt")
    if(length(cluster_names) == 1){
      clust_multi <- sjmisc::to_dummy(input_dt[,cluster_names])
      names(clust_multi) <- paste("P", 1:ncol(clust_multi), sep="")
      input_dt[,names(clust_multi)] <- clust_multi
      cluster_names <- names(clust_multi)
    }

    label_category1 <- paste("P", which(cluster_names %in% label_category1), sep = "")

    if(!sjmisc::is_empty(reference)) reference <- paste("P", which(cluster_names %in% reference), sep = "")
    if(!sjmisc::is_empty(comparison)) comparison <- paste("P", which(cluster_names %in% comparison), sep = "")
    if(customized){
      if_PCD <- FALSE
      label_category1 <- reference
      comparison <- paste(comparison, collapse=",")
    }
    label_category1 <- paste(label_category1, collapse=",")

    if(if_continuous == FALSE & tolower(trimws(cluster_label_position)) == "predicted"){
      validators <- list(validator(
        predicted_cluster_combination = if(customized){c(label_category1,comparison)}else{label_category1},
        predicted_cluster_n = cluster_names,
        validator_source_variables = predictors_names,
        listwise_deletion_variables = listwise_deletion_variables,
        validator_type = "direct",
        seed_num = c(seed_num_split = as.integer(seed_numbers['seed_num_split']),
                     seed_num_kfold = as.integer(seed_numbers['seed_num_kfold']),
                     seed_num_supervised_model = as.integer(seed_numbers['seed_num_supervised_model'])),
        supervised_model = supervised_method,
        alpha = glmnet_specs['alpha'],
        lambda = glmnet_specs['lambda'],
        validator_source_all_missing = outcome_obs$outcome_source_all_missing))
      print("validator is")
      print(validators)
    }else if(if_continuous == FALSE & tolower(trimws(cluster_label_position)) %in% c("predictor","none"))
    {
      validators <- list(validator(validator_cutpoint = outcome_obs$outcome_cutpoint,
                                   validator_cutpoint_sign = outcome_obs$outcome_cutpoint_sign,
                                   validator_cutpoint_max_min_mean = outcome_obs$outcome_cutpoint_max_min_mean,
                                   predicted_cluster_combination = if(customized){c(label_category1,comparison)}else{label_category1},
                                   predicted_cluster_n = cluster_names,
                                   validator_source_variables = outcome_obs$outcome_source_variables,
                                   listwise_deletion_variables = listwise_deletion_variables,
                                   validator_type = "flip",
                                   flip_outcome_type = tolower(trimws(outcome_obs$outcome_type)),
                                   flipped_predictors_variables = predictors_names,
                                   flipped_predictors_cluster = ifelse(cluster_label_position == "predictor",TRUE,FALSE),
                                   flipped_predictors_pp = FALSE,
                                   seed_num = c(seed_num_split = as.integer(seed_numbers['seed_num_split']),
                                                seed_num_kfold = as.integer(seed_numbers['seed_num_kfold']),
                                                seed_num_supervised_model = as.integer(seed_numbers['seed_num_supervised_model'])),
                                   supervised_model = supervised_method,
                                   alpha = glmnet_specs['alpha'],
                                   lambda = glmnet_specs['lambda'],
                                   validator_source_all_missing = outcome_obs$outcome_source_all_missing))
    }else if(if_continuous == TRUE & tolower(trimws(cluster_label_position)) %in% c("predictor","none")){
      validators <- list(validator(validator_cutpoint = outcome_obs$outcome_cutpoint,
                                   validator_cutpoint_sign = outcome_obs$outcome_cutpoint_sign,
                                   validator_cutpoint_max_min_mean = outcome_obs$outcome_cutpoint_max_min_mean,
                                   predicted_cluster_combination = if(customized){c(label_category1,comparison)}else{label_category1},
                                   predicted_cluster_n = cluster_names,
                                   validator_source_variables = outcome_obs$outcome_source_variables,
                                   listwise_deletion_variables = listwise_deletion_variables,
                                   validator_type = "continuous",
                                   flipped_predictors_variables = predictors_names,
                                   flipped_predictors_cluster = ifelse(cluster_label_position == "predictor",TRUE,FALSE),
                                   flipped_predictors_pp = FALSE,
                                   seed_num = c(seed_num_split = as.integer(seed_numbers['seed_num_split']),
                                                seed_num_kfold = as.integer(seed_numbers['seed_num_kfold']),
                                                seed_num_regression_model = as.integer(seed_numbers['seed_num_regression_model'])),
                                   supervised_model = supervised_method,
                                   validator_source_all_missing = outcome_obs$outcome_source_all_missing,
                                   contVarName = outcome_obs$outcome_continuous))

    }
    if(dir.exists(output_path_prefix) == FALSE){
      dir.create(output_path_prefix)
    }
    useobs <- useObsSplitter(useobs)
    if(is.null(useobs)==FALSE){
      print(input_dt)
      text_useobs <- paste("input_dt <- dplyr::filter(input_dt,",useobs,")",sep = "")
      print("text_useobs is:")
      print(text_useobs)
      eval(parse(text = text_useobs))
      input_dt <- as.data.frame(input_dt)
    }
    print("start to run syncF")
    input_dt <- input_dt[stats::complete.cases(input_dt[,cluster_names]),]
    print(!all(apply(input_dt[,cluster_names],2,FUN = function(x){all(x %in% c(0,1))})))
    if(cluster_label_position == "predictor") predictors_names <- c(predictors_names, "cluster")
    if(!all(apply(input_dt[,cluster_names],2,FUN = function(x){all(x %in% c(0,1))}))){
      if(if_PCD){
        if(if_continuous){
          res <- validAllModelPCD_Cont(cluster_names ,
                                        r_pseudo = r_PCD,
                                        K_fold,
                                        repeated_folds_R = repeated_CV,
                                        input_dt,
                                        x_names = variable_names,
                                        validators,
                                        seed_num,
                                        output_path_prefix,
                                        validation_data_fraction = train_fraction,
                                        if_listwise_deletion,
                                        y_names,
                                        lr_maxiter,
                                        kappa_results_threshold_final_metrics,
                                        optimize_prob_thresh = 0.5,
                                        pcd_dropping_pct,
                                        if_CV,
                                        label_category1 = if(customized){c(label_category1,comparison)}else{label_category1})

          if(train_fraction == 1){
            res <- res %>%
              transmute(Supervised_method = supervised_method,
                        Supervised_spec1 = "-",
                        Supervised_spec2 = "-",
                        Supervised_spec3 = "-",
                        Cluster_n = length(cluster_names),
                        Cluster_names = paste(cluster_names,collapse = ""),
                        Label_category1 = combination_of_class_probabilities,
                        Label_position = cluster_label_position,
                        Predictors = ifelse(length(predictors_names) < 2, predictors_names, paste(predictors_names[1:2],collapse = " ")),
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
          }else
          {
            res <- res %>%
              transmute(Supervised_method = supervised_method,
                        Supervised_spec1 = "-",
                        Supervised_spec2 = "-",
                        Supervised_spec3 = "-",
                        Cluster_n = length(cluster_names),
                        Cluster_names = paste(cluster_names,collapse = ""),
                        Label_category1 = combination_of_class_probabilities,
                        Label_position = cluster_label_position,
                        Predictors = ifelse(length(predictors_names) < 2, predictors_names, paste(predictors_names[1:2],collapse = " ")),
                        MSE_train = MSE_cv,
                        MSE_test = MSE_test,
                        MSE_SE_train = MSE_SE_cv,
                        MSE_SE_test = MSE_SE_test,
                        RMSE_train = RMSE_cv,
                        RMSE_test = RMSE_test,
                        RMSE_SE_test = RMSE_SE_test,
                        MAE_train = MAE_cv,
                        MAE_test = MAE_test,
                        MAE_SE_train = MAE_SE_cv,
                        MAE_SE_test = MAE_SE_test,
                        r_square_train = r_square_cv,
                        r_square_test = r_square_test,
                        r_square_SE_train = r_square_SE_cv,
                        r_square_SE_test = r_square_SE_test,
                        adj_r_square_train = adj_r_square_cv,
                        adj_r_square_test = adj_r_square_test,
                        adj_r_square_SE_train = adj_r_square_SE_cv,
                        adj_r_square_SE_test = adj_r_square_SE_test,
                        aic_train = aic_cv,
                        aic_test = aic_test,
                        aic_SE_train = aic_SE_cv,
                        aic_SE_test = aic_SE_test
              )
          }
          write.csv(
            res,
            paste(
              output_path_prefix,
              "predclust_results.csv",
              sep = ""
            )
          )
          return(res)
        }
        res <- validAllModelPCD(cluster_names ,
                                r_pseudo = r_PCD,
                                K_fold,
                                repeated_folds_R = repeated_CV,
                                input_dt,
                                x_names = variable_names,
                                validators,
                                seed_num,
                                output_path_prefix,
                                validation_data_fraction = train_fraction,
                                if_listwise_deletion,
                                y_names,
                                lr_maxiter,
                                kappa_results_threshold_final_metrics,
                                optimize_prob_thresh = 0.5,
                                pcd_dropping_pct,
                                if_CV,
                                label_category1 = if(customized){c(label_category1,comparison)}else{label_category1})
        if(train_fraction == 1){
          res <- res %>%
            transmute(Supervised_method = supervised_method,
                      Supervised_spec1 = "-",
                      Supervised_spec2 = "-",
                      Supervised_spec3 = "-",
                      Cluster_n = length(cluster_names),
                      Cluster_names = paste(cluster_names,collapse = ""),
                      Label_category1 = combination_of_class_probabilities,
                      Label_position = cluster_label_position,
                      Predictors = ifelse(length(predictors_names) < 2, predictors_names, paste(predictors_names[1:2],collapse = " ")),
                      kappa = kappa_mean,
                      kappa_SE = kappa_sd,
                      sensitivity = sensitivity_mean,
                      sensitivity_SE = sensitivity_sd,
                      specificity = specificity_mean,
                      specificity_SE = specificity_sd,
                      accuracy = accuracy_mean,
                      accuracy_SE = accuracy_sd,
                      AUC = AUC_mean,
                      AUC_SE = AUC_sd
            )
        }else
          {
          res <- res %>%
            transmute(Supervised_method = supervised_method,
                      Supervised_spec1 = "-",
                      Supervised_spec2 = "-",
                      Supervised_spec3 = "-",
                      Cluster_n = length(cluster_names),
                      Cluster_names = paste(cluster_names,collapse = ""),
                      Label_category1 = combination_of_class_probabilities,
                      Label_position = cluster_label_position,
                      Predictors = ifelse(length(predictors_names) < 2, predictors_names, paste(predictors_names[1:2],collapse = " ")),
                      kappa_train = kappa_mean_cv,
                      kappa_test = kappa_mean_test,
                      kappa_SE_train = kappa_sd_cv,
                      kappa_SE_test = kappa_sd_test,
                      sensitivity_train = sensitivity_mean_cv,
                      sensitivity_test = sensitivity_mean_test,
                      sensitivity_SE_train = sensitivity_sd_cv,
                      sensitivity_SE_test = sensitivity_sd_test,
                      specificity_train = specificity_mean_cv,
                      specificity_test = specificity_mean_test,
                      specificity_SE_train = specificity_sd_cv,
                      specificity_SE_test = specificity_sd_test,
                      accuracy_train = accuracy_mean_cv,
                      accuracy_test = accuracy_mean_test,
                      accuracy_SE_train = accuracy_sd_cv,
                      accuracy_SE_test = accuracy_sd_test,
                      AUC_train = AUC_mean_cv,
                      AUC_test = AUC_mean_test,
                      AUC_SE_train = AUC_sd_cv,
                      AUC_SE_test = AUC_sd_test
            )
        }
      }else
        {
        if(if_continuous){
          res <- validAllModel_Cont(cluster_names,
                                     K_fold,
                                     repeated_folds_R = repeated_CV,
                                     input_dt,
                                     x_names = variable_names,
                                     validators,
                                     seed_num,
                                     output_path_prefix,
                                     validation_data_fraction = train_fraction,
                                     if_listwise_deletion,
                                     y_names,
                                     lr_maxiter,
                                     kappa_results_threshold_final_metrics,
                                     combined_posterior_prob_threshold,
                                     optimize_prob_thresh = 0.5,
                                     pcd_dropping_pct,
                                     if_CV,
                                     label_category1 = if(customized){c(label_category1,comparison)}else{label_category1},
                                    customized = customized,
                                    used_clusters = used_clusters,
                                    cohend_SD = cohend_SD)
          cluster_names_n = length(cluster_names)
          if(train_fraction == 1){
            res <- res %>%
              transmute(Supervised_method = supervised_method,
                        Supervised_spec1 = "-",
                        Supervised_spec2 = "-",
                        Supervised_spec3 = "-",
                        Cluster_n = cluster_names_n,
                        Cluster_names = cluster_names,
                        Label_category1 = label_group1,
                        Label_position = cluster_label_position,
                        Predictors = ifelse(length(predictors_names) < 2, predictors_names, paste(predictors_names[1:2],collapse = " ")),
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
          }else
            {
            res <- res %>%
              transmute(Supervised_method = supervised_method,
                        Supervised_spec1 = "-",
                        Supervised_spec2 = "-",
                        Supervised_spec3 = "-",
                        Cluster_n = length(cluster_names),
                        Cluster_names = paste(cluster_names,collapse = ""),
                        Label_category1 = combination_of_class_probabilities,
                        Label_position = cluster_label_position,
                        Predictors = ifelse(length(predictors_names) < 2, predictors_names, paste(predictors_names[1:2],collapse = " ")),
                        MSE_train = MSE_cv,
                        MSE_test = MSE_test,
                        MSE_SE_train = MSE_SE_cv,
                        MSE_SE_test = MSE_SE_test,
                        RMSE_train = RMSE_cv,
                        RMSE_test = RMSE_test,
                        RMSE_SE_train = RMSE_SE_train,
                        RMSE_SE_test = RMSE_SE_test,
                        MAE_train = MAE_cv,
                        MAE_test = MAE_test,
                        MAE_SE_train = MAE_SE_cv,
                        MAE_SE_test = MAE_SE_test,
                        r_square_train = r_square_cv,
                        r_square_test = r_square_test,
                        r_square_SE_train = r_square_SE_cv,
                        r_square_SE_test = r_square_SE_test,
                        adj_r_square_train = adj_r_square_cv,
                        adj_r_square_test = adj_r_square_test,
                        adj_r_square_SE_train = adj_r_square_SE_cv,
                        adj_r_square_SE_test = adj_r_square_SE_test,
                        aic_train = aic_cv,
                        aic_test = aic_test,
                        aic_SE_train = aic_SE_cv,
                        aic_SE_test = aic_SE_test
              )
          }
          write.csv(
            res,
            paste(
              output_path_prefix,
              "predclust_results.csv",
              sep = ""
            )
          )
          return(res)
        }
        res <- validAllModel(cluster_names,
                             K_fold,
                             repeated_folds_R = repeated_CV,
                             input_dt,
                             x_names = variable_names,
                             validators,
                             seed_num,
                             output_path_prefix,
                             validation_data_fraction = train_fraction,
                             if_listwise_deletion,
                             y_names,
                             lr_maxiter,
                             kappa_results_threshold_final_metrics,
                             combined_posterior_prob_threshold,
                             optimize_prob_thresh = 0.5,
                             pcd_dropping_pct,
                             if_CV,
                             label_category1 = if(customized){c(label_category1,comparison)}else{label_category1},
                             customized = customized,
                             used_clusters = used_clusters)
        if(train_fraction == 1){
          res <- res %>%
            transmute(Supervised_method = supervised_method,
                      Supervised_spec1 = "-",
                      Supervised_spec2 = "-",
                      Supervised_spec3 = "-",
                      Cluster_n = length(cluster_names),
                      Cluster_names = paste(cluster_names,collapse = ""),
                      Label_category1 = label_group1,
                      Label_position = cluster_label_position,
                      Predictors = ifelse(length(predictors_names) < 2, predictors_names, paste(predictors_names[1:2],collapse = " ")),
                      kappa = kappa,
                      kappa_SE = kappa_SE,
                      sensitivity = sensitivity,
                      sensitivity_SE = sensitivity_SE,
                      specificity = specificity,
                      specificity_SE = specificity_SE,
                      accuracy = accuracy,
                      accuracy_SE = accuracy_SE,
                      AUC = AUC,
                      AUC_SE = AUC_SE
            )
        }else{
          res <- res %>%
            transmute(Supervised_method = supervised_method,
                      Supervised_spec1 = "-",
                      Supervised_spec2 = "-",
                      Supervised_spec3 = "-",
                      Cluster_n = length(cluster_names),
                      Cluster_names = paste(cluster_names,collapse = ""),
                      Label_category1 = combination_of_class_probabilities,
                      Label_position = cluster_label_position,
                      Predictors = ifelse(length(predictors_names) < 2, predictors_names, paste(predictors_names[1:2],collapse = " ")),
                      kappa_train = kappa_mean_cv,
                      kappa_test = kappa_mean_test,
                      kappa_SE_train = kappa_sd_cv,
                      kappa_SE_test = kappa_sd_test,
                      sensitivity_train = sensitivity_mean_cv,
                      sensitivity_test = sensitivity_mean_test,
                      sensitivity_SE_train = sensitivity_sd_cv,
                      sensitivity_SE_test = sensitivity_sd_test,
                      specificity_train = specificity_mean_cv,
                      specificity_test = specificity_mean_test,
                      specificity_SE_train = specificity_sd_cv,
                      specificity_SE_test = specificity_sd_test,
                      accuracy_train = accuracy_mean_cv,
                      accuracy_test = accuracy_mean_test,
                      accuracy_SE_train = accuracy_sd_cv,
                      accuracy_SE_test = accuracy_sd_test,
                      AUC_train = AUC_mean_cv,
                      AUC_test = AUC_mean_test,
                      AUC_SE_train = AUC_sd_cv,
                      AUC_SE_test = AUC_sd_test
            )
        }
      }
    }else
    {
      if(customized){
        input_dt <- input_dt[rowSums(input_dt[,used_clusters, drop=F]) == 1,]
      }
      if(if_continuous){
        res <- validAllModel_Cat_Cont(cluster_names,
                                       K_fold,
                                       repeated_folds_R = repeated_CV,
                                       input_dt,
                                       x_names = variable_names,
                                       validators,
                                       seed_num,
                                       output_path_prefix,
                                       validation_data_fraction = train_fraction,
                                       if_listwise_deletion,
                                       y_names,
                                       lr_maxiter,
                                       kappa_results_threshold_final_metrics,
                                       optimize_prob_thresh = 0.5,
                                       pcd_dropping_pct,
                                       if_CV,
                                       label_category1 = if(customized){c(label_category1,comparison)}else{label_category1},
                                      customized = customized,
                                      used_clusters = used_clusters,
                                      cohend_SD = cohend_SD)
        if(train_fraction == 1){
          res <- res %>%
            transmute(Supervised_method = supervised_method,
                      Supervised_spec1 = "-",
                      Supervised_spec2 = "-",
                      Supervised_spec3 = "-",
                      Cluster_n = length(cluster_names),
                      Cluster_names = cluster_names,
                      Label_category1 = label_group1,
                      Label_position = cluster_label_position,
                      Predictors = ifelse(length(predictors_names) < 2, predictors_names, paste(predictors_names[1:2],collapse = " ")),
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
        }else{
          res <- res %>%
            transmute(Supervised_method = supervised_method,
                      Supervised_spec1 = "-",
                      Supervised_spec2 = "-",
                      Supervised_spec3 = "-",
                      Cluster_n = length(cluster_names),
                      Cluster_names = paste(cluster_names,collapse = ""),
                      Label_category1 = combination_of_class_probabilities,
                      Label_position = cluster_label_position,
                      Predictors = ifelse(length(predictors_names) < 2, predictors_names, paste(predictors_names[1:2],collapse = " ")),
                      MSE_train = MSE_cv,
                      MSE_test = MSE_test,
                      MSE_SE_train = MSE_SE_cv,
                      MSE_SE_test = MSE_SE_test,
                      RMSE_train = RMSE_cv,
                      RMSE_SE_train = RMSE_SE_cv,
                      RMSE_test = RMSE_test,
                      RMSE_SE_test = RMSE_SE_test,
                      MAE_train = MAE_cv,
                      MAE_test = MAE_test,
                      MAE_SE_train = MAE_SE_cv,
                      MAE_SE_test = MAE_SE_test,
                      r_square_train = r_square_cv,
                      r_square_test = r_square_test,
                      r_square_SE_train = r_square_SE_cv,
                      r_square_SE_test = r_square_SE_test,
                      adj_r_square_train = adj_r_square_cv,
                      adj_r_square_test = adj_r_square_test,
                      adj_r_square_SE_train = adj_r_square_SE_cv,
                      adj_r_square_SE_test = adj_r_square_SE_test,
                      aic_train = aic_cv,
                      aic_test = aic_test,
                      aic_SE_train = aic_SE_cv,
                      aic_SE_test = aic_SE_test
            )
        }
        write.csv(
          res,
          paste(
            output_path_prefix,
            "predclust_results.csv",
            sep = ""
          )
        )
        return(res)
      }
      res <- validAllModel_Cat(cluster_names,
                               K_fold,
                               repeated_folds_R = repeated_CV,
                               input_dt,
                               x_names = variable_names,
                               validators,
                               seed_num,
                               output_path_prefix,
                               validation_data_fraction = train_fraction,
                               if_listwise_deletion,
                               y_names,
                               lr_maxiter,
                               kappa_results_threshold_final_metrics,
                               optimize_prob_thresh = 0.5,
                               pcd_dropping_pct,
                               if_CV,
                               label_category1 = if(customized){c(label_category1,comparison)}else{label_category1},
                               customized = customized,
                               used_clusters = used_clusters)
      if(train_fraction == 1){
        res <- res %>%
          transmute(Supervised_method = supervised_method,
                    Supervised_spec1 = "-",
                    Supervised_spec2 = "-",
                    Supervised_spec3 = "-",
                    Cluster_n = length(cluster_names),
                    Cluster_names = paste(cluster_names,collapse = ""),
                    Label_category1 = label_group1,
                    Label_position = cluster_label_position,
                    Predictors = ifelse(length(predictors_names) < 2, predictors_names, paste(predictors_names[1:2],collapse = " ")),
                    kappa = kappa,
                    kappa_SE = kappa_SE,
                    sensitivity = sensitivity,
                    sensitivity_SE = sensitivity_SE,
                    specificity = specificity,
                    specificity_SE = specificity_SE,
                    accuracy = accuracy,
                    accuracy_SE = accuracy_SE,
                    AUC = AUC,
                    AUC_SE = AUC_SE
          )
      }else{
        res <- res %>%
          transmute(Supervised_method = supervised_method,
                    Supervised_spec1 = "-",
                    Supervised_spec2 = "-",
                    Supervised_spec3 = "-",
                    Cluster_n = length(cluster_names),
                    Cluster_names = paste(cluster_names,collapse = ""),
                    Label_category1 = combination_of_class_probabilities,
                    Label_position = cluster_label_position,
                    Predictors = ifelse(length(predictors_names) < 2, predictors_names, paste(predictors_names[1:2],collapse = " ")),
                    kappa_train = kappa_mean_cv,
                    kappa_test = kappa_mean_test,
                    kappa_SE_train = kappa_sd_cv,
                    kappa_SE_test = kappa_sd_test,
                    sensitivity_train = sensitivity_mean_cv,
                    sensitivity_test = sensitivity_mean_test,
                    sensitivity_SE_train = sensitivity_sd_cv,
                    sensitivity_SE_test = sensitivity_sd_test,
                    specificity_train = specificity_mean_cv,
                    specificity_test = specificity_mean_test,
                    specificity_SE_train = specificity_sd_cv,
                    specificity_SE_test = specificity_sd_test,
                    accuracy_train = accuracy_mean_cv,
                    accuracy_test = accuracy_mean_test,
                    accuracy_SE_train = accuracy_sd_cv,
                    accuracy_SE_test = accuracy_sd_test,
                    AUC_train = AUC_mean_cv,
                    AUC_test = AUC_mean_test,
                    AUC_SE_train = AUC_sd_cv,
                    AUC_SE_test = AUC_sd_test
          )
      }
    }

    write.csv(
      res,
      paste(
        output_path_prefix,
        "predclust_results.csv",
        sep = ""
      )
    )
    res
  }
}
