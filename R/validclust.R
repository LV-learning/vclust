#' Validate Binary Coarsened Clusters By Validators
#'
#' Generates binary labels by regrouping clusters into two coarsened
#' clusters using all possible ways of splits, and systematically validates
#' the generated labels using a priori sets of validators defined by the users.
#'
#'
#' @param sync_genclust A Boolean variable indicates whether validation is
#' conducted directly using the results from genclust.
#' If \code{sync_genclust = TRUE}, all model and estimation specifications used in
#' genclust will be automatically imported into validclust.
#' If \code{sync_genclust = FALSE}, validclust is used as a stand-alone procedure,
#' which is useful when using clustering models or methods that are not currently
#' covered in genclust. In this case, the user is required to provide the
#' details about the data and clustering results.
#' @param info_genclust This argument will be applied when \code{sync_genclust = FALSE}
#' and ignored when \code{sync_genclust = TRUE}. users can use the format
#'\code{info_genclust = list(subcomponents)}
#' There are a few subcomponents described below.
#'
#' \itemize{
#' \item{\code{data_path:}}{ When \code{sync_genclust = FALSE}, the user needs to specify the folder
#' path here that stores the data that contains clustering results and intended validators.
#' A string indicates the path of the input data. The data should be in csv format.
#' For example, "/Users/username/Desktop/inputdata.csv" for Mac user or
#' "D:/folder/inputdata.csv" for Windows user. Use "/" instead of "\" for the path.}
#' \item{\code{output_path_prefix:}}{ The user needs to specify the folder path that will store validation results.
#' The path should be absolute path (full path) when using Windows operation system. }
#' \item{\code{variable_names:}}{ When \code{sync_genclust = FALSE},
#' the user needs to specify variable names.
#' A string vector indicates names of variables in the data specified in data_path.
#' For example, \cr
#' \code{variable_names = c("e1","e2","e3","f1","f2","z1","q1","w1","w2","w3")}.
#' These variable names will overwrite the original names when the data file
#' already has variables names (i.e., header).
#' The user can choose to use those original names
#' by specifying \code{variable_names = NULL}.}
#' \item{\code{naString:}}{A string indicates what string is interpreted as NA value
#' in the input data.}
#' \item{\code{cluster_names: }}{ A string vector indicates names of clusters.
#' When \code{sync_genclust = FALSE},
#' the user needs to specify the names of clusters.
#' For example, when validating outcome labels based on 3-cluster clustering,
#' \code{cluster_names = c("e1","e2","e3")} and when based on 2-cluster clustering,
#' \code{cluster_names = c("f1","f2")}. Note that the total should add up to 1.
#' That is, \deqn{e1+e2+e3=1} and \deqn{f1+f2=1}
#' For example, when using cluster membership in probabilities (soft clustering),
#' an individual may have \deqn{e1=0.3, e2=0.1, e3=0.6}, which add up to 1.
#' When using observed or hard cluster membership (one unit or person belongs to only one cluster),
#' for a person who belongs to the third cluster, \deqn{e1=0, e2=0, e3=1}
#'
#' Note that, when sync_genclust = FALSE, the current version allows only one set of cluster names.
#' For example, \code{cluster_names=c("e1","e2","e3")}.}
#' }
#' @param validators
#' A list specifies one or more validator objects following the format below.
#'
#' \code{validators = list(}
#'                   \code{validator(subcomponents),}
#'                   \code{validator(subcomponents),}
#'                   \code{validator(subcomponents),}
#'                   \code{…}
#'                 \code{)}
#'
#' The subcomponents include the following:
#'
#' \itemize{
#' \item{\code{listwise_deletion_variables: }}{ A vector indicates variables to be used to conduct
#' listwise deletion.\cr
#' For example,
#'  \code{listwise_deletion_variables = c("a1","b1")}.
#' The user is allowed to use listwise deletion with variables that are not being used in the validclust procedure.
#' The user is also allowed to use different variables for listwise deletion for different validators.
#' Note that the rest of subcomponent arguments will no longer apply to the deleted cases.\cr
#' If \code{sync_genclust = TRUE} and listwise_deletion_variables has been already used in the genclust step,
#' this argument can be used to specify additional deletion.}
#' \item{\code{validator_source_variables: }}{ A list of variables to be used to
#' construct a validator.\cr
#' \code{For example, validator_source_variables = c("a1","a2","a3","a4")}.}
#' \item{\code{validator_source_all_missing:}}{ An integer specifies which value
#' to take when all variables listed in validator_source_variables are missing.
#' The three possible options are NA, 1, or 0.\cr
#' If \code{validator_source_all_missing = NA},
#' the validator of these individuals or units will be treated as missing.
#' The default is 0.}
#' \item{\code{validator_type:}}{A string indicates the type of each set of validators.
#' There are 3 allowed types:
#'
#' "binary", when a single validator is already binary (0/1).
#'
#' "cutpoint", when a single binary validator needs to be created based on a cutpoint applied to a single or multiple variables.
#'
#' "combination", when a single continuous variable or a set of multiple
#' variables (continuous and/or binary) are used together as a set of predictors of cluster membership.
#' }
#' \item{\code{validator_cutpoint:}}{ A numeric value/vector specifies a
#' threshold or multiple thresholds to create a binary validator.\cr
#' For example, \code{validator_cutpoint = 12}, or \code{validator_cutpoint = c(12, 13, 14)}.}
#' \item{\code{validator_cutpoint_sign:}}{ A character value/vector specifies comparison operator(s)
#' to be used with thresholds. Available options include >=, <=, >, <, ==, GE, LE, GT, LT, and EQ.
#' When using a vector of multiple thresholds, the signs will be applied to each cutpoint.}
#' \item{\code{validator_cutpoint_max_min_mean:}}{A string specifies a function to use to
#' summarize multiple variables into a single validator. The options include max, min, and mean.\cr
#' For example, \code{max_min_mean = "max"}.
#'
#' When validator_cutpoint is a single value,
#' all cutpoint related arguments can be used together.\cr
#' For example, if \code{validator_source_variables = c('a', 'b', 'c')},\cr
#' \code{validator_cutpoint  = 12}, \code{validator_cutpoint_sign =">="},\cr
#' and \code{validator_cutpoint_max_min_mean="max"}, all cases with max(a, b, c) >= 12
#' will be assigned the value of 1, and the rest the value of 0.
#'
#' When validator_cutpoint has multiple values,
#' validator_max_min_mean will be ignored.\cr
#' For example, when \code{validator_source_variables = c('a','b','c')},\cr
#' \code{validator_cutpoint = c(12, 13, 14)},\cr
#' \code{validator_cutpoint_sign = c('>=','<','>')},\cr
#' all cases with a>=12 and b<13 and c>14 will be assigned the value of 1,
#' and the rest the value of 0.
#' }
#' }
#'
#' The procedure validclust generates binary labels by regrouping all provided
#'  clusters into two coarsened clusters using all possible ways of splits.
#'  \code{When sync_genclust = TRUE}, this could lead to a very large pool of
#'  candidate labels to be validated, which will significantly slow down the validation procedure.
#'  There are three ways to reduce the pool of candidate labels using the following
#'  three arguments, class_range, kappa_filter_maxN, and kappa_filter_value.
#'
#' @param class_range When sync_genclust=TRUE, the user can specify the desired
#' range of clusters that will be included in validation.
#' For example, with \code{class_range = 2:4},
#' clustering results with 2, 3, and 4 clusters will be validated.
#' When \code{sync_genclust=FALSE}, this argument will be ignored.
#' Instead, the set of clusters defined in cluster_names will be validated.
#' @param kappa_filter_maxN An integer indicates the maximum number of candidate
#' labels to be validated. When it is NULL, no filter is applied. In this method,
#' candidate labels are ranked by roughly calculating Cohen’s Kappa between each
#' candidate label and the primary validator (the first one on the validator list)
#'  without cross validation.
#'  For example, if \code{kappa_filter_maxN = 500}, only the top 500 labels based on
#'  Kappa will enter the validation procedure. The threshold is used to
#'  choose combinations with the best Cohen’s kappa.
#' @param kappa_filter_value An alternative way of limiting the number of
#' candidate labels to be validated is to apply a minimum Kappa value.
#' For example, if \code{kappa_filter_value = 0.15}, only the labels with Kappa value
#' of 0.15 or greater will enter the validation procedure. When it is NULL, no filter is applied.
#' @param kappa_filter_results The user can also specify the number of labels to
#' be included in the summary file (i.e., validclust_results.csv). When it is NULL,
#' all candidate labels that went through validation will appear in the summary.
#' @param useobs The user may specify a text string that indicates
#' observations to use. For example, if we want to exclude observations with x=9 and x=13,
#' we can set \code{useobs = "(x ne 9) and (x ne 13)"}.
#' If \code{sync_genclust = TRUE} and useobs has been already used,
#' this argument can be used to specify additional observations to be excluded.
#' @param if_CV A Boolean variable indicates whether K-fold cross validation
#' is used in the validation step.
#' @param K_fold An integer indicates the number of folds in cross-validation.
#' It is applicable when \code{if_CV = TRUE}.
#' @param seed_num_kfold When \code{if_CV = TRUE}, the user may provide a seed
#' number for randomly dividing the data into K folds.

#' @return The validation results will be provided as a csv file (validclust_results.csv)
#' in the user-specified folder. For each validator set and each candidate label,
#' Cohen’s Kappa, accuracy, sensitivity, specificity, and AUC estimates are provided
#' (their means and standard errors if K-fold cross validation is used).
#'
#' \itemize{
#' \item{\code{Model_type:}}{ When \code{genclust_sync=TRUE},
#' the clustering method used in the genclust procedure (specified in model_type) will be shown here. }
#' \item{\code{Model_spec1 to Model_spec3:}}{When \code{genclust_sync=TRUE},
#' specific model specifications used in the genclust procedure will be shown here. }
#' \item{\code{Cluster_n:}}{ The total number of clusters or classes in each clustering method.}
#' \item{\code{Cluster_names:}}{ When \code{genclust_sync=TRUE},
#' each cluster will be named starting with “P” and then numbered
#' following the original cluster order in each clustering result in the genclust procedure.
#' When \code{genclust_sync=FALSE}, the names and the order provided in cluster_names will be used.}
#' \item{\code{label_category1:}}{ In the validclust procedure,
#' in each clustering, all clusters are split into two categories to generate
#' binary labels. The clusters categorized in the first category will
#' be shown under label_category1. The rest are categorized into the second category.}
#' \item{\code{Validator:}}{ Each validator in the order specified in \code{validators = list( )}.}
#' \item{\code{Kappa, sensitivity, specificity, accuracy, AUC:}}{ These are the measures
#' of association between the validators and the binary labels generated based on clustering.
#' \code{When if_CV = TRUE}, the provided values are the means across K folds.}
#' \item{\code{Kappa_SE, sensitivity_SE, specificity_SE, accuracy_SE, AUC_SE:}}{When \code{if_CV = TRUE},
#' these are the standard deviations across K folds.}
#' }
#'
#' @references Jo, B., Hastie, T. J., Li, Z., Youngstrom, E. A., Findling, R. L., & Horwitz, S. M. (2023). Reorienting Latent Variable Modeling for Supervised Learning. Multivariate Behavioral Research, 1-15.
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarise if_else arrange desc group_modify ungroup transmute
#' @importFrom utils read.csv write.csv
#' @export
#'
validclust <- function(sync_genclust,
                       info_genclust,
                       useobs,
                       if_CV,
                       K_fold,
                       seed_num_kfold,
                       class_range,
                       kappa_filter_maxN,
                       kappa_filter_value,
                       kappa_filter_results,
                       validators,
                       customized = F,
                       reference = NULL,
                       comparison = NULL,
                       if_continuous = F
){
  base::suppressWarnings(try(RNGkind(sample.kind = "Rounding"), silent = TRUE))
  assign("global_parameters_valid", list(), envir = .GlobalEnv)
  if(customized & length(class_range) > 1) stop("The class_range can't have multiple values when customized = TRUE")
  if(customized & sjmisc::is_empty(reference)) stop("Please specify reference for customized = TRUE")
  label_category1 <- NULL
  repeated_CV = 1
  if_PCD <- FALSE
  kappa_filter_threshold <- kappa_filter_maxN
  kappa_results_threshold <- kappa_filter_value
  kappa_results_threshold_final_metrics <- kappa_filter_results
  combined_posterior_prob_threshold <- 0.5
  if_listwise_deletion <- FALSE
  train_fraction <- 1
  lr_maxiter <- 100
  pcd_dropping_pct <- c(0.1,0.1,1)
  for(i in 1:length(validators)){
    validators[[i]]$seed_num['seed_num_kfold'] = seed_num_kfold
  }

  if(isTRUE(sync_genclust)){
    if(customized){
      if(sjmisc::is_empty(comparison)){
        all_clusters <- paste("P",1:class_range, sep="")
        comparison <- all_clusters[!all_clusters %in% reference]
      }else{
        comparison <- paste(comparison, collapse=",")
      }
      reference <- paste(reference, collapse=",")
      label_category1 <- c(reference,comparison)
    }
    class_range <- max(global_parameters$class_range[1],class_range[1]):min(global_parameters$class_range[length(global_parameters$class_range)],class_range[length(class_range)])
    #folder_path = '/Users/zetanli/Desktop/myproject roc max predicted_cluster /testdiffseed_gmm_runif_validclust/gmm/cP3/',
    folder_path <- global_parameters$folder_path
    listwise_deletion_variables <- global_parameters$listwise_deletion_variables
    #data_path="/Users/zetanli/Desktop/stanfordwork/mplus_projects/scarepgb616.csv",
    data_path <-  global_parameters$data_path
    #output_path_prefix = "/Users/zetanli/Desktop/myproject roc max predicted_cluster /testdiffseed_gmm_runif_validclust_nopcd/",
    output_path_prefix <- global_parameters$output_path_prefix
    useObs <- global_parameters$useobs
    model_type <- global_parameters$model_type
    covariates <- global_parameters$covariates
    is_covariates <- !sjmisc::is_empty(covariates)
    variable_names <- global_parameters$variable_names
    naString <- global_parameters$naString
    y_names <- global_parameters$y_names
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
    if(tolower(model_type) %in% c("gmm","growth mixture model")){
      if(if_continuous){
        res <- dichPseudoByPathAllModelNoPCD_Cont(folder_path,
                                                   ##model classes
                                                   n_range = class_range,
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
                                                   label_category1 = label_category1,
                                                   customized = customized)
        res <- res %>%
          transmute(model_type = "GMM",
                    model_spec1 = global_parameters$GMM_trend,
                    model_spec2 = ifelse(global_parameters$GMM_random_intercept,"random intercept","-"),
                    model_spec3 = ifelse(is_covariates,"covariates","-"),
                    n_clusters = n_classes,
                    cluster_names = sapply(res$n_classes,FUN=function(x)paste(paste("P",1:x,sep=""),collapse = "")),
                    label_group1 = combination_of_class_probabilities,
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

        write.csv(
          res,
          paste(
            output_path_prefix,
            "valid_results.csv",
            sep = ""
          )
        )
        return(res)
      }

      res <- dichPseudoByPathAllModelNoPCD(folder_path,
                                           ##model classes
                                           n_range = class_range,
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
                                           label_category1 = label_category1,
                                           customized = customized)
      res <- res %>%
        transmute(model_type = "GMM",
                  model_spec1 = global_parameters$GMM_trend,
                  model_spec2 = ifelse(global_parameters$GMM_random_intercept,"random intercept","-"),
                  model_spec3 = ifelse(is_covariates,"covariates","-"),
                  n_clusters = n_classes,
                  cluster_names = sapply(res$n_classes,FUN=function(x)paste(paste("P",1:x,sep=""),collapse = "")),
                  label_group1 = combination_of_class_probabilities,
                  validator = validation_group,
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
      write.csv(
        res,
        paste(
          output_path_prefix,
          "valid_results.csv",
          sep = ""
        )
      )
      res


    }else if(tolower(model_type) %in% c("mclust", "gaussian mixture model","mbc","model based clustering", "model-based clustering")){

      if(if_continuous){
        res <- dichPseudoByPathAllModelNoPCD_Cont(folder_path,
                                                  ##model classes
                                                  n_range = class_range,
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
                                                  label_category1 = label_category1,
                                                  customized = customized)
        res <- res %>%
          transmute(model_type = "MBC",
                    model_spec1 = global_parameters$MBCtype,
                    model_spec2 = "-",
                    model_spec3 = "-",
                    n_clusters = n_classes,
                    cluster_names = sapply(res$n_classes,FUN=function(x)paste(paste("P",1:x,sep=""),collapse = "")),
                    label_group1 = combination_of_class_probabilities,
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
        write.csv(
          res,
          paste(
            output_path_prefix,
            "valid_results.csv",
            sep = ""
          )
        )
        return(res)
      }

      res <- dichPseudoByPathAllModelNoPCDMclust(folder_path,
                                                 ##model classes
                                                 n_range = class_range,
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
                                                 label_category1 = label_category1,
                                                 customized = customized)
      res <- res %>%
        transmute(model_type = "MBC",
                  model_spec1 = global_parameters$MBCtype,
                  model_spec2 = "-",
                  model_spec3 = "-",
                  n_clusters = n_classes,
                  cluster_names = sapply(res$n_classes,FUN=function(x)paste(paste("P",1:x,sep=""),collapse = "")),
                  label_group1 = combination_of_class_probabilities,
                  validator = validation_group,
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
      write.csv(
        res,
        paste(
          output_path_prefix,
          "valid_results.csv",
          sep = ""
        )
      )
      res

    }else if(tolower(model_type) %in% c("k-means","kmeans","k means","k_means")){
      if(if_continuous){
        res <- dichPseudoByPathAllModelKmeans_Cont(folder_path,
                                                    ##model classes
                                                    n_range = class_range,
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
                                                    optimize_prob_thresh = 0.5,
                                                    pcd_dropping_pct,
                                                    if_CV,
                                                    label_category1)
        res <- res %>%
          transmute(model_type = "K-means",
                    model_spec1 = "-",
                    model_spec2 = "-",
                    model_spec3 = "-",
                    n_clusters = n_classes,
                    cluster_names = sapply(res$n_classes,FUN=function(x)paste(paste("P",1:x,sep=""),collapse = "")),
                    label_group1 = combination_of_class_probabilities,
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

        write.csv(
          res,
          paste(
            output_path_prefix,
            "valid_results.csv",
            sep = ""
          )
        )
        return(res)
      }
      res <- dichPseudoByPathAllModelKmeans(folder_path,
                                            ##model classes
                                            n_range = class_range,
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
                                            optimize_prob_thresh = 0.5,
                                            pcd_dropping_pct,
                                            if_CV,
                                            label_category1)
      res <- res %>%
        transmute(model_type = "K-means",
                  model_spec1 = "-",
                  model_spec2 = "-",
                  model_spec3 = "-",
                  n_clusters = n_classes,
                  cluster_names = sapply(res$n_classes,FUN=function(x)paste(paste("P",1:x,sep=""),collapse = "")),
                  label_group1 = combination_of_class_probabilities,
                  validator = validation_group,
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
      write.csv(
        res,
        paste(
          output_path_prefix,
          "valid_results.csv",
          sep = ""
        )
      )
      res


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
                                               label_category1)
        res <- res %>%
          transmute(model_type = "Ogroup",
                    model_spec1 = "-",
                    model_spec2 = "-",
                    model_spec3 = "-",
                    n_clusters = n_classes,
                    cluster_names = sapply(res$n_classes,FUN=function(x)paste(paste("P",1:x,sep=""),collapse = "")),
                    label_group1 = combination_of_class_probabilities,
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

        write.csv(
          res,
          paste(
            output_path_prefix,
            "valid_results.csv",
            sep = ""
          )
        )
        return(res)
      }
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
                                       label_category1)
      res <- res %>%
        transmute(model_type = "Ogroup",
                  model_spec1 = "-",
                  model_spec2 = "-",
                  model_spec3 = "-",
                  n_clusters = n_classes,
                  cluster_names = sapply(res$n_classes,FUN=function(x)paste(paste("P",1:x,sep=""),collapse = "")),
                  label_group1 = combination_of_class_probabilities,
                  validator = validation_group,
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
      write.csv(
        res,
        paste(
          output_path_prefix,
          "valid_results.csv",
          sep = ""
        )
      )
      res
    }
  }else{

    global_parameters_valid$cluster_names <<- info_genclust[['cluster_names']]
    print("start input_dt")
    input_dt <- inputDataPrepare(data_path = info_genclust[['data_path']],
                                 x_names = info_genclust[['variable_names']],
                                 naString = info_genclust[['naString']])
    print("end input_dt")

    if(length(info_genclust[['cluster_names']]) == 1){
      clust_multi <- sjmisc::to_dummy(input_dt[,info_genclust[['cluster_names']]])
      names(clust_multi) <- paste("P", 1:ncol(clust_multi), sep="")
      input_dt[,names(clust_multi)] <- clust_multi
      info_genclust[['cluster_names']] <- names(clust_multi)
    }

    class_range <- length(info_genclust[['cluster_names']])
    if(!sjmisc::is_empty(reference)) reference <- paste("P", which(info_genclust[['cluster_names']] %in% reference), sep = "")
    if(!sjmisc::is_empty(comparison)) comparison <- paste("P", which(info_genclust[['cluster_names']] %in% comparison), sep = "")

    if(customized){
      if(sjmisc::is_empty(comparison)){
        all_clusters <- paste("P",1:class_range, sep="")
        comparison <- all_clusters[!all_clusters %in% reference]
      }else{
        comparison <- paste(comparison, collapse=",")
      }
      reference <- paste(reference, collapse=",")
      label_category1 <- c(reference,comparison)
    }

    global_parameters_valid$output_path_prefix <<- info_genclust[['output_path_prefix']]
    global_parameters_valid$data_path <<- info_genclust[['data_path']]
    global_parameters_valid$variable_names <<- info_genclust[['variable_names']]
    global_parameters_valid$naString <<- info_genclust[['naString']]

    output_path_prefix <- info_genclust[['output_path_prefix']]


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
    input_dt <- input_dt[stats::complete.cases(input_dt[,info_genclust[['cluster_names']]]),]
    print("start to run syncF")
    if(!all(apply(input_dt[,info_genclust$cluster_names],2,FUN = function(x){all(x %in% c(0,1))}))){
      if(if_continuous){
        res <- validAllModel_Cont(cluster_names = info_genclust[['cluster_names']],
                             K_fold,
                             repeated_folds_R = repeated_CV,
                             input_dt,
                             x_names = info_genclust[['variable_names']],
                             validators,
                             seed_num,
                             output_path_prefix,
                             validation_data_fraction = 1,
                             if_listwise_deletion,
                             y_names,
                             lr_maxiter,
                             kappa_results_threshold_final_metrics = kappa_results_threshold_final_metrics,
                             combined_posterior_prob_threshold,
                             optimize_prob_thresh = 0.5,
                             pcd_dropping_pct,
                             if_CV,
                             label_category1 = label_category1,
                             kappa_filter_threshold = kappa_filter_threshold,
                             kappa_results_threshold = kappa_results_threshold,
                             customized = customized)
        write.csv(
          res,
          paste(
            output_path_prefix,
            "valid_results.csv",
            sep = ""
          )
        )
        return(res)
      }
      res <- validAllModel(cluster_names = info_genclust[['cluster_names']],
                           K_fold,
                           repeated_folds_R = repeated_CV,
                           input_dt,
                           x_names = info_genclust[['variable_names']],
                           validators,
                           seed_num,
                           output_path_prefix,
                           validation_data_fraction = 1,
                           if_listwise_deletion,
                           y_names,
                           lr_maxiter,
                           kappa_results_threshold_final_metrics = kappa_results_threshold_final_metrics,
                           combined_posterior_prob_threshold,
                           optimize_prob_thresh = 0.5,
                           pcd_dropping_pct,
                           if_CV,
                           label_category1 = label_category1,
                           kappa_filter_threshold = kappa_filter_threshold,
                           kappa_results_threshold = kappa_results_threshold,
                           customized = customized)

    }else{
      if(if_continuous){
        res <- validAllModel_Cat_Cont(cluster_names = info_genclust[['cluster_names']],
                                 K_fold,
                                 repeated_folds_R = repeated_CV,
                                 input_dt,
                                 x_names = info_genclust[['variable_names']],
                                 validators,
                                 seed_num,
                                 output_path_prefix,
                                 validation_data_fraction = 1,
                                 if_listwise_deletion,
                                 y_names,
                                 lr_maxiter,
                                 kappa_results_threshold_final_metrics = kappa_results_threshold_final_metrics,
                                 optimize_prob_thresh = 0.5,
                                 pcd_dropping_pct,
                                 if_CV,
                                 label_category1 = label_category1,
                                 kappa_filter_threshold = kappa_filter_threshold,
                                 kappa_results_threshold = kappa_results_threshold)
        write.csv(
          res,
          paste(
            output_path_prefix,
            "valid_results.csv",
            sep = ""
          )
        )
        return(res)
      }
      res <- validAllModel_Cat(cluster_names = info_genclust[['cluster_names']],
                               K_fold,
                               repeated_folds_R = repeated_CV,
                               input_dt,
                               x_names = info_genclust[['variable_names']],
                               validators,
                               seed_num,
                               output_path_prefix,
                               validation_data_fraction = 1,
                               if_listwise_deletion,
                               y_names,
                               lr_maxiter,
                               kappa_results_threshold_final_metrics = kappa_results_threshold_final_metrics,
                               optimize_prob_thresh = 0.5,
                               pcd_dropping_pct,
                               if_CV,
                               label_category1 = label_category1,
                               kappa_filter_threshold = kappa_filter_threshold,
                               kappa_results_threshold = kappa_results_threshold)
    }
    write.csv(
      res,
      paste(
        output_path_prefix,
        "valid_results.csv",
        sep = ""
      )
    )
    res

  }

}

