#' Conducts unsupervised clustering using existing clustering methods.
#'
#' @param model_type A string indicates a clustering method. Currently available
#' options include GMM (growth mixture modeling), MBC (model-based clustering), and Kmeans.
#' An additional option is Ogroups, where the user generates observed subgroups
#' without conducting clustering.
#'
#' For GMM, commercial software \href{http://statmodel.com/}{Mplus} is used (Muthén and Muthén, 1998-2017).
#'
#' For MBC, R package \href{https://cran.r-project.org/web/packages/mclust/index.html}{mclust} is used (Scrucca, Fop, Murphy, and Raftery, 2016).
#'
#' For K-means, R function kmeans is used.
#'
#' For example, \code{model_type="MBC"}, \code{model_type="GMM"}, \code{model_type="Kmeans"}, or \code{model_type="Ogroups"}.
#' @param class_range An integer vector specifies the desired number of clusters.
#' For example, \code{class_range = 2:4} means clustering with 2, 3, and 4 clusters.
#' @param min_units An integer indicates the minimum number of units in each cluster.
#' If the number is less than the minimum, unsupervised clustering will stop.
#' For example, when the unit of analysis is a person and \code{min_units=10},
#' clustering will stop if the smallest cluster has less than 10 people.
#' @param data_path A string indicates the path of the input data.
#' The data should be in csv format.
#' For example, "/Users/username/Desktop/inputdata.csv" for Mac user
#' or "D:/folder/inputdata.csv" for Windows user.
#' @param variable_names A text string indicates names of variables from data_path,
#' where names are separated by white spaces, or commas. For example,
#' when input data has 9 columns, a1, a2, a3, a4, b1, b2, b3, cov1, and cov2,
#' \code{variable_names = "a1, a2, a3, a4, b1, b2, b3, cov1, cov2"}.
#' These variable names will overwrite the original names when the data
#' file already has variables names (i.e., header). The user can choose
#' to use those original names by specifying \code{variable_names = NULL}.
#' @param y_names A string vector specifies the variable names used as
#' multivariate outcomes in unsupervised clustering.
#' When these are repeated measures used with GMM, they
#' should be chronologically ordered. For example,
#' \code{y_names = c(a1, a2, a3, a4)}.
#' When \code{model_type = Ogroups}, specified cupoints are directly applied to
#' the variables listed under y_names.
#' @param output_path_prefix A string indicates the output folder path of model results.
#' The path should be absolute path (full path) when using Windows operation system.
#' Remember to use "/" instead of "\" for the path.
#' @param useobs A text string indicates observations to use.
#' This one is the same as USEOBS in Mplus.
#' This one is a filter to screen out observations (rows for most cases).
#' For example, if we want to exclude observations with id=9 and id=13,
#' we can set \code{useobs = "(id ne 9) and (id ne 13)"}.
#' @param listwise_deletion_variables The user can specify listwise deletion based on
#' specific variables listed in variable_names.
#' For example, \code{listwise_deletion_variables = c("a1", "b1")}.
#' The user is also allowed to use listwise deletion with variables that are not
#' being used in the genclust procedure. The use of useobs and listwise_deletion_variables
#' is particularly important when model_type=Ogroups because it affects interpretation of subgroups.
#' @param clustering_data_fraction A single value indicates the fraction of the
#' samples to be used in unsupervised clustering. The value range is (0, 1] and the default is 1.
#' @param seed_num An integer vector indicates seed numbers for clustering and
#' imputing missing data, which may affect the results depending on the clustering method.
#' The vector should follow the below format.
#' \code{Seed_num = c(seed_num_clustering = 4561234,}
#'              \code{seed_num_impute_missing = 4561234)}
#' @param kmeans_gap_stats_B An integer indicates the number of bootstrap samples (B) used to calculate gap statistics.
#' @param kmeans_iter An integer indicates the number of iterations used in Kmeans clustering.
#' @param MBCtype A string indicates the desired type of MBC model.
#' One of the 14 types of constraints on the covariance matrix can be specified in line with
#' mclust (EEE, EEI, EEV, EII, EVE, EVI, EVV, VEE, VEI, VEV, VII, VVE, VVI, VVV).
#' @param Ogroups_cutpoint A numeric value/vector specifies a threshold/thresholds
#' to form observed subgroups without conducting clustering.
#' @param Ogroups_cutpoint_sign A character value/vector specifies a/multiple
#' comparison operator(s). Available options include >=, <=, >, <, ==, GE, LE, GT, LT, EQ.
#' When Ogroups_cutpoint is a vector with multiple cutpoints, the Ogroups_cutpoint_sign will be applied to each cutpoint.
#' @param Ogroups_cutpoint_max_min_mean A character specifies what aggregation
#' function is used to construct subgroups. Available options are max, min, and mean.
#'
#' When \code{model_type = Ogroups} and Ogroups_cutpoint is a single value,
#' above three arguments are used to define subgroups.
#' For example, if \code{y_names = c('a', 'b', 'c'),  Ogroup_cutpoint = 12, Ogroups_cutpoint_sign=">=", and cutpoint_max_min_mean="max"},
#' all cases with \deqn{max(a, b, c) >= 12} will be assigned the value of 1, and the rest the value of 0.
#'
#' When model_type = Ogroups and Ogroups_cutpoint is a vector with multiple thresholds,
#' Ogroups_cutpoint_max_min_mean will be ignored.
#' For example, if \code{y_names = c('a', 'b', 'c'),  Ogroup_cutpoint = c(12, 13, 14),
#' and Ogroups_cutpoint_sign = c('>=', '<', '>')},
#' all cases with \deqn{a>=12 and b<13 and c>14} will be assigned the value of 1,
#' and the rest the value of 0.
#' Formation of observed groups using more complex manipulations should be
#' conducted externally before using this program.
#' @param GMM_time_scores An integer vector specifies time measures at each time point when GMM is used.
#' This one should have the same length as y_names.
#' For example, \code{y_names = c(a1, a2, a3)} and \code{time_scores = c(0, 1, 2)}
#' may mean that a1 is measured at baseline, a2 at 1 year, and a3 at 2 years from the baseline.
#' @param GMM_covariates A string contains covariates used in clustering.
#' Currently, this option applies only to GMM.
#' For example, if \code{covariates="cov1 cov2 cov3"},
#' GMM runs with and without using these covariates as predictors of
#' growth parameters (intercept and slope) and the cluster membership.
#' If \code{covariates = NA}, GMM runs without covariates.
#' @param GMM_random_intercept A Boolean variable indicates whether GMM is
#' conducted allowing for a random intercept. If \code{GMM_random_intercept = TRUE},
#' GMM is conducted with allowing for a random intercept.
#' If \code{GMM_random_intercept = FALSE}, GMM is conducted without allowing for a random intercept.
#' @param GMM_trend For modeling of longitudinal trends, we use polynomial growth.
#' Our program can support linear, quadratic, and cubic growth.
#' For example, \code{GMM_trend="linear"}. The current version of the program uses quadratic growth as a default.
#' @param GMM_initial_starts An integer indicates the number of initial stage starting
#' values in maximum likelihood optimization of GMM.
#' @param GMM_final_optimizations An integer indicates the number of final stage
#' optimizations in maximum likelihood optimization of GMM.
#'
#' @references Jo, B., Hastie, T. J., Li, Z., Youngstrom, E. A., Findling, R. L., & Horwitz, S. M. (2023). Reorienting Latent Variable Modeling for Supervised Learning. Multivariate Behavioral Research, 1-15.
#' @return
#' Clustering results are saved in the folder specified in output_path_prefix.
#' The summary will be provided as a csv file (genclust_results.csv).
#'
#' @export
#'
genclust <- function(model_type,
                     class_range,
                     min_units = 10,
                    data_path,
                    variable_names,
                    y_names,
                    output_path_prefix = 'output/',
                    useobs,
                    listwise_deletion_variables,
                    clustering_data_fraction = 1,
                    seed_num = c(seed_num_unsupervised_model = 4561234,
                                 seed_num_impute_missing = 4561234),
                    kmeans_gap_stats_B = 50,
                    kmeans_iter = 25,
                    MBCtype,
                    Ogroups_cutpoint,
                    Ogroups_cutpoint_sign,
                    Ogroups_cutpoint_max_min_mean,
                    GMM_time_scores,
                    GMM_covariates,
                    GMM_random_intercept,
                    GMM_trend = "quadratic",
                    GMM_initial_starts = 500,
                    GMM_final_optimizations = 50
                    ){
  assign("global_parameters", list(), envir = .GlobalEnv)
  global_parameters$listwise_deletion_variables <<- listwise_deletion_variables
  global_parameters$useobs <<- useobs
  global_parameters$variable_names <<- variable_names
  global_parameters$y_names <<- y_names
  global_parameters$model_type <<- model_type
  global_parameters$covariates <<- GMM_covariates
  global_parameters$MBCtype <<- MBCtype
  global_parameters$GMM_trend <<- GMM_trend
  global_parameters$GMM_random_intercept <<- GMM_random_intercept
  print("start")
  useObs <- useobs
  try(RNGkind(sample.kind = "Rounding"), silent = TRUE)
  if (dir.exists(output_path_prefix) == FALSE) {
    dir.create(output_path_prefix)
  }
  x_names <- variable_names
  output_path_prefix <-
    paste(output_path_prefix, gsub(" ", "_", model_type), "/", sep = "")

  if (dir.exists(output_path_prefix) == FALSE) {
    dir.create(output_path_prefix)
  }

  if (clustering_data_fraction <= 0) {
    stop("GMM data fraction is <= 0")
  }

  if (clustering_data_fraction > 1) {
    stop("GMM data fraction is > 1")
  }

  if (clustering_data_fraction != 1) {
    input_dt_frac <- read.csv(data_path, header = FALSE)
    input_dt_frac <-
      dplyr::sample_frac(input_dt_frac, clustering_data_fraction)
    #data_path <- substr(data_path,start = 1,stop = nchar(data_path)-4)
    #data_path <- paste(data_path,"_fraction.csv",sep="")
    data_path <-
      paste(output_path_prefix , "input_data_fraction.csv", sep = "")
    print("data_path is")
    print(data_path)
    write.table(
      input_dt_frac,
      data_path,
      sep = ",",
      col.names = FALSE,
      row.names = FALSE
    )
  }
  global_parameters$output_path_prefix <<- output_path_prefix
  global_parameters$data_path <<- data_path

  if (tolower(model_type) %in% c("gmm", "growth mixture model")) {
    print('valid frac is:')
    ##Create dir
    if(dir.exists(output_path_prefix ) == FALSE){
      dir.create(output_path_prefix)
    }

    if(sjmisc::is_empty(x_names)){
      x_names1 <- x_names
    }else{
      x_names1 <- gsub(","," ",x_names)
      x_names1 <- trimws(x_names1)
      x_names1 <- strsplit(x_names1," |\t|\n|\f|\v|\r")[[1]]
      x_names1 <- x_names1[x_names1!=""]
      #RNGkind(sample.kind = "Rounding")
      print("x_names1 is: ")
      print(x_names1)
    }
    input_dt <- inputDataPrepare(data_path = data_path,
                                 x_names = x_names1)
    input_dt <- input_dt[stats::complete.cases(input_dt[,c(listwise_deletion_variables)]),]
    input_dt[is.na(input_dt)] <- 9999
    input_dt <- subset(input_dt,select = -original_id)

    data_path <- paste(paste(strsplit(data_path,"/")[[1]][1:(length(strsplit(data_path,"/")[[1]])-1)],collapse = "/"),"/input_dt_ld.csv",sep = "")
    write.table(input_dt,
              file = data_path,
              sep = ",",
              row.names = FALSE,
              col.names = FALSE)
    global_parameters$data_path <<- data_path
    x_names <- paste(names(input_dt),collapse="\n")
    global_parameters$variable_names <<- x_names

    stop_at_n <- runAndCheckConv(class_range = class_range,
                                 is_random_i = GMM_random_intercept,
                                 is_covariates = !sjmisc::is_empty(GMM_covariates),
                                 covariates = GMM_covariates,
                                 trend = GMM_trend,
                                 y_names = y_names,
                                 time_scores = GMM_time_scores,
                                 initial_starts = GMM_initial_starts,
                                 final_optimizations = GMM_final_optimizations,
                                 output_path_prefix = output_path_prefix,
                                 data_path = data_path,
                                 threshold = min_units,
                                 x_names = x_names,
                                 useObs = useObs
    )
    global_parameters$folder_path <<- stop_at_n[[2]]
    global_parameters$class_range <<- class_range[1]:stop_at_n[[1]]
    aicbic_res <- getAICBICforAll(
      folder_path = stop_at_n[[2]],
      n_range = class_range[1]:stop_at_n[[1]],
      output_path_prefix = output_path_prefix
    )
    names(aicbic_res) <- c("AIC","BIC","Sample_Size_Adjusted_BIC","n_classes")
    aicbic_res <- aicbic_res%>%
      transmute(model_type = "GMM",
                model_spec1 = GMM_trend,
                model_spec2 = ifelse(GMM_random_intercept,"random intercept","-"),
                model_spec3 = ifelse(!sjmisc::is_empty(GMM_covariates),"covariates","-"),
                n_clusters = n_classes,
                cluster_names = sapply(aicbic_res$n_classes,FUN=function(x)paste(paste("P",1:x,sep=""),collapse = "")),
                AIC = AIC,
                BIC = BIC,
                Sample_Size_Adjusted_BIC = Sample_Size_Adjusted_BIC
                )
    write.csv(aicbic_res,
              paste(output_path_prefix,
                    "genclust_results.csv",
                    sep = "")
              )
    return(
      stop_at_n
    )
  } else if (tolower(model_type) %in% c("gaussian mixture model", "mclust","mbc")) {
    ##Create dir
    if(dir.exists(output_path_prefix ) == FALSE){
      dir.create(output_path_prefix)
    }
    if(sjmisc::is_empty(x_names)){
      x_names1 <- x_names
    }else{
      x_names1 <- gsub(","," ",x_names)
      x_names1 <- trimws(x_names1)
      x_names1 <- strsplit(x_names1," |\t|\n|\f|\v|\r")[[1]]
      x_names1 <- x_names1[x_names1!=""]
      #RNGkind(sample.kind = "Rounding")
      print("x_names1 is: ")
      print(x_names1)
    }
    input_dt <- inputDataPrepare(data_path = data_path,
                                 x_names = x_names1)
    #print(input_dt)
    # input_dt[input_dt == 9999] <- NA
    # input_dt[input_dt == "*"] <- NA
    # input_dt[input_dt == "NA"] <- NA
    # input_dt[input_dt == "<NA>"] <- NA
    #print(names(input_dt))
    ##check and delete if y_names are all NA
    print("y_names are: ")
    print(y_names)
    #print(!apply(is.na(input_dt[,y_names]),1,all))
    input_dt <- input_dt[!apply(is.na(input_dt[,y_names,drop=FALSE]),1,all),]
    ##impute NA for y_names
    ##check if has NA
    useObs <- useObsSplitter(useObs)
    if(is.null(useObs)==FALSE){
      text_useobs <- paste("input_dt <- dplyr::filter(input_dt,",useObs,")",sep = "")
      print("text_useobs is:")
      print(text_useobs)
      eval(parse(text = text_useobs))
      input_dt <- as.data.frame(input_dt)
    }
    input_dt_impute <- data.frame(input_dt)
    if(all(!is.na(input_dt_impute[,y_names])) == FALSE){
      input_dt_impute[,y_names] <- mclust::imputeData(input_dt_impute[,y_names],seed=seed_num['seed_num_impute_missing'])
    }
    input_dt_impute <- input_dt_impute[stats::complete.cases(input_dt_impute[,c(listwise_deletion_variables)]),]
    stop_at_n <- runAndCheckConvMclust(class_range = class_range,
                                       y_names = y_names,
                                       input_dt = input_dt_impute,
                                       output_path_prefix = output_path_prefix,
                                       threshold = min_units,
                                       modelNames = MBCtype
    )
    global_parameters$folder_path <<- stop_at_n[[2]]
    global_parameters$class_range <<- class_range[1]:stop_at_n[[1]]
    aicbic_res <- getAICBICforAllMclust(
      folder_path = stop_at_n[[2]],
      n_range = class_range[1]:stop_at_n[[1]],
      output_path_prefix = output_path_prefix
    )
    names(aicbic_res) <- c("AIC","BIC","modelName","n_classes")
    aicbic_res <- aicbic_res%>%
      transmute(model_type = "MBC",
                model_spec1 = modelName,
                model_spec2 = "-",
                model_spec3 = "-",
                n_clusters = n_classes,
                cluster_names = sapply(aicbic_res$n_classes,FUN=function(x)paste(paste("P",1:x,sep=""),collapse = "")),
                AIC = AIC,
                BIC = BIC
      )
    write.csv(aicbic_res,
              paste(output_path_prefix,
                    "genclust_results.csv",
                    sep = "")
    )
    return(stop_at_n)

  } else if (tolower(model_type) %in% c("k-means", "kmeans", "k means", "k_means")) {
    print("output_path_prefix is:")
    print(output_path_prefix)
    if(dir.exists(output_path_prefix ) == FALSE){
      dir.create(output_path_prefix)
    }
    if(sjmisc::is_empty(x_names)){
      x_names1 <- x_names
    }else{
      x_names1 <- gsub(","," ",x_names)
      x_names1 <- trimws(x_names1)
      x_names1 <- strsplit(x_names1," |\t|\n|\f|\v|\r")[[1]]
      x_names1 <- x_names1[x_names1!=""]
      #RNGkind(sample.kind = "Rounding")
      print("x_names1 is: ")
      print(x_names1)
    }
    input_dt <- inputDataPrepare(data_path = data_path,
                                 x_names = x_names1)
    #print(input_dt)
    # input_dt[input_dt == 9999] <- NA
    # input_dt[input_dt == "*"] <- NA
    # input_dt[input_dt == "NA"] <- NA
    # input_dt[input_dt == "<NA>"] <- NA
    #print(names(input_dt))
    ##check and delete if y_names are all NA
    print("y_names are: ")
    print(y_names)
    #print(!apply(is.na(input_dt[,y_names]),1,all))
    input_dt <- input_dt[!apply(is.na(input_dt[,y_names,drop=FALSE]),1,all),]

    useObs <- useObsSplitter(useObs)


    if(is.null(useObs)==FALSE){
      text_useobs <- paste("input_dt <- dplyr::filter(input_dt,",useObs,")",sep = "")
      print("text_useobs is:")
      print(text_useobs)
      eval(parse(text = text_useobs))
      input_dt <- as.data.frame(input_dt)
    }

    input_dt_impute <- data.frame(input_dt)
    if(all(!is.na(input_dt_impute[,y_names])) == FALSE){
      input_dt_impute[,y_names] <- mclust::imputeData(input_dt_impute[,y_names],seed=seed_num['seed_num_impute_missing'])
    }
    input_dt_impute <- input_dt_impute[stats::complete.cases(input_dt_impute[,c(listwise_deletion_variables)]),]
    set.seed(seed_num['seed_num_unsupervised_model'])
    stop_at_n <- runAndCheckConvKmeans(class_range = class_range,
                                       y_names = y_names,
                                       input_dt = input_dt_impute,
                                       output_path_prefix = output_path_prefix,
                                       threshold = min_units,
                                       kmeans_gap_stats_B = kmeans_gap_stats_B,
                                       kmeans_iter = kmeans_iter,
                                       seed_num = seed_num
    )
    global_parameters$folder_path <<- stop_at_n[[2]]
    global_parameters$class_range <<- class_range[1]:stop_at_n[[1]]
    aicbic_res <- getAICBICforAllKmeans(
      folder_path = stop_at_n[[2]],
      n_range = class_range[1]:stop_at_n[[1]],
      output_path_prefix = output_path_prefix
    )
    gap_stats <- read.csv(
      paste(output_path_prefix,
            "gap_stats.csv",
            sep = ""),
      header = TRUE,
      stringsAsFactors = FALSE
    )
    gap_stats <- gap_stats[, c("gap", "SE.sim", "n_classes")]
    names(gap_stats) <-
      c("gap_stats", "gap_stats_standard_error", "n_classes")
    aicbic_res <-
      merge(
        aicbic_res,
        gap_stats,
        by.x = "n_classes",
        by.y = "n_classes",
        all = FALSE,
        all.x = TRUE
      )
    aicbic_res <- aicbic_res%>%
      transmute(model_type = "Kmeans",
                model_spec1 = "-",
                model_spec2 = "-",
                model_spec3 = "-",
                n_clusters = n_classes,
                cluster_names = sapply(aicbic_res$n_classes,FUN=function(x)paste(paste("P",1:x,sep=""),collapse = "")),
                withinss = withinss,
                betweenss = betweenss,
                mean_silhouette = mean_silhouette,
                gap_stats = gap_stats,
                gap_stats_SE = gap_stats_standard_error,
                gap_stats_B = kmeans_gap_stats_B
      )
    write.csv(aicbic_res,
              paste(output_path_prefix,
                    "genclust_results.csv",
                    sep = "")
    )
    print(aicbic_res)
    return(stop_at_n)

  } else if (tolower(model_type) %in% c("O","o", "ogroups", "o groups", "ogroup", "o group")) {
    print("output_path_prefix is:")
    print(output_path_prefix)
    if(dir.exists(output_path_prefix ) == FALSE){
      dir.create(output_path_prefix)
    }
    if(sjmisc::is_empty(x_names)){
      x_names1 <- x_names
    }else{
      x_names1 <- gsub(","," ",x_names)
      x_names1 <- trimws(x_names1)
      x_names1 <- strsplit(x_names1," |\t|\n|\f|\v|\r")[[1]]
      x_names1 <- x_names1[x_names1!=""]
      #RNGkind(sample.kind = "Rounding")
      print("x_names1 is: ")
      print(x_names1)
    }
    input_dt <- inputDataPrepare(data_path = data_path,
                                 x_names = x_names1)
    #print(input_dt)
    # input_dt[input_dt == 9999] <- NA
    # input_dt[input_dt == "*"] <- NA
    # input_dt[input_dt == "NA"] <- NA
    # input_dt[input_dt == "<NA>"] <- NA
    #print(names(input_dt))
    ##check and delete if y_names are all NA
    print("y_names are: ")
    print(y_names)
    #print(!apply(is.na(input_dt[,y_names]),1,all))
    input_dt <- input_dt[!apply(is.na(input_dt[,y_names,drop=FALSE]),1,all),]
    useObs <- useObsSplitter(useObs)

    if(is.null(useObs)==FALSE){
      text_useobs <- paste("input_dt <- dplyr::filter(input_dt,",useObs,")",sep = "")
      print("text_useobs is:")
      print(text_useobs)
      eval(parse(text = text_useobs))
      input_dt <- as.data.frame(input_dt)
    }
    input_dt <- input_dt[stats::complete.cases(input_dt[,c(listwise_deletion_variables)]),]

    stop_at_n <- createBinaryO(
      y_names = y_names,
      input_dt = input_dt,
      output_path_prefix = output_path_prefix,
      O_threshold = Ogroups_cutpoint,
      O_sign = Ogroups_cutpoint_sign,
      O_max_min_mean = Ogroups_cutpoint_max_min_mean
    )
    global_parameters$folder_path <<- stop_at_n[[2]]
    global_parameters$class_range <<- class_range[1]:stop_at_n[[1]]
    return(stop_at_n)
  }
}
