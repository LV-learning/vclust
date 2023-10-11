get_choose_m_from_whichSplit <- function(x){
  split_res <- strsplit(x,"No")
  choose_m <- split_res[[1]][1]
  num_and_comb <- split_res[[1]][2]
  choose_m <- strsplit(choose_m,"C")[[1]][2]
  return(as.numeric(choose_m))
}


get_num_from_whichSplit <- function(x){
  split_res <- strsplit(x,"No")
  num_and_comb <- split_res[[1]][2]
  split_res <- strsplit(num_and_comb,"comb")
  return(split_res[[1]][1])

}

get_comb_from_whichSplit <- function(x){
  split_res <- strsplit(x,"No")
  num_and_comb <- split_res[[1]][2]
  split_res <- strsplit(num_and_comb,"comb")
  return(split_res[[1]][2])

}


getAICBICAModel <- function(cur_folder_path){
  splitted_path <- unlist(strsplit(cur_folder_path,'/'))
  out_file_name <- paste(splitted_path[length(splitted_path)],'.out',sep = "")
  out_file_path <- paste(cur_folder_path,out_file_name,sep = "")

  outFile <- file(out_file_path,"r")
  outFile_read <- readLines(outFile,n=-1)
  close(outFile)

  aic_line <- outFile_read[grepl("AIC",outFile_read)]
  aic <- unlist(strsplit(trimws(aic_line)," "))
  aic <- aic[length(aic)]
  bic_line <- outFile_read[grepl("BIC",outFile_read)][1]
  bic <- unlist(strsplit(trimws(bic_line)," "))
  bic <- bic[length(bic)]
  bic_adjusted_line <- outFile_read[grepl("Sample-Size Adjusted BIC",outFile_read)]
  bic_adjusted <- unlist(strsplit(trimws(bic_adjusted_line)," "))
  bic_adjusted <- bic_adjusted[length(bic_adjusted)]

  res <- c(aic,bic,bic_adjusted)
  return(res)
}

getAICBICforAll <- function(folder_path,
                            n_range,
                            output_path_prefix){
  folder_path <- substr(folder_path,1,nchar(folder_path)-2)
  print(folder_path)
  final_aicbic_results <- data.frame()
  for(cur_class in n_range){
    aicbic <- c(NA,NA,NA,cur_class)
    base::suppressWarnings(try({
      cur_folder_path <- paste(folder_path,
                               as.character(cur_class),
                               '/',sep = "")
      print(cur_folder_path)
      ###Here should be a function
      aicbic <- getAICBICAModel(cur_folder_path = cur_folder_path)
      ###return aic bic as a vector

      aicbic <- c(aicbic,cur_class)
    },
    silent = TRUE))
    final_aicbic_results <- rbind(final_aicbic_results,aicbic)
  }
  names(final_aicbic_results) <- c("Akaike (AIC)","Bayesian (BIC)","Sample-Size Adjusted BIC","n_classes")

  #write.csv(final_aicbic_results,paste(output_path_prefix,"aicbic_results.csv",sep = ""))
  return(final_aicbic_results)
}


replaceExclusiveClass <- function(dt){
  finished_list <- dt$exclusive_class_pointer
  for(i in 1:length(dt$exclusive_class)){
    if(finished_list[i]==TRUE){next}
    finished_list[i] <- TRUE
    cur_exclusive <- dt$exclusive_class[i]
    cur_comb <- dt$combination_of_class_probabilities[i]
    loc_exclusive <- match(cur_comb,dt$exclusive_class)
    finished_list[loc_exclusive] <- TRUE
    dt$exclusive_class[loc_exclusive] <- cur_exclusive
  }
  return(dt)
}



##get aic bic and model name for Mclust
getAICBICforAllMclust <- function(folder_path,
                                  n_range,
                                  output_path_prefix){

  folder_path <- substr(folder_path,1,nchar(folder_path)-2)

  final_aicbic_results <- data.frame()
  for(cur_class in n_range){
    aicbic <- c(NA,NA,NA,cur_class)
    #base::suppressWarnings()
    base::suppressWarnings(try({
      cur_folder_path <- paste(folder_path,
                               as.character(cur_class),
                               '/',sep = "")
      ###Here should be a function
      aicbic <- utils::read.csv(paste(cur_folder_path,'aicbic.csv',sep=''))
      unlink(paste(cur_folder_path,'aicbic.csv',sep=''))
      aicbic <- c(aicbic$aic,aicbic$bic,aicbic$modelName)
      ###return aic bic as a vector
      aicbic <- c(aicbic,cur_class)
    },
    silent = TRUE))
    final_aicbic_results <- rbind(final_aicbic_results,aicbic)
  }
  names(final_aicbic_results) <- c("Akaike (AIC)","Bayesian (BIC)","modelName","n_classes")

  #write.csv(final_aicbic_results,paste(output_path_prefix,"aicbic_results.csv",sep = ""))
  return(final_aicbic_results)
}

##get aic bic and model name for Mclust
getAICBICforAllKmeans <- function(folder_path,
                                  n_range,
                                  output_path_prefix){

  folder_path <- substr(folder_path,1,nchar(folder_path)-2)

  final_aicbic_results <- data.frame()
  for(cur_class in n_range){
    aicbic <- c(NA,NA,NA,cur_class)
    base::suppressWarnings(try({
      cur_folder_path <- paste(folder_path,
                               as.character(cur_class),
                               '/',sep = "")
      ###Here should be a function
      aicbic <- utils::read.csv(paste(cur_folder_path,'model_metrics.csv',sep=''))
      unlink(paste(cur_folder_path,'model_metrics.csv',sep=''))
      aicbic <- c(aicbic$withinss,aicbic$betweenss,aicbic$mean_silhouette)
      ###return aic bic as a vector
      aicbic <- c(aicbic,cur_class)
    },
    silent=TRUE))
    final_aicbic_results <- rbind(final_aicbic_results,aicbic)
  }
  names(final_aicbic_results) <- c("withinss","betweenss","mean_silhouette","n_classes")

  #write.csv(final_aicbic_results,paste(output_path_prefix,"aicbic_results.csv",sep = ""))
  return(final_aicbic_results)
}


###combinations of splitting clusters into binary
allCombOfAModelFromCategory <- function(pp_dt,n){
  class_cols_dt <- as.data.frame(pp_dt[,ncol(pp_dt)])
  class_cols_dt <- sjmisc::to_dummy(class_cols_dt)
  final_list <- list()
  for(i in 1:(n-1)){
    tmp_list <- chooseMClassFromN(class_cols_dt=class_cols_dt,m=i,n=n)
    final_list[[i]] <- tmp_list
  }
  return(final_list)
}

dichProbAllCombOfAModelFromCategory <- function(all_comb_dt_aModel){
  final_res <- data.frame()
  for(m_dt in all_comb_dt_aModel){
    res <- m_dt
    res <- stats::reshape(res,
                   direction = "long",
                   varying = names(res),
                   times = names(res),
                   v.names = "risk_level",
                   timevar = "whichSplit",
                   ids = row.names(res),
                   idvar = "original_id")
    final_res <- rbind(final_res,res)
  }
  return(final_res)
}




chooseMClassFromNOpt <- function(class_cols_dt,m,n){
  names(class_cols_dt) <- sapply(1:ncol(class_cols_dt),
                                 FUN=function(x){paste("P",x,sep = "")})
  combinations <- utils::combn(seq(1,n,1),m)
  #final_out <- data.frame()

  final_out<-apply(combinations,2,FUN = function(x){
    temp <- rowSums(class_cols_dt[,x,drop=FALSE])
    temp <- as.data.frame(temp)
    comb_names1 <- names(class_cols_dt[,x,drop=FALSE])
    comb_names1 <- paste(comb_names1,collapse = "")
    comb_names2 <- c(paste("C",
                           as.character(m),
                           "No",
                           9,
                           "comb",
                           comb_names1,
                           sep=''))
    names(temp) <- comb_names2
    return(temp)
  })
  return(as.data.frame(final_out))
}


allCombOfAModelOpt <- function(pp_dt,n){
  class_cols_dt <- as.data.frame(pp_dt[,(ncol(pp_dt)-n):(ncol(pp_dt)-1)])
  final_list <- list()
  for(i in 1:(n-1)){
    tmp_list <- chooseMClassFromNOpt(class_cols_dt=class_cols_dt,m=i,n=n)
    final_list[[i]] <- tmp_list
  }
  return(final_list)
}

allCombOfAModelFromCategoryOpt <- function(pp_dt,n,label_category1=NULL){
  class_cols_dt <- as.data.frame(pp_dt[,ncol(pp_dt)])
  class_cols_dt <- sjmisc::to_dummy(class_cols_dt)
  final_list <- list()
  j <- 1
  for(i in 1:(n-1)){
    tmp_list <- chooseMClassFromNOpt(class_cols_dt=class_cols_dt,m=i,n=n)
    if(!is.null(label_category1)){
      if(label_category1 %in% names(tmp_list)){
        tmp_list <- tmp_list[,label_category1,drop=FALSE]
        final_list[[j]] <- tmp_list
        j <- j + 1
      }
    }else{
      final_list[[i]] <- tmp_list
    }
  }
  return(final_list)
}


dichProbAllCombOfAModelFromCategoryOpt <- function(all_comb_dt_aModel,use_combinations){
  final_res <- data.frame()
  for(m_dt in all_comb_dt_aModel){
    m_dt <- m_dt[,names(m_dt) %in% use_combinations,drop=FALSE]
    if(ncol(m_dt) != 0){
      res <- m_dt
      res <- stats::reshape(res,
                     direction = "long",
                     varying = names(res),
                     times = names(res),
                     v.names = "risk_level",
                     timevar = "whichSplit",
                     ids = row.names(res),
                     idvar = "original_id")
      final_res <- rbind(final_res,res)
    }
  }
  return(final_res)
}

dichProbAllCombOfAModelOpt <- function(all_comb_dt_aModel,use_combinations){
  final_res <- data.frame()
  for(m_dt in all_comb_dt_aModel){
    m_dt <- m_dt[,names(m_dt) %in% use_combinations,drop=FALSE]
    if(ncol(m_dt) != 0){
      res <- as.data.frame(dichProbM(m_dt))
      res <- stats::reshape(res,
                     direction = "long",
                     varying = names(res),
                     times = names(res),
                     v.names = "risk_level",
                     timevar = "whichSplit",
                     ids = row.names(res),
                     idvar = "original_id")
      final_res <- rbind(final_res,res)
    }
  }
  return(final_res)
}

#/Users/zetanli/Desktop/stanfordwork/mplus_projects/mplus_automation/mplus_input_range_class_test/icP2/icP2.pp
#/Users/zetanli/Desktop/stanfordwork/mplus_projects/mplus_automation/mplus_input_range_class_test/icP3/icP3.pp

read1Line <- function(aline){
  after_split <- strsplit(aline,split=" ")[[1]]
  after_split <- after_split[after_split != ""]
  #after_split <- as.character(after_split,stringAsFactor = FALSE)
  return(after_split)
}

readAllLines <- function(pp_file_read){
  pp_file_read <- lapply(pp_file_read,read1Line)
  pp_file_read_length <- length(pp_file_read)
  pp_file_read <- unlist(pp_file_read)
  pp_file_read[pp_file_read == "*"] <- NA
  pp_file_read <- sapply(pp_file_read,as.numeric)
  pp_file_matrix <- matrix(unlist(pp_file_read), nrow = pp_file_read_length, byrow = TRUE)
  return(pp_file_matrix)
}

read_pp <- function(data_path){
  pp_file <- file(data_path,"r")
  f_read <- readLines(pp_file)
  close(pp_file)

  matrix_pp <- readAllLines(f_read)
  return(matrix_pp)
}

#a <- read_pp("/Users/zetanli/Desktop/stanfordwork/mplus_projects/myproject/mplus_input/cP2/cP2.pp")

# chooseMClassFromN <- function(class_cols_dt,m,n){
#   combinations <- combn(class_cols_dt,m,simplify = FALSE)
#   final_out <- data.frame()
#   for(i in 1:length(combinations)){
#     temp <- rowSums(combinations[[i]])
#     temp <- as.data.frame(temp)
#     names(temp) <- c(paste("J",as.character(m),"split",as.character(i),sep=''))
#     if(i==1){
#       final_out = temp
#     }else{
#       final_out <- cbind(final_out,temp)}
#   }
#   return(final_out)
# }

chooseMClassFromN <- function(class_cols_dt,m,n){
  names(class_cols_dt) <- sapply(1:ncol(class_cols_dt),
                                 FUN=function(x){paste("P",x,sep = "")})
  combinations <- utils::combn(class_cols_dt,m,simplify = FALSE)
  final_out <- data.frame()
  for(i in 1:length(combinations)){
    temp <- rowSums(combinations[[i]])
    temp <- as.data.frame(temp)
    comb_names1 <- names(combinations[[i]])
    comb_names1 <- paste(comb_names1,collapse = "")
    comb_names2 <- c(paste("C",
                           as.character(m),
                           "No",
                           as.character(i),
                           "comb",
                           comb_names1,
                           sep=''))
    names(temp) <- comb_names2
    if(i==1){
      final_out = temp
    }else{
      final_out <- cbind(final_out,temp)}
  }
  return(final_out)
}



allCombOfAModel <- function(pp_dt,n){
  class_cols_dt <- as.data.frame(pp_dt[,(ncol(pp_dt)-n):(ncol(pp_dt)-1)])
  final_list <- list()
  for(i in 1:(n-1)){
    tmp_list <- chooseMClassFromN(class_cols_dt=class_cols_dt,m=i,n=n)
    final_list[[i]] <- tmp_list
  }
  return(final_list)
}

#no pseudoclass
# t1 = allCombOfAModel(a,6)
# t1[[1]]


##MN_dt: a data frame with all splits of a k when n choose k for a model with n.
dichProbM <- function(MN_dt){
  final_out <- apply(MN_dt,c(1,2),FUN=function(x){ifelse(x>=0.5,1,0)})
  return(final_out)
}

#dichProbM(t1[[1]])

# tt1 <- list()
# j <- 0
# for(i in t1){
#   j <- j + 1
#   tt1[[j]] <- dichProbM(i)
# }



dichProbAllCombOfAModel <- function(all_comb_dt_aModel){
  final_res <- data.frame()
  for(m_dt in all_comb_dt_aModel){
    res <- as.data.frame(dichProbM(m_dt))
    res <- stats::reshape(res,
                   direction = "long",
                   varying = names(res),
                   times = names(res),
                   v.names = "risk_level",
                   timevar = "whichSplit",
                   ids = row.names(res),
                   idvar = "original_id")
    final_res <- rbind(final_res,res)
  }
  return(final_res)
}

# tttt1 <- as.data.frame(tttt[[1]])
# reshape(tttt1,
#         direction = "long",
#         varying = names(tttt1),
#         times = names(tttt1),
#         v.names = "risk_level",
#         timevar = "whichSplit",
#         ids = row.names(tttt1),
#         idvar = "original_id")
#with pseudoclass
pseudoVec <- function(probVec,r_pseudo,seed_num){

  row_nums <- length(probVec)
  pseudo_entries <- matrix(-99,row_nums,r_pseudo)
  set.seed(seed_num)
  for ( j in 1:r_pseudo){
    pseudo_entries[,j]=stats::rbinom(row_nums,1,probVec)}
  pseudo_entries <- as.data.frame(pseudo_entries)
  names(pseudo_entries) <- sapply(1:r_pseudo,FUN=function(x){paste("pseudo",x,sep="")})
  return(pseudo_entries)
}



###Not currently in use

##MN_dt: data frame that choose m from n
dichProbMPseudo <- function(MN_dt,r_pseudo,seed_num){
  res_list <- apply(MN_dt,2,pseudoVec,r_pseudo,seed_num)
  names_list <- names(res_list)
  len_list <- length(res_list)
  for(i in 1:len_list){
    names(res_list[[names_list[i]]]) <- sapply(names(res_list[[names_list[i]]]),
                                               FUN=function(x){paste(names_list[i],"_",x,sep="")})
  }
  ##list to data frame.
  res_dt <- res_list[[names_list[1]]]
  for(i in 2:len_list){
    res_dt <- cbind(res_dt,res_list[[names_list[i]]])
  }
  return(res_dt)
}


#To Do: Change prob = 1.01 to 1.00


##all_dt: results from allCombOfAModel
dichProbAllPseudoAModel <- function(all_dt,r_pseudo,seed_num){
  res <- sapply(all_dt,FUN=function(x){dichProbMPseudo(x,r_pseudo,seed_num)})
  ##list to data frame
  return(res)
}

#dichProbMPseudo(allCombOfAModel(a,6)[[1]],3,432123)
#dichProbAllPseudoAModel(allCombOfAModel(a,6),r_pseudo=3,seed_num=123456)
#aaa <- sapply(t1,FUN=function(x){dichProbMPseudo(x,20)})

getROC <- function(threshold,real_y,predicted_prob){
  predicted_y <- ifelse(predicted_prob >= threshold, 1, 0)
  TP <- real_y == 1 & predicted_y == 1
  P <- real_y == 1
  FP <- real_y == 0 & predicted_y == 1
  N <- real_y == 0
  TPR <- sum(TP)/sum(P)
  FPR <- sum(FP)/sum(N)
  res <- data.frame(threshold = threshold,TPR,FPR)
  res
}

getROCAll <- function(real_y,predicted_prob){
  res <- data.frame()
  thresholds <- seq(0,1,0.1)
  for(t in thresholds){
    tmp <- getROC(threshold = t,
                 real_y = real_y,
                 predicted_prob = predicted_prob)
    res <- rbind(res,tmp)
  }
  res
}
# y = c(1,0,1,0,1,1,1,1,0,0,1)
# proba = c(0.3,0.6,0.7,0.3,0.4,0.9,0.6,0.2,0.4,0.8,0.9)
