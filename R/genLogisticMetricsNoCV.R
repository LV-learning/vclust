#' @importFrom stats binomial
genLogisticMetricsNoCV <- function(y,
                                   ## should be the right part of the formula
                                   ## and the format should be string
                                   predictor_part,
                                   input_and_pp_and_var_df,
                                   y_test,
                                   input_and_pp_and_var_df_test,
                                   lr_maxiter){
  dt_y <- cbind(input_and_pp_and_var_df,y)
  dt_y_test <- cbind(input_and_pp_and_var_df_test,'y' = y_test)
  #print("dims of dt_y and dt_y_test in genLogisticMetricsNoCV are:")
  #print(dim(dt_y))
  #print(dim(dt_y_test))
  logistic_formula <- stats::as.formula(paste("y ~ ", predictor_part, sep = ""))
  ##logistics regression based on training dataset
  pCL <- stats::glm(logistic_formula,data=dt_y,family=binomial,control = list(maxit=lr_maxiter))
  ##prediction on hold out validation set
  pCLpr <- stats::predict(pCL,dt_y_test,type="response")
  roc_res <- getROCAll(y_test,pCLpr)
  ##if high risk for predicted results
  pCLpos <- ifelse(pCLpr >=0.5, 1, 0)
  dt_y_test$predicted_y <- pCLpos
  #print(logistic_formula)
  #print(pCLpr)
  ##if low risk for predicted results
  pCLneg <- ifelse(pCLpr < 0.5, 1, 0)
  ##if high risk for pcd
  gc6pos <- ifelse(y_test==1, 1, 0)
  ##if low risk for pcd
  gc6neg <- ifelse(y_test==0, 1, 0)
  #print("dims of pCLpos, pCLneg,gc6pos,gc6neg and are:")
  #print(length(pCLpos))
  #print(length(pCLneg))
  #print(length(gc6pos))
  #print(length(gc6neg))


  pCLTN=ifelse(pCLneg==1&gc6neg==1, 1, 0)
  pCLTP=ifelse(pCLpos==1&gc6pos==1, 1, 0)
  pCLNP=ifelse(pCLneg==1&gc6neg==0, 1, 0)
  pCLPN=ifelse(pCLpos==1&gc6pos==0, 1, 0)

  NN=sum(pCLTN)/sum(gc6pos+gc6neg)
  PP=sum(pCLTP)/sum(gc6pos+gc6neg)
  NP=sum(pCLNP)/sum(gc6pos+gc6neg)
  PN=sum(pCLPN)/sum(gc6pos+gc6neg)

  ##compute metrics
  accgmc12=sum(pCLTP+pCLTN)/sum(gc6pos+gc6neg)
  aucmc12=(sum(pCLTP)/sum(gc6pos)
           -sum(pCLPN)/sum(gc6neg)+1)/2
  sensgmc12=sum(pCLTP)/sum(gc6pos)
  spcgmc12=sum(pCLTN)/sum(gc6neg)
  kappamc12=(sum(pCLTP+pCLTN)/sum(gc6pos+gc6neg)
             - (sum(gc6pos)/sum(gc6pos+gc6neg)*sum(pCLpos)/sum(pCLpos+pCLneg)+
                  (1-sum(gc6pos)/sum(gc6pos+gc6neg))*(1-sum(pCLpos)/sum(pCLpos+pCLneg))))/
    (1- (sum(gc6pos)/sum(gc6pos+gc6neg)*sum(pCLpos)/sum(pCLpos+pCLneg)+
           (1-sum(gc6pos)/sum(gc6pos+gc6neg))*(1-sum(pCLpos)/sum(pCLpos+pCLneg))))
  if(is.nan(accgmc12)){
    accgmc12 = NA
  }
  if(is.nan(aucmc12)){
    aucmc12 = NA
  }
  if(is.nan(sensgmc12)){
    sensgmc12 = NA
  }
  if(is.nan(spcgmc12)){
    spcgmc12 = NA
  }
  if(is.nan(kappamc12)){
    kappamc12 = NA
  }
  return(list(c(accgmc12,aucmc12,sensgmc12,spcgmc12,kappamc12),roc_res,dt_y_test[,c('original_id','y','predicted_y')]))
}
