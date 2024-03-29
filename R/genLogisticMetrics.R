#' @importFrom stats binomial
genLogisticMetrics <- function(cvFolds_dt,
                               ## a binary column of low/high risk
                               y,
                               ## should be the right part of the formula
                               ## and the format should be string
                               predictor_part,
                               ##current fold of K_folds
                               cur_fold,
                               cur_r,
                               input_and_pp_and_var_df,
                               lr_maxiter){
  #print("dims of y in genLogisticMetrics:")
  #print(length(y))
  vset <- cvFolds_dt$subsets[cvFolds_dt$which==cur_fold,cur_r]
  ## train set
  tset <- cvFolds_dt$subsets[cvFolds_dt$which!=cur_fold,cur_r]
  dt_y <- cbind(input_and_pp_and_var_df,y)
  logistic_formula <- stats::as.formula(paste("y ~ ", predictor_part, sep = ""))
  ##logistics regression based on training dataset
  pCL <- stats::glm(logistic_formula,data=dt_y[tset,],family=binomial,control = list(maxit=lr_maxiter))


  ##prediction on hold out validation set
  pCLpr <- stats::predict(pCL,dt_y[vset,],type="response")

  roc_res <- getROCAll(y[vset],pCLpr)
  #print("PCLpr is:")
  #print(pCLpr)
  ##if high risk for predicted results
  pCLpos <- ifelse(pCLpr >=0.5, 1, 0)
  ##if low risk for predicted results
  pCLneg <- ifelse(pCLpr < 0.5, 1, 0)
  ##if high risk for pcd
  gc6pos <- ifelse(y[vset]==1, 1, 0)
  ##if low risk for pcd
  gc6neg=ifelse(y[vset]==0, 1, 0)
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
  #print("####################################################")
  # print("c(accgmc12,aucmc12,sensgmc12,spcgmc12,kappamc12) is: ")
  #print(pCLTN)
  #print(gc6pos)
  # print(sum(pCLTP+pCLTN)/sum(gc6pos+gc6neg))
  # print((sum(gc6pos)/sum(gc6pos+gc6neg)*sum(pCLpos)/sum(pCLpos+pCLneg)+
  #          (1-sum(gc6pos)/sum(gc6pos+gc6neg))*(1-sum(pCLpos)/sum(pCLpos+pCLneg))))
  #print(c(accgmc12,aucmc12,sensgmc12,spcgmc12,kappamc12))
  #print("####################################################")
  return(list(c(accgmc12,aucmc12,sensgmc12,spcgmc12,kappamc12),roc_res))
}
