#' @importFrom stats binomial
genLogisticMetricsOpt <- function(y,
                                  ## should be the right part of the formula
                                  ## and the format should be string
                                  predictor_part,
                                  input_and_pp_and_var_df,
                                  lr_maxiter){
  dt_y <- cbind(input_and_pp_and_var_df,y)
  logistic_formula <- stats::as.formula(paste("y ~ ", predictor_part, sep = ""))
  ##logistics regression based on training dataset
  pCL <- stats::glm(logistic_formula,data=dt_y,family=binomial,control = list(maxit=lr_maxiter))
  ##prediction on hold out validation set
  pCLpr <- stats::predict(pCL,dt_y,type="response")
  roc_res <- getROCAll(y,pCLpr)
  ##if high risk for predicted results
  pCLpos <- ifelse(pCLpr >=0.5, 1, 0)
  ##if low risk for predicted results
  pCLneg <- ifelse(pCLpr < 0.5, 1, 0)
  ##if high risk for pcd
  gc6pos <- ifelse(y==1, 1, 0)
  ##if low risk for pcd
  gc6neg <- ifelse(y==0, 1, 0)
  #print("dims of pCLpos, pCLneg,gc6pos,gc6neg and are:")
  #print(length(pCLpos))
  #print(length(pCLneg))
  #print(length(gc6pos))
  #print(length(gc6neg))
  pCLTN=ifelse(pCLneg==1&gc6neg==1, 1, 0)
  pCLTP=ifelse(pCLpos==1&gc6pos==1, 1, 0)
  pCLNP=ifelse(pCLneg==1&gc6neg==0, 1, 0)
  pCLPN=ifelse(pCLpos==1&gc6pos==0, 1, 0)
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
  return(list(c(accgmc12,aucmc12,sensgmc12,spcgmc12,kappamc12),roc_res))
}
