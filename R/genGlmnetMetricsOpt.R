genGlmnetMetricsOpt <- function(y,
                                ## should be the right part of the formula
                                ## and the format should be string
                                predictor_part,
                                input_and_pp_and_var_df,
                                lr_maxiter,
                                alpha,
                                lambda){
  x <- as.matrix(input_and_pp_and_var_df[,trimws(strsplit(predictor_part,"\\+")[[1]])])
  pCL <- glmnet::glmnet(x = x,
                y = y,
                family = "binomial",
                alpha = alpha,
                standardize = FALSE,
                lambda = lambda)
  ##prediction on hold out validation set
  pCLpr <- stats::predict(pCL,x,type="response")
  roc_res <- getROCAll(y,pCLpr)
  #print("dt_y$predictor_part is")
  #print(predictor_part)
  #print(warnings())
  #print("dim of pCLpr is:")
  #print(length(pCLpr))
  # print("PCLpr is:")
  # print(pCLpr)
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
