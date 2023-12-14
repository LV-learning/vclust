genLinearRegression <- function(cvFolds_dt,
                               ## a binary column of low/high risk
                               y,
                               ## should be the right part of the formula
                               ## and the format should be string
                               predictor_part,
                               ##current fold of K_folds
                               cur_fold,
                               cur_r,
                               input_and_pp_and_var_df
                               ){
  #print("dims of y in genLogisticMetrics:")
  #print(length(y))
  vset <- cvFolds_dt$subsets[cvFolds_dt$which==cur_fold,cur_r]
  ## train set
  tset <- cvFolds_dt$subsets[cvFolds_dt$which!=cur_fold,cur_r]
  dt_y <- cbind(input_and_pp_and_var_df,y)
  regression_formula <- stats::as.formula(paste("y ~ ", predictor_part, sep = ""))
  ##logistics regression based on training dataset
  lmod <- stats::lm(regression_formula,data=dt_y[tset,])
  summ_mod <- summary(lmod)
  ##prediction on hold out validation set
  prediction <- stats::predict(lmod,dt_y[vset,])
  MSE <- sum((dt_y[vset,"y"] - prediction)^2)/length(prediction)
  RMSE <- MSE^0.5
  MAE <- sum(abs(dt_y[vset,"y"] - prediction))/length(prediction)
  r_square <- summ_mod$r.squared
  adj_r_square <- summ_mod$adj.r.squared

  return(list(c(MSE, RMSE, MAE, r_square, adj_r_square)))
}
