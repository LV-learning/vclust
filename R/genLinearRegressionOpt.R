genLinearRegressionOpt <- function(y,
                                  ## should be the right part of the formula
                                  ## and the format should be string
                                  predictor_part,
                                  input_and_pp_and_var_df){
  dt_y <- cbind(input_and_pp_and_var_df,y)
  logistic_formula <- stats::as.formula(paste("y ~ ", predictor_part, sep = ""))
  ##logistics regression based on training dataset
  lmod <- stats::lm(logistic_formula,data=dt_y)
  summ_mod <- summary(lmod)
  ##prediction on hold out validation set
  prediction <- stats::predict(lmod,dt_y)
  MSE <- sum((dt_y[,"y"] - prediction)^2)/length(prediction)
  RMSE <- MSE^0.5
  MAE <- sum(abs(dt_y[,"y"] - prediction))/length(prediction)
  r_square <- summ_mod$r.squared
  adj_r_square <- summ_mod$adj.r.squared
  return(list(c(MSE, RMSE, MAE, r_square, adj_r_square)))
}
