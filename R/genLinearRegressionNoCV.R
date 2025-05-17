genLinearRegressionNoCV <- function(y,
                                   ## should be the right part of the formula
                                   ## and the format should be string
                                   predictor_part,
                                   input_and_pp_and_var_df,
                                   y_test,
                                   input_and_pp_and_var_df_test){
  dt_y <- cbind(input_and_pp_and_var_df,y)
  dt_y_test <- cbind(input_and_pp_and_var_df_test,'y' = y_test)
  #print("dims of dt_y and dt_y_test in genLogisticMetricsNoCV are:")
  #print(dim(dt_y))
  #print(dim(dt_y_test))
  regression_formula <- stats::as.formula(paste("y ~ ", predictor_part, sep = ""))
  ##logistics regression based on training dataset
  lmod <- stats::lm(regression_formula,data=dt_y)
  summ_mod <- summary(lmod)
  ##prediction on hold out validation set
  prediction <- stats::predict(lmod,dt_y_test)
  dt_y_test$predicted_y <- prediction

  MSE <- sum((dt_y_test$y - prediction)^2)/length(prediction)
  RMSE <- MSE^0.5
  MAE <- sum(abs(dt_y_test$y - prediction))/length(prediction)
  r_square <- summ_mod$r.squared
  adj_r_square <- summ_mod$adj.r.squared
  aic <- stats::AIC(lmod)

  prediction_dt <- data.frame(cluster=dt_y_test[,"cluster"], prediction=prediction)
  prediction_dt <- prediction_dt[prediction_dt$cluster != 0,]
  return(list(c(MSE, RMSE, MAE, r_square, adj_r_square, aic),dt_y_test[,c('original_id','y','predicted_y')], mean(prediction_dt$prediction,na.rm=T), as.data.frame(summ_mod$coefficients)))
}
