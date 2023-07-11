dichPseudoPrepare.validator_flip <- function(validator,
                                             pp_dt,
                                             ##model classes
                                             n,
                                             input_dt,
                                             if_listwise_deletion,
                                             y_names){
  ##check NA for validators
  ##construct dich variable
  ##listwise deletion
  dt <- pp_dt
  cat("input result in dichPseudoPrepare is ",dim(input_dt),"\n")
  cat("dt result in dichPseudoPrepare is ",dim(dt))
  combined_dt <- cbind(input_dt,dt)
  remove(dt)
  remove(input_dt)
  ##listwise deletion
  if(if_listwise_deletion){
    combined_dt <- combined_dt[stats::complete.cases(combined_dt[,y_names]),]
  }
  # fp2 <- str_split(folder_path,"")[[1]]
  # fp2 <- c(fp2[1:(length(fp2)-2)],as.character(n),"/")
  # fp2 <- paste(fp2,collapse = "")
  # print(fp2)
  # combined_dt_out <- cbind(combined_dt,original_rownames = rownames(combined_dt))
  # write.csv(combined_dt_out,
  #           file=paste(fp2,"combined_dt_for_val_listwise_deletion.csv",sep = ""),
  #           row.names = FALSE)
  # remove(combined_dt_out)
  combined_dt <- createTargetDichVars(validator = validator,
                                      dt = combined_dt)
  combined_dt <- removeValVarNa(validator = validator,
                                dt = combined_dt)
  combined_dt <- combined_dt[stats::complete.cases(combined_dt[,c(validator$listwise_deletion_variables,"variable_start_to_end")]),]
  print("final combined_dt dimension is ")
  print(dim(combined_dt))
  combined_dt
}
