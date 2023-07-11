glmnet_specs <- function(...){
  if(is.null(names(c(...)))) stop("glmnet_specs should have names for each argument")
  c(...)
}
