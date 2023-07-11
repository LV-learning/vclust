getClassCountsStr <- function(str_vec){
  len <- length(str_vec)
  status <- 'not start'
  class_count_vec <- c()
  for (i in seq(2,len)){

    if(str_vec[i] == "CLASSIFICATION QUALITY"  & status == 'start'){
      break
    }

    if(status == 'start' & !(str_vec[i] %in% c("","Latent","Classes","Class Counts and Proportions"))){
      class_count_vec <- append(class_count_vec,str_vec[i])
    }

    if(str_vec[i-1] == "FINAL CLASS COUNTS AND PROPORTIONS FOR THE LATENT CLASSES" &
       str_vec[i] == "BASED ON THEIR MOST LIKELY LATENT CLASS MEMBERSHIP"){
      status <- 'start'
    }

  }

  return(class_count_vec)
}
