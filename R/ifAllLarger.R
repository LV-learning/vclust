##Check if all entries of a column are larger than the threshold number
# data_frame: A data frame includes target column
# field: A char variable, which is the name of target column
# threshold: An integer variable, which is the threshold

ifAllLarger <- function(data_frame,field,threshold){

  return(all(data_frame[,field] >= threshold))

}
