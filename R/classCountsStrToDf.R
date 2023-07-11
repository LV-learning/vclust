classCountsStrToDf <- function(str_vec){

  counts_df <- data.frame(stringsAsFactors = FALSE)
  for(aline in str_vec){
    aline <- stringr::str_split(aline," ")[[1]]
    aline <- aline[aline != ""]
    counts_df <- rbind(counts_df,t(aline),stringsAsFactors = FALSE)
  }
  names(counts_df) = c("class","count","proportion")
  counts_df$count = as.numeric(counts_df$count)
  return(counts_df)
}
