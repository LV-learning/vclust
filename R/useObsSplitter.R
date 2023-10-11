useObsSplitter <- function(useObs){


  useObs <- gsub("(?i)ne","!=",useObs)
  useObs <- gsub("(?i)eq","==",useObs)
  useObs <- gsub("(?i)ge",">=",useObs)
  useObs <- gsub("(?i)le","<=",useObs)
  useObs <- gsub("(?i)gt",">",useObs)
  useObs <- gsub("(?i)lt","<",useObs)
  useObs <- gsub("(?i)and","\\&",useObs)
  useObs <- gsub("(?i)or","\\|",useObs)
  useObs <- gsub("(?i)not","\\!",useObs)

  useObs <- gsub("\t|\n|\f|\v|\r"," ",useObs)
  useObs <- gsub("  |   |    |     |      |       "," ",useObs)
  return(useObs)
}
