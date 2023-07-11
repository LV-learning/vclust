createGMMInput <- function(n_classes,
                           is_random_i,
                           is_covariates,
                           trend,
                           covariates,
                           initial_starts,
                           final_optimizations,
                           output_path_prefix,
                           data_path,
                           x_names,
                           useObs,
                           y_names,
                           time_scores){
  title <- 'TITLE:pgb only model '
  y_use <- paste(y_names,collapse = " \n             ")
  useV <- paste('     USEV  = ',y_use, sep = '')
  outputName <- ''
  if(is_random_i == TRUE){
    title <- paste(title,'i',sep='')
    if(trend == 'linear'){
      modelRandom <- 'ps@0;'
    }else if(trend == 'quadratic'){
      modelRandom <- 'pq@0; ps@0;'
    }else if(trend == 'cubic'){
      modelRandom <- 'pcub@0; pq@0; ps@0;'
    }
    outputName <- paste(outputName,'i',sep='')
  }
  else{
    if(trend == 'linear'){
      modelRandom <- 'ps@0; pi@0;'
    }else if(trend == 'quadratic'){
      modelRandom <- 'pq@0; ps@0; pi@0;'
    }else if(trend == 'cubic'){
      modelRandom <- 'pcub@0; pq@0; ps@0; pi@0;'
    }
  }
  if(is_covariates == TRUE){
    title <- paste(title, 'x', sep='')
    outputName <- paste(outputName,'x',sep='')
    useV <- paste(useV, ' ', covariates, sep = '')
    if(trend == 'linear'){
      modelXVariates_part1 <- paste('pi ps on ',covariates,';',sep = '')
    }else if(trend == 'quadratic'){
      modelXVariates_part1 <- paste('pi ps pq on ',covariates,';',sep = '')
    }else if(trend == 'cubic'){
      modelXVariates_part1 <- paste('pi ps pq pcub on ',covariates,';',sep = '')
    }

    modelXVariates_part2  <- ''
    for (class in seq(1,n_classes-1)){
      modelXVariates_part2 <- paste(modelXVariates_part2,
                                    'pc#',as.character(class),
                                    ' on ',
                                    covariates,
                                    ';\n',
                                    sep = '')
    }
  }
  else{
    modelXVariates_part1 <- ''
    modelXVariates_part2  <- ''
  }

  title <- paste(title,
                 'cP',
                 as.character(n_classes),
                 ifelse(is_covariates,paste(' x:',covariates,sep = ''),''),
                 sep = '')

  outputName_noSuffix <- paste(outputName,
                               "cP",
                               as.character(n_classes),
                               sep="")

  outputName <- paste(outputName,
                      "cP",
                      as.character(n_classes),
                      ".inp",
                      sep="")

  useV <- paste(useV, ';', sep = '')

  dataSdata <- paste('DATA: FILE = ',
                     data_path,
                     ';\n',
                     'SAVEDATA: FILE = ',
                     '\"',
                     outputName_noSuffix,
                     '.pp',
                     '\"',
                     '; SAVE=cprobabilities;',
                     sep = '')

  varDefine <-
    'VARIABLE:
     NAMES = '

  varDefine <- paste(varDefine, x_names, ';', sep = "")

  classp <- paste('     CLASSES = pc(', as.character(n_classes),');',sep = '')
  Missing <- '     MISSING = all(9999);
     MISSING = *;'

  useObs <- paste("USEOBS = ",useObs, ";",sep = "")

  analysis <- paste('ANALYSIS:TYPE = MIXTURE; process=2; starts =',
                    as.character(initial_starts),
                    as.character(final_optimizations),
                    sep = ' ')
  analysis <- paste(analysis,';',sep = '')

  if(trend == 'linear'){
    modelTop1 <- 'MODEL:
%OVERALL%
pi ps| '
  }else if(trend == 'quadratic'){
    modelTop1 <- 'MODEL:
%OVERALL%
pi ps pq| '
  }else if(trend == 'cubic'){
    modelTop1 <- 'MODEL:
%OVERALL%
pi ps pq pcub| '
  }


  modelTop2 <- paste(sapply(1:length(time_scores),FUN = function(x){paste(y_names[x],'@',time_scores[x],sep = '')}),
                     collapse = " \n          ")

  modelTop <- paste(modelTop1,modelTop2,';',sep = "")

  if(trend == 'linear'){
    modelBottom <- '%pc#1%
  [pi];
  [ps];'
  }else if(trend == 'quadratic'){
    modelBottom <- '%pc#1%
  [pi];
  [ps];
  [pq];'
  }else if(trend == 'cubic'){
    modelBottom <- '%pc#1%
  [pi];
  [ps];
  [pq];
  [pcub];'
  }
  output <- 'OUTPUT: SAMPSTAT;'
  plotall1 <- 'Plot:
Type is plot1 plot2 plot3;
Series = '
  plotall2 <- sapply(1:length(y_names),FUN = function(x){paste(y_names[x]," (",time_scores[x],")",sep="")})
  plotall2 <- paste(plotall2,collapse = "\n ")
  plotall <- paste(plotall1, plotall2, ";",sep = "")
  if(!dir.exists(output_path_prefix)){
    dir.create(output_path_prefix)
  }

  output_path_prefix <- paste(output_path_prefix,outputName_noSuffix,'/',sep='')

  if(!dir.exists(output_path_prefix)){
    dir.create(output_path_prefix)
  }

  output_path <- paste(output_path_prefix ,outputName,sep='')
  outfile <- file(output_path,"w")
  writeLines(c(title,dataSdata,varDefine,useV,classp,Missing,useObs,analysis,modelTop,modelRandom,
               modelXVariates_part1,modelXVariates_part2,modelBottom,output,plotall),outfile)
  close(outfile)
  return(list(output_path_prefix,outputName_noSuffix))
}
