# R scripts for extracting and plotting data stored in Mplus graphic
# information in GH5 files.  Uses the rhdf5 package for loading the
# the GH5 file.
#
# Version history:
# 2013-09-13 File Version 3 for Mplus Version 7.3
# 2014-04-30 Fix for sample and estimated means.
# 2014-10-07 Fix IRT ICC and IIC functions, turning properties into integers
# 2014-10-08 Add functions for Discrete survival curves
# 2014-11-20 Fix estimated probabilities function, turning categories into integers.
# 2014-11-21 Add legend to plot of estimated probabilities.
# 2015-03-30 Fix plot for factors
# 2015-06-01 Fix for case-sensitivity on loop label for mplus.get.loop.estimates.
#            Fix estimated probablities and sample propotions functions, turning categories into integers
#                and using model_group_labels instead of generic "Class" in legend.
# 2015-06-09 Add option for mplus.plot.loop to plot multiple labels.
# 2015-09-09 Fix mplus.get.bayesian.autocorrelation and mplus.plot.bayesian.autocorrelation - dimension
#            of parameter was incorrect.  Fix mplus.plot.bayesian.distribution for alignment when ndist
#            is the same as the number of iterations.
# 2015-10-30 Fix mplus.get.sample_proportions and mplus.get.estimated_probabilities when nominal variable
#            present which throws off count of categories for the categorical variables.
# 2015-11-16 Add functions for the new plots that are added in Version 7.4:
#            moderation, sensitivity, bootstrap distribution
# 2016-10-28 Add mplus.plot.qqnorm for normal QQ plots
# 2016-11-03 mplus.get.data should set all 999 to NA for missing values
# 2017-05-22 Add mplus.plot.eigenvalues and mplus.get.eigenvalues
# 2017-06-15 Add option for plotting sample proportions and estimated probabilities for all
#            categories of a single variable to mplus.plot.sample_proportions and mplus.plot.estimated_probabilities.
#            Also add mplus.plot.sample_proportions_and_estimated_probabilities for plotting both.
# 2017-08-09 Fix loop plots for multiple labels in mplus.plot.loop.  Also add more arguments for customizations.
# 2017-10-17 Fix mplus.plot.irt.icc and mplus.plot.irt.iic getting mean of factor in other groups.
# 2021-01-13 Fix functions for bayesian/predictive since attributes changed to datasets.
#
# Written by: Thuy Nguyen
#             Muthen & Muthen
#
# Reference:
#
# Bernd Fischer and Gregoire Pau (). rhdf5: HDF5 interface to R. R
# package version 2.4.0.
#



######################################################################################################
# Supporting functions
######################################################################################################


##########################################################################
#
# mplus.get.group.attribute - supporting function for getting attribute
#
# arguments:
#	file - the quoted name of an existing GH5 file
#   groupstr - the name of the group for the attribute
#   attrstr - the name of the attribute
#
# eg. mplus.get.group.attribute('ex8.1.gh5','individual_data','var_names')
#
mplus.get.group.attribute <- function(file, groupstr, attrstr) {
  if ( !(file.exists(file))) {
    cstr <- paste("- file does not exist:",file,"\n")
    stop(cstr)
  }

  gh5 <- rhdf5::h5dump(file, load=TRUE)

  fid <- rhdf5::H5Fopen(file)
  gid <- rhdf5::H5Gopen(fid, groupstr)
  atid <- rhdf5::H5Aopen(gid, attrstr)

  attr <- rhdf5::H5Aread(atid)

  rhdf5::H5Aclose(atid)
  rhdf5::H5Gclose(gid)
  rhdf5::H5Fclose(fid)

  attr <- gsub("(^\\s+|\\s+$)", "", attr, perl=TRUE)

  return(attr)
}
##########################################################################
#
# mplus.get.estimated_means - plot estimated means for the quoted process
#
# arguments:
#	file - the quoted name of an existing GH5 file, required
#	procstr - the quoted name of a series, not required.  Defaults to 'process1' (the first process)
#	classidx - the class index, not required - 0 for all classes.  Default to 0.
#
# eg. mplus.get.estimated_means('ex8.1.gh5','process1',3)
#
mplus.get.estimated_means <-function(file,procstr='process1',classidx=0) {
  if (missing(file)) {
    stop(" - name of the GH5 file is required")
  }
  if (!(file.exists(file))) {
    cstr <- paste("- file does not exist:",file,"\n")
    stop(cstr)
  }

  # check that the series exists
  gh5 <- rhdf5::h5dump(file, load=TRUE)

  # check that the series exists
  if (!("process_data" %in% names(gh5))) {
    stop("- requires series information\n\nUse the SERIES option in Mplus to specify series information for processes\nwith estimated means.\n")
  }

  allpnames <- attr(gh5$process_data,"names")
  pind <- pmatch(procstr, allpnames, nomatch=0)
  if (pind == 0) {
    cstr <- paste("- process does not exist:",procstr,"\n\n")
    stop(cstr)
  }

  # get the process
  proc <- gh5$process_data[[procstr]]

  # get the series type in properties
  # Replace the line below with series of low-level function calls
  cstr2 <- paste(c("process_data"),"/",procstr,"", sep="")
  prop <- mplus.get.group.attribute(file,cstr2,'properties')

  series_type <- prop[1]
  if ( ! (series_type == 1 || series_type == 2) ) {
    cstr <- paste("- process does not have estimated means:",procstr,"\n\n")
    stop(cstr)
  }

  # set up the array for the estimated means
  dims <- attr(proc$time_scores,"dim")
  # if all classes, dimension it by number of classes.  Otherwise, just dimension by 1.
  if (classidx == 0) {
    yy <- array(0, c(dims[1],dims[2]))
  } else {
    # check that the classidx is within range.
    if (classidx < 0 || classidx > dims[2]) {
      cstr <- paste("- classidx is out of range, 1 to ",dims[2],": ",classidx,"\n\n")
      stop(cstr)
    }
    yy <- array(0, c(dims[1],1))
  }

  # get the indices of variables in the series
  var_names <- mplus.get.group.attribute(file,cstr2,'var_names')

  if (series_type == 1) {
    mean_vars <- mplus.get.group.attribute(file,'means_and_variances_data/y_estimated_means','variables')
  } else {
    mean_vars <- mplus.get.group.attribute(file,'means_and_variances_data/latent_estimated_means','variables')
  }
  var_indices <- pmatch(var_names, mean_vars, nomatch=0)

  # type 1 is estimated means for observed variables
  if (series_type == 1) {
    if (classidx == 0) {
      for (i in c(1:dims[2])) {
        for (j in c(1:dims[1])) {
          yy[j,i] <- gh5$means_and_variances_data$y_estimated_means$values[var_indices[j],i]
        }
      }
    } else {
      for (j in c(1:dims[1])) {
        yy[j,i] <- gh5$means_and_variances_data$y_estimated_means$values[var_indices[j],classidx]
      }
    }
  }

  # type 2 is estimated means for latent variables
  if (series_type == 2) {
    if (classidx == 0) {
      for (i in c(1:dims[2])) {
        for (j in c(1:dims[1])) {
          yy[j,i] <- gh5$means_and_variances_data$latent_estimated_means$values[var_indices[j],i]
        }
      }
    } else {
      for (j in c(1:dims[1])) {
        yy[j,i] <- gh5$means_and_variances_data$latent_estimated_means$values[var_indices[j],classidx]
      }
    }
  }

  # return the means
  return(yy)
}
