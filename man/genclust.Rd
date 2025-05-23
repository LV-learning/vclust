% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/genclust.R
\name{genclust}
\alias{genclust}
\title{Conducts unsupervised clustering using existing clustering methods.}
\usage{
genclust(
  model_type,
  class_range,
  min_units = 10,
  data_path,
  variable_names,
  naString = NULL,
  y_names,
  output_path_prefix = "output/",
  useobs,
  listwise_deletion_variables,
  clustering_data_fraction = 1,
  seed_num = c(seed_num_unsupervised_model = 4561234, seed_num_impute_missing = 4561234),
  kmeans_gap_stats_B = 50,
  kmeans_iter = 25,
  MBCtype,
  Ogroups_cutpoint,
  Ogroups_cutpoint_sign,
  Ogroups_cutpoint_max_min_mean,
  GMM_time_scores,
  GMM_covariates,
  GMM_random_intercept,
  GMM_trend = "quadratic",
  GMM_initial_starts = 500,
  GMM_final_optimizations = 50,
  GMM_ID = NULL,
  GMM_AUXILIARY = NULL
)
}
\arguments{
\item{model_type}{A string indicates a clustering method. Currently available
options include GMM (growth mixture modeling), MBC (model-based clustering), and Kmeans.
An additional option is Ogroups, where the user generates observed subgroups
without conducting clustering.

For GMM, commercial software \href{http://statmodel.com/}{Mplus} is used (Muthén and Muthén, 1998-2017).

For MBC, R package \href{https://cran.r-project.org/web/packages/mclust/index.html}{mclust} is used (Scrucca, Fop, Murphy, and Raftery, 2016).

For K-means, R function kmeans is used.

For example, \code{model_type="MBC"}, \code{model_type="GMM"}, \code{model_type="Kmeans"}, or \code{model_type="Ogroups"}.}

\item{class_range}{An integer vector specifies the desired number of clusters.
For example, \cr
\code{class_range = 2:4} means clustering with 2, 3, and 4 clusters.}

\item{min_units}{An integer indicates the minimum number of units in each cluster.
If the number is less than the minimum, unsupervised clustering will stop.
For example, when the unit of analysis is a person and \code{min_units=10},
clustering will stop if the smallest cluster has less than 10 people.}

\item{data_path}{A string indicates the path of the input data.
The data should be in csv format.
For example, "/Users/username/Desktop/inputdata.csv" for Mac user
or "D:/folder/inputdata.csv" for Windows user.}

\item{variable_names}{A text string indicates names of variables from data_path,
where names are separated by white spaces, or commas. For example,
when input data has 9 columns, a1, a2, a3, a4, b1, b2, b3, cov1, and cov2,
\code{variable_names = "a1, a2, a3, a4, b1, b2, b3, cov1, cov2"} or \cr
\code{variable_names = "a1 a2 a3 a4 b1 b2 b3 cov1 cov2"}.
These variable names will overwrite the original names when the data
file already has variables names (i.e., header). The user can choose
to use those original names by specifying \code{variable_names = NULL}.}

\item{naString}{A string indicates what string is interpreted as NA value
in the input data.}

\item{y_names}{A string vector specifies the variable names used as
multivariate outcomes in unsupervised clustering.
When these are repeated measures used with GMM, they
should be chronologically ordered. For example,
\code{y_names = c(a1, a2, a3, a4)}.
When \code{model_type = Ogroups}, specified cupoints are directly applied to
the variables listed under y_names.}

\item{output_path_prefix}{A string indicates the output folder path of model results.
The path should be absolute path (full path) when using Windows operation system.
Remember to use "/" instead of "\" for the path.}

\item{useobs}{A text string indicates observations to use.
This one is the same as USEOBS in Mplus.
This one is a filter to screen out observations (rows for most cases).
For example, if we want to exclude observations with id=9 and id=13,
we can set \code{useobs = "(id ne 9) and (id ne 13)"}.}

\item{listwise_deletion_variables}{The user can specify listwise deletion based on
specific variables listed in variable_names.
For example, \code{listwise_deletion_variables = c("a1", "b1")}.
The user is also allowed to use listwise deletion with variables that are not
being used in the genclust procedure. The use of useobs and listwise_deletion_variables
is particularly important when model_type=Ogroups because it affects interpretation of subgroups.}

\item{clustering_data_fraction}{A single value indicates the fraction of the
samples to be used in unsupervised clustering. The value range is (0, 1] and the default is 1.}

\item{seed_num}{An integer vector indicates seed numbers for clustering and
imputing missing data, which may affect the results depending on the clustering method.
The vector should follow the below format.
\code{Seed_num = c(seed_num_clustering = 4561234,}
\code{seed_num_impute_missing = 4561234)}}

\item{kmeans_gap_stats_B}{An integer indicates the number of bootstrap samples (B) used to calculate gap statistics.}

\item{kmeans_iter}{An integer indicates the number of iterations used in Kmeans clustering.}

\item{MBCtype}{A string indicates the desired type of MBC model.
One of the 14 types of constraints on the covariance matrix can be specified in line with
mclust (EEE, EEI, EEV, EII, EVE, EVI, EVV, VEE, VEI, VEV, VII, VVE, VVI, VVV).}

\item{Ogroups_cutpoint}{A numeric value/vector specifies a threshold/thresholds
to form observed subgroups without conducting clustering.}

\item{Ogroups_cutpoint_sign}{A character value/vector specifies a/multiple
comparison operator(s). Available options include >=, <=, >, <, ==, GE, LE, GT, LT, EQ.
When Ogroups_cutpoint is a vector with multiple cutpoints, the Ogroups_cutpoint_sign will be applied to each cutpoint.}

\item{Ogroups_cutpoint_max_min_mean}{A character specifies what aggregation
function is used to construct subgroups. Available options are max, min, and mean.

When \code{model_type = Ogroups} and Ogroups_cutpoint is a single value,
above three arguments are used to define subgroups.
For example,\cr
if \code{y_names = c('a', 'b', 'c')}, \code{Ogroup_cutpoint = 12}, \code{Ogroups_cutpoint_sign=">="},\cr
and \code{cutpoint_max_min_mean="max"}, all cases with \deqn{max(a, b, c) >= 12} will be assigned the value of 1, and the rest the value of 0.

When model_type = Ogroups and Ogroups_cutpoint is a vector with multiple thresholds,
Ogroups_cutpoint_max_min_mean will be ignored.
For example, if \code{y_names = c('a', 'b', 'c')},  \code{Ogroup_cutpoint = c(12, 13, 14)},\cr
and \code{Ogroups_cutpoint_sign = c('>=', '<', '>')},
all cases with \deqn{a>=12 and b<13 and c>14} will be assigned the value of 1,
and the rest the value of 0.
Formation of observed groups using more complex manipulations should be
conducted externally before using this program.}

\item{GMM_time_scores}{An integer vector specifies time measures at each time point when GMM is used.
This one should have the same length as y_names.
For example, \code{y_names = c(a1, a2, a3)} and \code{time_scores = c(0, 1, 2)}
may mean that a1 is measured at baseline, a2 at 1 year, and a3 at 2 years from the baseline.}

\item{GMM_covariates}{A string contains covariates used in clustering.
Currently, this option applies only to GMM.
For example, if \code{covariates="cov1 cov2 cov3"},
GMM runs with using these covariates as predictors of
growth parameters (intercept and slope) and the cluster membership.
If \code{covariates = NA}, GMM runs without covariates.}

\item{GMM_random_intercept}{A Boolean variable indicates whether GMM is
conducted allowing for a random intercept. If \code{GMM_random_intercept = TRUE},
GMM is conducted with allowing for a random intercept.
If \code{GMM_random_intercept = FALSE}, GMM is conducted without allowing for a random intercept.}

\item{GMM_trend}{For modeling of longitudinal trends, we use polynomial growth.
Our program can support linear, quadratic, and cubic growth.
For example, \code{GMM_trend="linear"}. The current version of the program uses quadratic growth as a default.}

\item{GMM_initial_starts}{An integer indicates the number of initial stage starting
values in maximum likelihood optimization of GMM.}

\item{GMM_final_optimizations}{An integer indicates the number of final stage
optimizations in maximum likelihood optimization of GMM.}

\item{GMM_ID}{A string specifies the variable name of ID in the input file.
This ID variable will be included in the final .pp file. If it is NULL, rownames will
be used}

\item{GMM_AUXILIARY}{A string vector specifies several additional variables
which are intended to included in the final .pp file for subsequent analyses.}
}
\value{
Clustering results are saved in the folder specified in output_path_prefix.
The summary will be provided as a csv file (genclust_results.csv).
}
\description{
Conducts unsupervised clustering using existing clustering methods.
}
\references{
Jo, B., Hastie, T. J., Li, Z., Youngstrom, E. A., Findling, R. L., & Horwitz, S. M. (2023). Reorienting Latent Variable Modeling for Supervised Learning. Multivariate Behavioral Research, 1-15.
}
