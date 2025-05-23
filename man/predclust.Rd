% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predclust.R
\name{predclust}
\alias{predclust}
\title{Conducts supervised learning treating a validated/selected cluster label as a known input or output variable}
\usage{
predclust(
  sync_genclust,
  sync_validclust,
  output_path_prefix,
  data_path,
  variable_names,
  naString = NULL,
  predictors_names,
  cluster_names,
  label_category1,
  cluster_label_position,
  outcome_obs,
  supervised_method,
  glmnet_specs,
  seed_numbers,
  useobs,
  listwise_deletion_variables,
  train_fraction,
  if_CV,
  K_fold,
  repeated_CV,
  if_PCD,
  r_PCD,
  lr_maxiter,
  customized = F,
  reference = NULL,
  comparison = NULL,
  cohend_SD = NULL
)
}
\arguments{
\item{sync_genclust}{A Boolean variable indicates whether predclust will use
the input data and clustering results from genclust.}

\item{sync_validclust}{A Boolean variable indicates whether predclust will
use the input data and validation results from validclust.
Our program doesn’t support the case when \code{sync_validclust = T} and
\code{sync_genclust = T}.
Here are two counterparts for this case,
\enumerate{
\item When used \code{sync_genclust = T} in validclust, \code{sync_validclust = T}
and \code{sync_genclust = T} is same to \code{sync_genclust = T} and \code{sync_validclust = F}
\item When used \code{sync_genclust = F} in validclust, \code{sync_validclust = T}
and \code{sync_genclust = T} is same to \code{sync_genclust = F} and
\code{sync_validclust = T}.
}}

\item{output_path_prefix}{The user needs to specify the folder path that will
store supervised learning results.
The path should be absolute path (full path) when using
Windows operation system.
For example, “/Users/username/Desktop” for Mac user or “D:/folder” for Windows user.
Use “/” instead of “\\” for the path.}

\item{data_path}{If \code{sync_genclust = FALSE} and \code{sync_validclust = FALSE},
the user is expected to specify the folder path that stores the data
that will be used in predclust.
The data should be in the csv format.
The information provided here will supersede the information from genclust and validclust.}

\item{variable_names}{When data_path is used, the user needs to
specify variable names.
For example, \cr
\code{variable_names = }\cr
\code{c('x', 'e1', 'e2', 'e3', 'f1', 'f2', 'z1', 'q1', 'w1', 'w2', 'w3', 'u1', 'u2')}.
These variable names will overwrite the original names when the data file already has variables names (i.e., header).
The user can choose to use those original names by specifying \code{variable_names = NULL}.}

\item{naString}{A string indicates what string is interpreted as NA value
in the input data.}

\item{predictors_names}{A string vector indicates names of variables to be used as predictors (input variables).
For example, \code{predictors_names = c("x","w1","w2","w3","u1","u2")}.}

\item{cluster_names}{When data_path is not used, \code{sync_genclust = TRUE},\cr
and \code{sync_validclust = FALSE},
the user is expected to use the cluster names from the
summary of the genclust procedure provided in genclust_results.csv.
For example, \code{cluster_names = c("P1", "P2", "P3")}

When data_path is not used and \code{sync_validclust = TRUE},
the user is expected to use the cluster names from the summary of the
validclust procedure provided in validclust_results.csv.

When data_path is used, the user is expected to use the cluster names
from the variables listed in variable_names. Note that,
when using cluster membership in probabilities (soft clustering),
the total should add up to 1.
For example, an individual may have \code{e1=0.3, e2=0.1, e3=0.6}, which add up to 1.
When using observed or hard cluster membership (one unit or person belongs to one cluster),
for a person who belongs to the third cluster, \code{e1=0, e2=0, e3=1}.}

\item{label_category1}{The user needs to specify
which clusters will be categorized into the first category of
the label that will be used in predclust. The rest are automatically
categorized into the second category. For example, based on a 5-cluster
clustering solution, if \code{cluster_names= c("P1", "P2", "P3", "P4", "P5")} and
\code{label_category1= c("P1", "P3")} each unit or person will have the probability
of P1+P3 of belonging to the first category and the probability of P2+P4+P5
of belonging to the second category of the label.}

\item{cluster_label_position}{A string indicates the location of the
cluster label in prediction. \cr
When \code{cluster_label_position="predictor"},
the cluster label defined in label_category1 will be used as a predictor.\cr
When \code{cluster_label_position="predicted"}, the cluster label
will be used as an outcome predicted by provided predictors (input variables).\cr
If \code{cluster_label_position="none"}, the cluster label will be omitted in supervised learning.}

\item{outcome_obs}{When \code{cluster_label_position = "predictor"}
or \code{cluster_label_position = "none"},
the user is expected to specify the outcome variable to be predicted by
the cluster label and other provided predictors.
This argument comes with the following subcomponents.
\itemize{
\item{\code{outcome_type:}}{ In the current version, only a binary variable is
allowed to be used as a prediction (classification) outcome. There are 2 allowed types:
\code{outcome_type="binary"}, when a single outcome variable is already binary (0/1).
\code{outcome_type="cutpoint"}, when a single binary variable will be created based on
a cutpoint (or cutpoints) applied to a single or multiple variables.
}
\item{\code{outcome_source_variables:}}{ The user may specify a single binary
outcome or set of source variables that will be used to create a binary
outcome. For example, outcome_source_variables= c("a","b","c").}
\item{\code{outcome_source_all_missing:}}{ An integer specifies which value to
take when all variables listed in outcome_source_variables are missing.
The three possible options are NA, 1, or 0.
If \code{outcome_source_all_missing = NA},
the outcome of these individuals or units will be treated as missing.
The default is 0.}
\item{\code{outcome_cutpoint:}}{ A numeric value/vector specifies a threshold or
multiple thresholds to create a binary outcome.
For example, \code{outcome_cutpoint=12}, or \code{outcome_cutpoint=c(12,13,14)}.}
\item{\code{outcome_cutpoint_sign:}}{ A character value/vector specifies
comparison operator(s) to be used with thresholds.
Available options include >=, <=, >, <, ==, GE, LE, GT, LT, and EQ.
When using a vector of multiple thresholds, the signs will be
applied to each cutpoint.}
\item{\code{outcome_cutpoint_max_min_mean:}}{ A string specifies a function to use to
summarize multiple variables into a single variable.
The options include max, min, and mean.
For example, \code{outcome_cutpoint_max_min_mean="max"}.
}
}

When outcome_cutpoint is a single value, all cutpoint related arguments
can be used together. For example, if \code{outcome_source_variables=c("a","b","c")},
\code{outcome_cutpoint  = 12}, \code{outcome_cutpoint_sign =">="},\cr
and \code{outcome_cutpoint_max_min_mean="max"}, all cases with \deqn{max(a, b, c) >= 12}
will be assigned the value of 1, and the rest the value of 0.

When outcome_cutpoint has multiple values, outcome_max_min_mean will be ignored.
For example, when \code{outcome_source_variables=c("a","b","c")},
\code{outcome_cutpoint = c(12, 13, 14)}, \cr
\code{outcome_cutpoint_sign = c(">=", "<", ">")},
all cases with \deqn{a>=12 and b<13 and c>14} will be assigned the value of 1,
and the rest the value of 0.}

\item{supervised_method}{A string indicates the type of supervised learning.
In the current version, we allow logistic regression and glmnet.
That is, \code{supervised_method="logistic"}, or \code{supervised_method="glmnet"}.}

\item{glmnet_specs}{When \href{https://cran.r-project.org/web/packages/glmnet/index.html}{glmnet} is used, the user may utilize the same
arguments used in glmnet such as family, lambda, alpha, etc.
That is,

\code{glmnet_specs(family=”binomial”, alpha=1, nlambda=100, lambda = NULL…)}

Note that, in the current version of predclust, we only allow \code{family="binomial"}
and one pair of lambda/alpha.

The user can also employ an external program called superclust
(beta version available), which implements various supervised
learning methods with cluster labels in probabilities.}

\item{seed_numbers}{An integer vector includes 4 items with respect
to seed numbers of splitting train/test datasets, cross-validation,
pseudoclass draws as well as the supervised model.
Their names are seed_num_split, seed_num_kfold, seed_num_pcd, and
seed_num_supervised_model respectively. For example, \code{seed_numbers =}

\code{c(seed_num_split = 4561234,}

\code{seed_num_kfold = 4561234,}

\code{seed_num_pcd = 4561234,}

\code{seed_num_supervised_model = 4561234)}}

\item{useobs}{The user may specify a text string that indicates observations to use.
For example, if we want to exclude observations with \code{x=9} and \code{x=13},
we can set \code{useobs="(x ne 9) and (x ne 13)"}.
If useobs has been already used under genclust and/or validclust,
this argument can be used to specify additional observations to be excluded.}

\item{listwise_deletion_variables}{The user can specify listwise deletion
based on specific variables. For example, \code{listwise_deletion_variables = c("a1","b1")}.
This feature is useful when the user wants to conduct listwise
deletion with variables that are not being used in the predclust procedure.
As a default, the program uses the standard listwise deletion method
for the variables included in the predclust procedure.}

\item{train_fraction}{A single value between 0 and 1 indicating the
fraction of the samples for
the train/test split. For example, \code{train_fraction = 0.7} means that
70\% are used as the train data and 30\% are used as the test data.
The program uses 0.7 as the default.}

\item{if_CV}{A Boolean variable indicates whether K-fold cross validation
is used in supervised learning.}

\item{K_fold}{An integer indicates the number of folds in cross-validation.
The default is 10. It is applicable when \code{if_CV = TRUE}.}

\item{repeated_CV}{An integer indicates the number of repeated K-fold CV.
It is applicable when \code{if_CV = TRUE}.}

\item{if_PCD}{A Boolean variable indicates whether pseudo class draws will
be used to take into account uncertainties in cluster or latent class
assignment (Jo et al., 2017). This argument is relevant when soft clustering methods are used.}

\item{r_PCD}{When \code{if_PCD = TRUE}, the user needs to specify the number of pseudo class draws. The default is 20.}

\item{lr_maxiter}{An integer indicates maximum iterations in logistic
regression, which is the default supervised learning method in this program. The default is 25.}
}
\value{
The supervised learning results will be provided as a csv file (predclust_results.csv)
in the user-specified folder. For each supervised model, Cohen’s Kappa, accuracy,
sensitivity, specificity, and AUC estimates are provided
(their means and standard errors if K-fold cross validation and/or pseudoclass draws are used).
\itemize{
\item{\code{Supervised_method:}}{ The employed supervised learning method.}
\item{\code{Supervised_spec1 to Supervised_spec3:}}{Further details regarding the employed supervised learning method.}
\item{\code{Cluster_n:}}{ The total number of clusters or classes used in creating a cluster label.}
\item{\code{Cluster_names:}}{ The names of all clusters used in creating a cluster label.}
\item{\code{Label_category1:}}{ The clusters categorized in the first category when generating a binary cluster label.}
\item{\code{Label_position:}}{ Whether the cluster label defined in label_category1 is used as a predictor (predictor),
or as an outcome predicted by provided predictors (predicted), or the cluster label is omitted in supervised learning (none).}
\item{\code{Predictors:}}{ The names of the first two variables used as predictors (input variables) in supervised learning.}
\item{\code{Kappa, sensitivity, specificity, accuracy, AUC:}}{ These are the
measures of association between the cluster label and the predicted label.
When \code{if_CV = TRUE} and/or \code{if_PCD = TRUE}, the provided values are the means across K folds and R pseudoclass draws.
These measures are reported separately for the training and test data.}
\item{\code{Kappa_SE, sensitivity_SE, specificity_SE, accuracy_SE, AUC_SE:}}{ When
\code{if_CV = TRUE} and/or \code{if_PCD = TRUE}, these are the standard deviations across
K folds and R pseudoclass draws. These measures are reported separately for the training and test data.}

}
}
\description{
Conducts supervised learning treating a validated/selected cluster label as a known input or output variable.
A label identified as a good outcome from the
validation step (validclust) is recommended to be used as a prediction
output (Jo et al., in press). A label identified as a good predictor of
an outcome is recommended to be used as a prediction input.
Note that predclust can be used as a standalone procedure or in
conjunction with genclust and/or validclust.
}
\references{
Jo, B., Hastie, T. J., Li, Z., Youngstrom, E. A., Findling, R. L., & Horwitz, S. M. (2023). Reorienting Latent Variable Modeling for Supervised Learning. Multivariate Behavioral Research, 1-15.
}
