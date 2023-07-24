# vclust

The `vclust` package implements a 3-step approach to facilitate the use of unsupervised clustering with the focus on user-defined validation. 

In step 1, it conducts unsupervised clustering based on multivariate outcomes using existing clustering methods such as growth mixture modeling (GMM), model-based clustering (MBC), and K-means clustering. In step 2, in each clustering, latent classes or clusters are regrouped into two coarsened clusters using all possible ways of splits, resulting in a large pool of binary labels. These labels are systematically validated using a priori sets of validators defined by the users. In step 3, the validated and selected labels are deployed in supervised learning.

## Installation

```
install.packages("devtools")
devtools::install_github("LV-learning/vclust")

```

## Reference

Jo, B., Hastie, T. J., Li, Z., Youngstrom, E. A., Findling, R. L., & Horwitz, S. M. (2023). Reorienting Latent Variable Modeling for Supervised Learning. Multivariate Behavioral Research, 1-15.
