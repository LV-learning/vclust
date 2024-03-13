
library(vclust)

kmeans_unsupervised = genclust(model_type = "kmeans",
         class_range = 2:3,
         min_units = 10,
         data_path = '/Users/zetanli/Desktop/stanfordwork/mplus_projects/scarepgb616.csv',
         variable_names = 'PGBI0	PGBI6	PGBI12	PGBI18	PGBI24	PGBI30
     PGBI36	PGBI42	PGBI48	id	BLBP	SCAREP0	SCAREP6	SCAREP12
     SCAREP18	SCAREP24	SCAREP30	SCAREP36	SCAREP42
     SCAREP48	bage	sex	cdrsb	medicaik	blbpx	site
     adm076	anycbp	melevat	delevat	apgb10	anycanx	anycadhd
     anycdbd	anycpdd	anycdep	anycadj	psychdx	dbddx
     adhddx	aanymeds	atherapy	aanyadep	aanyant
     lithiumb	aanymood	baseins	medicaid	nmiss0 nmiss6
     nmiss12 nmiss18 nmiss24 nmiss30 nmiss36 nmiss42 nmiss48 nmissall9',
         y_names = c('PGBI0','PGBI6','PGBI12','PGBI18','PGBI24'),
         output_path_prefix = '/Users/zetanli/Desktop/test_predclust_TF_nopcd_724/',
         seed_num = c(seed_num_unsupervised_model = 4561234,
                      seed_num_impute_missing = 4561234),
         useobs = "(id ne 10051) and (id ne 10076) and (id ne 10132) and (id ne 10139)
    and (id ne 10193) and (id ne 10262) and (id ne 10501) and (id ne 10587)
    and (id ne 10616) and (id ne 10759) and (id ne 10767) and (id ne 10809)
    and (id ne 10834) and (id ne 10895) and (id ne 10906) and (id ne 10926)
    and (id ne 11097) and (id ne 11124) and (id ne 11239) and (id ne 11352)
    and (id ne 11405) and (id ne 11558) and (id ne 11659) and (id ne 15013)
    and (id ne 21015) and (id ne 21040) and (id ne 21041) and (id ne 21045)
    and (id ne 21058) and (id ne 21062) and (id ne 22010) and (id ne 22041)
    and (id ne 22046) and (id ne 22085) and (id ne 22089) and (id ne 22097)
    and (id ne 22101) and (id ne 22138) and (id ne 22184) and (id ne 22194)
    and (id ne 22220) and (id ne 22232) and (id ne 31001) and (id ne 31025)
    and (id ne 31041) and (id ne 31046) and (id ne 31065) and (id ne 31073)
    and (id ne 31087) and (id ne 31140) and (id ne 31234) and (id ne 33102)
    and (id ne 33128) and (id ne 33183) and (id ne 33202) and (id ne 34022)
    and (id ne 34040) and (id ne 34063) and (id ne 34125) and (id ne 34180)
    and (id ne 34195) and (id ne 34244) and (id ne 34311) and (id ne 34430)
    and (id ne 34478) and (id ne 34746) and (id ne 35062) and (id ne 35086)
    and (id ne 35093) and (id ne 40028) and (id ne 40043) and (id ne 40044)
    and (id ne 40076) and (id ne 40090) and (id ne 40093) and (id ne 40291)
    and (id ne 40303) and (id ne 40355) and (id ne 40477) and (id ne 40601)
    and (id ne 40619) and (id ne 40691) and (id ne 40699) and (id ne 40705)
    and (id ne 40781) and (id ne 40806) and (id ne 41011) and (id ne 41085)
    and (id ne 41157)",
         listwise_deletion_variables = NULL,
         clustering_data_fraction = 1,
         kmeans_gap_stats_B = 50,
         kmeans_iter = 25,
         MBCtype = "VEI", #mclust, model based clustering
         Ogroups_cutpoint = 12,
         Ogroups_cutpoint_sign = ">=",
         Ogroups_cutpoint_max_min_mean = "max",
         GMM_time_scores = c(0,0.5,1,1.5,2),
         GMM_covariates = NULL,
         GMM_random_intercept = FALSE,
         GMM_trend = "quadratic",
         GMM_initial_starts = 500,
         GMM_final_optimizations = 50
)

res1 = validclust(sync_genclust = TRUE,
           info_genclust = NULL,
           useobs="(id ne 10051) and (id ne 10076) and (id ne 10132) and (id ne 10139)
    and (id ne 10193) and (id ne 10262) and (id ne 10501) and (id ne 10587)
    and (id ne 10616) and (id ne 10759) and (id ne 10767) and (id ne 10809)
    and (id ne 10834) and (id ne 10895) and (id ne 10906) and (id ne 10926)
    and (id ne 11097) and (id ne 11124) and (id ne 11239) and (id ne 11352)
    and (id ne 11405) and (id ne 11558) and (id ne 11659) and (id ne 15013)
    and (id ne 21015) and (id ne 21040) and (id ne 21041) and (id ne 21045)
    and (id ne 21058) and (id ne 21062) and (id ne 22010) and (id ne 22041)
    and (id ne 22046) and (id ne 22085) and (id ne 22089) and (id ne 22097)
    and (id ne 22101) and (id ne 22138) and (id ne 22184) and (id ne 22194)
    and (id ne 22220) and (id ne 22232) and (id ne 31001) and (id ne 31025)
    and (id ne 31041) and (id ne 31046) and (id ne 31065) and (id ne 31073)
    and (id ne 31087) and (id ne 31140) and (id ne 31234) and (id ne 33102)
    and (id ne 33128) and (id ne 33183) and (id ne 33202) and (id ne 34022)
    and (id ne 34040) and (id ne 34063) and (id ne 34125) and (id ne 34180)
    and (id ne 34195) and (id ne 34244) and (id ne 34311) and (id ne 34430)
    and (id ne 34478) and (id ne 34746) and (id ne 35062) and (id ne 35086)
    and (id ne 35093) and (id ne 40028) and (id ne 40043) and (id ne 40044)
    and (id ne 40076) and (id ne 40090) and (id ne 40093) and (id ne 40291)
    and (id ne 40303) and (id ne 40355) and (id ne 40477) and (id ne 40601)
    and (id ne 40619) and (id ne 40691) and (id ne 40699) and (id ne 40705)
    and (id ne 40781) and (id ne 40806) and (id ne 41011) and (id ne 41085)
    and (id ne 41157)",
           if_CV = TRUE,
           K_fold = 10,
           seed_num_kfold = 4561234,
           class_range = 2:3,
           kappa_filter_maxN = 4,
           kappa_filter_value = NULL,
           kappa_filter_results = 0.01,
           validator = list( ## validator1 Z
             vclust::validator(validator_cutpoint = 12,validator_cutpoint_sign = "GE",validator_cutpoint_max_min_mean = "max",
                               validator_source_variables = c("PGBI6","PGBI12","PGBI18","PGBI24"),
                               listwise_deletion_variables = c("PGBI0","SCAREP0"),
                               validator_position = "predictor",
                               validator_type = "cutpoint"),

             ##validator2 Q
             vclust::validator(validator_cutpoint = 12,validator_cutpoint_sign = "GE",validator_cutpoint_max_min_mean = "max",
                               validator_source_variables = c("PGBI30","PGBI36","PGBI42","PGBI48"),
                               listwise_deletion_variables = c("PGBI0","SCAREP0"),
                               validator_position = "predictor",
                               validator_type = "cutpoint"),

             ##validator3 W
             vclust::validator(validator_cutpoint = 12,validator_cutpoint_sign = "GE",validator_cutpoint_max_min_mean = "max",
                               validator_source_variables = c("BLBP","cdrsb","SCAREP0"),
                               listwise_deletion_variables = c("PGBI0","SCAREP0"),
                               validator_position = "predictor",
                               validator_type = "combination"))
)

devtools::load_all()

res2 = validclust(sync_genclust = TRUE,
                  info_genclust = NULL,
                  useobs="(id ne 10051) and (id ne 10076) and (id ne 10132) and (id ne 10139)
    and (id ne 10193) and (id ne 10262) and (id ne 10501) and (id ne 10587)
    and (id ne 10616) and (id ne 10759) and (id ne 10767) and (id ne 10809)
    and (id ne 10834) and (id ne 10895) and (id ne 10906) and (id ne 10926)
    and (id ne 11097) and (id ne 11124) and (id ne 11239) and (id ne 11352)
    and (id ne 11405) and (id ne 11558) and (id ne 11659) and (id ne 15013)
    and (id ne 21015) and (id ne 21040) and (id ne 21041) and (id ne 21045)
    and (id ne 21058) and (id ne 21062) and (id ne 22010) and (id ne 22041)
    and (id ne 22046) and (id ne 22085) and (id ne 22089) and (id ne 22097)
    and (id ne 22101) and (id ne 22138) and (id ne 22184) and (id ne 22194)
    and (id ne 22220) and (id ne 22232) and (id ne 31001) and (id ne 31025)
    and (id ne 31041) and (id ne 31046) and (id ne 31065) and (id ne 31073)
    and (id ne 31087) and (id ne 31140) and (id ne 31234) and (id ne 33102)
    and (id ne 33128) and (id ne 33183) and (id ne 33202) and (id ne 34022)
    and (id ne 34040) and (id ne 34063) and (id ne 34125) and (id ne 34180)
    and (id ne 34195) and (id ne 34244) and (id ne 34311) and (id ne 34430)
    and (id ne 34478) and (id ne 34746) and (id ne 35062) and (id ne 35086)
    and (id ne 35093) and (id ne 40028) and (id ne 40043) and (id ne 40044)
    and (id ne 40076) and (id ne 40090) and (id ne 40093) and (id ne 40291)
    and (id ne 40303) and (id ne 40355) and (id ne 40477) and (id ne 40601)
    and (id ne 40619) and (id ne 40691) and (id ne 40699) and (id ne 40705)
    and (id ne 40781) and (id ne 40806) and (id ne 41011) and (id ne 41085)
    and (id ne 41157)",
                  if_CV = TRUE,
                  K_fold = 10,
                  seed_num_kfold = 4561234,
                  class_range = 2:3,
                  kappa_filter_maxN = 4,
                  kappa_filter_value = NULL,
                  kappa_filter_results = 0.01,
                  validator = list( ## validator1 Z
                    vclust::validator(validator_cutpoint = 12,validator_cutpoint_sign = "GE",validator_cutpoint_max_min_mean = "max",
                                      validator_source_variables = c("PGBI6","PGBI12","PGBI18","PGBI24"),
                                      listwise_deletion_variables = c("PGBI0","SCAREP0"),
                                      validator_position = "predictor",
                                      validator_type = "cutpoint"),

                    ##validator2 Q
                    vclust::validator(validator_cutpoint = 12,validator_cutpoint_sign = "GE",validator_cutpoint_max_min_mean = "max",
                                      validator_source_variables = c("PGBI30","PGBI36","PGBI42","PGBI48"),
                                      listwise_deletion_variables = c("PGBI0","SCAREP0"),
                                      validator_position = "predictor",
                                      validator_type = "cutpoint"),

                    ##validator3 W
                    vclust::validator(validator_cutpoint = 12,validator_cutpoint_sign = "GE",validator_cutpoint_max_min_mean = "max",
                                      validator_source_variables = c("BLBP","cdrsb","SCAREP0"),
                                      listwise_deletion_variables = c("PGBI0","SCAREP0"),
                                      validator_position = "predictor",
                                      validator_type = "combination"))
)

test_that("validclust kmeans", {

  expect_equal(res1, res2)
})


library(vclust)

mbc_unsupervised = genclust(model_type = "MBC",
                               class_range = 2:3,
                               min_units = 10,
                               data_path = '/Users/zetanli/Desktop/stanfordwork/mplus_projects/scarepgb616.csv',
                               variable_names = 'PGBI0	PGBI6	PGBI12	PGBI18	PGBI24	PGBI30
     PGBI36	PGBI42	PGBI48	id	BLBP	SCAREP0	SCAREP6	SCAREP12
     SCAREP18	SCAREP24	SCAREP30	SCAREP36	SCAREP42
     SCAREP48	bage	sex	cdrsb	medicaik	blbpx	site
     adm076	anycbp	melevat	delevat	apgb10	anycanx	anycadhd
     anycdbd	anycpdd	anycdep	anycadj	psychdx	dbddx
     adhddx	aanymeds	atherapy	aanyadep	aanyant
     lithiumb	aanymood	baseins	medicaid	nmiss0 nmiss6
     nmiss12 nmiss18 nmiss24 nmiss30 nmiss36 nmiss42 nmiss48 nmissall9',
                               y_names = c('PGBI0','PGBI6','PGBI12','PGBI18','PGBI24'),
                               output_path_prefix = '/Users/zetanli/Desktop/test_predclust_TF_nopcd_724/',
                               seed_num = c(seed_num_unsupervised_model = 4561234,
                                            seed_num_impute_missing = 4561234),
                               useobs = "(id ne 10051) and (id ne 10076) and (id ne 10132) and (id ne 10139)
    and (id ne 10193) and (id ne 10262) and (id ne 10501) and (id ne 10587)
    and (id ne 10616) and (id ne 10759) and (id ne 10767) and (id ne 10809)
    and (id ne 10834) and (id ne 10895) and (id ne 10906) and (id ne 10926)
    and (id ne 11097) and (id ne 11124) and (id ne 11239) and (id ne 11352)
    and (id ne 11405) and (id ne 11558) and (id ne 11659) and (id ne 15013)
    and (id ne 21015) and (id ne 21040) and (id ne 21041) and (id ne 21045)
    and (id ne 21058) and (id ne 21062) and (id ne 22010) and (id ne 22041)
    and (id ne 22046) and (id ne 22085) and (id ne 22089) and (id ne 22097)
    and (id ne 22101) and (id ne 22138) and (id ne 22184) and (id ne 22194)
    and (id ne 22220) and (id ne 22232) and (id ne 31001) and (id ne 31025)
    and (id ne 31041) and (id ne 31046) and (id ne 31065) and (id ne 31073)
    and (id ne 31087) and (id ne 31140) and (id ne 31234) and (id ne 33102)
    and (id ne 33128) and (id ne 33183) and (id ne 33202) and (id ne 34022)
    and (id ne 34040) and (id ne 34063) and (id ne 34125) and (id ne 34180)
    and (id ne 34195) and (id ne 34244) and (id ne 34311) and (id ne 34430)
    and (id ne 34478) and (id ne 34746) and (id ne 35062) and (id ne 35086)
    and (id ne 35093) and (id ne 40028) and (id ne 40043) and (id ne 40044)
    and (id ne 40076) and (id ne 40090) and (id ne 40093) and (id ne 40291)
    and (id ne 40303) and (id ne 40355) and (id ne 40477) and (id ne 40601)
    and (id ne 40619) and (id ne 40691) and (id ne 40699) and (id ne 40705)
    and (id ne 40781) and (id ne 40806) and (id ne 41011) and (id ne 41085)
    and (id ne 41157)",
                               listwise_deletion_variables = NULL,
                               clustering_data_fraction = 1,
                               kmeans_gap_stats_B = 50,
                               kmeans_iter = 25,
                               MBCtype = "VEI", #mclust, model based clustering
                               Ogroups_cutpoint = 12,
                               Ogroups_cutpoint_sign = ">=",
                               Ogroups_cutpoint_max_min_mean = "max",
                               GMM_time_scores = c(0,0.5,1,1.5,2),
                               GMM_covariates = NULL,
                               GMM_random_intercept = FALSE,
                               GMM_trend = "quadratic",
                               GMM_initial_starts = 500,
                               GMM_final_optimizations = 50
)

res1 = validclust(sync_genclust = TRUE,
                  info_genclust = NULL,
                  useobs="(id ne 10051) and (id ne 10076) and (id ne 10132) and (id ne 10139)
    and (id ne 10193) and (id ne 10262) and (id ne 10501) and (id ne 10587)
    and (id ne 10616) and (id ne 10759) and (id ne 10767) and (id ne 10809)
    and (id ne 10834) and (id ne 10895) and (id ne 10906) and (id ne 10926)
    and (id ne 11097) and (id ne 11124) and (id ne 11239) and (id ne 11352)
    and (id ne 11405) and (id ne 11558) and (id ne 11659) and (id ne 15013)
    and (id ne 21015) and (id ne 21040) and (id ne 21041) and (id ne 21045)
    and (id ne 21058) and (id ne 21062) and (id ne 22010) and (id ne 22041)
    and (id ne 22046) and (id ne 22085) and (id ne 22089) and (id ne 22097)
    and (id ne 22101) and (id ne 22138) and (id ne 22184) and (id ne 22194)
    and (id ne 22220) and (id ne 22232) and (id ne 31001) and (id ne 31025)
    and (id ne 31041) and (id ne 31046) and (id ne 31065) and (id ne 31073)
    and (id ne 31087) and (id ne 31140) and (id ne 31234) and (id ne 33102)
    and (id ne 33128) and (id ne 33183) and (id ne 33202) and (id ne 34022)
    and (id ne 34040) and (id ne 34063) and (id ne 34125) and (id ne 34180)
    and (id ne 34195) and (id ne 34244) and (id ne 34311) and (id ne 34430)
    and (id ne 34478) and (id ne 34746) and (id ne 35062) and (id ne 35086)
    and (id ne 35093) and (id ne 40028) and (id ne 40043) and (id ne 40044)
    and (id ne 40076) and (id ne 40090) and (id ne 40093) and (id ne 40291)
    and (id ne 40303) and (id ne 40355) and (id ne 40477) and (id ne 40601)
    and (id ne 40619) and (id ne 40691) and (id ne 40699) and (id ne 40705)
    and (id ne 40781) and (id ne 40806) and (id ne 41011) and (id ne 41085)
    and (id ne 41157)",
                  if_CV = TRUE,
                  K_fold = 10,
                  seed_num_kfold = 4561234,
                  class_range = 2:3,
                  kappa_filter_maxN = 4,
                  kappa_filter_value = NULL,
                  kappa_filter_results = 0.01,
                  validator = list( ## validator1 Z
                    vclust::validator(validator_cutpoint = 12,validator_cutpoint_sign = "GE",validator_cutpoint_max_min_mean = "max",
                                      validator_source_variables = c("PGBI6","PGBI12","PGBI18","PGBI24"),
                                      listwise_deletion_variables = c("PGBI0","SCAREP0"),
                                      validator_position = "predictor",
                                      validator_type = "cutpoint"),

                    ##validator2 Q
                    vclust::validator(validator_cutpoint = 12,validator_cutpoint_sign = "GE",validator_cutpoint_max_min_mean = "max",
                                      validator_source_variables = c("PGBI30","PGBI36","PGBI42","PGBI48"),
                                      listwise_deletion_variables = c("PGBI0","SCAREP0"),
                                      validator_position = "predictor",
                                      validator_type = "cutpoint"),

                    ##validator3 W
                    vclust::validator(validator_cutpoint = 12,validator_cutpoint_sign = "GE",validator_cutpoint_max_min_mean = "max",
                                      validator_source_variables = c("BLBP","cdrsb","SCAREP0"),
                                      listwise_deletion_variables = c("PGBI0","SCAREP0"),
                                      validator_position = "predictor",
                                      validator_type = "combination"))
)

devtools::load_all()

res2 = validclust(sync_genclust = TRUE,
                  info_genclust = NULL,
                  useobs="(id ne 10051) and (id ne 10076) and (id ne 10132) and (id ne 10139)
    and (id ne 10193) and (id ne 10262) and (id ne 10501) and (id ne 10587)
    and (id ne 10616) and (id ne 10759) and (id ne 10767) and (id ne 10809)
    and (id ne 10834) and (id ne 10895) and (id ne 10906) and (id ne 10926)
    and (id ne 11097) and (id ne 11124) and (id ne 11239) and (id ne 11352)
    and (id ne 11405) and (id ne 11558) and (id ne 11659) and (id ne 15013)
    and (id ne 21015) and (id ne 21040) and (id ne 21041) and (id ne 21045)
    and (id ne 21058) and (id ne 21062) and (id ne 22010) and (id ne 22041)
    and (id ne 22046) and (id ne 22085) and (id ne 22089) and (id ne 22097)
    and (id ne 22101) and (id ne 22138) and (id ne 22184) and (id ne 22194)
    and (id ne 22220) and (id ne 22232) and (id ne 31001) and (id ne 31025)
    and (id ne 31041) and (id ne 31046) and (id ne 31065) and (id ne 31073)
    and (id ne 31087) and (id ne 31140) and (id ne 31234) and (id ne 33102)
    and (id ne 33128) and (id ne 33183) and (id ne 33202) and (id ne 34022)
    and (id ne 34040) and (id ne 34063) and (id ne 34125) and (id ne 34180)
    and (id ne 34195) and (id ne 34244) and (id ne 34311) and (id ne 34430)
    and (id ne 34478) and (id ne 34746) and (id ne 35062) and (id ne 35086)
    and (id ne 35093) and (id ne 40028) and (id ne 40043) and (id ne 40044)
    and (id ne 40076) and (id ne 40090) and (id ne 40093) and (id ne 40291)
    and (id ne 40303) and (id ne 40355) and (id ne 40477) and (id ne 40601)
    and (id ne 40619) and (id ne 40691) and (id ne 40699) and (id ne 40705)
    and (id ne 40781) and (id ne 40806) and (id ne 41011) and (id ne 41085)
    and (id ne 41157)",
                  if_CV = TRUE,
                  K_fold = 10,
                  seed_num_kfold = 4561234,
                  class_range = 2:3,
                  kappa_filter_maxN = 4,
                  kappa_filter_value = NULL,
                  kappa_filter_results = 0.01,
                  validator = list( ## validator1 Z
                    vclust::validator(validator_cutpoint = 12,validator_cutpoint_sign = "GE",validator_cutpoint_max_min_mean = "max",
                                      validator_source_variables = c("PGBI6","PGBI12","PGBI18","PGBI24"),
                                      listwise_deletion_variables = c("PGBI0","SCAREP0"),
                                      validator_position = "predictor",
                                      validator_type = "cutpoint"),

                    ##validator2 Q
                    vclust::validator(validator_cutpoint = 12,validator_cutpoint_sign = "GE",validator_cutpoint_max_min_mean = "max",
                                      validator_source_variables = c("PGBI30","PGBI36","PGBI42","PGBI48"),
                                      listwise_deletion_variables = c("PGBI0","SCAREP0"),
                                      validator_position = "predictor",
                                      validator_type = "cutpoint"),

                    ##validator3 W
                    vclust::validator(validator_cutpoint = 12,validator_cutpoint_sign = "GE",validator_cutpoint_max_min_mean = "max",
                                      validator_source_variables = c("BLBP","cdrsb","SCAREP0"),
                                      listwise_deletion_variables = c("PGBI0","SCAREP0"),
                                      validator_position = "predictor",
                                      validator_type = "combination"))
)

test_that("validclust mbc", {
  expect_equal(res1, res2)
})



library(vclust)

res1 = validclust(sync_genclust=FALSE,
                  info_genclust=list(data_path="/Users/zetanli/Desktop/vclust-github/testc2.csv",
                                     output_path_prefix="/Users/zetanli/Desktop/test_validclustF/", variable_names=NULL,naString=c('9999','9999.000',"*"),
                                     cluster_names=c("INRFIXC4")),
                  useobs="(ptid ne 9999)",
                  if_CV=TRUE, K_fold=5,
                  seed_num_kfold=87654321,
                  class_range=4:4,
                  kappa_filter_maxN=NULL, kappa_filter_value=NULL, kappa_filter_results=NULL,
                  validators=list(validator(#validator_type = "cutpoint",
                    validator_position = "predictor",
                    validator_source_all_missing = NA, validator_type = "combination",
                    validator_source_variables = c("FSC4"),
                    listwise_deletion_variables =c("FSC4"),
                    validator_cutpoint=0, validator_cutpoint_sign = "GT",
                    validator_cutpoint_max_min_mean="max"
                  )),customized=F,
                  reference = "P1",
                  comparison = c("P2","P3","P4")
)

devtools::load_all()

res2 = validclust(sync_genclust=FALSE,
                  info_genclust=list(data_path="/Users/zetanli/Desktop/vclust-github/testc2.csv",
                                     output_path_prefix="/Users/zetanli/Desktop/test_validclustF/", variable_names=NULL,naString=c('9999','9999.000',"*"),
                                     cluster_names=c("INRFIXC4")),
                  useobs="(ptid ne 9999)",
                  if_CV=TRUE, K_fold=5,
                  seed_num_kfold=87654321,
                  class_range=4:4,
                  kappa_filter_maxN=NULL, kappa_filter_value=NULL, kappa_filter_results=NULL,
                  validators=list(validator(#validator_type = "cutpoint",
                    validator_position = "predictor",
                    validator_source_all_missing = NA, validator_type = "combination",
                    validator_source_variables = c("FSC4"),
                    listwise_deletion_variables =c("FSC4"),
                    validator_cutpoint=0, validator_cutpoint_sign = "GT",
                    validator_cutpoint_max_min_mean="max"
                  )),customized=F,
                  reference = "P1",
                  comparison = c("P2","P3","P4")
)

test_that("validclust user input", {
  expect_equal(res1, res2)
})

library(vclust)
res3 = predclust(sync_genclust=T,
                 sync_validclust=F,
                 output_path_prefix = NULL, #
                 data_path = NULL, #
                 variable_names = NULL, #
                 predictors_names = c("BLBP","cdrsb","SCAREP0","PGBI0","bage","sex","medicaik"),
                 cluster_names = c("P1","P2","P3"), #
                 label_category1 = c("P1","P2"), #
                 cluster_label_position = "predicted", #
                 outcome_obs = list(outcome_type = "binary",
                                    outcome_source_variables = c("PGBI30","PGBI36","PGBI42","PGBI48"),
                                    outcome_source_all_missing = 0,
                                    outcome_cutpoint = 12,
                                    outcome_cutpoint_sign = ">=",
                                    outcome_cutpoint_max_min_mean = "max"),
                 supervised_method = "logistic", #
                 glmnet_specs = NULL,
                 seed_numbers = c(seed_num_split = 4561234,
                                  seed_num_kfold = 4561234,
                                  seed_num_pcd = 4561234,
                                  seed_num_supervised_model = 4561234),
                 useobs = "(id ne 10051) and (id ne 10076) and (id ne 10132) and (id ne 10139)
    and (id ne 10193) and (id ne 10262) and (id ne 10501) and (id ne 10587)
    and (id ne 10616) and (id ne 10759) and (id ne 10767) and (id ne 10809)
    and (id ne 10834) and (id ne 10895) and (id ne 10906) and (id ne 10926)
    and (id ne 11097) and (id ne 11124) and (id ne 11239) and (id ne 11352)
    and (id ne 11405) and (id ne 11558) and (id ne 11659) and (id ne 15013)
    and (id ne 21015) and (id ne 21040) and (id ne 21041) and (id ne 21045)
    and (id ne 21058) and (id ne 21062) and (id ne 22010) and (id ne 22041)
    and (id ne 22046) and (id ne 22085) and (id ne 22089) and (id ne 22097)
    and (id ne 22101) and (id ne 22138) and (id ne 22184) and (id ne 22194)
    and (id ne 22220) and (id ne 22232) and (id ne 31001) and (id ne 31025)
    and (id ne 31041) and (id ne 31046) and (id ne 31065) and (id ne 31073)
    and (id ne 31087) and (id ne 31140) and (id ne 31234) and (id ne 33102)
    and (id ne 33128) and (id ne 33183) and (id ne 33202) and (id ne 34022)
    and (id ne 34040) and (id ne 34063) and (id ne 34125) and (id ne 34180)
    and (id ne 34195) and (id ne 34244) and (id ne 34311) and (id ne 34430)
    and (id ne 34478) and (id ne 34746) and (id ne 35062) and (id ne 35086)
    and (id ne 35093) and (id ne 40028) and (id ne 40043) and (id ne 40044)
    and (id ne 40076) and (id ne 40090) and (id ne 40093) and (id ne 40291)
    and (id ne 40303) and (id ne 40355) and (id ne 40477) and (id ne 40601)
    and (id ne 40619) and (id ne 40691) and (id ne 40699) and (id ne 40705)
    and (id ne 40781) and (id ne 40806) and (id ne 41011) and (id ne 41085)
    and (id ne 41157)", #
                 listwise_deletion_variables = c("PGBI0","SCAREP0"), #
                 train_fraction = 0.7, #
                 if_CV = T, #
                 K_fold = 10, #
                 repeated_CV = 1, #
                 if_PCD = F, #
                 r_PCD = 20, #
                 lr_maxiter = 100)


devtools::load_all()

res4 = predclust(sync_genclust=T,
                 sync_validclust=F,
                 output_path_prefix = NULL, #
                 data_path = NULL, #
                 variable_names = NULL, #
                 predictors_names = c("BLBP","cdrsb","SCAREP0","PGBI0","bage","sex","medicaik"),
                 cluster_names = c("P1","P2","P3"), #
                 label_category1 = c("P1","P2"), #
                 cluster_label_position = "predicted", #
                 outcome_obs = list(outcome_type = "binary",
                                    outcome_source_variables = c("PGBI30","PGBI36","PGBI42","PGBI48"),
                                    outcome_source_all_missing = 0,
                                    outcome_cutpoint = 12,
                                    outcome_cutpoint_sign = ">=",
                                    outcome_cutpoint_max_min_mean = "max"),
                 supervised_method = "logistic", #
                 glmnet_specs = NULL,
                 seed_numbers = c(seed_num_split = 4561234,
                                  seed_num_kfold = 4561234,
                                  seed_num_pcd = 4561234,
                                  seed_num_supervised_model = 4561234),
                 useobs = "(id ne 10051) and (id ne 10076) and (id ne 10132) and (id ne 10139)
    and (id ne 10193) and (id ne 10262) and (id ne 10501) and (id ne 10587)
    and (id ne 10616) and (id ne 10759) and (id ne 10767) and (id ne 10809)
    and (id ne 10834) and (id ne 10895) and (id ne 10906) and (id ne 10926)
    and (id ne 11097) and (id ne 11124) and (id ne 11239) and (id ne 11352)
    and (id ne 11405) and (id ne 11558) and (id ne 11659) and (id ne 15013)
    and (id ne 21015) and (id ne 21040) and (id ne 21041) and (id ne 21045)
    and (id ne 21058) and (id ne 21062) and (id ne 22010) and (id ne 22041)
    and (id ne 22046) and (id ne 22085) and (id ne 22089) and (id ne 22097)
    and (id ne 22101) and (id ne 22138) and (id ne 22184) and (id ne 22194)
    and (id ne 22220) and (id ne 22232) and (id ne 31001) and (id ne 31025)
    and (id ne 31041) and (id ne 31046) and (id ne 31065) and (id ne 31073)
    and (id ne 31087) and (id ne 31140) and (id ne 31234) and (id ne 33102)
    and (id ne 33128) and (id ne 33183) and (id ne 33202) and (id ne 34022)
    and (id ne 34040) and (id ne 34063) and (id ne 34125) and (id ne 34180)
    and (id ne 34195) and (id ne 34244) and (id ne 34311) and (id ne 34430)
    and (id ne 34478) and (id ne 34746) and (id ne 35062) and (id ne 35086)
    and (id ne 35093) and (id ne 40028) and (id ne 40043) and (id ne 40044)
    and (id ne 40076) and (id ne 40090) and (id ne 40093) and (id ne 40291)
    and (id ne 40303) and (id ne 40355) and (id ne 40477) and (id ne 40601)
    and (id ne 40619) and (id ne 40691) and (id ne 40699) and (id ne 40705)
    and (id ne 40781) and (id ne 40806) and (id ne 41011) and (id ne 41085)
    and (id ne 41157)", #
                 listwise_deletion_variables = c("PGBI0","SCAREP0"), #
                 train_fraction = 0.7, #
                 if_CV = T, #
                 K_fold = 10, #
                 repeated_CV = 1, #
                 if_PCD = F, #
                 r_PCD = 20, #
                 lr_maxiter = 100)


test_that("predclust  mbc", {
  expect_equal(res3, res4)
})


library(vclust)

kmeans_unsupervised = genclust(model_type = "kmeans",
                               class_range = 2:3,
                               min_units = 10,
                               data_path = '/Users/zetanli/Desktop/stanfordwork/mplus_projects/scarepgb616.csv',
                               variable_names = 'PGBI0	PGBI6	PGBI12	PGBI18	PGBI24	PGBI30
     PGBI36	PGBI42	PGBI48	id	BLBP	SCAREP0	SCAREP6	SCAREP12
     SCAREP18	SCAREP24	SCAREP30	SCAREP36	SCAREP42
     SCAREP48	bage	sex	cdrsb	medicaik	blbpx	site
     adm076	anycbp	melevat	delevat	apgb10	anycanx	anycadhd
     anycdbd	anycpdd	anycdep	anycadj	psychdx	dbddx
     adhddx	aanymeds	atherapy	aanyadep	aanyant
     lithiumb	aanymood	baseins	medicaid	nmiss0 nmiss6
     nmiss12 nmiss18 nmiss24 nmiss30 nmiss36 nmiss42 nmiss48 nmissall9',
                               y_names = c('PGBI0','PGBI6','PGBI12','PGBI18','PGBI24'),
                               output_path_prefix = '/Users/zetanli/Desktop/test_predclust_TF_nopcd_724/',
                               seed_num = c(seed_num_unsupervised_model = 4561234,
                                            seed_num_impute_missing = 4561234),
                               useobs = "(id ne 10051) and (id ne 10076) and (id ne 10132) and (id ne 10139)
    and (id ne 10193) and (id ne 10262) and (id ne 10501) and (id ne 10587)
    and (id ne 10616) and (id ne 10759) and (id ne 10767) and (id ne 10809)
    and (id ne 10834) and (id ne 10895) and (id ne 10906) and (id ne 10926)
    and (id ne 11097) and (id ne 11124) and (id ne 11239) and (id ne 11352)
    and (id ne 11405) and (id ne 11558) and (id ne 11659) and (id ne 15013)
    and (id ne 21015) and (id ne 21040) and (id ne 21041) and (id ne 21045)
    and (id ne 21058) and (id ne 21062) and (id ne 22010) and (id ne 22041)
    and (id ne 22046) and (id ne 22085) and (id ne 22089) and (id ne 22097)
    and (id ne 22101) and (id ne 22138) and (id ne 22184) and (id ne 22194)
    and (id ne 22220) and (id ne 22232) and (id ne 31001) and (id ne 31025)
    and (id ne 31041) and (id ne 31046) and (id ne 31065) and (id ne 31073)
    and (id ne 31087) and (id ne 31140) and (id ne 31234) and (id ne 33102)
    and (id ne 33128) and (id ne 33183) and (id ne 33202) and (id ne 34022)
    and (id ne 34040) and (id ne 34063) and (id ne 34125) and (id ne 34180)
    and (id ne 34195) and (id ne 34244) and (id ne 34311) and (id ne 34430)
    and (id ne 34478) and (id ne 34746) and (id ne 35062) and (id ne 35086)
    and (id ne 35093) and (id ne 40028) and (id ne 40043) and (id ne 40044)
    and (id ne 40076) and (id ne 40090) and (id ne 40093) and (id ne 40291)
    and (id ne 40303) and (id ne 40355) and (id ne 40477) and (id ne 40601)
    and (id ne 40619) and (id ne 40691) and (id ne 40699) and (id ne 40705)
    and (id ne 40781) and (id ne 40806) and (id ne 41011) and (id ne 41085)
    and (id ne 41157)",
                               listwise_deletion_variables = NULL,
                               clustering_data_fraction = 1,
                               kmeans_gap_stats_B = 50,
                               kmeans_iter = 25,
                               MBCtype = "VEI", #mclust, model based clustering
                               Ogroups_cutpoint = 12,
                               Ogroups_cutpoint_sign = ">=",
                               Ogroups_cutpoint_max_min_mean = "max",
                               GMM_time_scores = c(0,0.5,1,1.5,2),
                               GMM_covariates = NULL,
                               GMM_random_intercept = FALSE,
                               GMM_trend = "quadratic",
                               GMM_initial_starts = 500,
                               GMM_final_optimizations = 50
)

res3 = predclust(sync_genclust=T,
                 sync_validclust=F,
                 output_path_prefix = NULL, #
                 data_path = NULL, #
                 variable_names = NULL, #
                 predictors_names = c("BLBP","cdrsb","SCAREP0","PGBI0","bage","sex","medicaik"),
                 cluster_names = c("P1","P2","P3"), #
                 label_category1 = c("P1","P2"), #
                 cluster_label_position = "predicted", #
                 outcome_obs = list(outcome_type = "binary",
                                    outcome_source_variables = c("PGBI30","PGBI36","PGBI42","PGBI48"),
                                    outcome_source_all_missing = 0,
                                    outcome_cutpoint = 12,
                                    outcome_cutpoint_sign = ">=",
                                    outcome_cutpoint_max_min_mean = "max"),
                 supervised_method = "logistic", #
                 glmnet_specs = NULL,
                 seed_numbers = c(seed_num_split = 4561234,
                                  seed_num_kfold = 4561234,
                                  seed_num_pcd = 4561234,
                                  seed_num_supervised_model = 4561234),
                 useobs = "(id ne 10051) and (id ne 10076) and (id ne 10132) and (id ne 10139)
    and (id ne 10193) and (id ne 10262) and (id ne 10501) and (id ne 10587)
    and (id ne 10616) and (id ne 10759) and (id ne 10767) and (id ne 10809)
    and (id ne 10834) and (id ne 10895) and (id ne 10906) and (id ne 10926)
    and (id ne 11097) and (id ne 11124) and (id ne 11239) and (id ne 11352)
    and (id ne 11405) and (id ne 11558) and (id ne 11659) and (id ne 15013)
    and (id ne 21015) and (id ne 21040) and (id ne 21041) and (id ne 21045)
    and (id ne 21058) and (id ne 21062) and (id ne 22010) and (id ne 22041)
    and (id ne 22046) and (id ne 22085) and (id ne 22089) and (id ne 22097)
    and (id ne 22101) and (id ne 22138) and (id ne 22184) and (id ne 22194)
    and (id ne 22220) and (id ne 22232) and (id ne 31001) and (id ne 31025)
    and (id ne 31041) and (id ne 31046) and (id ne 31065) and (id ne 31073)
    and (id ne 31087) and (id ne 31140) and (id ne 31234) and (id ne 33102)
    and (id ne 33128) and (id ne 33183) and (id ne 33202) and (id ne 34022)
    and (id ne 34040) and (id ne 34063) and (id ne 34125) and (id ne 34180)
    and (id ne 34195) and (id ne 34244) and (id ne 34311) and (id ne 34430)
    and (id ne 34478) and (id ne 34746) and (id ne 35062) and (id ne 35086)
    and (id ne 35093) and (id ne 40028) and (id ne 40043) and (id ne 40044)
    and (id ne 40076) and (id ne 40090) and (id ne 40093) and (id ne 40291)
    and (id ne 40303) and (id ne 40355) and (id ne 40477) and (id ne 40601)
    and (id ne 40619) and (id ne 40691) and (id ne 40699) and (id ne 40705)
    and (id ne 40781) and (id ne 40806) and (id ne 41011) and (id ne 41085)
    and (id ne 41157)", #
                 listwise_deletion_variables = c("PGBI0","SCAREP0"), #
                 train_fraction = 0.7, #
                 if_CV = T, #
                 K_fold = 10, #
                 repeated_CV = 1, #
                 if_PCD = F, #
                 r_PCD = 20, #
                 lr_maxiter = 100)


devtools::load_all()

res4 = predclust(sync_genclust=T,
                 sync_validclust=F,
                 output_path_prefix = NULL, #
                 data_path = NULL, #
                 variable_names = NULL, #
                 predictors_names = c("BLBP","cdrsb","SCAREP0","PGBI0","bage","sex","medicaik"),
                 cluster_names = c("P1","P2","P3"), #
                 label_category1 = c("P1","P2"), #
                 cluster_label_position = "predicted", #
                 outcome_obs = list(outcome_type = "binary",
                                    outcome_source_variables = c("PGBI30","PGBI36","PGBI42","PGBI48"),
                                    outcome_source_all_missing = 0,
                                    outcome_cutpoint = 12,
                                    outcome_cutpoint_sign = ">=",
                                    outcome_cutpoint_max_min_mean = "max"),
                 supervised_method = "logistic", #
                 glmnet_specs = NULL,
                 seed_numbers = c(seed_num_split = 4561234,
                                  seed_num_kfold = 4561234,
                                  seed_num_pcd = 4561234,
                                  seed_num_supervised_model = 4561234),
                 useobs = "(id ne 10051) and (id ne 10076) and (id ne 10132) and (id ne 10139)
    and (id ne 10193) and (id ne 10262) and (id ne 10501) and (id ne 10587)
    and (id ne 10616) and (id ne 10759) and (id ne 10767) and (id ne 10809)
    and (id ne 10834) and (id ne 10895) and (id ne 10906) and (id ne 10926)
    and (id ne 11097) and (id ne 11124) and (id ne 11239) and (id ne 11352)
    and (id ne 11405) and (id ne 11558) and (id ne 11659) and (id ne 15013)
    and (id ne 21015) and (id ne 21040) and (id ne 21041) and (id ne 21045)
    and (id ne 21058) and (id ne 21062) and (id ne 22010) and (id ne 22041)
    and (id ne 22046) and (id ne 22085) and (id ne 22089) and (id ne 22097)
    and (id ne 22101) and (id ne 22138) and (id ne 22184) and (id ne 22194)
    and (id ne 22220) and (id ne 22232) and (id ne 31001) and (id ne 31025)
    and (id ne 31041) and (id ne 31046) and (id ne 31065) and (id ne 31073)
    and (id ne 31087) and (id ne 31140) and (id ne 31234) and (id ne 33102)
    and (id ne 33128) and (id ne 33183) and (id ne 33202) and (id ne 34022)
    and (id ne 34040) and (id ne 34063) and (id ne 34125) and (id ne 34180)
    and (id ne 34195) and (id ne 34244) and (id ne 34311) and (id ne 34430)
    and (id ne 34478) and (id ne 34746) and (id ne 35062) and (id ne 35086)
    and (id ne 35093) and (id ne 40028) and (id ne 40043) and (id ne 40044)
    and (id ne 40076) and (id ne 40090) and (id ne 40093) and (id ne 40291)
    and (id ne 40303) and (id ne 40355) and (id ne 40477) and (id ne 40601)
    and (id ne 40619) and (id ne 40691) and (id ne 40699) and (id ne 40705)
    and (id ne 40781) and (id ne 40806) and (id ne 41011) and (id ne 41085)
    and (id ne 41157)", #
                 listwise_deletion_variables = c("PGBI0","SCAREP0"), #
                 train_fraction = 0.7, #
                 if_CV = T, #
                 K_fold = 10, #
                 repeated_CV = 1, #
                 if_PCD = F, #
                 r_PCD = 20, #
                 lr_maxiter = 100)


test_that("predclust  kmeans", {
  expect_equal(res3, res4)
})


library(vclust)
res5 = predclust(sync_genclust=F,
                 sync_validclust=F,
                 output_path_prefix = '/Users/zetanli/Desktop/vclust projects folder/test_predclust_FF_nopcd/',
                 data_path = "/Users/zetanli/Desktop/vclust projects folder/scarepgbi616wProb_pred.csv",
                 variable_names = c("V1","V2","V3","PGBI0","PGBI6","PGBI12","PGBI18","PGBI24","PGBI30","PGBI36","PGBI42","PGBI48","id","BLBP","SCAREP0","SCAREP6","SCAREP12","SCAREP18","SCAREP24","SCAREP30","SCAREP36","SCAREP42","SCAREP48","bage","sex","cdrsb","medicaik","blbpx","site","adm076","anycbp","melevat","delevat","apgb10","anycanx","anycadhd","anycdbd","anycpdd","anycdep","anycadj","psychdx","dbddx","adhddx","aanymeds","atherapy","aanyadep","aanyant","lithiumb","aanymood","baseins","medicaid","nmiss0","nmiss6","nmiss12","nmiss18","nmiss24","nmiss30","nmiss36","nmiss42","nmiss48","nmissall9","PGBI3048"),
                 #cluster_label_position = "predicted":combination: cluster = "BLBP","cdrsb","SCAREP0","PGBI0","bage","sex","medicaik"
                 #cluster_label_position = "predictor": combination: binary_validator = "BLBP","cdrsb","SCAREP0","PGBI0","bage","sex","medicaik","cluster"
                 #cluster_label_position = "none": combination: binary_validator = "BLBP","cdrsb","SCAREP0","PGBI0","bage","sex","medicaik"
                 predictors_names = c("BLBP","cdrsb","SCAREP0","PGBI0","bage","sex","medicaik"),
                 cluster_names = c("V1","V2","V3"), #
                 label_category1 = c("V1","V2"), #
                 cluster_label_position = "predictor", #
                 outcome_obs = list(outcome_type = "cutpoint",
                                    outcome_source_variables = c("PGBI30","PGBI36","PGBI42","PGBI48"),
                                    outcome_source_all_missing = 1,
                                    outcome_cutpoint = 12,
                                    outcome_cutpoint_sign = ">=",
                                    outcome_cutpoint_max_min_mean = "max"),
                 supervised_method = "logistic", #
                 #glmnet_specs(family="binomial",alpha=1,lambda=0.3),
                 glmnet_specs = NULL,
                 seed_numbers = c(seed_num_split = 4561234,
                                  seed_num_kfold = 4561234,
                                  seed_num_pcd = 4561234,
                                  seed_num_supervised_model = 4561234),
                 useobs = "(id ne 10051) and (id ne 10076) and (id ne 10132) and (id ne 10139)
    and (id ne 10193) and (id ne 10262) and (id ne 10501) and (id ne 10587)
    and (id ne 10616) and (id ne 10759) and (id ne 10767) and (id ne 10809)
    and (id ne 10834) and (id ne 10895) and (id ne 10906) and (id ne 10926)
    and (id ne 11097) and (id ne 11124) and (id ne 11239) and (id ne 11352)
    and (id ne 11405) and (id ne 11558) and (id ne 11659) and (id ne 15013)
    and (id ne 21015) and (id ne 21040) and (id ne 21041) and (id ne 21045)
    and (id ne 21058) and (id ne 21062) and (id ne 22010) and (id ne 22041)
    and (id ne 22046) and (id ne 22085) and (id ne 22089) and (id ne 22097)
    and (id ne 22101) and (id ne 22138) and (id ne 22184) and (id ne 22194)
    and (id ne 22220) and (id ne 22232) and (id ne 31001) and (id ne 31025)
    and (id ne 31041) and (id ne 31046) and (id ne 31065) and (id ne 31073)
    and (id ne 31087) and (id ne 31140) and (id ne 31234) and (id ne 33102)
    and (id ne 33128) and (id ne 33183) and (id ne 33202) and (id ne 34022)
    and (id ne 34040) and (id ne 34063) and (id ne 34125) and (id ne 34180)
    and (id ne 34195) and (id ne 34244) and (id ne 34311) and (id ne 34430)
    and (id ne 34478) and (id ne 34746) and (id ne 35062) and (id ne 35086)
    and (id ne 35093) and (id ne 40028) and (id ne 40043) and (id ne 40044)
    and (id ne 40076) and (id ne 40090) and (id ne 40093) and (id ne 40291)
    and (id ne 40303) and (id ne 40355) and (id ne 40477) and (id ne 40601)
    and (id ne 40619) and (id ne 40691) and (id ne 40699) and (id ne 40705)
    and (id ne 40781) and (id ne 40806) and (id ne 41011) and (id ne 41085)
    and (id ne 41157)", #
                 listwise_deletion_variables = c("PGBI0","SCAREP0"), #
                 train_fraction = 0.7, #
                 if_CV = T, #
                 K_fold = 10, #
                 repeated_CV = 1, #
                 if_PCD = T, #
                 r_PCD = 20, #
                 lr_maxiter = 100)

devtools::load_all()

res6 = predclust(sync_genclust=F,
                 sync_validclust=F,
                 output_path_prefix = '/Users/zetanli/Desktop/vclust projects folder/test_predclust_FF_nopcd/',
                 data_path = "/Users/zetanli/Desktop/vclust projects folder/scarepgbi616wProb_pred.csv",
                 variable_names = c("V1","V2","V3","PGBI0","PGBI6","PGBI12","PGBI18","PGBI24","PGBI30","PGBI36","PGBI42","PGBI48","id","BLBP","SCAREP0","SCAREP6","SCAREP12","SCAREP18","SCAREP24","SCAREP30","SCAREP36","SCAREP42","SCAREP48","bage","sex","cdrsb","medicaik","blbpx","site","adm076","anycbp","melevat","delevat","apgb10","anycanx","anycadhd","anycdbd","anycpdd","anycdep","anycadj","psychdx","dbddx","adhddx","aanymeds","atherapy","aanyadep","aanyant","lithiumb","aanymood","baseins","medicaid","nmiss0","nmiss6","nmiss12","nmiss18","nmiss24","nmiss30","nmiss36","nmiss42","nmiss48","nmissall9","PGBI3048"),
                 #cluster_label_position = "predicted":combination: cluster = "BLBP","cdrsb","SCAREP0","PGBI0","bage","sex","medicaik"
                 #cluster_label_position = "predictor": combination: binary_validator = "BLBP","cdrsb","SCAREP0","PGBI0","bage","sex","medicaik","cluster"
                 #cluster_label_position = "none": combination: binary_validator = "BLBP","cdrsb","SCAREP0","PGBI0","bage","sex","medicaik"
                 predictors_names = c("BLBP","cdrsb","SCAREP0","PGBI0","bage","sex","medicaik"),
                 cluster_names = c("V1","V2","V3"), #
                 label_category1 = c("V1","V2"), #
                 cluster_label_position = "predictor", #
                 outcome_obs = list(outcome_type = "cutpoint",
                                    outcome_source_variables = c("PGBI30","PGBI36","PGBI42","PGBI48"),
                                    outcome_source_all_missing = 1,
                                    outcome_cutpoint = 12,
                                    outcome_cutpoint_sign = ">=",
                                    outcome_cutpoint_max_min_mean = "max"),
                 supervised_method = "logistic", #
                 #glmnet_specs(family="binomial",alpha=1,lambda=0.3),
                 glmnet_specs = NULL,
                 seed_numbers = c(seed_num_split = 4561234,
                                  seed_num_kfold = 4561234,
                                  seed_num_pcd = 4561234,
                                  seed_num_supervised_model = 4561234),
                 useobs = "(id ne 10051) and (id ne 10076) and (id ne 10132) and (id ne 10139)
    and (id ne 10193) and (id ne 10262) and (id ne 10501) and (id ne 10587)
    and (id ne 10616) and (id ne 10759) and (id ne 10767) and (id ne 10809)
    and (id ne 10834) and (id ne 10895) and (id ne 10906) and (id ne 10926)
    and (id ne 11097) and (id ne 11124) and (id ne 11239) and (id ne 11352)
    and (id ne 11405) and (id ne 11558) and (id ne 11659) and (id ne 15013)
    and (id ne 21015) and (id ne 21040) and (id ne 21041) and (id ne 21045)
    and (id ne 21058) and (id ne 21062) and (id ne 22010) and (id ne 22041)
    and (id ne 22046) and (id ne 22085) and (id ne 22089) and (id ne 22097)
    and (id ne 22101) and (id ne 22138) and (id ne 22184) and (id ne 22194)
    and (id ne 22220) and (id ne 22232) and (id ne 31001) and (id ne 31025)
    and (id ne 31041) and (id ne 31046) and (id ne 31065) and (id ne 31073)
    and (id ne 31087) and (id ne 31140) and (id ne 31234) and (id ne 33102)
    and (id ne 33128) and (id ne 33183) and (id ne 33202) and (id ne 34022)
    and (id ne 34040) and (id ne 34063) and (id ne 34125) and (id ne 34180)
    and (id ne 34195) and (id ne 34244) and (id ne 34311) and (id ne 34430)
    and (id ne 34478) and (id ne 34746) and (id ne 35062) and (id ne 35086)
    and (id ne 35093) and (id ne 40028) and (id ne 40043) and (id ne 40044)
    and (id ne 40076) and (id ne 40090) and (id ne 40093) and (id ne 40291)
    and (id ne 40303) and (id ne 40355) and (id ne 40477) and (id ne 40601)
    and (id ne 40619) and (id ne 40691) and (id ne 40699) and (id ne 40705)
    and (id ne 40781) and (id ne 40806) and (id ne 41011) and (id ne 41085)
    and (id ne 41157)", #
                 listwise_deletion_variables = c("PGBI0","SCAREP0"), #
                 train_fraction = 0.7, #
                 if_CV = T, #
                 K_fold = 10, #
                 repeated_CV = 1, #
                 if_PCD = T, #
                 r_PCD = 20, #
                 lr_maxiter = 100)
test_that("predclust  user input", {
  expect_equal(res5, res6)
})
