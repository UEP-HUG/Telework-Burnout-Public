source(here("Other", "Function_regression_data.R"))

dat <- tibble(readRDS(here("data", "Generated datasets", "clean_dataset_gmh.rds")))

# dat <- tibble(readRDS(here("data", "Generated datasets", "clean_dataset.rds")))
# # Select only population-based sample
# dat <- dat %>% filter(serocov_pop.x | sp3_juin_2021.x | sp2_novdec_2020.x | sp4_avril_2022.x |pop_pilote.x)


source(here("code", "Regression_covariates.R"))
covariates = c(covariates, "events_burn_out", "UCLA_dichotomized")

outcomes = c("burnout_score", "burnout_interp_dich30", "burn_out")
outcomes_family = c("gaussian", "binomial", "binomial")

c <- UEP_regression_data(data = dat, 
                       outcomes = outcomes, outcomes_family = outcomes_family,
                       main_variable = "wfh_exposure",
                       covariates = covariates
                       )
