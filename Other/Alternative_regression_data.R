# An alternative file for generating the main regression data for EE-MBI

pacman::p_load(
  tidyverse,
  here
  )

# Dataset based on variables from new DAG
dat <- readRDS(here("data", "Generated datasets", "clean_dataset.rds"))

## Regression variable selection ####
### Outcomes ####
# Make sure your logistic regression outcomes are factors or binary (1/0)

# Enter the variable for your outcome
outcomes <- c("burnout_score","burnout_interp_dich30", "burn_out")
outcomes_clean_name = NULL
# Set the family that will be used in the glm models
outcomes_family <- c("gaussian", "binomial", "binomial")
outcomes_table <- tibble(outcomes, outcomes_family)

### Covariates ####
# Specify exposure and covariates
main_variable = c("wfh_exposure", "health_general_dich")
main_variable_clean_name = c("Teleworking group", "General Health")
source(here("code", "Regression_covariates.R"))
covariates <- c("age_cat", "sex_en")

# Default parameters just for testing the function
i = 1 ; j = 1 ; k = 2 ; estimate_rounding = 2


source(here("Other", "Function_regression_data.R"))
dat2 <- UEP_regression_data(data = dat, outcomes = outcomes, outcomes_family = outcomes_family,main_variable = main_variable, main_variable_clean_name = main_variable_clean_name,covariates = covariates)
