# Load packages and dataset ####
pacman::p_load(    # Installs / loads packages if they are not already installed / loaded
  here,         # File locator
  tidyverse,    # data management + ggplot2 graphics
  janitor,      # adding totals and percents to tables
  scales,       # easily convert proportions to percents  
  lubridate,    # manipulate date objects
  DescTools,    # package for Cramer's V
  ggcorrplot    # correlation matrix
)

# Dataset
dat <- readRDS(here("data", "Generated datasets", "clean_dataset.rds"))
# Read in the covariates
source(here("code", "Regression_covariates.R"))
covariates = covariates[ !covariates == 'Occupation_label_2']
clean_labels <- clean_labels %>% 
  filter(!variable %in% c(
    "Occupation_label_2", # it breaks when I add this in, might be an issue with number of observations in each level? I took it out and added in Occupation_label_1 in its place (ISCO-08 level 1)
    "wfh_exposure"))

# Throw in some others just to check if they follow the expected pattern
covariates <- c(covariates, 
                "Occupation_label_1"
                , "mental_state_dich" 
                , "hh_income_cat_en"
                , "finance_situation"
                )

# Create an empty dataframe of the pairwise values to be calculated
cramer <- tibble(variable = vctrs::vec_rep(covariates, length(covariates)),
                 match = vctrs::vec_rep_each(covariates, length(covariates)),
                 cramer = NA)

# Run a loop to calculate Cramer's V between each variable pair, and populate the cramer dataframe
for (i in 1:nrow(cramer)) {
  if(cramer[i,1] == cramer[i,2]){
    cramer[i,3] <- 1} else {
      a <- dat %>% 
        select(all_of(
          c(as.character(cramer[i,1]), 
            as.character(cramer[i,2]))
        )) %>% table()
      cramer[i,3] <- CramerV(a)}
}

# Make wide to compute corrplots
cramer_wide <- cramer %>% 
  pivot_wider(
    names_from = match,
    values_from = cramer
  )

# Convert to a matrix format
cramer_wide <- as.data.frame(cramer_wide)
rownames(cramer_wide) <- cramer_wide[,1]
cramer_wide <- cramer_wide %>% select(-variable)

# Generate the correlation matrix
ggcorrplot(
  cramer_wide,
  hc.order = TRUE,
  type = "lower",
  lab = TRUE
)

# VIF (GVIF) ####
outcomes <- c("burnout_score","burnout_interp_dich30", "burn_out")
# Set the family that will be used in the glm models
outcomes_family <- c("gaussian", "binomial", "binomial")

source(here("code", "Regression_covariates.R"))

r_model <- c("wfh_exposure", covariates) %>%
  str_c(collapse = "+") %>% # combine variables separated by a plus
  ## combine the variables with outcome in formula style
  str_c(outcomes[1]," ~ ", .) %>%
  glm(family = outcomes_family[1], data = dat)

car::vif(r_model) ## Interpret GVIF^(1/(2*Df)) as this takes into account the degrees of freedom
