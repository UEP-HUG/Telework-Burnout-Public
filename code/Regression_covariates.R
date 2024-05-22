pacman::p_load(
  tidyverse
)
# Enter the variable for your exposure
exposure <- "wfh_exposure"

# Enter the variables for your covariates
covariates <- c(
  "age_cat"
  , "sex_en"
  , "education_rec_en"

  , "hh_livewith_rec_en"
  , "num_young_children_dich"

  , "overcrowded"
  , "noisy"
  , "quiet_room"

  , "health_general_dich"

  , "Occupation_label_2"
  # , "job_sector"
  , "pandemic_related_work_change"
  # , "work_interruption_dich"
  , "percent_change_cat_r"
)

# Enter a "clean" name for your exposure and covariates
# (covariates need to be in the same order!)
clean_name <- c(
  "Teleworking group"       #Exposure

  , "Age group, years"    # Rest of covariates
  , "Sex"
  , "Education"

  , "Living arrangement"
  , "Young children"

  , "Household density"
  , "Noise level at home"
  , "Access to a quiet room"

  , "Self-reported general health"

  , "ISCO Occupation label"
  # , "Job sector"
  , "Pandemic-related work changes"
  # , "Work interruptions"
  , "Change in contracted hours"
)

# Make a table of the variables and their "clean" names ####
variable <- c(exposure, covariates) 
clean_labels <- tibble(variable, clean_name)
rm(variable, clean_name) # remove these to avoid any confusion later

## Open the clean_labels table to double-check!! ####
