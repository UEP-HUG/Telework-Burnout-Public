pacman::p_load(
  tidyverse
)
# Enter the variable for your exposure
exposure <- enframe(c("wfh_exposure" = "Teleworking group"))

outcomes <- enframe(c(
  "burnout_score" = "EE-MBI score"
  ,"burnout_interp_dich30" = "Severe EE-MBI (>=30)"
  , "burn_out" = "Diagnosed burnout in past year"
)) |> 
  mutate(
  outcomes_type = c("gaussian", "binomial", "binomial"),
  outcomes_type_weighted = c("gaussian", "quasibinomial", "quasibinomial")
)

# New ####
# Enter the variables for your covariates

covariates <- enframe(c(
  "age_cat" = "Age group, years"
  # age = "Age, years"
  , "sex_en" = "Sex"
  , "education_rec_en" = "Education"

  , "hh_livewith_rec_en" = "Living arrangement"
  , "num_young_children_dich" = "Young children"

  , "overcrowded" = "Household density"
  , "noisy" = "Noise level at home"
  , "quiet_room" = "Access to a quiet room"

  , "health_general_dich" = "Self-reported general health"
  
  , "pandemic_related_work_change" = "Pandemic-related work changes"
  , "percent_change_cat_r" = "Change in contract percentages"
  , "Occupation_label_2" = "ISCO occupation group"
))

# Make a table of the variables and their "clean" names ####
clean_labels <- bind_rows(exposure, outcomes |> select(-outcomes_type), covariates)
## Open the clean_labels table to double-check!! ####