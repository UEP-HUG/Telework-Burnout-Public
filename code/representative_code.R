pacman::p_load(
  here,
  tidyverse,
  survey,
  srvyr
)

# Dataset based on variables from new DAG
dat <- tibble(readRDS(here("data", "Generated datasets", "clean_dataset.rds")))
GE_pop_edu_active <- read_csv(here("data", "GE_active_weights.csv"), ) |> select(-1) |>
  rename(age_cat_weighting = age_cat)

table_df <- dat

table_df_pop <- table_df |> 
  # Apply a filter to only keep participants from population-representative surveys
  filter(serocov_pop.x | pop_pilote.x | sp2_novdec_2020.x |sp3_juin_2021.x|sp4_avril_2022.x)

table_df_WORK <- table_df |> 
  filter(serocov_work.x|work_pilote.x)

# Make survey design object ####
## make a survey design object that has no weights assigned
table_df_pop_unweighted <- survey::svydesign(ids = ~0, data = table_df_pop)
## Apply weights to the survey design object using the population weights
table_df_pop_weighted = srvyr::as_survey_design(
  postStratify(table_df_pop_unweighted, ~sex + age_cat_weighting + education_rec_fr, 
               GE_pop_edu_active, partial = TRUE)
)
rm(table_df_pop_unweighted) # Don't need this anymore