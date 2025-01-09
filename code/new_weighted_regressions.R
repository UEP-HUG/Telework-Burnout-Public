pacman::p_load(
  here,
  tidyverse,
  survey,
  srvyr,
  gtsummary
)

source(here::here("code", "representative_code.R"))
# Specify exposure and covariates
source(here::here("code", "Regression_covariates.R"))

# Function ####

#' Function to run the regression analyses on our data, either weighting the data (population-based) or not.
#'
#' @param df The dataframe to analzye
#' @param type String specifying whether to run a Multivariable or Univariable analysis
#' @param m_design String specifying whether the dataframe should be weighted before running the analyses
#' @param sensitivity_variables Optional string specifying sensitivity variables to exclude as covariates in the multivariable analyses
#'
#' @return A table with the regression estimate results
#'
#' @examples
#' regression_weighted_fun(df = table_df_pop)
regression_weighted_fun <- function(df, type = "Multivariable", m_design = "weighted", sensitivity_variables = NULL){
  source(here("code", "Regression_covariates.R"))
  GE_pop_edu_active <- read_csv(here("data", "GE_active_weights.csv"), ) |> select(-1) |>
    rename(age_cat_weighting = age_cat)
  wfh_ref_level = levels(df[["wfh_exposure"]])[1]
  if(m_design == "weighted"){
    # Make survey design object ####
    ## make a survey design object that has no weights assigned
    table_df_unweighted <- survey::svydesign(ids = ~0, data = df)
    ## Apply weights to the survey design object using the population weights
    df = srvyr::as_survey_design(
      postStratify(table_df_unweighted, ~sex + age_cat_weighting + education_rec_fr, 
                   GE_pop_edu_active, partial = TRUE)
    )
    
    design <- df$variables}else{design = df}
  wfh_n <- design |> count(wfh_exposure)
  mbi_n <- design |> count(wfh_exposure) |> 
    mutate(
      outcome = "burnout_score",
      n_obs = NA
    )
  mbi_dich_n <- design |> group_by(wfh_exposure) |>  
    count(burnout_interp_dich30) |> 
    filter(burnout_interp_dich30 == "Severe") |> 
    rename(outcome = burnout_interp_dich30, n_obs = n) |> 
    mutate(outcome = "burnout_interp_dich30") |> 
    right_join(wfh_n)
  bo_n <- design |> group_by(wfh_exposure) |>  
    count(burn_out) |> 
    filter(burn_out == 1) |> 
    rename(outcome = burn_out, n_obs = n) |> 
    mutate(outcome = "burn_out") |> 
    right_join(wfh_n)
  design_n <- bind_rows(mbi_n, mbi_dich_n, bo_n) |> rename(term = wfh_exposure)
  
  if(type == "Multivariable"){
    model_variables <- c(exposure$name, covariates$name)
    model_variables <- model_variables[!model_variables %in% c(sensitivity_variables)]
  }else{
    model_variables <- c(exposure$name)
  }
  # Initialize an empty list to store results for each i
  all_results <- list()
  
  # Loop through each level of i
  for (i in seq_along(outcomes$name)) {
    if(m_design == "weighted"){
      m1 <- model_variables %>% 
        str_c(collapse = "+") %>% 
        str_c(outcomes$name[i], " ~ ", .) %>% 
        svyglm(family = outcomes$outcomes_type_weighted[i], 
               design = df)
    }else{
      m1 <- model_variables %>%     ## Take exposure and covariate variable names
        str_c(collapse = "+") %>%                 ## combine separated by a plus
        str_c(outcomes$name[i]," ~ ", .) %>%        ## combine with outcome in formula style
        glm(family = outcomes$outcomes_type[i],                  ## define the family as binomial for logistic regression
            data = df) 
    }
    
    results_pop_weighted <- broom::tidy(m1, conf.int = TRUE, 
                                        exponentiate = (outcomes$outcomes_type_weighted[i] != "gaussian")) |> 
      mutate(
        p_value = case_when(p.value < 0.001 ~ "<0.001", 
                            .default = as.character(round(p.value, 3)))
      ) |> 
      filter(str_detect(term, "wfh_exposure")) |> 
      mutate(
        term = str_remove(term, "wfh_exposure"),
        ref = FALSE
      ) |> 
      add_row(
        term = wfh_ref_level, 
        ref = TRUE, 
        estimate = case_when(i == 1 ~ 0, .default = 1), 
        .before = 1
      ) |> 
      mutate(outcome = outcomes$name[i],
             model = type,
             design = m_design,
             coefficients_label = case_when(outcomes$outcomes_type_weighted[i] == "gaussian" ~ "Beta",
                                            .default = "OR"),
             full_estimate = case_when(            # combine OR, CI, and p-value symbol in one variable
               ref == TRUE ~ "(ref)",
               ref == FALSE ~ paste0(
                 sprintf("%1.2f", estimate), " [", 
                 sprintf("%1.2f", conf.low),", ", 
                 sprintf("%1.2f", conf.high), "]")
             )) |> 
      left_join(design_n)
    
    # Append the results to the list
    all_results[[i]] <- results_pop_weighted
  }
  
  # Bind all the results together into one dataset
  dplyr::bind_rows(all_results)
}

# Weighted POP data ####
mv_POP <- regression_weighted_fun(df = table_df_pop)
uv_POP <- regression_weighted_fun(df = table_df_pop, type = "Univariable")
comb_POP <- bind_rows(mv_POP, uv_POP) |> 
  left_join(mv_POP |> select(term, outcome, full_estimate) |> rename(full_estimate_mv = full_estimate)) |> 
  left_join(uv_POP |> select(term, outcome, full_estimate) |> rename(full_estimate_uv = full_estimate))
tab_POP <- comb_POP |> filter(model == "Multivariable") |>  select(outcome,term, n:full_estimate_uv) #|> 
# mutate(perc_cases = case_when(is.na(n_obs) ~ NA, 
#                               .default = paste0(sprintf("%1.2f", (n_obs/n)*100), "%")
# )) |> relocate(perc_cases, .after = n_obs)

# Unweighted WORK data ####
mv_WORK <- regression_weighted_fun(df = table_df_WORK, m_design = "unweighted")
uv_WORK <- regression_weighted_fun(df = table_df_WORK, m_design = "unweighted", type = "Univariable")
comb_WORK <- bind_rows(mv_WORK, uv_WORK) |> 
  left_join(mv_WORK |> select(term, outcome, full_estimate) |> rename(full_estimate_mv = full_estimate)) |> 
  left_join(uv_WORK |> select(term, outcome, full_estimate) |> rename(full_estimate_uv = full_estimate))
tab_WORK <- comb_WORK |> filter(model == "Multivariable") |>  select(outcome,term, n:full_estimate_uv) #|> 
# mutate(perc_cases = case_when(is.na(n_obs) ~ NA, 
#                                     .default = paste0(sprintf("%1.2f", (n_obs/n)*100), "%")
#                                     )) |> relocate(perc_cases, .after = n_obs)

# Sensitivity analyis  ####
## No change (pandemic-related or contract) ####
### Weighted POP ####
# n = 948
mv_SENS_nochange <- regression_weighted_fun(
  df = table_df_pop |> 
    filter(
      percent_change_cat_r == "No change",
      pandemic_related_work_change == "No change"
    ) |> droplevels(),
  sensitivity_variables = c("percent_change_cat_r", "pandemic_related_work_change")
)
uv_SENS_nochange <- regression_weighted_fun(
  df = table_df_pop |> 
    filter(
      percent_change_cat_r == "No change",
      pandemic_related_work_change == "No change"
    ) |> droplevels(),
  sensitivity_variables = c("percent_change_cat_r", "pandemic_related_work_change"), 
  type = "Univariable")
comb_SENS_nochange <- bind_rows(mv_SENS_nochange, uv_SENS_nochange) |> 
  left_join(mv_SENS_nochange |> select(term, outcome, full_estimate) |> rename(full_estimate_mv = full_estimate)) |> 
  left_join(uv_SENS_nochange |> select(term, outcome, full_estimate) |> rename(full_estimate_uv = full_estimate))
tab_SENS_nochange <- comb_SENS_nochange |> filter(model == "Multivariable") |>  select(outcome,term, n:full_estimate_uv)

### Unweighted WORK ####
# n = 870
mv_SENS_nochange_WORK <- regression_weighted_fun(
  df = table_df_WORK |> 
    filter(
      percent_change_cat_r == "No change",
      pandemic_related_work_change == "No change"
    ) |> droplevels(),
  sensitivity_variables = c("percent_change_cat_r", "pandemic_related_work_change"),
  m_design = "unweighted"
)
uv_SENS_nochange_WORK <- regression_weighted_fun(
  df = table_df_WORK |> 
    filter(
      percent_change_cat_r == "No change",
      pandemic_related_work_change == "No change"
    ) |> droplevels(),
  sensitivity_variables = c("percent_change_cat_r", "pandemic_related_work_change"), 
  type = "Univariable",
  m_design = "unweighted"
)
comb_SENS_nochange_WORK <- bind_rows(mv_SENS_nochange_WORK, uv_SENS_nochange_WORK) |> 
  left_join(mv_SENS_nochange_WORK |> select(term, outcome, full_estimate) |> rename(full_estimate_mv = full_estimate)) |> 
  left_join(uv_SENS_nochange_WORK |> select(term, outcome, full_estimate) |> rename(full_estimate_uv = full_estimate))
tab_SENS_nochange_WORK <- comb_SENS_nochange_WORK |> filter(model == "Multivariable") |>  select(outcome,term, n:full_estimate_uv)

## Not possible as reference ####
### Weighted POP data ####
mv_POP_SENS_notpossible <- regression_weighted_fun(df = table_df_pop |> 
                                                     mutate(wfh_exposure = fct_relevel(wfh_exposure, 
                                                                                       "Not possible")))
uv_POP_SENS_notpossible <- regression_weighted_fun(df = table_df_pop |> 
                                                     mutate(wfh_exposure = fct_relevel(wfh_exposure, 
                                                                                       "Not possible")), 
                                                   type = "Univariable")
comb_POP_SENS_notpossible <- bind_rows(mv_POP_SENS_notpossible, uv_POP_SENS_notpossible) |> 
  left_join(mv_POP_SENS_notpossible |> select(term, outcome, full_estimate) |> rename(full_estimate_mv = full_estimate)) |> 
  left_join(uv_POP_SENS_notpossible |> select(term, outcome, full_estimate) |> rename(full_estimate_uv = full_estimate))
tab_POP_SENS_notpossible <- comb_POP_SENS_notpossible |> filter(model == "Multivariable") |>  select(outcome,term, n:full_estimate_uv) #|> 
# mutate(perc_cases = case_when(is.na(n_obs) ~ NA, 
#                               .default = paste0(sprintf("%1.2f", (n_obs/n)*100), "%")
# )) |> relocate(perc_cases, .after = n_obs)

### Unweighted WORK data ####
mv_WORK_SENS_notpossible <- regression_weighted_fun(
  df = table_df_WORK |> 
    mutate(wfh_exposure = fct_relevel(wfh_exposure, 
                                      "Not possible")), 
  m_design = "unweighted"
)
uv_WORK_SENS_notpossible <- regression_weighted_fun(
  df = table_df_WORK |> 
    mutate(wfh_exposure = fct_relevel(wfh_exposure, 
                                      "Not possible")), 
  m_design = "unweighted", 
  type = "Univariable"
)
comb_WORK_SENS_notpossible <- bind_rows(mv_WORK_SENS_notpossible, uv_WORK_SENS_notpossible) |> 
  left_join(mv_WORK_SENS_notpossible |> select(term, outcome, full_estimate) |> rename(full_estimate_mv = full_estimate)) |> 
  left_join(uv_WORK_SENS_notpossible |> select(term, outcome, full_estimate) |> rename(full_estimate_uv = full_estimate))
tab_WORK_SENS_notpossible <- comb_WORK_SENS_notpossible |> filter(model == "Multivariable") |>  select(outcome,term, n:full_estimate_uv) #|> 
# mutate(perc_cases = case_when(is.na(n_obs) ~ NA, 
#                                     .default = paste0(sprintf("%1.2f", (n_obs/n)*100), "%")
#                                     )) |> relocate(perc_cases, .after = n_obs)

# Select and set clean names of your variables
table_df_pop_weighted |>
  select(all_of(clean_labels$name)) |> 
  tbl_svysummary(
    # by = burnout_interp_dich30,
    # include = c(wfh_exposure, age_cat, sex_en, education_rec_en),
    percent = "column",
    digits = list(gtsummary::all_categorical() ~ c(0, 1)) 
    ,statistic = list(all_categorical() ~ "{n_unweighted} ({p}%)")
  )

# pak::pak("UEP-HUG/UEPtools") # intall the UEPtools package
model_variables_metadata <- table_df_pop |> 
  select(all_of(clean_labels$name)) |> 
  UEPtools::metadata_generator_any(variable_names = clean_labels) |> 
  select(-Variable, -outcomes_type_weighted)

write_csv2(model_variables_metadata, file = here::here("output", "publication figures", "variables_data.csv"))
