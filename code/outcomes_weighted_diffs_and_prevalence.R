source(here::here("code", "representative_code.R"))

a_POP <- table_df_pop |> select(burnout_score, burn_out, burnout_interp_dich30) |> mutate(source = "POP")
a_WORK <- table_df_WORK |> select(burnout_score, burn_out, burnout_interp_dich30) |> mutate(source = "WORK")

a_COMB <- bind_rows(a_POP, a_WORK)

# Test for overall differences in the outcomes by population source
summary(glm(data = a_COMB, burnout_score ~ source)) # p = 0.024
summary(glm(data = a_COMB, burnout_interp_dich30 ~ source, family = "binomial")) # p = 0.202
summary(glm(data = a_COMB, burn_out ~ source)) # p = 0.804

table_df_pop |> count(wfh_exposure) |> mutate(perc = n/sum(n))
table_df |> count(wfh, wfh_exposure)


# Get weighted median for EE-MBI and weighted prevalence for severe EE-MBI and burnout
table_df_pop_weighted_analysis <- table_df_pop_weighted |> 
  mutate(burnout_interp_dich30 = case_when(burnout_interp_dich30 == "Severe" ~ 1,
                                           burnout_interp_dich30 == "Not Severe" ~ 0))

table_df_pop_weighted_analysis |> 
  # group_by(wfh_exposure) |> 
  summarise(
    weighted_median = srvyr::survey_quantile(burnout_score, c(0.25, 0.5, 0.75))
  ) |> 
  mutate(
    full_coef = paste0(weighted_median_q50, " (", weighted_median_q25, ", ", weighted_median_q75, ")")
         ) |> 
  relocate(full_coef)

table_df_pop_weighted_analysis |> 
  group_by(
    # wfh_exposure, 
    burnout_interp_dich30) |> 
  summarise(
    survey_mean(vartype = "ci", 
                proportion = TRUE, 
                prop_method = "xlogit")
  ) |> 
  filter(burnout_interp_dich30 == 1) |> 
  mutate(coef = paste0(round(coef*100,1), "%"))

table_df_pop_weighted_analysis |> 
  group_by(
    # wfh_exposure, 
    burn_out) |> 
  summarise(
    survey_mean(vartype = "ci", 
                proportion = TRUE, 
                prop_method = "xlogit")
  ) |> 
  filter(burn_out == 1) |> 
  mutate(coef = paste0(round(coef*100,1), "%"))
