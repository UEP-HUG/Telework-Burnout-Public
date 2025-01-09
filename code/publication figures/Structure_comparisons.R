pacman::p_load(
  here,
  tidyverse,
  flextable
)
source(here::here("code", "representative_code.R"))
GE_pop_edu_active <- read_csv(here("data", "GE_active_weights.csv"), ) |> select(-1) |>
  rename(age_cat_weighting = age_cat) |> 
  filter(age_cat_weighting %in% c("25-44", "45-64"))

# GE ####
sex_GE <- GE_pop_edu_active |> group_by(sex) |> 
  summarise(n = sum(Freq)) |> 
  ungroup() |> 
  mutate(percent = sprintf("%1.1f", n/sum(n)*100),
         GE = paste0(prettyNum(n, big.mark = ","), " (", percent, "%)")) |> 
  rename(var = 1)

age_GE <- GE_pop_edu_active |> group_by(age_cat_weighting) |> 
  summarise(n = sum(Freq)) |> 
  ungroup() |> 
  mutate(percent = sprintf("%1.1f", n/sum(n)*100),
         GE = paste0(prettyNum(n, big.mark = ","), " (", percent, "%)")) |> 
  rename(var = 1)

education_GE <- GE_pop_edu_active |> group_by(education_rec_fr) |> 
  summarise(n = sum(Freq)) |> 
  ungroup() |> 
  mutate(percent = sprintf("%1.1f", n/sum(n)*100),
         GE = paste0(prettyNum(n, big.mark = ","), " (", percent, "%)")) |> 
  rename(var = 1)

sum_GE <- bind_rows(sex_GE, age_GE, education_GE) |> select(-c(n:percent))


# POP ####
sex_POP <- table_df_pop |> group_by(sex) |> 
  summarise(n = n()) |> 
  ungroup() |> 
  mutate(percent = sprintf("%1.1f", n/sum(n)*100),
         POP = paste0(prettyNum(n, big.mark = ","), " (", percent, "%)")) |> 
  rename(var = 1)

age_POP <- table_df_pop |> group_by(age_cat_weighting) |> 
  summarise(n = n()) |> 
  ungroup() |> 
  mutate(percent = sprintf("%1.1f", n/sum(n)*100),
         POP = paste0(prettyNum(n, big.mark = ","), " (", percent, "%)")) |> 
  rename(var = 1)

education_POP <- table_df_pop |> group_by(education_rec_fr) |> 
  summarise(n = n()) |> 
  ungroup() |> 
  mutate(percent = sprintf("%1.1f", n/sum(n)*100),
         POP = paste0(prettyNum(n, big.mark = ","), " (", percent, "%)")) |> 
  rename(var = 1)

sum_POP <- bind_rows(sex_POP, age_POP, education_POP) |> select(-c(n:percent))


# WORK ####
sex_WORK <- table_df_WORK |> group_by(sex) |> 
  summarise(n = n()) |> 
  ungroup() |> 
  mutate(percent = sprintf("%1.1f", n/sum(n)*100),
         WORK = paste0(prettyNum(n, big.mark = ","), " (", percent, "%)")) |> 
  rename(var = 1)

age_WORK <- table_df_WORK |> group_by(age_cat_weighting) |> 
  summarise(n = n()) |> 
  ungroup() |> 
  mutate(percent = sprintf("%1.1f", n/sum(n)*100),
         WORK = paste0(prettyNum(n, big.mark = ","), " (", percent, "%)")) |> 
  rename(var = 1)

education_WORK <- table_df_WORK |> group_by(education_rec_fr) |> 
  summarise(n = n()) |> 
  ungroup() |> 
  mutate(percent = sprintf("%1.1f", n/sum(n)*100),
         WORK = paste0(prettyNum(n, big.mark = ","), " (", percent, "%)")) |> 
  rename(var = 1)

sum_WORK <- bind_rows(sex_WORK, age_WORK, education_WORK) |> select(-c(n:percent))

# Combined ####
sum_combined <- sum_GE |> left_join(sum_POP) |> left_join(sum_WORK) |> 
  mutate(var = case_match(
    var,
    "Femme" ~ "Female",
    "Homme" ~ "Male",
    "Tertiaire" ~ "Tertiary",
    "Secondaire" ~ "Secondary",
    "Primaire" ~ "Primary",
    .default = var
    ))

write_csv2(sum_combined, here::here("output", "publication figures", "structure_comparisons.csv"))
