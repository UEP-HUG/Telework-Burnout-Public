# Descriptive tables for the publication
# Load packages and dataset ####
pacman::p_load(    # Installs / loads packages if they are not already installed / loaded
  here,         # File locator
  tidyverse,    # data management + ggplot2 graphics 
  gtsummary,    # summary statistics and tests
  gt,
  janitor,      # adding totals and percents to tables
  scales,       # easily convert proportions to percents  
  flextable,    # converting tables to pretty images
  # devEMF,       # Save vectorized figures as EMF
  lubridate,     # manipulate date objects
  forcats
)

dat <- readRDS(here("data", "Generated datasets", "clean_dataset_incl_salariee.rds")) %>% 
  mutate(work_situation.y_en = case_match(
    work_situation.y,
    "En activité salariée" ~ "Salaried employee",
    "En arrêt de travail de longue durée (pour cause de maladie ou accident), à l’AI (assurance-invalidité), en invalidité" ~ "Long-term absence",
    "En congé parental/maternité ou paternité" ~ "Parental leave",
    "En recherche d’emploi" ~ "Searching for a job",
    "En congé sans solde ou pour convenances personnelles" ~ "Personal / unpaid leave",
    "A la retraite depuis moins de 12 mois" ~ "Retired within last 12 months",
    "En formation depuis moins de 12 mois" ~ "In training",
    "En indépendant-e (« freelance ») depuis moins de 12 mois" ~ "Independent / Freelance",
    "Other" ~ "Other"
  ))

nsal_burnout <- dat %>% group_by(work_situation.y_en) %>%
  summarise(mburn = mean(burn_out, na.rm = TRUE),
            N = n(),
            perc_b = paste0(round(mburn,3)*100, "%")) %>% 
  select(-mburn) %>% 
  arrange(-N) %>%
  as_flextable(show_coltype = FALSE) %>% 
  set_header_labels(
    work_situation.y_en = "Work situation",
    N = "N",
    perc_b = "Diagnosed burnout"
  ) %>% 
  theme_zebra() %>% 
  align(i = 1, align = "center", part = "header")


# Save as docx ####
save_as_docx(
  "Table S" = nsal_burnout, path = here("output", "publication figures", paste0(format(Sys.time(), "%Y-%m-%d-%H%M_"),"Nonsalaried_burnout.docx")))

dat |> count(work_situation.y_en != "Salaried employee")
dat |> group_by(work_situation.y_en != "Salaried employee") |> 
  summarise(
    mburn = mean(burn_out, na.rm = TRUE),
    N = n(),
    perc_b = paste0(round(mburn,4)*100, "%")
  )
