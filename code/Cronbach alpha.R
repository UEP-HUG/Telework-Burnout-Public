# Load packages and dataset ####
pacman::p_load(    # Installs / loads packages if they are not already installed / loaded
  here,         # File locator
  tidyverse,    # data management + ggplot2 graphics
  janitor,      # adding totals and percents to tables
  scales,       # easily convert proportions to percents  
  lubridate,     # manipulate date objects
  psych         # package for Cronbach's alpha
)

# Dataset
dat <- tibble(readRDS(here("data", "Generated datasets", "clean_dataset.rds")))

# Calculate Cronbach's alpha

# Helper function to convert 100 to 0 (not sure why those are coded as 100?)
hund_zero <- function(x) {
  if_else(x == 100, 0, x)
}

d_mbi = dat %>% 
  filter(!is.na(burnout_score)) %>%
  select(
    participant_id,
    starts_with("burn_out_scale_")) %>%
  mutate(across(
    !participant_id,
    hund_zero
    ))

# Run Cronbach's alpha ####
d_mbi %>% select(-participant_id) %>% 
  psych::alpha(cumulative = TRUE)#$total # = 0.94 with no transformation, and 0.93 with basically every other transformation I've tried for normalizing each item distribution

# Check distribution of each item ####
# Make a long table
d_mbi_long <- d_mbi %>% 
  pivot_longer(
  cols = -c("participant_id"
            # , "burnout_score"
  ),
  names_to = "question",
  values_to = "score"
)

# Histograms
d_mbi_long %>% ggplot(aes(x = score))+
  geom_histogram()+
  facet_wrap(.~ question)

# QQ-plots
d_mbi_long %>% ggplot(aes(sample = score))+
  stat_qq(col="red")+ stat_qq_line()+
  facet_wrap(.~question)
