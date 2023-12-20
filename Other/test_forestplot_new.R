pacman::p_load(
  grid,
  tidyverse,
  here,
  forestplot
)

a <- dat2 %>% 
  filter(
    outcome == "burn_out",
    model == "Univariable"
         )

groupnames <- a %>%           # Crude way to ID the group labels and for later coloring
  rowid_to_column() %>% 
  filter(label) ; groupnames <- (groupnames$rowid)
coeff_label = unique(a$family)

if(coeff_label == "OR"){
  min.est = 0.67
  max.est = ceiling(max(a$estimate, na.rm = TRUE))+1} else {
    min.est = floor(min(a$estimate, na.rm = TRUE)-1)
    max.est = ceiling(max(a$estimate, na.rm = TRUE))
  }

if(coeff_label == "OR"){
  my_ticks = c(log(0.67), log(1), log(1.5), sapply(2:max.est, log))
  attr(my_ticks, "labels") <- c("0.67","1","1.5", paste0(2:max.est))} else {
    my_ticks = c(min.est:max.est)
    attr(my_ticks, "labels") <- c(min.est:max.est)
  }

# last_headers <- 


a %>% forestplot(
  labeltext = c(model_levels, full_estimate, n_obs),
  align = "lcl",
  
  # Set the estimate and the confidence intervals
  mean = estimate,
  lower = conf.low,
  upper = conf.high,
  
  # Point and line aesthetics
  boxsize = 0.3,
  ci.vertices = TRUE,
  ci.vertices.height = 0.12,
  clip = c(min.est, max.est), 
  # Set alignment of the column values
  # align = "llll",
  # hrzl_lines = list("7" = gpar(lty = 1)),
  is.summary = label,
  xlab = case_when(coeff_label == "Beta" ~ "Beta estimate (with 95% CI)",
                   .default = "Odds ratio (with 95% CI)"),
  xticks = my_ticks,
  xlog = (coeff_label == "OR")     # Show x-axis on log scale
    ) %>% 
  fp_set_zebra_style("#EFEFEF") %>% 
  fp_set_style(box = c("#4271bd"),
               line = c("#4271bd"),
               # Font settings
               txt_gp = fpTxtGp(summary = gpar(fontface = "bold", cex = 1),
                                label = gpar(cex = 0.9),
                                ticks = gpar(cex = 0.9),
                                xlab = gpar(cex = 1)))

