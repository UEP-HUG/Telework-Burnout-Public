# One file to bring them all

pacman::p_load(
  tidyverse,
  gtsummary,
  forestplot,
  here,
  gridExtra,
  forcats,
  svglite,
  devEMF # print figures into a vectorized format
)

# Section that needs your input -------------------------------------------

# Dataset based on variables from new DAG
dat <- tibble(readRDS(here("data", "Generated datasets", "clean_dataset.rds")))
# Default name for the output file
output_file = "Main_regression_figure_TW"

## Sensitivity analyses ####
# Uncomment one sensitivity section at a time and then run whole doc
# Don't forget to also uncomment respective output_file, so figure name shows correctly

### Compare never telework to not possible ####
dat <- dat %>% mutate(wfh_exposure = relevel(wfh_exposure, ref = "Never"))
output_file = "Sensitivity_regression_figure_NeverTW"


### Remove WORK sample (keep population-based only) ####
# dat <- dat %>% filter(!pop_source %in% c("SEROCoV-WORK", "work_pilote"))
# output_file = "Sensitivity_regression_figure_noWORK"


### remove participants with contract changes ####
# dat <- dat %>% filter(percent_change_cat_r %in% c("No change"))
# output_file = "Sensitivity_regression_figure_NOCHANGE"

## Regression variable selection ####
### Outcomes ####
# Make sure your logistic regression outcomes are binary (1/0)
dat <- dat %>% mutate(
  burnout_interp_dich30 = case_when(
    burnout_interp_dich30 == "Severe" ~ 1,
    burnout_interp_dich30 == "Not Severe" ~ 0,
    .default = NA),
  burnout_interp_dich27 = case_when(
    burnout_interp_dich27 == "Severe" ~ 1,
    burnout_interp_dich27 == "Not Severe" ~ 0,
    .default = NA))


# Enter the variable for your outcome
outcomes <- c("burnout_score","burnout_interp_dich30", "burn_out")
# Set the family that will be used in the glm models
outcomes_type <- c("gaussian", "binomial", "binomial")
outcomes_table <- tibble(outcomes, outcomes_type)

### Covariates ####
# Specify exposure and covariates
source(here("code", "Regression_covariates.R"))
# For the NO CHANGE sensitivity analysis, need to drop the percent_change_cat_r variable
if(output_file == "Sensitivity_regression_figure_NOCHANGE"){
  covariates = covariates[ !covariates == 'percent_change_cat_r']} else {
  covariates = covariates}

# Run below line if you want to get the data from file
if(output_file == "Main_regression_figure_TW"){
plot_data_r <- readRDS(here("data", "Generated datasets", "main_plot_data.rds"))
}

if(output_file == "Sensitivity_regression_figure_NeverTW"){
  plot_data_r <- readRDS(here("data", "Generated datasets", "NeverTW_plot_data.rds"))
}

if(output_file == "Sensitivity_regression_figure_noWORK"){
  plot_data_r <- readRDS(here("data", "Generated datasets", "noWORK_plot_data.rds"))
}

if(output_file == "Sensitivity_regression_figure_NOCHANGE"){
  plot_data_r <- readRDS(here("data", "Generated datasets", "NOCHANGE_plot_data.rds"))
}
# Then can skip to forestplots section

### ### ### ### ### ### ### ### ### ### ### ### ### ###
# Logistic regression estimates from loop ####
### ### ### ### ### ### ### ### ### ### ### ### ### ###
plot_data <- vector("list", length = length(outcomes))
for (i in 1:length(outcomes)) {
  
  ##Univariable logistic regression models ####
  uv_model <- dat %>% 
    select(all_of(outcomes[i]), all_of(exposure), all_of(covariates)) %>% 
    tbl_uvregression(
      method = glm,           # Set the family
      exponentiate = (outcomes_type[i] == "binomial"),
      y = outcomes[i],
      method.args = list(family = outcomes_type[i])
    )
  
  ##Multivariable logistic regression model ####
  # Create model
  mv_model <- c(exposure, covariates) %>%     ## begin with vector of exposure and covariate column names
    str_c(collapse = "+") %>%                 ## combine all names of the variables of interest separated by a plus
    str_c(outcomes[i]," ~ ", .) %>%        ## combine the names of variables of interest with outcome in formula style
    glm(family = outcomes_type[i],                  ## define the family as binomial for logistic regression
        data = dat) %>%                         ## define your dataset
    tbl_regression(exponentiate = (outcomes_type[i] == "binomial"))
  
  
  ### ### ### ### ### ### ### ### ### ### ### ### ###
  ## Extract estimates and CIs from the models ####
  ### ### ### ### ### ### ### ### ### ### ### ### ###
  
  cols <- c(n_event = NA) # Add an n_event column for the uv_estimates (see add_column below)
  
  ### UV model estimates ####
  uv_estimates <- uv_model$table_body %>% # Extract the estimates table
    left_join(clean_labels) %>%  # Clean the variable names
    add_column(!!!cols[!names(cols) %in% names(.)]) %>% # adds the new column only if it doesn't already exist
    select(clean_name, var_type,  reference_row, row_type, N,
           label, n_obs, n_event, coefficients_label, estimate, conf.low,
           conf.high, ci, p.value)  %>%
    mutate(
      ci = case_when(
        coefficients_label == "Beta" ~ trimws(paste0(format(round(conf.low,2), nsmall = 2),"," ,format(round(conf.high,2), nsmall = 2))),
        .default = ci),
      row_type = case_when(row_type == "label" ~ TRUE, .default = FALSE),  # set as TRUE/FALSE for the aesthetics later in forestplots
      model = "Unadjusted",
      new_label = case_when(         # Create a new clean label to take into account the variable label and reference lines
        row_type ~ clean_name,
        reference_row == TRUE ~ paste0("    ", label),
        .default = paste0("    ",label)),
      is.summary = row_type,
      N_uv = if_else(reference_row == TRUE, N, NA), # Get N from the UV models
      N_mv = NA,   # same for MV but will be filled in later steps
      n_comb_uv = case_when(row_type ~ NA,
                            coefficients_label == "Beta" ~ paste0(n_obs),
                            .default = paste0(n_obs, " (",n_event,")")),   # combine UV N and N event into a single variable      n_comb_mv = NA,  # Same for mv, but it's empty here and will be filled in later steps
      n_perc_uv = case_when(row_type ~ NA,
                            coefficients_label == "Beta" ~ paste0(n_obs),
                            .default = paste0(n_obs, " (",round(n_event / n_obs *100, 1), "%)")),
      n_perc_mv = NA,
      p.sig = case_when(                         # show p-values symbolically
        reference_row ~ NA,
        p.value < 0.001 ~ "***",
        p.value < 0.01 ~ "**",
        p.value <= 0.05 ~ "*",
        .default = NA
      ),
      full_estimate_uv = case_when(            # combine OR, CI, and p-value symbol in one variable
        reference_row == TRUE ~ "(ref)",
        n_event == 0 ~ "NA",
        is.na(p.sig) & reference_row == FALSE ~ paste0(format(round(estimate,2), nsmall = 2), " (", ci,")"),
        reference_row == FALSE ~ paste0(format(round(estimate,2), nsmall = 2), " (", ci,")",p.sig),
      ),
      full_estimate_mv = NA                    # create empty mv variable to be filled later (vice versa for the mv table)
    )
  
  
  ### MV Model estimates ####
  mv_estimates <- mv_model$table_body %>% # Extract the estimates table
    left_join(clean_labels) %>%  # Clean the variable names
    add_column(!!!cols[!names(cols) %in% names(.)]) %>% 
    select(clean_name, var_type,  reference_row, row_type, N, 
           label, n_obs, n_event, coefficients_label, estimate, conf.low, 
           conf.high, ci, p.value) %>% 
    mutate(
      ci = case_when(
        coefficients_label == "Beta" ~ trimws(paste0(format(round(conf.low,2), nsmall = 2),"," ,format(round(conf.high,2), nsmall = 2))),
        .default = ci),
      row_type = case_when(row_type == "label" ~ TRUE, .default = FALSE),
      model = "Adjusted",
      new_label = case_when(         # Create a new label to take into account the reference lines
        row_type ~ clean_name,
        reference_row == TRUE ~ paste0("    ", label),
        .default = paste0("    ",label)),
      is.summary = row_type,
      N_uv = NA,
      N_mv = if_else(reference_row == TRUE, N, NA),
      n_comb_uv = NA,
      n_comb_mv = case_when(row_type ~ NA,
                            coefficients_label == "Beta" ~ paste0(n_obs),
                            .default = paste0(n_obs, " (",n_event,")")),   # combine UV N and N event into a single variable      
      n_perc_uv = NA,
      n_perc_mv = case_when(row_type ~ NA,
                            coefficients_label == "Beta" ~ paste0(n_obs),
                            .default = paste0(n_obs, " (",round(n_event / n_obs *100, 1), "%)")),
      p.sig = case_when(
        reference_row ~ NA,
        p.value < 0.001 ~ "***",
        p.value < 0.01 ~ "**",
        p.value <= 0.05 ~ "*",
        .default = NA
      ),
      full_estimate_uv = NA,
      full_estimate_mv = case_when(
        reference_row == TRUE ~ "(ref)",
        n_event == 0 ~ "NA",
        is.na(p.sig) & reference_row == FALSE ~ paste0(format(round(estimate,2), nsmall = 2), " (", ci,")"),
        reference_row == FALSE ~ paste0(format(round(estimate,2), nsmall = 2), " (", ci,")",p.sig),
      )
    )
  
  
  ## fill in the empty rows for the two tables' full estimates ####
  # (they need to be identical for the combined forest plot)
  uv_estimates <- uv_estimates %>% mutate(full_estimate_mv = mv_estimates$full_estimate_mv
                                          , n_comb_mv = mv_estimates$n_comb_mv
                                          , N_mv = mv_estimates$N_mv
                                          , n_perc_mv = mv_estimates$n_perc_mv
  )
  mv_estimates <- mv_estimates %>% mutate(full_estimate_uv = uv_estimates$full_estimate_uv
                                          , n_comb_uv = uv_estimates$n_comb_uv
                                          , N_uv = uv_estimates$N_uv
                                          , n_perc_uv = uv_estimates$n_perc_uv
  )
  
  # Row bind the two tables to make a final combined table
  combi <- rbind(uv_estimates, mv_estimates)
  # Optional remove p-value symbols if you don't need them
  combi <- combi %>% mutate(full_estimate_uv = str_remove_all(full_estimate_uv, "[*]"),
                            full_estimate_mv = str_remove_all(full_estimate_mv, "[*]"))
  
  combi <- combi %>% mutate(
    estimate = case_when(
      reference_row & coefficients_label == "OR" ~ 1,
      reference_row & coefficients_label == "Beta" ~0,
      .default = estimate),
    conf.low = case_when(
      reference_row & coefficients_label == "OR" ~ 1,
      reference_row & coefficients_label == "Beta" ~0,
      .default = conf.low),
    conf.high = case_when(
      reference_row & coefficients_label == "OR" ~ 1,
      reference_row & coefficients_label == "Beta" ~0,
      .default = conf.high)
  )
  # Store data for each of the outcomes in a list
  plot_data[i] <- list(combi)
}

# Save the main plot data so you don't have to always run it
if(output_file == "Main_regression_figure_TW"){
  saveRDS(plot_data, here("data", "Generated datasets", "main_plot_data.rds"))
}

# Save the main plot data so you don't have to always run it
if(output_file == "Sensitivity_regression_figure_NeverTW"){
  saveRDS(plot_data, here("data", "Generated datasets", "NeverTW_plot_data.rds"))
}

# Save the main plot data so you don't have to always run it
if(output_file == "Sensitivity_regression_figure_noWORK"){
  saveRDS(plot_data, here("data", "Generated datasets", "noWORK_plot_data.rds"))
}

# Save the main plot data so you don't have to always run it
if(output_file == "Sensitivity_regression_figure_NOCHANGE"){
  saveRDS(plot_data, here("data", "Generated datasets", "NOCHANGE_plot_data.rds"))
}

plot_data_r <- plot_data

# See this example: https://www.rdocumentation.org/packages/forestplot/versions/3.1.3/topics/forestplot

# Forest plots in a loop full model ####
plot_outcomes <- vector("list", length = length(outcomes))
for (i in 1:length(outcomes)) {
  coeff_label <- plot_data_r[[i]]$coefficients_label[1]
  # Set the ticks on log scale for logistic regression
  if(coeff_label == "OR"){
    my_ticks = c(log(0.67), log(1), log(1.5), log(2), log(3),log(4),log(5), log(6))
    attr(my_ticks, "labels") <- c("0.67","1","1.5", "2","3","4","5","6")} else {
      my_ticks = c(-1:6)
      attr(my_ticks, "labels") <- c(-1:6)
    }
  plot_outcome <- plot_data_r[[i]] %>% group_by(model) %>%
  filter(clean_name %in% c("Teleworking group")) %>% # Can restrict figure to only some of the variables
  # filter(!clean_name %in% c("ISCO Occupation label")) %>% # Can restrict figure to only some of the variables
  forestplot(labeltext = c(new_label,  
                           n_perc_uv, full_estimate_uv, 
                           n_perc_mv, full_estimate_mv),
             fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),  # Set shapes of the symbols
             lty.ci = c(1, 2),                                  # Set the linetypes
             clip = case_when(coeff_label == "Beta" ~ c(-1, 6.2),
                              .default = c(0.6, 6.2)),                               # Cut off the x-axis
             vertices = TRUE,
             ci.vertices = TRUE,
             ci.vertices.height = 0.12,
             boxsize = .5,
             graphwidth = unit(2.5, "inches"),   # Set graphwidth
             colgap = unit(3, "mm"),
             line.margin = .1, # Add this to avoid crowding
             xlog = (coeff_label == "OR"),      # Show x-axis on log scale
             
             # Set the estimate and the confidence intervals
             mean = estimate,
             lower = conf.low,
             upper = conf.high,
             
             # Set alignment of the column values
             align = "llclc",
             # hrzl_lines = list("7" = gpar(lty = 1)),
             is.summary = row_type,
             # title = "*ISCO occupation group is adjusted for but not displayed here",
             xlab = case_when(coeff_label == "Beta" ~ "Beta estimate (with 95% CI)",
                              .default = "Odds ratio (with 95% CI)"),
             
             # Set the tick marks on the x-axis as needed
             xticks = my_ticks,
             grid = structure(case_when(coeff_label == "Beta" ~ c(0), .default = c(1)), 
                              gp=gpar(lty=1, lwd=1, col = "#999999"))
             # Optional legend box formatting and position
             , legend_args = fpLegend(
               gp = gpar(col = "transparent", fill = "transparent"),
               padding = unit(1, "mm"),
               pos = list(x = 0.5, y = 0.75)
             )
  ) %>% 
  # Choose position of the forestplot
  fp_decorate_graph(graph.pos = 2) %>% 
  # fp_add_lines(h_3 = gpar(lty = 2)) %>%
  fp_set_style(box = c("#4271bd", "#990011FF"),
               line = c("#4271bd", "#990011FF"),
               # Font settings
               txt_gp = fpTxtGp(summary = gpar(fontface = "bold", cex = 1),
                                label = gpar(cex = 0.9),
                                ticks = gpar(cex = 0.9),
                                xlab = gpar(cex = 1)
               )
  ) %>%
  # Add lines for each group
  fp_add_lines("#999999") %>%
  # Name the headers
  fp_add_header(
    new_label = c("",""),
    # new_label = case_when(i == 1 ~ c("(a) EE-MBI score",""),
    #                       i == 2 ~ c("(b) Severe emotional","      exhaustion"),
    #                       i == 3 ~ c("(c) Diagnosed burnout","")) %>% fp_align_left(),
    n_perc_uv = case_when(coeff_label == "Beta" ~ c(paste0("                "), "N"), .default = c("N", "(% cases)  ")) %>% fp_align_center(),
    n_perc_mv = case_when(coeff_label == "Beta" ~ c(paste0("                "), "N"), .default = c("N", "(% cases)  ")) %>% fp_align_center(),
    full_estimate_uv = c("Unadjusted", case_when(coeff_label == "Beta" ~ "Beta (95% CI)", .default = "OR (95% CI)")) %>% fp_align_center(),
    full_estimate_mv = c("Adjusted", case_when(coeff_label == "Beta" ~ "Beta (95% CI)", .default = "OR (95% CI)")) %>% fp_align_center()
  ) %>%
  fp_set_zebra_style("#EFEFEF"); plot_outcome
  plot_outcomes[i] <- list(plot_outcome)
  # rm(plot_outcome)
}

p1 <- plot_outcomes[[1]]
p2 <- plot_outcomes[[2]]
p3 <- plot_outcomes[[3]]

# Print the plot ####
{
emf(here("output", "publication figures", 
                paste0(format(Sys.time(), "%Y-%m-%d-%H%M_"),output_file, ".emf")),
    width = 9, height = 7, bg = "white")
## Combine the three figures
grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow = 3, ncol = 1, heights = 2)))
pushViewport(viewport(layout.pos.row = 1))
plot(p1)
grid.text("(a) EE-MBI score", x = unit(0.02, "npc"), y = unit(0.88, "npc"), just = "left")
popViewport()
pushViewport(viewport(layout.pos.row = 2))
plot(p2)
grid.text("(b) Severe emotional exhaustion", x = unit(0.02, "npc"), y = unit(0.88, "npc"), just = "left")
popViewport()
pushViewport(viewport(layout.pos.row = 3))
plot(p3)
grid.text("(c) Diagnosed burnout", x = unit(0.02, "npc"), y = unit(0.88, "npc"), just = "left")
popViewport()
dev.off()
}
 
