pacman::p_load(
  forestplot
)

source(here::here("code", "new_weighted_regressions.R"))

# Setup ---------------------------------------------------------------------------------------

# Vector of object names from each analysis
results_objects <- c("comb_POP", 
                     "comb_WORK",
                     "comb_SENS_nochange",
                     "comb_SENS_nochange_WORK",
                     "comb_POP_SENS_notpossible",
                     "comb_WORK_SENS_notpossible")

# START of loop for each of the analyses ---------------------------------------------------------
for(k in seq_along(results_objects)){
# Select data
output_file = results_objects[k]
results_df <- get(output_file)


## Function to generate the forestplot ####
make_forestplot <- function(results_df){
plot_outcomes <- vector("list", length = length(outcomes$name))

### Run loop for each outome ####
for (i in seq_along(outcomes$name)) {
  results_df_filtered <- results_df[results_df$outcome == outcomes$name[i],]
  coeff_label <- results_df_filtered$coefficients_label[1]
  
  if(coeff_label == "OR"){
    min.est = plyr::round_any(min(results_df_filtered$estimate, na.rm = TRUE),0.1, floor)
    max.est = ceiling(max(results_df_filtered$estimate, na.rm = TRUE))+1} else {
      min.est = plyr::round_any(min(results_df_filtered$estimate, na.rm = TRUE),0.25, floor)
      max.est = plyr::round_any(max(results_df_filtered$estimate, na.rm = TRUE),0.25, ceiling)
    }
  
  if(coeff_label == "OR"){
    my_ticks = c(
      0.67, 1, 1.5,
      2,3,4,6,8,12
      )
    attr(my_ticks, "labels") <- c(
      "0.67","1","1.5",
      sprintf("%1.0f", c(2,3,4,6,8,12))
      )} else {
      my_ticks = seq(-2,12, 2)
      attr(my_ticks, "labels") <- sprintf("%1.0f", seq(-2,12, 2))
    }
  
  
  plot_outcome <- results_df_filtered[results_df_filtered$outcome == outcomes$name[i],] %>% 
    mutate(
      model = factor(case_when(
        model == "Univariable" ~ "Unadjusted",
        model == "Multivariable" ~ "Adjusted"
      ), levels = c("Adjusted", "Unadjusted")),
      estimate = case_when(ref & coefficients_label == "Beta" ~ 0, 
                           ref & coefficients_label == "OR" ~ 1, .default = estimate),
      conf.low = case_when(ref & coefficients_label == "Beta" ~ 0,
                           ref & coefficients_label == "OR" ~ 1, .default = conf.low),
      conf.high = case_when(ref & coefficients_label == "Beta" ~ 0, 
                            ref & coefficients_label == "OR" ~ 1, .default = conf.high),
      n_nobs_comb = case_when(coefficients_label == "Beta" ~ paste0(n), 
                              .default = paste0(n, " (", n_obs,")"))
    ) |> 
  group_by(model) |> 
    ### Forestplot ####
  forestplot(labeltext = c(term,  
                           n_nobs_comb, 
                           full_estimate_mv, full_estimate_uv),
             fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),  # Set shapes of the symbols
             lty.ci = c(1, 2),                                  # Set the linetypes
             clip = case_when(coeff_label == "Beta" ~ c(-4, 13),
                              .default = c(0.6, 13)),                       # Cut off the x-axis
             vertices = TRUE,
             ci.vertices = TRUE,
             ci.vertices.height = 0.12,
             boxsize = .5,
             graphwidth = unit(2.5, "inches"),   # Set graphwidth
             colgap = unit(3, "mm"),
             line.margin = .1, # Add this to avoid crowding
             xticks = my_ticks,
             xlog = (coeff_label == "OR"),      # Show x-axis on log scale

             # Set the estimate and the confidence intervals
             mean = estimate,
             lower = conf.low,
             upper = conf.high,
             
             # Set alignment of the column values
             align = "llcc",
             # hrzl_lines = list("7" = gpar(lty = 1)),
             # is.summary = ref
             # title = "*ISCO occupation group is adjusted for but not displayed here",
             xlab = case_when(coeff_label == "Beta" ~ "Beta estimate (with 95% CI)",
                              .default = "Odds ratio (with 95% CI)"),
             # 
             # # Set the tick marks on the x-axis as needed
             grid = structure(case_when(coeff_label == "Beta" ~ c(0), .default = c(1)),
                              gp=gpar(lty=1, lwd=1, col = "#999999"))
             # # Optional legend box formatting and position
             , legend_args = fpLegend(
               gp = gpar(col = "transparent", fill = "transparent"),
               padding = unit(1, "mm"),
               pos = list(x = 0.5, y = 0.85)
             )
  ) %>% 
  # Choose position of the forestplot
  fp_decorate_graph(graph.pos = 2) %>% 
  # fp_add_lines(h_3 = gpar(lty = 2)) %>%
  fp_set_style(box = c("#990011FF", "#4271bd"),
               line = c("#990011FF", "#4271bd"),
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
    term = c("","Teleworking group"),
    # new_label = case_when(i == 1 ~ c("(a) EE-MBI score",""),
    #                       i == 2 ~ c("(b) Severe emotional","      exhaustion"),
    #                       i == 3 ~ c("(c) Diagnosed burnout","")) %>% fp_align_left(),
    n_nobs_comb = case_when(coeff_label == "Beta" ~ c(paste0("                "), "N"), 
                            .default = c("N", "(# cases)  ")) %>% fp_align_center(),
    full_estimate_uv = c("Unadjusted", case_when(coeff_label == "Beta" ~ "Beta (95% CI)", 
                                                 .default = "OR (95% CI)")) %>% fp_align_center(),
    full_estimate_mv = c("Adjusted", case_when(coeff_label == "Beta" ~ "Beta (95% CI)", 
                                               .default = "OR (95% CI)")) %>% fp_align_center()
  ) %>%
  fp_set_zebra_style("#EFEFEF")
plot_outcomes[i] <- list(plot_outcome)
}
return(plot_outcomes)
}


## Print the plot ------------------------------------------------------------------------------

fp_object <- make_forestplot(results_df = eval(parse(text = output_file)));fp_object[[1]]
p1 <- fp_object[[1]]
p2 <- fp_object[[2]]
p3 <- fp_object[[3]]

### TIFF output ####
{
  # Open filesave as TIFF 
  tiff(filename = here("output", "publication figures",
                       paste0(format(Sys.time(), "%Y-%m-%d-%H%M_"),output_file, ".tif")),
       width=9, height=7, units = "in", res = 700, compression = "lzw")
  
  # Combine the three figures
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(nrow = 3, ncol = 1, heights=unit(1/3, "npc"), 
                                             just = "left")))
  pushViewport(viewport(layout.pos.row = 1))
  plot(p1)
  grid.text("(a) EE-MBI score", x = unit(0.02, "npc"), y = unit(0.88, "npc"), 
            just = "left")
  popViewport()
  pushViewport(viewport(layout.pos.row = 2))
  plot(p2)
  grid.text("(b) Severe emotional exhaustion", x = unit(0.02, "npc"), y = unit(0.88, "npc"), 
            just = "left")
  popViewport()
  pushViewport(viewport(layout.pos.row = 3))
  plot(p3)
  grid.text("(c) Diagnosed burnout", x = unit(0.02, "npc"), y = unit(0.88, "npc"), 
            just = "left")
  popViewport()
  
  dev.off() # Close the plotting object to complete the filesave
}

### SVG output ####
{
  # Open filesave as SVG 
  svg(filename = here("output", "publication figures",
                       paste0(format(Sys.time(), "%Y-%m-%d-%H%M_"),output_file, ".svg")),
       width=9, height=7, onefile = FALSE)
  
  # Combine the three figures
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(nrow = 3, ncol = 1, heights=unit(1/3, "npc"), 
                                             just = "left")))
  pushViewport(viewport(layout.pos.row = 1))
  plot(p1)
  grid.text("(a) EE-MBI score", x = unit(0.02, "npc"), y = unit(0.88, "npc"), 
            just = "left")
  popViewport()
  pushViewport(viewport(layout.pos.row = 2))
  plot(p2)
  grid.text("(b) Severe emotional exhaustion", x = unit(0.02, "npc"), y = unit(0.88, "npc"), 
            just = "left")
  popViewport()
  pushViewport(viewport(layout.pos.row = 3))
  plot(p3)
  grid.text("(c) Diagnosed burnout", x = unit(0.02, "npc"), y = unit(0.88, "npc"), 
            just = "left")
  popViewport()
  
  dev.off() # Close the plotting object to complete the filesave
}
# END of loop for each of the analyses ####
}