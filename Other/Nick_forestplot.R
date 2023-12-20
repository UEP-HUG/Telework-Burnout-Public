# this dataset is generated after having run run the "Main_regression_figures_Telework.R file at least once
plot_data_r <- readRDS(here("data", "Generated datasets", "main_plot_data.rds"))

# Nick's fig ####
pacman::p_load(ggtext)# for better text rendering support
m1_lin_adj_dat = plot_data[[1]] |>
  filter(model=="Adjusted", clean_name == "Teleworking group") |>
  drop_na(n_obs) |>
  select(label, n_obs, estimate, conf.low, conf.high, full_estimate_mv)
ggplot(m1_lin_adj_dat,
       aes(x = estimate,
           y = fct_rev(fct_inorder(label)))) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), colour = "#990011FF", height = 0.25, linewidth = 1.2) +
  geom_point(colour = "#990011FF", size = 5) +
  geom_segment(data = tibble(x = 0, xend = 0, y = 0.8, yend = 4),
               mapping = aes(x=x,y=y,xend=xend,yend=yend)) +
  geom_richtext(x = 8, aes(label = full_estimate_mv), fill = NA, label.colour = NA, size = 4.5, colour = "grey30") +
  geom_richtext(x = 11, aes(label = n_obs), fill = NA, label.colour = NA, size = 4.5, colour = "grey30") +
  theme_minimal(18) +
  scale_x_continuous(limits = c(NA, 12), breaks = seq(-1,6)) +
  labs(y = NULL, x = "Beta estimate (with 95% CI)") +
  theme(panel.grid = element_blank(), axis.title.x = element_text(hjust = 0.12)) +
  coord_cartesian(xlim = c(min(m1_lin_adj_dat$conf.low, na.rm = TRUE), 12), ylim = c(1.4,5.4), clip = "off") +
  annotate("richtext", x = -3, y = 5.8, size = 4.5, colour = "grey10",
           label = "**Teleworking<br>group**", fill = NA, label.colour = NA) +
  annotate("richtext", x = 11, y = 5.8, size = 4.5, colour = "grey10",
           label = "**N**", fill = NA, label.colour = NA) +
  annotate("richtext", x = 8, y = 5.8, size = 4.5, colour = "grey10",
           label = "**Adjusted<br>Beta (95% CI)**", fill = NA, label.colour = NA)
 ggsave(here::here("output", paste0(format(Sys.time(), "%Y-%m-%d-%H%M_"),"nick_m1_lin_adj.pdf")),
        width = 7, height = 6)

# Nick's fig_Anshu ####
pacman::p_load(ggtext)# for better text rendering support

m1_lin_adj_dat = plot_data[[1]] |>
  filter(model == "Adjusted" & clean_name == "Teleworking group") |>
  drop_na(n_obs) |>
  select(model,label, n_obs, estimate, conf.low, conf.high, full_estimate_mv, full_estimate_uv)

ggplot(data = m1_lin_adj_dat,
       aes(x = estimate,
           color = model,
           y = fct_rev(fct_inorder(label)))) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, linetype = model), height = 0.4, linewidth = 1.2,
                 position = position_dodge(width = 0.7)) +
  annotate("rect", xmin = -6, xmax = 18, ymin = 1.5, ymax = 2.5,
           alpha = .1)+
  annotate("rect", xmin = -6, xmax = 18, ymin = 3.5, ymax = 4.5,
           alpha = .1)+
  scale_linetype_manual(values=c("Adjusted" = 2,
                               "Unadjusted" =  1,
                               "Reference" = 0))+
   geom_point(size = 5, position = position_dodge(width = 0.7), aes(shape = model)) +
   scale_shape_manual(values=c("Adjusted" = 16,
                               "Unadjusted" =  15,
                               "Reference" = 18))+
   geom_segment(mapping = aes(x=0,y=0.7,xend=0,yend=5.2), color = "black") +
   scale_color_manual(values = c("Adjusted" = "#990011FF",
                                 "Unadjusted" =  "#4271bd",
                                 "Reference" = "grey30"))+
   geom_point(aes(x=0,y=4.9),colour="grey30", size = 6, shape = 18)+
   geom_richtext(data = m1_lin_adj_dat %>% filter(model == "Unadjusted"),
                 x = 8, aes(label = full_estimate_uv), fill = NA, label.colour = NA, size = 4.5, colour = "grey30") +
   geom_richtext(data = m1_lin_adj_dat %>% filter(model == "Unadjusted"),
                 x = 11, aes(label = n_obs), fill = NA, label.colour = NA, size = 4.5, colour = "grey30") +
   geom_segment(mapping = aes(x=12,y=0.8,xend=12,yend=6), color = "grey30") +
   geom_richtext(data = m1_lin_adj_dat %>% filter(model == "Adjusted"),
                 x = 14, aes(label = full_estimate_mv), fill = NA, label.colour = NA, size = 4.5, colour = "grey30") +
   geom_richtext(data = m1_lin_adj_dat %>% filter(model == "Adjusted"),
                 x = 17, aes(label = n_obs), fill = NA, label.colour = NA, size = 4.5, colour = "grey30") +
   theme_minimal(18) +
   scale_x_continuous(limits = c(NA, 18), breaks = seq(-1,6)) +
   labs(y = NULL, x = "Beta estimate (with 95% CI)") +
   theme(panel.grid = element_blank(), axis.title.x = element_text(hjust = 0.1)) +
   coord_cartesian(xlim = c(min(m1_lin_adj_dat$conf.low, na.rm = TRUE), 18), ylim = c(1.4,5.4), clip = "off") +
   annotate("richtext", x = -3, y = 5.8, size = 4.5, colour = "grey10",
            label = "**Teleworking<br>group**", fill = NA, label.colour = NA) +
   annotate("richtext", x = 8, y = 5.8, size = 4.5, colour = "grey10",
          label = "**Unadjusted<br>Beta (95% CI)**", fill = NA, label.colour = NA) +
   annotate("richtext", x = 11, y = 5.8, size = 4.5, colour = "grey10",
            label = "**N**", fill = NA, label.colour = NA) +
   annotate("richtext", x = 14, y = 5.8, size = 4.5, colour = "grey10",
            label = "**Adjusted<br>Beta (95% CI)**", fill = NA, label.colour = NA) +
   annotate("richtext", x = 17, y = 5.8, size = 4.5, colour = "grey10",
            label = "**N**", fill = NA, label.colour = NA)+
   theme(legend.title = element_blank(),
         legend.position = c(0.25,0.98))
#
ggsave(here::here("output", paste0(format(Sys.time(), "%Y-%m-%d-%H%M_"),"nick_anshu.svg")),
        width = 10, height = 3)


## Put it into a loop ####
plot_nick <- vector("list", length = length(outcomes))
for (i in 1:length(outcomes)) {
  coeff_label <- plot_data[[i]]$coefficients_label[1]

  m1_lin_adj_dat = plot_data[[i]] |>
    filter(clean_name == "Teleworking group") |>
    drop_na(n_obs) |>
    select(model,label, n_obs, estimate, conf.low, conf.high, full_estimate_mv, full_estimate_uv)

  ggplot(data = m1_lin_adj_dat,
         aes(x = estimate,
             color = model,
             y = fct_rev(fct_inorder(label)))) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, linetype = model), height = 0.4, linewidth = 1.2,
                   position = position_dodge(width = 0.7)) +
    annotate("rect", xmin = 0, xmax = 18, ymin = 1.5, ymax = 2.5,
             alpha = .1)+
    annotate("rect", xmin = 0, xmax = 18, ymin = 3.5, ymax = 4.5,
             alpha = .1)+
    scale_linetype_manual(values=c("Adjusted" = 2,
                                   "Unadjusted" =  1,
                                   "Reference" = 0))+
    geom_point(size = 5, position = position_dodge(width = 0.7), aes(shape = model)) +
    scale_shape_manual(values=c("Adjusted" = 16,
                                "Unadjusted" =  15,
                                "Reference" = 18))+
    geom_segment(mapping = aes(x = 1,y=0.7,xend=if_else(coeff_label == "Beta", 0, 1),yend=5.2), color = "black") +
    scale_color_manual(values = c("Adjusted" = "#990011FF",
                                  "Unadjusted" =  "#4271bd",
                                  "Reference" = "grey30"))+
    geom_point(aes(x=if_else(coeff_label == "Beta", 0, 1),y=4.9),colour="grey30", size = 6, shape = 18)+
    geom_richtext(data = m1_lin_adj_dat %>% filter(model == "Unadjusted"),
                  x = 8, aes(label = full_estimate_uv), fill = NA, label.colour = NA, size = 4.5, colour = "grey30") +
    geom_richtext(data = m1_lin_adj_dat %>% filter(model == "Unadjusted"),
                  x = 11, aes(label = n_obs), fill = NA, label.colour = NA, size = 4.5, colour = "grey30") +
    geom_segment(mapping = aes(x=12,y=0.8,xend=12,yend=6), color = "grey30") +
    geom_richtext(data = m1_lin_adj_dat %>% filter(model == "Adjusted"),
                  x = 14, aes(label = full_estimate_mv), fill = NA, label.colour = NA, size = 4.5, colour = "grey30") +
    geom_richtext(data = m1_lin_adj_dat %>% filter(model == "Adjusted"),
                  x = 17, aes(label = n_obs), fill = NA, label.colour = NA, size = 4.5, colour = "grey30") +
    theme_minimal(18) +
    scale_x_continuous(limits = c(-1, 26),
                       trans = "log10",
                       breaks = c(0.67, 1,1.5,2,3,4,5,6)
                       # breaks = c(log(0.5), log(0.67),log(1), log(2), log(3),
                       #                                log(4), log(5), log(6))
                       ) +
    labs(y = NULL, x = if_else(coeff_label == "Beta", "Beta estimate (with 95% CI)", "Odds ratio (with 95% CI)")) +
    theme(panel.grid = element_blank(), axis.title.x = element_text(hjust = 0.1)) +
    coord_cartesian(xlim = c(min(m1_lin_adj_dat$conf.low, na.rm = TRUE), 18), ylim = c(1.4,5.4), clip = "off") +
    annotate("richtext", x = 0, y = 5.8, size = 4.5, colour = "grey10",
             label = "**Teleworking<br>group**", fill = NA, label.colour = NA) +
    annotate("richtext", x = 8, y = 5.8, size = 4.5, colour = "grey10",
             label = if_else(coeff_label == "Beta", "**Unadjusted<br>Beta (95% CI)**", "**Unadjusted<br>OR (95% CI)**"),
             fill = NA, label.colour = NA) +
    annotate("richtext", x = 11, y = 5.8, size = 4.5, colour = "grey10",
             label = "**N**", fill = NA, label.colour = NA) +
    annotate("richtext", x = 14, y = 5.8, size = 4.5, colour = "grey10",
             label = if_else(coeff_label == "Beta", "**Adjusted<br>Beta (95% CI)**", "**Adjusted<br>OR (95% CI)**"),
             fill = NA, label.colour = NA) +
    annotate("richtext", x = 17, y = 5.8, size = 4.5, colour = "grey10",
             label = "**N**", fill = NA, label.colour = NA)+
    theme(legend.title = element_blank(),
          legend.position = c(0.25,0.98))
}