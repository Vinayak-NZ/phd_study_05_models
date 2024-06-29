# ---- outcome-vis

# cost
data_complete_cases_cost_plot <- data_complete_cases

data_complete_cases_cost_plot <- data_complete_cases_cost_plot[!is.na(data_complete_cases_cost_plot$drg_yield), ]

group_collapse.labs <- c("Control", "Intervention")
names(group_collapse.labs) <- c("control", "intervention")

drg_yield_plot <- 
  ggplot(data_complete_cases_cost_plot, aes(x = drg_yield)) +
  geom_histogram(bins = 35, fill = "#4739a2") +
  labs(title = paste0("Characteristics of sample"), 
       subtitle = "Histogram of treatment cost distribution in sample",
       caption = "Data source: Obstetric Digital Health Intervention") +
  xlab("Cost of care (Euro)") + 
  ylab("Frequency") + 
  scale_x_continuous(limits = c(500, 20000), oob = scales::oob_keep) +
  scale_y_continuous(limits = c(0, 20)) +
  facet_wrap(~ group_collapse, 
             labeller = labeller(group_collapse = group_collapse.labs)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )

ggsave("output/drg_yield_plot.png", 
       plot = drg_yield_plot)
