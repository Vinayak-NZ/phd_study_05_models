
risk_profile_plot <- 
ggplot(data_complete_cases_risk) +
  geom_bar(aes(x = factor(pre_existing_risk_maternal, level = risk_level_order), 
               fill = factor(pre_existing_risk_fetal, level = risk_level_order),  
               y = (after_stat(count))/sum(after_stat(count))),
           position = "dodge") +
  scale_fill_manual(values = c("#46e7fd", "#e18b22")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = paste0("Characteristics of sample"), 
       subtitle = "Bar chart of risk profile of pregnancy ",
       caption = "Data source: Obstetric Digital Health Intervention", 
       fill = "Risk profile of fetus") +
  xlab("Risk profile of mother") + 
  ylab("Frequency") + 
  facet_wrap(~group_collapse, 
             labeller = labeller(group_collapse = group_collapse.labs)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")) 


complications_delivery_plot <- 
  ggplot(data_agg_complications) +
  geom_bar(aes(x=complications_delivery), fill = "#4739a2") +
  labs(title = paste0("Characteristics of sample"), 
       subtitle = "Bar chart of complications at delivery",
       caption = "Data source: Obstetric Digital Health Intervention") +
  xlab("Complications at delivery") + 
  ylab("Frequency") + 
  facet_wrap(~group_collapse, 
             labeller = labeller(group_collapse = group_collapse.labs)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        legend.key = element_rect(fill = NA),
        legend.key.width = unit(0, "pt"),
        legend.spacing.x = unit(0, "pt")) 

complications_maternal_plot <- 
  ggplot(data_complete_cases_complications) +
  geom_bar(aes(x=complications_maternal), fill = "#4739a2") +
  labs(title = paste0("Characteristics of sample"), 
       subtitle = "Bar chart of complications attributed to mother",
       caption = "Data source: Obstetric Digital Health Intervention") +
  xlab("Complications related to mother") + 
  ylab("Frequency") + 
  facet_wrap(~group_collapse, 
             labeller = labeller(group_collapse = group_collapse.labs)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        legend.key = element_rect(fill = NA),
        legend.key.width = unit(0, "pt"),
        legend.spacing.x = unit(0, "pt")) 
