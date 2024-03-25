raw_data_one_group_drg$group <- ifelse(raw_data_one_group_drg$year == 2018, "pre-intervention", "post-intervention")

raw_data_one_group_drg_subset <- raw_data_one_group_drg[, c("group", 
                                                      "drg_yield_total", 
                                                      "internal_cost")]

data_complete_cases <- raw_data_one_group_drg_subset[complete.cases(raw_data_one_group_drg_subset),]

data.table::setDT(data_complete_cases)

data_cost <- data_complete_cases[, .(reimbursement = sum(drg_yield_total), 
                                            actual_spend = sum(internal_cost)), 
                                        by = .(group)]

data_cost_long <- data.table::melt(data_cost, 
                                   id.vars = c("group"), 
                                   variable.name = "cost_type", 
                                   value.name = "cost")

level_order = c('pre-intervention', 'post-intervention')

ggplot(data_cost_long, 
       aes(x= factor(group, level = level_order), y = cost, fill = cost_type)) + 
  geom_bar(stat="identity", position = "dodge") + 
  labs(title = paste0("Total medical costs"), 
       subtitle = "Barplot of costs before and after intervention", 
       caption = "Data source: Aktionbündis Patientensicherheit", 
       fill='Cost type') +
  xlab("Period of data collection") + ylab("Costs (in €)") + 
  scale_fill_manual(values=c("#245F8A", "#5B8AAA", "#D7E3EC")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#E13530", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#205B87"),
        plot.caption = element_text(color = "#5B8AAA", face = "italic")
  )

data.table::setDT(data_complete_cases)

data_cost <- data_complete_cases[, .(reimbursement = mean(drg_yield_total), 
                                            actual_spend = mean(internal_cost)), 
                                        by = .(group)]

data_cost_long <- data.table::melt(data_cost, 
                                   id.vars = c("group"), 
                                   variable.name = "cost_type", 
                                   value.name = "cost")

level_order = c('pre-intervention', 'post-intervention')

ggplot(data_cost_long, 
       aes(x=factor(group, level = level_order), y = cost, fill = cost_type)) + 
  geom_bar(stat="identity", position = "dodge") + 
  labs(title = paste0("Average medical costs"), 
       subtitle = "Barplot of costs before and after intervention", 
       caption = "Data source: Aktionbündis Patientensicherheit", 
       fill = "Cost type") +
  xlab("Period of data collection") + ylab("Costs (in €)") + 
  scale_fill_manual(values=c("#245F8A", "#5B8AAA", "#D7E3EC")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#E13530", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#205B87"),
        plot.caption = element_text(color = "#5B8AAA", face = "italic")
  )
