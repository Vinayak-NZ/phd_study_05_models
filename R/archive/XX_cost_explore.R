## ---- compare-total-costs
data_complete_cases_subset <- data_complete_cases[, c("group_collapse", 
                                                      "drg_yield", 
                                                      "internal_cost")]

# by treatment group (total costs)
data.table::setDT(data_complete_cases_subset)

data_cost <- data_complete_cases_subset[, .(reimbursement = sum(drg_yield), 
                                     actual_spend = sum(internal_cost)), 
                                 by = .(group_collapse)]

data_cost_long <- data.table::melt(data_cost, 
                                   id.vars = c("group_collapse"), 
                                   variable.name = "cost_type", 
                                   value.name = "cost")

ggplot(data_cost_long, 
       aes(x=group_collapse, y = cost, fill = cost_type)) + 
  geom_bar(stat="identity", position = "dodge") + 
  labs(title = paste0("Total medical costs"), 
       subtitle = "Barplot of costs by treatment group", 
       caption = "Data source: Aktionbündis Patientensicherheit", 
       fill='Cost type') +
  xlab("Treatment group") + ylab("Costs (in €)") + 
  scale_fill_manual(values=c("#245F8A", "#5B8AAA", "#D7E3EC")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#E13530", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#205B87"),
        plot.caption = element_text(color = "#5B8AAA", face = "italic")
  )

## ---- compare-average-costs
data.table::setDT(data_complete_cases_subset)

data_cost <- data_complete_cases_subset[, .(reimbursement = mean(drg_yield), 
                                            actual_spend = mean(internal_cost)), 
                                        by = .(group_collapse)]

data_cost_long <- data.table::melt(data_cost, 
                                   id.vars = c("group_collapse"), 
                                   variable.name = "cost_type", 
                                   value.name = "cost")

ggplot(data_cost_long, 
       aes(x=group_collapse, y = cost, fill = cost_type)) + 
  geom_bar(stat="identity", position = "dodge") + 
  labs(title = paste0("Average medical costs"), 
       subtitle = "Barplot of costs by treatment group", 
       caption = "Data source: Aktionbündis Patientensicherheit", 
       fill = "Cost type") +
  xlab("Treatment group") + ylab("Costs (in €)") + 
  scale_fill_manual(values=c("#245F8A", "#5B8AAA", "#D7E3EC")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#E13530", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#205B87"),
        plot.caption = element_text(color = "#5B8AAA", face = "italic")
  )

