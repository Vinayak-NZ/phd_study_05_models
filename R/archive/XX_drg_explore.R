## ---- compare-complications-composition
data_complete_cases_subset <- data_complete_cases[,c("group_collapse", 
                                              "complications_indicator", 
                                              "drg", 
                                              "drg_yield", 
                                              "internal_cost")]

# by complications
group_drg <- prop.table(table(data_complete_cases_subset$group_collapse, 
                              data_complete_cases_subset$complications_indicator), 
                        margin = 1)*100

complications_count <- data_complete_cases_subset %>%
  count(group_collapse, complications_indicator) %>%
  group_by(group_collapse) %>%
  mutate(prop = n / sum(n))

ggplot(data = complications_count, 
       aes(group_collapse, 
           prop, 
           fill = complications_indicator)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Pregnancy outcomes",
       subtitle = "Barplot of outcome composition by treatment group", 
       caption = "Data source: Aktionbündis Patientensicherheit", 
       fill = "Outcome") +
  xlab("Treatment group") + 
  ylab("Proportion of outcomes") + 
  scale_fill_manual(values=c("#245F8A", "#5B8AAA", "#D7E3EC")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#E13530", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#205B87"),
        plot.caption = element_text(color = "#5B8AAA", face = "italic")
  )

## ---- compare-complications-cost
data.table::setDT(data_complete_cases)

data_cost <- data_complete_cases[, .(reimbursement = mean(drg_yield), 
                               actual_spend = mean(internal_cost)), 
                           by = .(group_collapse, complications_indicator)]

data_cost_long <- data.table::melt(data_cost, 
                                     id.vars = c("group_collapse", 
                                                 "complications_indicator"), 
                                     variable.name = "cost_type", 
                                     value.name = "cost")

ggplot(data_cost_long, 
                     aes(x=complications_indicator, y = cost, fill = cost_type)) + 
  geom_bar(stat="identity", position = "dodge") + 
  labs(title = "Average medical costs",
       subtitle = "Barplot of costs by treatment group and pregnancy outcomes", 
       caption = "Data source: Aktionbündis Patientensicherheit", 
       fill = "Cost type") +
  xlab("Pregnancy outcomes") + 
  ylab("Costs per patient (in €)") + 
  facet_grid(. ~ group_collapse) +
  scale_fill_manual(values=c("#245F8A", "#5B8AAA", "#D7E3EC")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#E13530", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#205B87"),
        plot.caption = element_text(color = "#5B8AAA", face = "italic")
  )

## ---- subset-average-costs-no-complications
data_complete_cases_subset <- data_complete_cases[, c("group_collapse", 
                                                      "complications_indicator",
                                                      "drg_yield", 
                                                      "internal_cost")]

data.table::setDT(data_complete_cases_subset)

data_complete_cases_subset <- 
  data_complete_cases_subset[data_complete_cases_subset$complications_indicator == "no-complications", ]

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
       subtitle = "Barplot of costs by treatment group for births without complications", 
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


## ---- subset-average-costs-complications
data_complete_cases_subset <- data_complete_cases[, c("group_collapse", 
                                                      "complications_indicator",
                                                      "drg_yield", 
                                                      "internal_cost")]

data.table::setDT(data_complete_cases_subset)

data_complete_cases_subset <- 
  data_complete_cases_subset[data_complete_cases_subset$complications_indicator == "complications", ]

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
       subtitle = "Barplot of costs by treatment group for births with complications", 
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

## ---- compare-drg
# drg classificaitons within each group (i.e. by group)
data_complete_cases_subset <- data_complete_cases[,c("group_collapse", 
                                                     "complications_indicator", 
                                                     "drg", 
                                                     "drg_yield", 
                                                     "internal_cost")]

group_drg <- prop.table(table(data_complete_cases_subset$group_collapse, 
                              data_complete_cases_subset$drg), 
                        margin = 1)*100

group_drg_count <- data_complete_cases_subset %>%
  count(group_collapse, drg) %>%
  group_by(group_collapse) %>%
  mutate(prop = n / sum(n))

nb.cols <- 14
mycolours <- colorRampPalette(brewer.pal(8, "PuBu"))(nb.cols)

ggplot(data = group_drg_count, aes(group_collapse, prop, fill = drg)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Pregnancy outcomes",
       subtitle = "Barplot of DRG composition by treatment group", 
       caption = "Data source: Aktionbündis Patientensicherheit", 
       fill = "DRG classification") +
  xlab("Treatment group") + 
  ylab("Proportion of woment with classificaiton") + 
  scale_fill_manual(values = mycolours) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#E13530", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#205B87"),
        plot.caption = element_text(color = "#5B8AAA", face = "italic")
  )

## ---- drg-analysis
drg_code_list <- c("O01A", "O01B", "O01C", "O01D", "O01E", "O01F", "O01G", "O02B", 
                   "O60A", "O60B", "O60C", "O60D", "O65A")

drg_plots <- lapply(drg_code_list, drg_analysis)