## ---- age-plot

ggplot(data_complete_cases, aes(x = age_delivery)) +
  geom_histogram(binwidth = round(sd(data_complete_cases[, "age_delivery"], na.rm = TRUE), 0), 
                 colour = "#EAF3F9", fill = "#205B87") +
  facet_grid(~ group_collapse) +
  labs(title = paste0("Age of women"), 
       subtitle = "Histogram of age by treatment group",
       caption = "Data source: Aktionbündis Patientensicherheit") +
  xlab("Age") + 
  ylab("Frequency") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#E13530", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#205B87"),
        plot.caption = element_text(color = "#5B8AAA", face = "italic")
        )

## ---- pre-existing-conditions

ggplot(data_complete_cases, aes(x = first_birth)) +
  geom_bar(aes(y = ..prop.., group = group_collapse), stat = "count", width=0.7, colour = "#EAF3F9", fill = "#205B87") +
  facet_grid(~ group_collapse) + 
  labs(title = paste0("Pre-existing conditions"), 
       subtitle = "Barplot of first births by treatment group", 
       caption = "Data source: Aktionbündis Patientensicherheit") +
  scale_y_continuous(labels=scales::percent) +
  xlab("First birth") + 
  ylab("Percentages") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#E13530", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#205B87"),
        plot.caption = element_text(color = "#5B8AAA", face = "italic")
  )

ggplot(data_complete_cases, aes(x = time_birth)) +
  geom_bar(aes(y = ..prop.., group = group_collapse), stat = "count", width=0.7, colour = "#EAF3F9", fill = "#205B87") +
  facet_grid(~ group_collapse) + 
  labs(title = paste0("Pre-existing conditions"), 
       subtitle = "Barplot of after hours delivery by treatment group", 
       caption = "Data source: Aktionbündis Patientensicherheit") +
  scale_y_continuous(labels=scales::percent) +
  xlab("After hour delivery") + 
  ylab("Percentages") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#E13530", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#205B87"),
        plot.caption = element_text(color = "#5B8AAA", face = "italic")
  )

ggplot(data_complete_cases, aes(x = maternal_risk_other)) +
  geom_bar(aes(y = ..prop.., group = group_collapse), stat = "count", width=0.7, colour = "#EAF3F9", fill = "#205B87") +
  facet_grid(~ group_collapse) + 
  labs(title = paste0("Pre-existing conditions"), 
       subtitle = "Barplot of maternal risk (not further classified) by treatment group", 
       caption = "Data source: Aktionbündis Patientensicherheit") +
  scale_y_continuous(labels=scales::percent) +
  xlab("Presence of maternal risk") + 
  ylab("Percentages") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#E13530", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#205B87"),
        plot.caption = element_text(color = "#5B8AAA", face = "italic")
  )

ggplot(data_complete_cases, aes(x = fetal_risk)) +
  geom_bar(aes(y = ..prop.., group = group_collapse), stat = "count", width=0.7, colour = "#EAF3F9", fill = "#205B87") +
  facet_grid(~ group_collapse) + 
  labs(title = paste0("Pre-existing conditions"), 
       subtitle = "Barplot of fetal risk by treatment group", 
       caption = "Data source: Aktionbündis Patientensicherheit") +
  scale_y_continuous(labels=scales::percent) +
  xlab("Presence of fetal risk") + 
  ylab("Percentages") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#E13530", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#205B87"),
        plot.caption = element_text(color = "#5B8AAA", face = "italic")
  )
