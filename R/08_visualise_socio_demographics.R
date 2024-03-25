# ---- demographics

# age
data_complete_cases_age_plot <- data_complete_cases

data_complete_cases_age_plot <- data_complete_cases_age_plot[data_complete_cases_age_plot$age_delivery > 18, ] 

data_complete_cases_age_plot <- data_complete_cases_age_plot[!is.na(data_complete_cases_age_plot$age_delivery), ]

group_collapse.labs <- c("Control", "Intervention")
names(group_collapse.labs) <- c("control", "intervention")

socio_demo_age <- 
  ggplot(data_complete_cases_age_plot, aes(x = age_delivery)) +
  geom_histogram(bins = 30, fill = "#4739a2") +
  labs(title = paste0("Characteristics of sample"), 
       subtitle = "Histogram of age distribution in sample",
       caption = "Data source: Obstetric Digital Health Intervention") +
  xlab("Age") + 
  ylab("Count") + 
  scale_x_continuous(limits = c(18, 50), oob = scales::oob_keep) +
  scale_y_continuous(limits = c(0, 20)) +
  facet_wrap(~ group_collapse, 
             labeller = labeller(group_collapse = group_collapse.labs)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )

ggsave("output/socio_demo_age.png", 
       plot = socio_demo_age)

# ---- health

# gestation-days

data_complete_cases_gest_plot <- data_complete_cases

data_complete_cases_gest_plot <- data_complete_cases_gest_plot[!is.na(data_complete_cases_gest_plot$gestation_days), ]

group_collapse.labs <- c("Control", "Intervention")
names(group_collapse.labs) <- c("control", "intervention")

health_gestation_days <- 
  ggplot(data_complete_cases_gest_plot, aes(x = gestation_days)) +
  geom_histogram(bins = 20, fill = "#4739a2") +
  labs(title = paste0("Characteristics of sample"), 
       subtitle = "Histogram of gestational age distribution",
       caption = "Data source: Obstetric Digital Health Intervention") +
  xlab("Age") + 
  ylab("Count") + 
  scale_x_continuous(limits = c(175, 300), oob = scales::oob_keep) +
  scale_y_continuous(limits = c(0, 30)) +
  facet_wrap(~ group_collapse, 
             labeller = labeller(group_collapse = group_collapse.labs)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )

ggsave("output/health_gestation_days.png", 
       plot = health_gestation_days)

# complications-drg

data_complete_cases_complications <- data_complete_cases

data_complete_cases_complications$complications_drg <- 
  ifelse(data_complete_cases_complications$complications_drg == "no-complications", "No", "Yes")

group_collapse.labs <- c("Control", "Intervention")
names(group_collapse.labs) <- c("control", "intervention")

complications_drg_plot <- 
  ggplot(data_complete_cases_complications) +
  geom_bar(aes(x=complications_drg), fill = "#4739a2") +
  labs(title = paste0("Characteristics of sample"), 
       subtitle = "Bar chart of complications based on diagnosis related groups (DRG)",
       caption = "Data source: Obstetric Digital Health Intervention") +
  xlab("DRGs with complications") + 
  ylab("Count") + 
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

ggsave("output/complications_drg_plot.png", 
       plot = complications_drg_plot)

# complications-maternal
data_complete_cases_complications <- data_complete_cases

data_complete_cases_complications$complications_maternal <- 
  ifelse(data_complete_cases_complications$complications_maternal == "no-complications", "No", "Yes")

group_collapse.labs <- c("Control", "Intervention")
names(group_collapse.labs) <- c("control", "intervention")

complications_maternal_plot <- 
  ggplot(data_complete_cases_complications) +
  geom_bar(aes(x=complications_maternal), fill = "#4739a2") +
  labs(title = paste0("Characteristics of sample"), 
       subtitle = "Bar chart of complications attributed to mother",
       caption = "Data source: Obstetric Digital Health Intervention") +
  xlab("Complications related to mother") + 
  ylab("Count") + 
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

ggsave("output/complications_maternal_plot.png", 
       plot = complications_maternal_plot)

# complications-newborn
data_complete_cases_complications <- data_complete_cases

data_complete_cases_complications$complications_fetal <- 
  ifelse(data_complete_cases_complications$complications_fetal == "no-complications", "No", "Yes")

group_collapse.labs <- c("Control", "Intervention")
names(group_collapse.labs) <- c("control", "intervention")

complications_fetus_plot <- 
  ggplot(data_complete_cases_complications) +
  geom_bar(aes(x=complications_fetal), fill = "#4739a2") +
  labs(title = paste0("Characteristics of sample"), 
       subtitle = "Bar chart of complications attributed to fetus",
       caption = "Data source: Obstetric Digital Health Intervention") +
  xlab("Complications related to fetus") + 
  ylab("Count") + 
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

ggsave("output/complications_fetus_plot.png", 
       plot = complications_fetus_plot)

# complications-delivery
data_complete_cases_complications <- data_complete_cases

data_complete_cases_complications$complications_delivery <- 
  ifelse(data_complete_cases_complications$complications_delivery == "no-complications", "No", "Yes")

group_collapse.labs <- c("Control", "Intervention")
names(group_collapse.labs) <- c("control", "intervention")

complications_delivery_plot <- 
  ggplot(data_complete_cases_complications) +
  geom_bar(aes(x=complications_delivery), fill = "#4739a2") +
  labs(title = paste0("Characteristics of sample"), 
       subtitle = "Bar chart of complications at delivery",
       caption = "Data source: Obstetric Digital Health Intervention") +
  xlab("Complications at delivery") + 
  ylab("Count") + 
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

ggsave("output/complications_delivery_plot.png", 
       plot = complications_delivery_plot)

# risk-profile
data_complete_cases_risk <- data_complete_cases

data_complete_cases_risk$pre_existing_risk_maternal <- 
  ifelse(data_complete_cases_risk$pre_existing_risk_maternal == "low", "Low", "High")

data_complete_cases_risk$pre_existing_risk_fetal <- 
  ifelse(data_complete_cases_risk$pre_existing_risk_fetal == "low", "Low", "High")

group_collapse.labs <- c("Control", "Intervention")
names(group_collapse.labs) <- c("control", "intervention")

risk_profile_plot <- 
  ggplot(data_complete_cases_risk) +
  geom_bar(aes(x=pre_existing_risk_maternal, fill=pre_existing_risk_fetal),
           position = "dodge") +
  scale_fill_manual(values = c("#46e7fd", "#e18b22")) +
  labs(title = paste0("Characteristics of sample"), 
       subtitle = "Bar chart of risk profile of pregnancy ",
       caption = "Data source: Obstetric Digital Health Intervention", 
       fill = "Risk profile of fetus") +
  xlab("Risk profile of mother") + 
  ylab("Count") + 
  facet_wrap(~group_collapse, 
             labeller = labeller(group_collapse = group_collapse.labs)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")) 

ggsave("output/risk_profile_plot.png", 
       plot = risk_profile_plot)
